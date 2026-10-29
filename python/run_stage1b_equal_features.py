"""Compare eight survival models using identical input features on identical CV folds.

This is a *separate benchmark*, NOT the primary Stage 1 CoxEM feature-ranking run.
All eight algorithms get the same training-fold predictors; technical zero-
variance features alone are dropped for every model. Stage 2A Top-N/1-SE and
CoxEM's own correlation screening / 300-macro-feature limit are both OFF.
CoxEM is freshly fitted with fixed_feature_set=True and its existing ensemble
sampling/OOB weighting. The primary CoxEM Stage 1 files are never read/changed.
"""

import argparse
import json
import time
from pathlib import Path

import numpy as np
import pandas as pd

from sksurv.datasets import get_x_y
from sksurv.ensemble import ExtraSurvivalTrees, RandomSurvivalForest
from sksurv.linear_model import (
    CoxnetSurvivalAnalysis,
    CoxPHSurvivalAnalysis,
    IPCRidge,
)
from sksurv.metrics import cumulative_dynamic_auc, concordance_index_censored
from sksurv.tree import ExtraSurvivalTree, SurvivalTree

from config import (
    CoxEM_n_estimator,
    dat_dir,
    feat_name_dict,
    fold_type,
    log_dir,
    make_dir,
    n_process,
    random_state,
    res_dir,
)
from utils import generate_safe_eval_times, get_logger, load_processed_fold


C_TYPE = "comorbidity"
TARGET = "all"
RESULT_FOLDER = "Stage 1B/equal_feature_benchmark"
LABEL_COLUMNS = ["status", "time"]

BASELINE_MODELS = (
    "extra_survival_trees",
    "random_survival_forest",
    "coxnet",
    "coxph",
    "ipc_ridge",
    "survival_tree",
    "extra_survival_tree",
    "coxem",
)


def build_model(model_name, seed):
    """Construct one baseline estimator with the prespecified settings."""
    if model_name == "extra_survival_trees":
        return ExtraSurvivalTrees(
            n_estimators=CoxEM_n_estimator,
            random_state=seed,
            n_jobs=1,
        )
    if model_name == "random_survival_forest":
        return RandomSurvivalForest(
            n_estimators=CoxEM_n_estimator,
            random_state=seed,
            n_jobs=1,
        )
    if model_name == "coxnet":
        return CoxnetSurvivalAnalysis(fit_baseline_model=True)
    if model_name == "coxph":
        # L2 regularization stabilizes fitting with correlated encoded columns.
        return CoxPHSurvivalAnalysis(alpha=0.01)
    if model_name == "ipc_ridge":
        return IPCRidge(random_state=seed)
    if model_name == "survival_tree":
        return SurvivalTree(random_state=seed)
    if model_name == "extra_survival_tree":
        return ExtraSurvivalTree(random_state=seed)
    raise ValueError(f"Unknown Stage 1B model: {model_name}")


def build_benchmark_coxem(view, fold):
    """Build a separate CoxEM with NO internal feature selection.

    CoxEM.py serializes each base learner to the module-level ``mod_dir``.
    Redirect only this process's CoxEM module variable to an isolated folder
    so benchmark fitting can never overwrite primary Stage 1 learner files.
    Each outer fold uses distinct model filenames and processes.
    """
    import CoxEM as coxem_module

    bench_root = Path(result_path(view, "coxem"))
    # CoxEM.py writes to module-level folders; isolate all such locations.
    isolated_dirs = {
        "mod_dir": bench_root / "learner_models",
        "tmp_dir": bench_root / "internal_tmp",
        "ana_dir": bench_root / "internal_analysis",
        "log_dir": bench_root / "internal_logs",
    }
    for attr, folder in isolated_dirs.items():
        folder.mkdir(parents=True, exist_ok=True)
        setattr(coxem_module, attr, str(folder))

    from CoxEM import CoxEM

    return CoxEM(
        c_type=C_TYPE,
        target=TARGET,
        task_id=fold,
        n_estimators=CoxEM_n_estimator,
        random_state=random_state,
        n_jobs=1,
        feature_map=None,
        use_valid=True,
        valid_metric="auc",
        verbose=0,
        fixed_feature_set=True,
        compute_train_fitness=False,
        compute_eval_perf=False,
    )


def model_parameters(model, model_name):
    """Record model specifications without mixing benchmark / primary models."""
    if model_name == "coxem":
        return {
            "n_estimators": model.n_estimators,
            "n_jobs": model.n_jobs,
            "fixed_feature_set": model.fixed_feature_set,
            "sampling_and_oob_weighting": "unchanged CoxEM implementation",
            "internal_feature_selection": "disabled for equal-feature benchmark",
            "reused_saved_stage1_model": False,
        }
    return serializable_parameters(model)


def result_path(view, model_name):
    """Return the result directory for one view and baseline model."""
    return Path(
        make_dir(
            f"{res_dir}/{RESULT_FOLDER}/{C_TYPE}_{TARGET}/{view}/{model_name}"
        )
    )


def load_fold(view, task_id):
    """Load one existing Stage 1 development-set cross-validation fold."""
    fold_dir = Path(dat_dir) / f"{C_TYPE}_{TARGET}_{view}"
    return load_processed_fold(fold_dir, fold_type, task_id)


def prepare_survival_data(data):
    """Separate identifiers, predictors and structured survival outcomes."""
    model_data = data.drop(columns=["eid"], errors="ignore")
    x, y = get_x_y(model_data, LABEL_COLUMNS, 1, True)
    return x, y


def shared_benchmark_features(x_train, x_valid):
    """Same outcome-blind, training-fold-only technical filter for all 8 models.

    No coefficient-based ranking, correlation redundancy filter, max-300 cap,
    or Stage 2A Top-N/1-SE is performed in this benchmark.
    """
    if list(x_train.columns) != list(x_valid.columns):
        raise ValueError("Training/validation predictor columns are not aligned")
    if not all(pd.api.types.is_numeric_dtype(dtype) for dtype in x_train.dtypes):
        raise TypeError("Benchmark predictors must be numeric after preprocessing")
    if not np.isfinite(x_train.to_numpy(dtype=float)).all():
        raise ValueError("Non-finite values in preprocessed training data")
    if not np.isfinite(x_valid.to_numpy(dtype=float)).all():
        raise ValueError("Non-finite values in preprocessed validation data")
    retained = x_train.columns[x_train.nunique(dropna=False).to_numpy() > 1].tolist()
    if not retained:
        raise ValueError("No variable predictor columns in this training fold")
    return x_train.loc[:, retained].copy(), x_valid.loc[:, retained].copy(), retained


def predict_risk(model_name, model, x):
    """Return scores oriented so that larger values consistently mean higher risk."""
    prediction = np.asarray(model.predict(x), dtype=float)
    if model_name == "ipc_ridge":
        return -prediction
    return prediction


def serializable_parameters(model):
    """Convert estimator parameters into values that can be written as JSON."""
    parameters = {}
    for name, value in model.get_params(deep=False).items():
        if isinstance(value, np.generic):
            value = value.item()
        if value is None or isinstance(value, (bool, int, float, str)):
            parameters[name] = value
        else:
            parameters[name] = repr(value)
    return parameters


def run_fold(view, model_name, task_id):
    """Fit and evaluate one baseline model on one development-set fold."""
    if view not in feat_name_dict:
        raise ValueError(f"Unknown view: {view}")
    if model_name not in BASELINE_MODELS:
        raise ValueError(f"Unknown Stage 1B model: {model_name}")
    if task_id < 0 or task_id >= n_process:
        raise ValueError(f"task_id must be between 0 and {n_process - 1}")

    output_dir = result_path(view, model_name)
    logger = get_logger(
        f"stage1b_{view}_{model_name}_{fold_type}_{task_id}",
        log_dir,
    )

    fold_start = time.time()
    train_data, valid_data = load_fold(view, task_id)
    x_train, y_train = prepare_survival_data(train_data)
    x_valid, y_valid = prepare_survival_data(valid_data)
    x_train, x_valid, retained_features = shared_benchmark_features(x_train, x_valid)
    seed = random_state
    model = (
        build_benchmark_coxem(view, task_id)
        if model_name == "coxem" else build_model(model_name, seed)
    )
    logger.info(
        "Equal-feature benchmark: view=%s, model=%s, fold=%s, "
        "train=%s, validation=%s, input_features=%s, retained_features=%s, "
        "CoxEM internal selection disabled",
        view, model_name, task_id, len(train_data), len(valid_data),
        len(train_data.columns) - 3, len(retained_features),
    )
    fit_start = time.time()
    # No outer-fold validation records/outcomes enter model fitting or CoxEM OOB weights.
    model.fit(x_train, y_train)
    fit_runtime_minutes = (time.time() - fit_start) / 60.0

    evaluation_start = time.time()
    risk_valid = predict_risk(model_name, model, x_valid)
    eval_times = generate_safe_eval_times(
        train_data["time"].to_numpy(dtype=float),
        valid_data["time"].to_numpy(dtype=float),
        365,
    )
    auc_values, _ = cumulative_dynamic_auc(
        y_train,
        y_valid,
        risk_valid,
        eval_times,
    )
    _, mean_auc = cumulative_dynamic_auc(
        y_train,
        y_valid,
        risk_valid,
        eval_times[2:],
    )

    event_name, time_name = y_valid.dtype.names
    c_index = concordance_index_censored(
        y_valid[event_name],
        y_valid[time_name],
        risk_valid,
    )[0]
    evaluation_runtime_minutes = (time.time() - evaluation_start) / 60.0
    elapsed_through_evaluation_minutes = (time.time() - fold_start) / 60.0

    auc_curve = pd.DataFrame(
        {
            "fold": task_id,
            "evaluation_index": np.arange(len(eval_times)),
            "time_days": eval_times,
            "time_years": eval_times / 365.0,
            "AUC": auc_values,
            "included_in_mean_auc": np.arange(len(eval_times)) >= 2,
        }
    )
    auc_curve.to_csv(
        output_dir / f"auc_curve_{fold_type}_{task_id}.csv",
        index=False,
    )

    prediction_data = valid_data[
        [column for column in ["eid", "status", "time"] if column in valid_data]
    ].copy()
    prediction_data["risk_score"] = risk_valid
    prediction_data.to_csv(
        output_dir / f"validation_predictions_{fold_type}_{task_id}.csv",
        index=False,
    )

    pd.DataFrame(
        {
            "feature": retained_features,
            "feature_index": np.arange(x_train.shape[1]),
        }
    ).to_csv(
        output_dir / f"features_{fold_type}_{task_id}.csv",
        index=False,
    )

    metrics = pd.DataFrame(
        [
            {
                "c_type": C_TYPE,
                "target": TARGET,
                "view": view,
                "model": model_name,
                "fold_type": fold_type,
                "fold": task_id,
                "seed": seed,
                "n_train": len(train_data),
                "n_valid": len(valid_data),
                "n_train_events": int(y_train[y_train.dtype.names[0]].sum()),
                "n_valid_events": int(y_valid[y_valid.dtype.names[0]].sum()),
                "n_features": x_train.shape[1],
                "feature_policy": "all_preprocessed_nonconstant_training_fold_predictors",
                "coxem_fixed_feature_set": model_name == "coxem",
                "corr_0p9_max300_filter_applied": False,
                "n_auc_times": len(eval_times),
                "n_mean_auc_times": len(eval_times[2:]),
                "mean_auc_from_third_point": float(mean_auc),
                "c_index": float(c_index),
                "fit_runtime_minutes": fit_runtime_minutes,
                "evaluation_runtime_minutes": evaluation_runtime_minutes,
                "elapsed_through_evaluation_minutes": (
                    elapsed_through_evaluation_minutes
                ),
                "risk_score_multiplier": -1 if model_name == "ipc_ridge" else 1,
                "reused_stage1_coxem_model": False,
                "stage1_coxem_source": "",
            }
        ]
    )
    metrics.to_csv(
        output_dir / f"metrics_{fold_type}_{task_id}.csv",
        index=False,
    )

    manifest = {
        "c_type": C_TYPE,
        "target": TARGET,
        "view": view,
        "model": model_name,
        "model_class": f"{type(model).__module__}.{type(model).__name__}",
        "fold_type": fold_type,
        "fold": task_id,
        "seed": seed,
        "ensemble_n_estimators": (
            CoxEM_n_estimator
            if model_name in {"extra_survival_trees", "random_survival_forest"}
            else None
        ),
        "prediction_orientation": (
            "negative predicted survival time; larger values indicate higher risk"
            if model_name == "ipc_ridge"
            else "native risk score; larger values indicate higher risk"
        ),
        "model_parameters": model_parameters(model, model_name),
        "reused_stage1_model": False,
        "feature_policy": "identical training-fold predictors; remove constants only",
        "stage2a_selection": False,
        "coxem_internal_feature_selection": False,
        "correlation_filter": False,
        "macro_feature_cap": None,
    }
    with open(
        output_dir / f"manifest_{fold_type}_{task_id}.json",
        "w",
        encoding="utf-8",
    ) as file:
        json.dump(manifest, file, indent=2, ensure_ascii=False)

    logger.info(
        f"Completed Stage 1B: mean AUC={mean_auc:.6f}, "
        f"C-index={c_index:.6f}, fit={fit_runtime_minutes:.3f} minutes, "
        f"evaluation={evaluation_runtime_minutes:.3f} minutes."
    )


def summarize_model_view(view, model_name):
    """Combine the ten fold-level outputs for one model and predictor view."""
    output_dir = result_path(view, model_name)
    metric_files = [
        output_dir / f"metrics_{fold_type}_{task_id}.csv"
        for task_id in range(n_process)
    ]
    auc_files = [
        output_dir / f"auc_curve_{fold_type}_{task_id}.csv"
        for task_id in range(n_process)
    ]
    missing_files = [
        str(path)
        for path in metric_files + auc_files
        if not path.exists()
    ]
    if missing_files:
        raise FileNotFoundError(
            "Stage 1B cannot be summarized because outputs are missing:\n"
            + "\n".join(missing_files)
        )

    fold_metrics = pd.concat(
        [pd.read_csv(path) for path in metric_files],
        ignore_index=True,
    ).sort_values("fold")
    all_auc_curves = pd.concat(
        [pd.read_csv(path) for path in auc_files],
        ignore_index=True,
    ).sort_values(["fold", "evaluation_index"])

    fold_metrics.to_csv(output_dir / "fold_metrics.csv", index=False)
    all_auc_curves.to_csv(output_dir / "auc_curves_all_folds.csv", index=False)

    mean_auc_values = fold_metrics["mean_auc_from_third_point"]
    c_index_values = fold_metrics["c_index"]
    summary = pd.DataFrame(
        [
            {
                "c_type": C_TYPE,
                "target": TARGET,
                "view": view,
                "model": model_name,
                "n_folds": len(fold_metrics),
                "mean_auc_mean": mean_auc_values.mean(),
                "mean_auc_sd": mean_auc_values.std(ddof=1),
                "mean_auc_se": mean_auc_values.std(ddof=1)
                / np.sqrt(len(mean_auc_values)),
                "c_index_mean": c_index_values.mean(),
                "c_index_sd": c_index_values.std(ddof=1),
                "fit_runtime_minutes_mean": fold_metrics[
                    "fit_runtime_minutes"
                ].mean(),
                "fit_runtime_minutes_total": fold_metrics[
                    "fit_runtime_minutes"
                ].sum(),
                "evaluation_runtime_minutes_mean": fold_metrics[
                    "evaluation_runtime_minutes"
                ].mean(),
                "elapsed_through_evaluation_minutes_mean": fold_metrics[
                    "elapsed_through_evaluation_minutes"
                ].mean(),
            }
        ]
    )
    summary.to_csv(output_dir / "summary.csv", index=False)
    return summary


def summarize_view(view):
    """Aggregate all model summaries available for a single predictor view."""
    summary_files = [
        (name, result_path(view, name) / "summary.csv")
        for name in BASELINE_MODELS
    ]
    missing = [name for name, filename in summary_files if not filename.is_file()]
    if missing:
        raise FileNotFoundError(
            f"Cannot make a full {view} comparison; missing model summaries: {missing}"
        )
    combined = pd.concat(
        [pd.read_csv(filename) for _, filename in summary_files],
        ignore_index=True,
    )
    output = Path(make_dir(f"{res_dir}/{RESULT_FOLDER}")) / f"baseline_summary_{view}.csv"
    combined.to_csv(output, index=False)
    return output


def summarize_all():
    """Combine available per-view comparisons, requiring every configured view."""
    summaries = []
    for view in feat_name_dict:
        view_summary = summarize_view(view)
        summaries.append(pd.read_csv(view_summary))
    combined = pd.concat(summaries, ignore_index=True)
    out_dir = Path(make_dir(f"{res_dir}/{RESULT_FOLDER}"))
    combined.to_csv(out_dir / "baseline_summary_with_coxem.csv", index=False)
    combined.to_csv(out_dir / "baseline_summary.csv", index=False)
    return out_dir / "baseline_summary_with_coxem.csv"


def parse_args():
    """Parse fold execution and aggregation options."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--view", choices=list(feat_name_dict))
    parser.add_argument("--model", choices=BASELINE_MODELS)
    parser.add_argument("--task-id", type=int)
    parser.add_argument("--aggregate", action="store_true")
    parser.add_argument("--aggregate-all", action="store_true")
    parser.add_argument("--aggregate-view", action="store_true")
    return parser.parse_args()


def main():
    """Dispatch one fold, one model-view aggregation or the final aggregation."""
    args = parse_args()
    if args.aggregate_all:
        summarize_all()
        return
    if args.aggregate_view:
        if args.view is None:
            raise ValueError("--view required for --aggregate-view")
        summarize_view(args.view)
        return
    if args.view is None or args.model is None:
        raise ValueError("--view and --model are required unless aggregating views.")
    if args.aggregate:
        summarize_model_view(args.view, args.model)
        return
    if args.task_id is None:
        raise ValueError("--task-id is required for fold-level execution.")
    run_fold(args.view, args.model, args.task_id)


if __name__ == "__main__":
    main()

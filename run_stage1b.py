"""Benchmark non-CoxEM survival models on the Stage 1 cross-validation folds."""

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
RESULT_FOLDER = "Stage 1B"
LABEL_COLUMNS = ["status", "time"]

BASELINE_MODELS = (
    "extra_survival_trees",
    "random_survival_forest",
    "coxnet",
    "coxph",
    "ipc_ridge",
    "survival_tree",
    "extra_survival_tree",
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
    seed = random_state
    model = build_model(model_name, seed)

    logger.info(
        f"Fit Stage 1B: view={view}, model={model_name}, fold={task_id}, "
        f"train={len(train_data)}, valid={len(valid_data)}, "
        f"features={x_train.shape[1]}."
    )
    fit_start = time.time()
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
            "feature": list(x_train.columns),
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
        "model_parameters": serializable_parameters(model),
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


def summarize_all():
    """Create one comparison table across all Stage 1B models and views."""
    summaries = []
    for view in feat_name_dict:
        for model_name in BASELINE_MODELS:
            summary_file = result_path(view, model_name) / "summary.csv"
            if not summary_file.exists():
                raise FileNotFoundError(f"Missing Stage 1B summary: {summary_file}")
            summaries.append(pd.read_csv(summary_file))
    combined = pd.concat(summaries, ignore_index=True)
    combined.to_csv(
        Path(make_dir(f"{res_dir}/{RESULT_FOLDER}")) / "baseline_summary.csv",
        index=False,
    )


def parse_args():
    """Parse fold execution and aggregation options."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--view", choices=list(feat_name_dict))
    parser.add_argument("--model", choices=BASELINE_MODELS)
    parser.add_argument("--task-id", type=int)
    parser.add_argument("--aggregate", action="store_true")
    parser.add_argument("--aggregate-all", action="store_true")
    return parser.parse_args()


def main():
    """Dispatch one fold, one model-view aggregation or the final aggregation."""
    args = parse_args()
    if args.aggregate_all:
        summarize_all()
        return
    if args.view is None or args.model is None:
        raise ValueError("--view and --model are required unless --aggregate-all is used.")
    if args.aggregate:
        summarize_model_view(args.view, args.model)
        return
    if args.task_id is None:
        raise ValueError("--task-id is required for fold-level execution.")
    run_fold(args.view, args.model, args.task_id)


if __name__ == "__main__":
    main()

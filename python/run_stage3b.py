#!/usr/bin/env python3
"""Compare predefined multimodal CoxEM combinations on the held-out cohort.

Keep the original full-window mean AUC and every previous output unchanged.
Additionally export the 3–10-year weighted AUC to a SEPARATE CSV.
All input/output paths are inherited from config.py.
"""


import argparse
import gc
import inspect
import os
import sys
import time

import numpy as np
import pandas as pd

from sksurv.datasets import get_x_y
from sksurv.metrics import cumulative_dynamic_auc, concordance_index_censored
from sksurv.nonparametric import SurvivalFunctionEstimator

# Selection of Result vs Result_2 is exclusively delegated to config.py.
# CLI switch is processed before config import; existing SLURM environment
# selection (UKB_USE_ENVIRO_2) remains supported without path overrides.
if "--enviro-2" in sys.argv:
    os.environ["UKB_USE_ENVIRO_2"] = "1"

from config import (
    CoxEM_n_estimator, comorbidity_targets, comorbidity_types, log_dir,
    main_verbose, make_dir, random_state, res_dir,
    stage3_coxem_n_jobs, stage3_stage2_result_folder,
    use_valid, valid_metric,
)
from CoxEM import CoxEM
from utils import (
    get_logger,
    generate_safe_eval_times,
    read_feature_lineage,
    active_feature_lineage,
    summarize_feature_lineage,
)


# =========================
# Stage 3B settings
# =========================

STAGE2_RESULT_FOLDER = stage3_stage2_result_folder
STAGE3B_RESULT_FOLDER = "Stage 3B"
STAGE3B_COXEM_N_JOBS = int(stage3_coxem_n_jobs)

STAGE3B_HORIZONS_DAYS = {
    "3y": 365 * 3,
    "5y": 365 * 5,
    "10y": 365 * 10,
    "15y": 365 * 15,
}

# Only the ADDITIONAL (3, 10] year summary uses the settings below.
# The original mean_time_dependent_auc remains byte-for-byte identical in logic.
MEAN_AUC_START_YEARS = 3
MEAN_AUC_END_YEARS = 10
DAYS_PER_YEAR = 365

SAVE_HELDOUT_RISK_SCORES = True
SAVE_FITTED_MODELS = False

VIEW_MAP = {
    "E": "enviro",
    "B": "biomarkers",
    "M": "metabolomics",
    "G": "gene",
}

PRIMARY_COMBINATIONS = [
    ("E",),
    ("E", "B"),
    ("E", "M"),
    ("E", "G"),
    ("E", "B", "M"),
    ("E", "B", "G"),
    ("E", "M", "G"),
    ("E", "B", "M", "G"),
]

SINGLE_OMIC_BASELINES = [
    ("B",),
    ("M",),
    ("G",),
]

ALL_COMBINATIONS = SINGLE_OMIC_BASELINES + PRIMARY_COMBINATIONS

ID_COL = "eid"
LABEL_COLS = ["status", "time"]


def combo_name(combo):
    """Return the display label for a predefined multimodal feature combination."""
    return "+".join(combo)


def combo_slug(combo):
    """Return a filesystem-safe identifier for a feature combination."""
    return "_".join(combo)


def outcome_tasks():
    """Enumerate configured comorbidity and population tasks."""
    return [
        (c_type, target)
        for c_type in comorbidity_types
        for target in comorbidity_targets[c_type]
    ]


def stage2_folder(c_type, target, view):
    """Return one outcome-view Stage 2 result directory."""
    return os.path.join(
        res_dir,
        STAGE2_RESULT_FOLDER,
        f"{c_type}_{target}_{view}",
    )


def read_stage2_view(c_type, target, view):
    """Load one view's Stage 2 train, test, feature, and lineage outputs."""
    folder = stage2_folder(c_type, target, view)

    train_file = os.path.join(folder, "train_data.csv")
    test_file = os.path.join(folder, "test_data.csv")
    feature_file = os.path.join(folder, "feature_list.csv")

    for f in (train_file, test_file, feature_file):
        if not os.path.exists(f):
            raise FileNotFoundError(f)

    train = pd.read_csv(train_file)
    test = pd.read_csv(test_file)
    feature_df = pd.read_csv(feature_file)

    if "Feature" not in feature_df.columns:
        raise ValueError(f"{feature_file}: missing Feature column")

    features = feature_df["Feature"].astype(str).tolist()

    for label, df in (("train", train), ("heldout", test)):
        required = [ID_COL] + LABEL_COLS
        missing = [x for x in required if x not in df.columns]
        if missing:
            raise ValueError(
                f"{c_type}/{target}/{view}/{label}: missing {missing}"
            )
        if df[ID_COL].duplicated().any():
            raise ValueError(
                f"{c_type}/{target}/{view}/{label}: duplicated eid"
            )

    missing_train = [f for f in features if f not in train.columns]
    missing_test = [f for f in features if f not in test.columns]

    if missing_train or missing_test:
        raise ValueError(
            f"{c_type}/{target}/{view}: feature_list mismatch; "
            f"missing_train={missing_train[:10]}, "
            f"missing_test={missing_test[:10]}"
        )

    train = train.set_index(ID_COL, drop=False).sort_index()
    test = test.set_index(ID_COL, drop=False).sort_index()

    lineage = active_feature_lineage(
        read_feature_lineage(
            os.path.join(folder, "feature_selection_trace.csv")
        ),
        features,
        view=view,
    )

    return train, test, features, lineage


def series_equal(a, b):
    """Compare two aligned series while treating paired missing values as equal."""
    if len(a) != len(b):
        return False

    a_num = pd.to_numeric(a, errors="coerce")
    b_num = pd.to_numeric(b, errors="coerce")

    if a_num.notna().sum() == len(a) and b_num.notna().sum() == len(b):
        return np.allclose(
            a_num.to_numpy(dtype=float),
            b_num.to_numpy(dtype=float),
            equal_nan=True,
        )

    return (
        a.fillna("__NA__").astype(str).to_numpy()
        ==
        b.fillna("__NA__").astype(str).to_numpy()
    ).all()


def load_outcome_views(c_type, target, logger):
    """Load every available Stage 2 view for one outcome."""
    view_data = {}

    ref_train_index = None
    ref_test_index = None
    ref_train_labels = None
    ref_test_labels = None

    for code, view in VIEW_MAP.items():
        logger.info(f"Read Stage 2: {c_type}/{target}/{view}")

        train, test, features, lineage = read_stage2_view(
            c_type,
            target,
            view,
        )

        if ref_train_index is None:
            ref_train_index = train.index.copy()
            ref_test_index = test.index.copy()
            ref_train_labels = train[
                [ID_COL] + LABEL_COLS
            ].copy()
            ref_test_labels = test[
                [ID_COL] + LABEL_COLS
            ].copy()
        else:
            if not ref_train_index.equals(train.index):
                raise ValueError(
                    f"{c_type}/{target}/{view}: development eid mismatch"
                )
            if not ref_test_index.equals(test.index):
                raise ValueError(
                    f"{c_type}/{target}/{view}: held-out eid mismatch"
                )

            for col in LABEL_COLS:
                if not series_equal(ref_train_labels[col], train[col]):
                    raise ValueError(
                        f"{c_type}/{target}/{view}: train {col} mismatch"
                    )
                if not series_equal(ref_test_labels[col], test[col]):
                    raise ValueError(
                        f"{c_type}/{target}/{view}: held-out {col} mismatch"
                    )

        view_data[code] = {
            "view": view,
            "train": train,
            "test": test,
            "features": features,
            "lineage": lineage,
        }

    return view_data, ref_train_labels, ref_test_labels


def build_combo_data(
    combo,
    view_data,
    ref_train_labels,
    ref_test_labels,
):
    """Construct aligned train and test matrices for one view combination."""
    train_features = {}
    test_features = {}
    provenance = {}
    lineage_frames = []

    for code in combo:
        dat = view_data[code]
        lineage_frames.append(dat["lineage"])

        for feature in dat["features"]:
            if feature not in train_features:
                train_features[feature] = dat["train"][feature].copy()
                test_features[feature] = dat["test"][feature].copy()
                provenance[feature] = [code]
            else:
                if not series_equal(
                    train_features[feature],
                    dat["train"][feature],
                ):
                    raise ValueError(
                        f"Duplicate TRAIN feature differs: {feature}"
                    )

                if not series_equal(
                    test_features[feature],
                    dat["test"][feature],
                ):
                    raise ValueError(
                        f"Duplicate HELDOUT feature differs: {feature}"
                    )

                provenance[feature].append(code)

    train_feature_df = pd.DataFrame(
        train_features,
        index=ref_train_labels.index,
    )
    test_feature_df = pd.DataFrame(
        test_features,
        index=ref_test_labels.index,
    )

    train = pd.concat(
        [ref_train_labels, train_feature_df],
        axis=1,
    )
    test = pd.concat(
        [ref_test_labels, test_feature_df],
        axis=1,
    )

    usable = []
    excluded = []

    for feature in train_features.keys():
        tr = pd.to_numeric(train[feature], errors="coerce")
        te = pd.to_numeric(test[feature], errors="coerce")

        reason = None

        if tr.isna().any() or te.isna().any():
            reason = "missing_or_non_numeric"
        elif not np.isfinite(tr.to_numpy(dtype=float)).all():
            reason = "non_finite_development"
        elif not np.isfinite(te.to_numpy(dtype=float)).all():
            reason = "non_finite_heldout"
        elif tr.nunique(dropna=True) <= 1:
            reason = "constant_in_development"

        if reason is None:
            usable.append(feature)
        else:
            excluded.append({
                "Feature": feature,
                "Reason": reason,
                "Views": ";".join(provenance[feature]),
            })

    if not usable:
        raise ValueError(
            f"No usable features for {combo_name(combo)}"
        )

    provenance_df = pd.DataFrame({
        "Feature": usable,
        "Views": [
            ";".join(provenance[f])
            for f in usable
        ],
        "n_views": [
            len(provenance[f])
            for f in usable
        ],
    })
    provenance_df = provenance_df.merge(
        summarize_feature_lineage(lineage_frames, usable),
        on="Feature",
        how="left",
    )

    excluded_df = pd.DataFrame(
        excluded,
        columns=["Feature", "Reason", "Views"],
    )
    excluded_df = excluded_df.merge(
        summarize_feature_lineage(lineage_frames, excluded_df["Feature"]),
        on="Feature",
        how="left",
    )

    train_out = train[
        [ID_COL] + LABEL_COLS + usable
    ].reset_index(drop=True)

    test_out = test[
        [ID_COL] + LABEL_COLS + usable
    ].reset_index(drop=True)

    return (
        train_out,
        test_out,
        usable,
        provenance_df,
        excluded_df,
    )


def make_fixed_coxem(
    c_type,
    target,
    combo,
    features,
):
    """Create a CoxEM model that keeps the Stage 2 feature set fixed."""
    parameters = inspect.signature(
        CoxEM.__init__
    ).parameters

    if "fixed_feature_set" not in parameters:
        raise RuntimeError(
            "Current CoxEM.py has no fixed_feature_set. "
            "Use the same CoxEM.py used for the completed Stage 3."
        )

    kwargs = dict(
        c_type=c_type,
        target=target,
        task_id=(
            f"stage3b_{c_type}_{target}_{combo_slug(combo)}"
        ),
        n_estimators=CoxEM_n_estimator,
        random_state=random_state,
        n_jobs=STAGE3B_COXEM_N_JOBS,
        feature_map={
            f: [f]
            for f in features
        },
        use_valid=use_valid,
        valid_metric=valid_metric,
        verbose=main_verbose,
        fixed_feature_set=True,
    )

    if "compute_train_fitness" in parameters:
        kwargs["compute_train_fitness"] = False

    if "compute_eval_perf" in parameters:
        kwargs["compute_eval_perf"] = False

    return CoxEM(**kwargs)


def horizon_auc(
    y_train,
    y_test,
    risk_test,
    horizon,
):
    """Calculate time-dependent AUC at one prediction horizon."""
    try:
        _, auc = cumulative_dynamic_auc(
            y_train,
            y_test,
            risk_test,
            np.asarray(
                [float(horizon)],
                dtype=float,
            ),
        )
        return float(auc)
    except Exception:
        return np.nan



def three_to_ten_year_mean_auc(y_train, y_test, risk_test):
    """KM-weighted annual-grid AUC mean strictly over (3, 10] years.

    The scikit-survival builtin mean may include survival mass before the
    first requested time point. Therefore calculate the window-specific
    ratio explicitly using the test-cohort Kaplan-Meier event-survival:

      sum_{k=4..10} AUC(k) [S(k-1)-S(k)] / [S(3)-S(10)].

    This is a *right-endpoint, annual-grid approximation* to the
    survival-weighted integral over 3–10 years, not a plain AUC average.
    It does not alter model fitting, predictions, C-index, or horizon AUCs.
    """
    times = np.arange(
        MEAN_AUC_START_YEARS, MEAN_AUC_END_YEARS + 1, dtype=float
    ) * DAYS_PER_YEAR
    train_time_field = y_train.dtype.names[1]
    event_field, time_field = y_test.dtype.names
    train_times = np.asarray(y_train[train_time_field], dtype=float)
    test_times = np.asarray(y_test[time_field], dtype=float)
    risk = np.asarray(risk_test, dtype=float)

    if len(risk) != len(y_test) or not np.isfinite(risk).all():
        raise ValueError("Invalid held-out risk scores for 3–10y AUC")
    if not (np.min(test_times) < times[0] and
            times[-1] < min(np.max(train_times), np.max(test_times))):
        raise ValueError(
            "3–10y evaluation window exceeds the train/test follow-up "
            "support; check the Stage 2 input and time units."
        )

    auc_values, _ = cumulative_dynamic_auc(
        y_train, y_test, risk, times
    )
    auc_values = np.asarray(auc_values, dtype=float)
    km = SurvivalFunctionEstimator().fit(y_test)
    surv = np.asarray(km.predict_proba(times), dtype=float)
    # AUC at y3 is retained for plotting, but its integration weight is zero.
    drops = np.r_[0.0, surv[:-1] - surv[1:]]
    if np.any(drops < -1e-10) or not np.isfinite(auc_values).all():
        raise ValueError("Nonfinite annual AUC or invalid KM survival decline")
    window_event_mass = float(surv[0] - surv[-1])
    if not np.isfinite(window_event_mass) or window_event_mass <= 0:
        raise ValueError("No KM event-survival decline between years 3 and 10")
    weights = drops / window_event_mass
    mean_auc = float(np.dot(auc_values[1:], weights[1:]))
    if not (np.min(auc_values[1:]) - 1e-9 <= mean_auc <=
            np.max(auc_values[1:]) + 1e-9):
        raise ValueError("3–10y mean AUC outside the annual AUC range")

    events = np.asarray(y_test[event_field], dtype=bool)
    window_curve = pd.DataFrame({
        "time_days": times,
        "time_years": times / DAYS_PER_YEAR,
        "AUC": auc_values,
        "KM_survival": surv,
        "interval_KM_survival_decrease": drops,
        "interval_weight_3to10y": weights,
        "n_dynamic_controls": [int(np.sum(test_times > t)) for t in times],
        "n_cumulative_events": [
            int(np.sum(events & (test_times <= t))) for t in times
        ],
        "mean_time_dependent_auc_3to10y": mean_auc,
        "integration_window": "(3y,10y]",
    })
    return mean_auc, window_curve


def evaluate_model(
    model,
    train_data,
    test_data,
    y_train,
    x_test,
    y_test,
):
    """Evaluate a fitted model and return summary metrics and its AUC curve."""
    risk_test = model.predict(x_test)

    eval_times = generate_safe_eval_times(
        train_data["time"].to_numpy(dtype=float),
        test_data["time"].to_numpy(dtype=float),
        365,
    )

    # 完整时间点 AUC 曲线
    auc_values, _ = cumulative_dynamic_auc(
        y_train,
        y_test,
        risk_test,
        eval_times
    )
    # 从第三个评估点开始计算 mean AUC
    _, mean_auc = cumulative_dynamic_auc(
        y_train,
        y_test,
        risk_test,
        eval_times[2:]
    )

    # 新增独立指标：严格 (3,10] 年窗口的 KM-weighted annual-grid mean AUC。
    # 绝不覆盖上面的旧 mean_auc，也不更改 stage3b_combination_performance.csv。
    mean_auc_3to10y, auc_window_3to10y = three_to_ten_year_mean_auc(
        y_train, y_test, risk_test
    )

    event_name, time_name = y_test.dtype.names

    c_index = concordance_index_censored(
        y_test[event_name],
        y_test[time_name],
        risk_test,
    )[0]

    horizon_metrics = {
        label: horizon_auc(
            y_train,
            y_test,
            risk_test,
            horizon,
        )
        for label, horizon in STAGE3B_HORIZONS_DAYS.items()
    }

    auc_curve = pd.DataFrame({
        "time_days": eval_times,
        "time_years": (
            np.asarray(eval_times, dtype=float) / 365.0
        ),
        "AUC": auc_values,
    })

    return (
        risk_test,
        float(mean_auc),
        float(c_index),
        horizon_metrics,
        auc_curve,
        auc_window_3to10y,
    )


def run_outcome(c_type, target):
    """Fit and compare every configured multimodal combination for one outcome."""
    logger = get_logger(
        f"Stage3B_{c_type}_{target}",
        log_dir,
    )

    logger.info(
        f"Stage 3B: {c_type}/{target}, "
        f"{len(ALL_COMBINATIONS)} combinations."
    )

    result_path = make_dir(
        os.path.join(
            res_dir,
            STAGE3B_RESULT_FOLDER,
            f"{c_type}_{target}",
        )
    )

    (
        view_data,
        ref_train_labels,
        ref_test_labels,
    ) = load_outcome_views(
        c_type,
        target,
        logger,
    )

    performance_rows = []
    auc_frames = []
    auc_window_frames = []
    provenance_frames = []
    exclusion_frames = []

    for combo in ALL_COMBINATIONS:
        name = combo_name(combo)
        slug = combo_slug(combo)
        start = time.time()

        logger.info(f"Fit combination: {name}")

        (
            train_data,
            test_data,
            features,
            provenance,
            excluded,
        ) = build_combo_data(
            combo,
            view_data,
            ref_train_labels,
            ref_test_labels,
        )

        train_model = train_data[
            LABEL_COLS + features
        ].copy()

        test_model = test_data[
            LABEL_COLS + features
        ].copy()

        x_train, y_train = get_x_y(
            train_model,
            LABEL_COLS,
            1,
            True,
        )

        x_test, y_test = get_x_y(
            test_model,
            LABEL_COLS,
            1,
            True,
        )

        model = make_fixed_coxem(
            c_type,
            target,
            combo,
            features,
        )

        # Held-out data are NEVER passed to fit().
        model.fit(
            x_train,
            y_train,
        )
        if len(combo) == 1:
            (
                names,
                importances,
                weighted_beta,
                beta_by_estimator
            ) = model.get_feature_importances()

            pd.DataFrame({
                "Feature": names,
                "Importance": importances,
                "Weighted_beta": weighted_beta
            }).to_csv(
                os.path.join(
                    result_path,
                    f"coxem_coefficients_{slug}.csv"
                ),
                index=False
            )



        (
            risk_test,
            mean_auc,
            c_index,
            horizon_metrics,
            auc_curve,
            auc_window_3to10y,
        ) = evaluate_model(
            model,
            train_data,
            test_data,
            y_train,
            x_test,
            y_test,
        )

        elapsed_min = (
            time.time() - start
        ) / 60.0

        n_omics_added = sum(
            x in combo
            for x in ("B", "M", "G")
        )

        performance_rows.append({
            "c_type": c_type,
            "target": target,
            "combination": name,
            "combination_slug": slug,
            "contains_environment": "E" in combo,
            "n_omics_added_to_environment": (
                n_omics_added
                if "E" in combo
                else np.nan
            ),
            "n_features": len(features),
            "mean_time_dependent_auc": mean_auc,
            "c_index": c_index,
            "auc_3y": horizon_metrics["3y"],
            "auc_5y": horizon_metrics["5y"],
            "auc_10y": horizon_metrics["10y"],
            "auc_15y": horizon_metrics["15y"],
            "n_development": len(train_data),
            "n_heldout": len(test_data),
            "elapsed_minutes": elapsed_min,
        })

        auc_curve["c_type"] = c_type
        auc_curve["target"] = target
        auc_curve["combination"] = name
        auc_curve["combination_slug"] = slug
        auc_frames.append(auc_curve)

        # 只追加写入新的 (3,10] 年 CSV，旧的曲线和性能汇总均保持不变。
        auc_window_3to10y["c_type"] = c_type
        auc_window_3to10y["target"] = target
        auc_window_3to10y["combination"] = name
        auc_window_3to10y["combination_slug"] = slug
        auc_window_frames.append(auc_window_3to10y)

        provenance["c_type"] = c_type
        provenance["target"] = target
        provenance["combination"] = name
        provenance_frames.append(provenance)

        if len(excluded):
            excluded["c_type"] = c_type
            excluded["target"] = target
            excluded["combination"] = name
            exclusion_frames.append(excluded)

        if SAVE_HELDOUT_RISK_SCORES:
            score_df = test_data[
                [ID_COL] + LABEL_COLS
            ].copy()

            score_df["risk_score"] = risk_test

            score_df.to_csv(
                os.path.join(
                    result_path,
                    f"heldout_risk_scores_{slug}.csv.gz",
                ),
                index=False,
                compression="gzip",
            )

        if SAVE_FITTED_MODELS:
            import joblib

            joblib.dump(
                model,
                os.path.join(
                    result_path,
                    f"model_{slug}.pkl",
                ),
            )

        logger.info(
            f"{name}: mean tdAUC={mean_auc:.4f}, "
            f"C-index={c_index:.4f}, "
            f"3y={horizon_metrics['3y']:.4f}, "
            f"5y={horizon_metrics['5y']:.4f}, "
            f"10y={horizon_metrics['10y']:.4f}, "
            f"15y={horizon_metrics['15y']:.4f}, "
            f"features={len(features)}, "
            f"time={elapsed_min:.1f} min."
        )

        del model
        del train_data
        del test_data
        del train_model
        del test_model
        del x_train
        del x_test
        del y_train
        del y_test
        del risk_test

        gc.collect()

    perf = pd.DataFrame(
        performance_rows
    )

    env_row = perf.loc[
        perf["combination"] == "E"
    ]

    if len(env_row) != 1:
        raise RuntimeError(
            "Environment-only reference model not found."
        )

    ref = env_row.iloc[0]

    for metric in [
        "mean_time_dependent_auc",
        "c_index",
        "auc_3y",
        "auc_5y",
        "auc_10y",
        "auc_15y",
    ]:
        perf[
            f"delta_{metric}_vs_E"
        ] = (
            perf[metric] -
            ref[metric]
        )

    perf.to_csv(
        os.path.join(
            result_path,
            "stage3b_combination_performance.csv",
        ),
        index=False,
    )

    pd.concat(
        auc_frames,
        ignore_index=True,
    ).to_csv(
        os.path.join(
            result_path,
            "stage3b_time_dependent_auc.csv",
        ),
        index=False,
    )

    # 唯一新增输出：年度 AUC、每段 KM 权重及 3–10 年平均 AUC。
    pd.concat(
        auc_window_frames,
        ignore_index=True,
    ).to_csv(
        os.path.join(
            result_path,
            "stage3b_time_dependent_auc_3to10y.csv",
        ),
        index=False,
    )

    pd.concat(
        provenance_frames,
        ignore_index=True,
    ).to_csv(
        os.path.join(
            result_path,
            "stage3b_feature_provenance.csv",
        ),
        index=False,
    )

    if exclusion_frames:
        pd.concat(
            exclusion_frames,
            ignore_index=True,
        ).to_csv(
            os.path.join(
                result_path,
                "stage3b_excluded_features.csv",
            ),
            index=False,
        )

    logger.info(
        f"Stage 3B completed: {c_type}/{target}"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--outcome-index",
        type=int,
        default=None,
        help=(
            "0-based outcome index. "
            "If omitted, all outcomes run sequentially."
        ),
    )

    parser.add_argument(
        "--enviro-2", action="store_true",
        help="Use config.py's _2 experiment (Result_2); no paths hardcoded",
    )

    args = parser.parse_args()

    tasks = outcome_tasks()

    make_dir(
        os.path.join(
            res_dir,
            STAGE3B_RESULT_FOLDER,
        )
    )

    if args.outcome_index is None:
        selected_tasks = tasks
    else:
        if not (
            0 <= args.outcome_index < len(tasks)
        ):
            raise ValueError(
                f"outcome-index must be 0..{len(tasks)-1}"
            )

        selected_tasks = [
            tasks[args.outcome_index]
        ]

    for c_type, target in selected_tasks:
        run_outcome(
            c_type,
            target,
        )

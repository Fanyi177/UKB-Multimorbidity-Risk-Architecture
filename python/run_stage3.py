"""Fit and evaluate the final integrated CoxEM model on the held-out cohort."""

import gc
import os
import time
import warnings

import joblib
import numpy as np
import pandas as pd

from sksurv.datasets import get_x_y
from sksurv.metrics import (
    cumulative_dynamic_auc,
    concordance_index_censored,
    integrated_brier_score,
)
from sksurv.nonparametric import kaplan_meier_estimator

from config import (
    CoxEM_n_estimator, comorbidity_targets, comorbidity_types, log_dir,
    main_verbose, make_dir, random_state, res_dir, stage3_calibration_bins,
    stage3_compute_ibs, stage3_coxem_n_jobs, stage3_fixed_feature_set,
    stage3_prediction_horizons_days, stage3_primary_views,
    stage3_result_folder, stage3_risk_quantiles, stage3_save_final_model,
    stage3_stage2_result_folder, use_valid, valid_metric,
)
from CoxEM import CoxEM
from utils import (
    get_logger,
    generate_safe_eval_times,
    read_feature_lineage,
    active_feature_lineage,
    summarize_feature_lineage,
)

warnings.filterwarnings("ignore")

ID_COL = "eid"
LABEL_COLS = ["status", "time"]


###############################################################################
# 1. Stage 2 inputs
###############################################################################

def stage2_path(c_type, target, view):
    """Return the configured Stage 2 result directory for one view."""
    return (
        f"{res_dir}/{stage3_stage2_result_folder}/"
        f"{c_type}_{target}_{view}"
    )


def read_required_csv(path):
    """Read a required CSV file and fail with its exact missing path."""
    if not os.path.exists(path):
        raise FileNotFoundError(path)
    return pd.read_csv(path)


def read_stage2_view(c_type, target, view):
    """Load one view's Stage 2 train, test, feature, and lineage outputs."""
    base = stage2_path(c_type, target, view)

    train = read_required_csv(f"{base}/train_data.csv")
    test = read_required_csv(f"{base}/test_data.csv")
    feature_list = read_required_csv(f"{base}/feature_list.csv")
    selection_summary = read_required_csv(f"{base}/selection_summary.csv")

    if ID_COL not in train.columns or ID_COL not in test.columns:
        raise ValueError(
            f"{c_type}/{target}/{view}: Stage 2 train/test must contain eid."
        )

    if train[ID_COL].duplicated().any() or test[ID_COL].duplicated().any():
        raise ValueError(
            f"{c_type}/{target}/{view}: duplicated eid detected."
        )

    if "Feature" not in feature_list.columns:
        raise ValueError(f"{base}/feature_list.csv has no Feature column.")

    features = feature_list["Feature"].astype(str).tolist()

    missing_train = [f for f in features if f not in train.columns]
    missing_test = [f for f in features if f not in test.columns]

    if missing_train or missing_test:
        raise ValueError(
            f"{c_type}/{target}/{view}: feature_list does not match "
            f"Stage 2 train/test. "
            f"missing_train={missing_train[:10]}, "
            f"missing_test={missing_test[:10]}"
        )

    return train, test, features, selection_summary


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


###############################################################################
# 2. Merge the four pre-specified Stage 2 final views by eid
###############################################################################

def build_integrated_data(c_type, target, LOGGER):
    """Merge all selected views by participant identifier and preserve provenance."""
    train_base = None
    test_base = None

    reference_train_index = None
    reference_test_index = None

    train_feature_dict = {}
    test_feature_dict = {}
    feature_views = {}
    lineage_frames = []
    manifest_rows = []

    for view in stage3_primary_views:
        LOGGER.info(
            f"Read Stage 2 final features: {c_type}, {target}, {view}"
        )

        train, test, features, selection_summary = read_stage2_view(
            c_type, target, view
        )

        stage2_lineage = read_feature_lineage(
            f"{stage2_path(c_type, target, view)}/"
            f"feature_selection_trace.csv"
        )
        lineage_frames.append(
            active_feature_lineage(stage2_lineage, features, view=view)
        )

        train = train.set_index(ID_COL, drop=False).sort_index()
        test = test.set_index(ID_COL, drop=False).sort_index()

        if reference_train_index is None:
            reference_train_index = train.index.copy()
            reference_test_index = test.index.copy()

            train_base = train[[ID_COL] + LABEL_COLS].copy()
            test_base = test[[ID_COL] + LABEL_COLS].copy()
        else:
            if not reference_train_index.equals(train.index):
                raise ValueError(
                    f"{c_type}/{target}/{view}: development eid set differs "
                    "across views."
                )

            if not reference_test_index.equals(test.index):
                raise ValueError(
                    f"{c_type}/{target}/{view}: held-out eid set differs "
                    "across views."
                )

            for col in LABEL_COLS:
                if not series_equal(train_base[col], train[col]):
                    raise ValueError(
                        f"{c_type}/{target}/{view}: development {col} differs "
                        "across views."
                    )
                if not series_equal(test_base[col], test[col]):
                    raise ValueError(
                        f"{c_type}/{target}/{view}: held-out {col} differs "
                        "across views."
                    )

        for feature in features:
            if feature not in train_feature_dict:
                train_feature_dict[feature] = train[feature].copy()
                test_feature_dict[feature] = test[feature].copy()
                feature_views[feature] = [view]
            else:
                # Repeated basic covariates such as recruitment_age/sex may
                # occur in more than one view. Keep one copy only after
                # verifying the values are identical.
                if not series_equal(
                    train_feature_dict[feature],
                    train[feature],
                ):
                    raise ValueError(
                        f"Duplicate feature {feature} differs across TRAIN views."
                    )

                if not series_equal(
                    test_feature_dict[feature],
                    test[feature],
                ):
                    raise ValueError(
                        f"Duplicate feature {feature} differs across TEST views."
                    )

                feature_views[feature].append(view)

        selected_n = (
            int(selection_summary["selected_n"].iloc[0])
            if "selected_n" in selection_summary.columns
            else len(features)
        )

        manifest_rows.append(
            {
                "c_type": c_type,
                "target": target,
                "view": view,
                "stage2_selected_n": selected_n,
                "n_features_exported": len(features),
                "stage2_path": stage2_path(c_type, target, view),
            }
        )

    train_features = pd.DataFrame(
        train_feature_dict,
        index=reference_train_index,
    )
    test_features = pd.DataFrame(
        test_feature_dict,
        index=reference_test_index,
    )

    train_data = pd.concat([train_base, train_features], axis=1)
    test_data = pd.concat([test_base, test_features], axis=1)

    provenance = pd.DataFrame(
        {
            "Feature": list(feature_views.keys()),
            "Views": [";".join(v) for v in feature_views.values()],
            "n_views": [len(v) for v in feature_views.values()],
        }
    )
    provenance = provenance.merge(
        summarize_feature_lineage(lineage_frames, feature_views.keys()),
        on="Feature",
        how="left",
    )

    return (
        train_data.reset_index(drop=True),
        test_data.reset_index(drop=True),
        provenance,
        pd.DataFrame(manifest_rows),
    )


###############################################################################
# 3. Final technical checks
###############################################################################

def prepare_final_features(train_data, test_data, provenance):
    """Keep finite, nonconstant final features shared by train and test data."""
    candidate_features = [
        c for c in train_data.columns
        if c not in [ID_COL] + LABEL_COLS
    ]

    usable = []
    excluded = []

    for feature in candidate_features:
        train_col = pd.to_numeric(
            train_data[feature],
            errors="coerce",
        )
        test_col = pd.to_numeric(
            test_data[feature],
            errors="coerce",
        )

        reason = None

        if train_col.isna().any() or test_col.isna().any():
            reason = "missing_or_non_numeric_after_stage2"
        elif not np.isfinite(train_col.to_numpy(dtype=float)).all():
            reason = "non_finite_in_development"
        elif not np.isfinite(test_col.to_numpy(dtype=float)).all():
            reason = "non_finite_in_heldout"
        elif train_col.nunique(dropna=True) <= 1:
            # Technical exclusion only; this is not performance-based
            # feature selection.
            reason = "constant_in_development"

        if reason is None:
            usable.append(feature)
        else:
            excluded.append(
                {
                    "Feature": feature,
                    "Reason": reason,
                }
            )

    if not usable:
        raise ValueError("No usable Stage 3 predictors remain.")

    model_provenance = provenance[
        provenance["Feature"].isin(usable)
    ].copy()

    excluded_df = pd.DataFrame(
        excluded,
        columns=["Feature", "Reason"],
    )

    return usable, model_provenance, excluded_df


###############################################################################
# 4. Training-derived score reference
###############################################################################

def build_risk_reference(risk_scores):
    """Build training-derived risk quantiles used for held-out categorization."""
    risk_scores = np.asarray(risk_scores, dtype=float)

    rows = [
        ["mean", np.mean(risk_scores)],
        ["sd", np.std(risk_scores, ddof=1)],
        ["min", np.min(risk_scores)],
        ["max", np.max(risk_scores)],
    ]

    for q in stage3_risk_quantiles:
        rows.append(
            [
                f"q{int(round(q * 100)):02d}",
                np.quantile(risk_scores, q),
            ]
        )

    return pd.DataFrame(
        rows,
        columns=["metric", "value"],
    )


def assign_train_quartile(risk_scores, reference_df):
    """Assign risk groups using thresholds estimated from training scores."""
    ref = dict(
        zip(
            reference_df["metric"],
            reference_df["value"],
        )
    )

    groups = pd.cut(
        pd.Series(risk_scores),
        bins=[
            -np.inf,
            float(ref["q25"]),
            float(ref["q50"]),
            float(ref["q75"]),
            np.inf,
        ],
        labels=["Q1", "Q2", "Q3", "Q4"],
        include_lowest=True,
    )

    return groups.astype(str).to_numpy()


###############################################################################
# 5. Calibration
###############################################################################

def km_risk_at_horizon(y, horizon):
    """Estimate Kaplan-Meier event risk at a fixed horizon."""
    event_name, time_name = y.dtype.names

    km_time, km_survival = kaplan_meier_estimator(
        y[event_name].astype(bool),
        y[time_name].astype(float),
    )

    idx = np.searchsorted(
        km_time,
        horizon,
        side="right",
    ) - 1

    survival = 1.0 if idx < 0 else float(km_survival[idx])

    return float(1.0 - survival)


def build_calibration_table(
    y_test,
    predicted_risk,
    horizon,
    n_bins,
):
    """Summarize predicted and observed risk within held-out calibration bins."""
    predicted_risk = np.asarray(
        predicted_risk,
        dtype=float,
    )

    # Rank first so ties do not collapse quantile groups unpredictably.
    ranks = pd.Series(predicted_risk).rank(method="first")

    groups = pd.qcut(
        ranks,
        q=min(int(n_bins), len(ranks)),
        labels=False,
        duplicates="drop",
    )

    event_name, _ = y_test.dtype.names
    rows = []

    for g in sorted(pd.Series(groups).dropna().unique()):
        mask = np.asarray(groups == g)
        y_group = y_test[mask]
        p_group = predicted_risk[mask]

        rows.append(
            {
                "horizon_days": float(horizon),
                "calibration_group": int(g) + 1,
                "n": int(mask.sum()),
                "events_observed": int(
                    np.sum(y_group[event_name])
                ),
                "mean_predicted_risk": float(
                    np.mean(p_group)
                ),
                "median_predicted_risk": float(
                    np.median(p_group)
                ),
                "observed_km_risk": km_risk_at_horizon(
                    y_group,
                    horizon,
                ),
            }
        )

    return pd.DataFrame(rows)


###############################################################################
# 6. One final all-view CoxEM
###############################################################################

def run_one_model(c_type, target):
    """Fit, evaluate, and save one final integrated outcome model."""
    start = time.time()

    LOGGER = get_logger(
        f"Stage3_{c_type}_{target}",
        log_dir,
    )

    LOGGER.info("=" * 70)
    LOGGER.info(
        f"Final integrated CoxEM: {c_type}, {target}"
    )
    LOGGER.info("=" * 70)

    result_path = make_dir(
        f"{res_dir}/{stage3_result_folder}/"
        f"{c_type}_{target}"
    )

    (
        train_data,
        test_data,
        provenance,
        input_manifest,
    ) = build_integrated_data(
        c_type,
        target,
        LOGGER,
    )

    (
        final_features,
        model_provenance,
        excluded_features,
    ) = prepare_final_features(
        train_data,
        test_data,
        provenance,
    )

    excluded_features = excluded_features.merge(
        provenance,
        on="Feature",
        how="left",
    )

    LOGGER.info(
        f"Stage 2 union = {len(provenance)} unique features; "
        f"final usable = {len(final_features)}."
    )

    provenance.to_csv(
        f"{result_path}/all_stage2_feature_provenance.csv",
        index=False,
    )
    model_provenance.to_csv(
        f"{result_path}/model_features.csv",
        index=False,
    )
    excluded_features.to_csv(
        f"{result_path}/excluded_features.csv",
        index=False,
    )
    input_manifest.to_csv(
        f"{result_path}/stage2_input_manifest.csv",
        index=False,
    )

    train_model_data = train_data[
        LABEL_COLS + final_features
    ].copy()
    test_model_data = test_data[
        LABEL_COLS + final_features
    ].copy()

    x_train, y_train = get_x_y(
        train_model_data,
        LABEL_COLS,
        1,
        True,
    )
    x_test, y_test = get_x_y(
        test_model_data,
        LABEL_COLS,
        1,
        True,
    )

    feature_map = {
        feature: [feature]
        for feature in final_features
    }

    model = CoxEM(
        c_type=c_type,
        target=target,
        task_id=f"stage3_{c_type}_{target}_integrated",
        n_estimators=CoxEM_n_estimator,
        random_state=random_state,
        n_jobs=stage3_coxem_n_jobs,
        feature_map=feature_map,
        use_valid=use_valid,
        valid_metric=valid_metric,
        verbose=main_verbose,
        fixed_feature_set=stage3_fixed_feature_set,
        compute_train_fitness=False,
        compute_eval_perf=False,
    )

    LOGGER.info(
        f"Fit development set only: n={len(train_data)}, "
        f"features={len(final_features)}, "
        f"CoxEM estimators={CoxEM_n_estimator}, "
        f"parallel workers={stage3_coxem_n_jobs}."
    )

    # IMPORTANT:
    # Held-out x_test/y_test are deliberately NOT passed into model.fit().
    model.fit(
        x_train,
        y_train,
    )

    if stage3_save_final_model:
        joblib.dump(
            model,
            f"{result_path}/final_model.pkl",
        )

    ###########################################################################
    # 6.1 Risk scores
    ###########################################################################

    risk_train = model.predict(x_train)
    risk_test = model.predict(x_test)

    reference_df = build_risk_reference(
        risk_train
    )
    reference_df.to_csv(
        f"{result_path}/risk_score_reference.csv",
        index=False,
    )

    train_score_df = train_data[
        [ID_COL] + LABEL_COLS
    ].copy()
    test_score_df = test_data[
        [ID_COL] + LABEL_COLS
    ].copy()

    train_score_df["risk_score"] = risk_train
    test_score_df["risk_score"] = risk_test

    train_score_df[
        "risk_group_train_cutoff"
    ] = assign_train_quartile(
        risk_train,
        reference_df,
    )

    test_score_df[
        "risk_group_train_cutoff"
    ] = assign_train_quartile(
        risk_test,
        reference_df,
    )

    ###########################################################################
    # 6.2 Held-out time-dependent AUC and C-index
    ###########################################################################

    eval_times = generate_safe_eval_times(
        train_data["time"].to_numpy(dtype=float),
        test_data["time"].to_numpy(dtype=float),
        365,
    )
    auc_values, _ = cumulative_dynamic_auc(
        y_train,
        y_test,
        risk_test,
        eval_times
    )
    _, mean_auc = cumulative_dynamic_auc(
        y_train,
        y_test,
        risk_test,
        eval_times[2:]
    )
    auc_df = pd.DataFrame(
        {
            "time_days": eval_times,
            "AUC": auc_values,
        }
    )
    auc_df.to_csv(
        f"{result_path}/time_dependent_auc.csv",
        index=False,
    )

    event_name, time_name = y_test.dtype.names

    c_index = concordance_index_censored(
        y_test[event_name],
        y_test[time_name],
        risk_test,
    )[0]

    ###########################################################################
    # 6.3 IBS
    ###########################################################################

    ibs = np.nan

    if stage3_compute_ibs:
        survival_test = (
            model.predict_survival_probabilities(
                x_test,
                eval_times,
            )
        )

        ibs = integrated_brier_score(
            y_train,
            y_test,
            survival_test,
            eval_times,
        )

    ###########################################################################
    # 6.4 Fixed-horizon absolute risk + calibration
    ###########################################################################

    fixed_rows = []
    calibration_frames = []

    common_min_time = max(
        float(np.min(train_data["time"])),
        float(np.min(test_data["time"])),
    )

    common_max_time = min(
        float(np.max(train_data["time"])),
        float(np.max(test_data["time"])),
    )

    for horizon in stage3_prediction_horizons_days:
        horizon = float(horizon)

        if not (
            common_min_time <= horizon < common_max_time
        ):
            LOGGER.info(
                f"Skip {horizon:.0f} days: outside common follow-up support "
                f"[{common_min_time:.1f}, {common_max_time:.1f})."
            )
            continue

        train_abs_risk = model.predict_absolute_risk(
            x_train,
            [horizon],
        )[:, 0]

        test_abs_risk = model.predict_absolute_risk(
            x_test,
            [horizon],
        )[:, 0]

        years = horizon / 365.0
        risk_col = f"absolute_risk_{years:g}y"

        train_score_df[risk_col] = train_abs_risk
        test_score_df[risk_col] = test_abs_risk

        # eval_times is annual, so 5y/10y should normally be exact members.
        auc_idx = np.where(
            np.isclose(
                np.asarray(eval_times, dtype=float),
                horizon,
            )
        )[0]

        horizon_auc = (
            float(auc_values[auc_idx[0]])
            if len(auc_idx) > 0
            else np.nan
        )

        calibration_df = build_calibration_table(
            y_test=y_test,
            predicted_risk=test_abs_risk,
            horizon=horizon,
            n_bins=stage3_calibration_bins,
        )

        calibration_df.to_csv(
            f"{result_path}/calibration_{years:g}y.csv",
            index=False,
        )

        calibration_frames.append(
            calibration_df
        )

        observed_risk = km_risk_at_horizon(
            y_test,
            horizon,
        )

        fixed_rows.append(
            {
                "horizon_days": horizon,
                "horizon_years": years,
                "time_dependent_auc": horizon_auc,
                "mean_predicted_risk": float(
                    np.mean(test_abs_risk)
                ),
                "observed_km_risk": observed_risk,
                "calibration_difference_pred_minus_observed": float(
                    np.mean(test_abs_risk) - observed_risk
                ),
            }
        )

    train_score_df.to_csv(
        f"{result_path}/train_risk_scores.csv",
        index=False,
    )

    test_score_df.to_csv(
        f"{result_path}/heldout_risk_scores.csv",
        index=False,
    )

    pd.DataFrame(fixed_rows).to_csv(
        f"{result_path}/fixed_horizon_performance.csv",
        index=False,
    )

    if calibration_frames:
        pd.concat(
            calibration_frames,
            ignore_index=True,
        ).to_csv(
            f"{result_path}/calibration_all_horizons.csv",
            index=False,
        )

    ###########################################################################
    # 6.5 Feature importance and coefficients
    ###########################################################################

    (
        feature_names,
        feature_importance,
        weighted_beta,
        beta_by_estimator,
    ) = model.get_feature_importances()

    beta_by_estimator = np.asarray(
        beta_by_estimator,
        dtype=float,
    )

    beta_sd = (
        np.std(
            beta_by_estimator,
            axis=0,
            ddof=1,
        )
        if beta_by_estimator.shape[0] > 1
        else np.full(
            len(feature_names),
            np.nan,
        )
    )

    feature_df = pd.DataFrame(
        {
            "Feature": feature_names,
            "Importance": feature_importance,
            "Weighted_beta": weighted_beta,
            "Base_learner_beta_SD": beta_sd,
        }
    )

    feature_df = feature_df.merge(
        model_provenance,
        on="Feature",
        how="left",
    )

    feature_df = feature_df.sort_values(
        "Importance",
        ascending=False,
    ).reset_index(drop=True)

    feature_df[
        "Importance_rank"
    ] = np.arange(
        1,
        len(feature_df) + 1,
    )

    feature_df.to_csv(
        f"{result_path}/feature_importance_and_coefficients.csv",
        index=False,
    )

    pd.DataFrame(
        {
            "base_learner": np.arange(
                model.n_estimators
            ),
            "weight": model.weights,
        }
    ).to_csv(
        f"{result_path}/coxem_weights.csv",
        index=False,
    )

    ###########################################################################
    # 6.6 Overall performance and model metadata
    ###########################################################################

    elapsed_minutes = (
        time.time() - start
    ) / 60.0

    performance = pd.DataFrame(
        [
            {
                "c_type": c_type,
                "target": target,
                "model": "CoxEM_integrated_all_views",
                "evaluation_set": "independent_heldout_40pct",
                "mean_time_dependent_auc": float(
                    mean_auc
                ),
                "c_index": float(c_index),
                "ibs": (
                    float(ibs)
                    if np.isfinite(ibs)
                    else np.nan
                ),
                "n_development": int(
                    len(train_data)
                ),
                "n_heldout": int(
                    len(test_data)
                ),
                "n_final_features": int(
                    len(final_features)
                ),
                "elapsed_minutes": float(
                    elapsed_minutes
                ),
            }
        ]
    )

    performance.to_csv(
        f"{result_path}/performance.csv",
        index=False,
    )

    pd.DataFrame(
        [
            {
                "c_type": c_type,
                "target": target,
                "stage2_source_folder": (
                    stage3_stage2_result_folder
                ),
                "views": "+".join(
                    stage3_primary_views
                ),
                "n_stage2_union_features": int(
                    len(provenance)
                ),
                "n_final_model_features": int(
                    len(final_features)
                ),
                "n_technical_exclusions": int(
                    len(excluded_features)
                ),
                "coxem_n_estimators": int(
                    CoxEM_n_estimator
                ),
                "coxem_n_jobs": int(
                    stage3_coxem_n_jobs
                ),
                "fixed_feature_set": bool(
                    stage3_fixed_feature_set
                ),
                "heldout_used_for_feature_selection": False,
                "heldout_used_in_model_fit": False,
            }
        ]
    ).to_csv(
        f"{result_path}/model_summary.csv",
        index=False,
    )

    LOGGER.info(
        f"Held-out: mean AUC={mean_auc:.4f}; "
        f"C-index={c_index:.4f}; IBS={ibs:.4f}; "
        f"time={elapsed_minutes:.1f} min."
    )

    summary_record = (
        performance.iloc[0].to_dict()
    )

    del model
    del train_data
    del test_data
    del x_train
    del x_test
    del y_train
    del y_test

    gc.collect()

    return summary_record


###############################################################################
# 7. Main
###############################################################################

if __name__ == "__main__":
    LOGGER = get_logger(
        "Stage_3_final_CoxEM",
        log_dir,
    )

    LOGGER.info(
        "Stage 3 starts: all-view CoxEM fitted on the 60% development set; "
        "the 40% held-out set is used only for final evaluation."
    )

    root_result_path = make_dir(
        f"{res_dir}/{stage3_result_folder}"
    )

    summary_records = []

    # Outcomes are deliberately sequential to control memory.
    # Within each final CoxEM, the base learners run in parallel.
    for c_type in comorbidity_types:
        for target in comorbidity_targets[c_type]:
            try:
                summary_records.append(
                    run_one_model(
                        c_type,
                        target,
                    )
                )
            except Exception as exc:
                LOGGER.exception(
                    f"Stage 3 failed for {c_type}/{target}: {exc}"
                )
                raise

    pd.DataFrame(
        summary_records
    ).to_csv(
        f"{root_result_path}/stage3_all_models_summary.csv",
        index=False,
    )

    LOGGER.info(
        "Stage 3 final CoxEM completed for all configured outcomes."
    )

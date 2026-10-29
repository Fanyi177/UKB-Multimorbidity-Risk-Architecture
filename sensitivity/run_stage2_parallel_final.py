"""Select Stage 2 features with fold-level parallel internal cross-validation."""

import gc
import math
import os
import warnings

import joblib
import numpy as np
import pandas as pd

from joblib import Parallel, delayed

from sksurv.datasets import get_x_y
from sksurv.metrics import (
    cumulative_dynamic_auc,
    concordance_index_censored
)

from config import (
    CoxEM_n_estimator, comorbidity_targets, comorbidity_types, dat_dir,
    feat_name_dict, fl_corr_type, fold_type, log_dir, make_dir,
    min_fl_c_corr, n_fea_sel, n_process, n_repeats, n_splits,
    num_features_per_step, random_state, res_dir, sample_ratio,
    stage2_coxem_n_jobs, stage2_metric_to_select_n, stage2_min_improvement,
    stage2_min_successful_folds, stage2_parallel_folds, stage2_patience,
    stage2_selection_rule, use_valid, valid_metric,
)
from utils import (
    get_logger,
    generate_safe_eval_times,
    read_feature_lineage,
    active_feature_lineage,
    build_stage2_feature_selection_trace,
    load_processed_fold,
)
from CoxEM import CoxEM

# Import scalar settings so loky workers receive the sensitivity-only values
# with the serialized stage2 functions; primary config.py keeps old behavior.
try:
    from config import (
        stage2_adaptive_auc_times as SENS_STAGE2_ADAPT,
        stage2_auc_min_cases as SENS_STAGE2_MIN_CASES,
        stage2_auc_min_controls as SENS_STAGE2_MIN_CONTROLS,
        stage2_max_consecutive_initial_failures as SENS_STAGE2_FAST_FAIL,
    )
except ImportError:
    SENS_STAGE2_ADAPT = False
    SENS_STAGE2_MIN_CASES = 2
    SENS_STAGE2_MIN_CONTROLS = 2
    SENS_STAGE2_FAST_FAIL = 3


# This function changes evaluation for sensitivity analyses ONLY. The original
# primary-analysis 10-fold time-dependent AUC calculation stays untouched.
def _calculate_stage2_mean_auc(y_train, y_valid, risks_valid, original_times):
    original_times = np.asarray(original_times, dtype=float)
    if not SENS_STAGE2_ADAPT:
        _, auc = cumulative_dynamic_auc(y_train, y_valid, risks_valid, original_times[2:])
        return float(auc), len(original_times[2:])

    if len(original_times) <= 2:
        raise ValueError("Fewer than three original AUC evaluation times")

    event_key, time_key = y_valid.dtype.names
    valid_event = np.asarray(y_valid[event_key], dtype=bool)
    valid_duration = np.asarray(y_valid[time_key], dtype=float)
    if valid_event.sum() < 2:
        raise ValueError(
            f"AUC not identifiable: validation fold contains {valid_event.sum()} events"
        )

    # Do NOT treat an undefined AUC as zero, 0.5, or an average of other folds.
    # A(t) requires both incident cases by t and event-free controls beyond t.
    # Use the same outcome-defined time grid for every Top-N candidate in a fold.
    candidate_times = original_times[2:]
    min_cases = int(SENS_STAGE2_MIN_CASES)
    min_controls = int(SENS_STAGE2_MIN_CONTROLS)
    case_counts = np.array([
        np.count_nonzero(valid_event & (valid_duration <= t)) for t in candidate_times
    ])
    control_counts = np.array([
        np.count_nonzero(valid_duration > t) for t in candidate_times
    ])
    supported = candidate_times[(case_counts >= min_cases) & (control_counts >= min_controls)]
    if len(supported) == 0:
        raise ValueError(
            "No valid annual AUC horizon with sufficient cases and controls: "
            f"valid_events={int(valid_event.sum())}, "
            f"range={float(np.min(valid_duration)):.1f}-"
            f"{float(np.max(valid_duration)):.1f} days"
        )

    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", message="invalid value encountered in divide", category=RuntimeWarning)
        auc_at_times, mean_auc = cumulative_dynamic_auc(
            y_train, y_valid, risks_valid, supported
        )
    if np.isfinite(mean_auc) and np.all(np.isfinite(auc_at_times)):
        return float(mean_auc), len(supported)

    # Some horizons can remain undefined due to censoring weights, even when
    # unweighted event/control counts are positive. Test horizons individually;
    # recompute scikit-survival's survival-weighted mean only on supported ones.
    individually_valid_times = []
    for t in supported:
        try:
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", message="invalid value encountered in divide", category=RuntimeWarning)
                val, mean_one = cumulative_dynamic_auc(
                    y_train, y_valid, risks_valid, np.asarray([t], dtype=float)
                )
            if np.all(np.isfinite(val)) and np.isfinite(mean_one):
                individually_valid_times.append(float(t))
        except (ValueError, ZeroDivisionError, ArithmeticError):
            continue

    if not individually_valid_times:
        raise ValueError(
            "AUC undefined at every candidate horizon: "
            f"valid_events={int(valid_event.sum())}; "
            f"supported_horizons={len(supported)}"
        )

    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", message="invalid value encountered in divide", category=RuntimeWarning)
        values, final_mean = cumulative_dynamic_auc(
            y_train, y_valid, risks_valid,
            np.asarray(individually_valid_times, dtype=float)
        )
    if not np.all(np.isfinite(values)) or not np.isfinite(final_mean):
        raise ValueError("AUC remained non-finite after checking event-supported horizons")
    return float(final_mean), len(individually_valid_times)


###############################################################################
### 1. Read processed independent train/test ####
###############################################################################

def prepare_independent_features(view, c_type, target):

    """Load processed independent train and test data for one view."""
    dat_path = f"{dat_dir}/{c_type}_{target}_{view}"

    train_data = pd.read_csv(
        f"{dat_path}/independent_train_processed.csv"
    )

    test_data = pd.read_csv(
        f"{dat_path}/independent_test_processed.csv"
    )

    return train_data, test_data


###############################################################################
### 2. Read one internal CV fold ####
###############################################################################

def prepare_fold_features(
    view,
    c_type,
    target,
    task_id
):

    """Load one processed internal cross-validation fold."""
    dat_path = f"{dat_dir}/{c_type}_{target}_{view}"

    return load_processed_fold(dat_path, fold_type, task_id)


###############################################################################
### 3. Technical feature detection ####
###############################################################################

def is_technical_feature(feature):

    """Identify generated missingness or technical indicator features."""
    if feature is None:
        return True

    s = str(feature).strip()

    if s == "":
        return True

    # Python missing indicator
    if s.lower().endswith("__missing"):
        return True

    # one-hot technical categories
    if "=" in s:

        last = s.split("=")[-1].strip().lower()

        if last in {
            "nan",
            "na",
            "<na>",
            "none",
            "null",
            "missing"
        }:
            return True

        if last in {
            "-1",
            "-1.0"
        }:
            return True

        try:
            value = float(last)

            if math.isnan(value):
                return True

        except Exception:
            pass

    return False


###############################################################################
### 4. Read and aggregate Stage 1 CoxEM feature importance ####
###############################################################################

def process_fi_from_stage1(
    res_path,
    feature_names
):

    """Aggregate Stage 1 feature importance across all successful folds."""
    fi_dict = {
        feature: 0.0
        for feature in feature_names
        if not is_technical_feature(feature)
    }


    for task_id in range(n_process):

        fi_file = (
            f"{res_path}/"
            f"CoxEM_Cox_raw_fi_{fold_type}_{task_id}.csv"
        )


        fi_data = np.loadtxt(
            fi_file,
            dtype=str,
            delimiter="\t"
        )


        # run_model.py saves:
        #
        # row 1 = feature names
        # row 2 = importance
        #
        fea_names = fi_data[0]

        fea_fis = fi_data[1].astype(float)


        valid_names = []

        valid_fis = []


        for feature, importance in zip(
            fea_names,
            fea_fis
        ):

            if is_technical_feature(feature):
                continue

            if feature not in fi_dict:
                continue

            valid_names.append(
                feature
            )

            valid_fis.append(
                float(importance)
            )


        valid_fis = np.asarray(
            valid_fis,
            dtype=float
        )


        total_fi = np.sum(
            valid_fis
        )


        if (
            len(valid_names) == 0 or
            not np.isfinite(total_fi) or
            total_fi <= 0
        ):
            continue


        valid_fis = (
            valid_fis /
            total_fi
        )


        for feature, importance in zip(
            valid_names,
            valid_fis
        ):

            fi_dict[feature] += (
                importance /
                n_process
            )


    return fi_dict


def process_signed_fi_from_stage1(
    res_path,
    feature_names
):
    """Aggregate signed Stage 1 importance across folds and L1-normalize it."""
    signed_fi_dict = {
        feature: 0.0
        for feature in feature_names
        if not is_technical_feature(feature)
    }

    for task_id in range(n_process):
        signed_fi_file = (
            f"{res_path}/"
            f"CoxEM_Cox_raw_signed_fi_{fold_type}_{task_id}.csv"
        )
        if not os.path.exists(signed_fi_file):
            model_file = (
                f"{res_path}/"
                f"CoxEM_Cox_model_{fold_type}_{task_id}.pkl"
            )
            model = joblib.load(model_file)
            fold_names, fold_scores = model.get_signed_feature_importances()
            np.savetxt(
                signed_fi_file,
                np.array([fold_names, list(fold_scores)]),
                fmt="%s",
                delimiter="\t"
            )
        signed_fi_data = np.loadtxt(
            signed_fi_file,
            dtype=str,
            delimiter="\t"
        )
        fold_names = signed_fi_data[0]
        fold_scores = signed_fi_data[1].astype(float)

        valid_names = []
        valid_scores = []
        for feature, score in zip(fold_names, fold_scores):
            if is_technical_feature(feature):
                continue
            if feature not in signed_fi_dict:
                continue
            if not np.isfinite(score):
                continue
            valid_names.append(feature)
            valid_scores.append(float(score))

        valid_scores = np.asarray(valid_scores, dtype=float)
        fold_l1 = np.sum(np.abs(valid_scores))
        if len(valid_names) == 0 or fold_l1 <= 0:
            continue

        valid_scores /= fold_l1
        for feature, score in zip(valid_names, valid_scores):
            signed_fi_dict[feature] += score / n_process

    aggregate_l1 = np.sum(np.abs(list(signed_fi_dict.values())))
    if np.isfinite(aggregate_l1) and aggregate_l1 > 0:
        signed_fi_dict = {
            feature: score / aggregate_l1
            for feature, score in signed_fi_dict.items()
        }

    return signed_fi_dict


###############################################################################
### 5. Fit CoxEM on one OUTER internal-CV fold ####
###############################################################################

def do_experiment_on_fold(
    c_type,
    target,
    view,
    selected_features,
    task_id
):

    ###########################################################################
    ### 5.1 Read fold data ####
    ###########################################################################

    """Fit and evaluate one Stage 2 candidate on one internal fold."""
    train_data, valid_data = prepare_fold_features(
        view,
        c_type,
        target,
        task_id
    )


    ###########################################################################
    ### 5.2 Variables present in both train and valid ####
    ###########################################################################

    available_features = [
        feature
        for feature in selected_features
        if (
            feature in train_data.columns and
            feature in valid_data.columns
        )
    ]

    unavailable_features = [
        feature
        for feature in selected_features
        if feature not in available_features
    ]


    if len(available_features) == 0:

        raise ValueError(
            "No selected features available in this fold."
        )


    ###########################################################################
    ### 5.3 Remove fold-specific constant predictors ####
    ###########################################################################

    usable_features = []


    for feature in available_features:

        n_unique = train_data[
            feature
        ].dropna().nunique()


        if n_unique > 1:

            usable_features.append(
                feature
            )

    constant_features = [
        feature
        for feature in available_features
        if feature not in usable_features
    ]


    if len(usable_features) == 0:

        raise ValueError(
            "All selected features are constant in this fold."
        )


    ###########################################################################
    ### 5.4 Subset data ####
    ###########################################################################

    base_cols = [
        "status",
        "time"
    ]


    if "eid" in train_data.columns:

        base_cols = [
            "eid"
        ] + base_cols


    keep_cols = (
        base_cols +
        usable_features
    )


    train_data = train_data[
        keep_cols
    ].dropna().reset_index(
        drop=True
    )


    valid_data = valid_data[
        keep_cols
    ].dropna().reset_index(
        drop=True
    )


    if len(train_data) < 20:

        raise ValueError(
            "Training sample size < 20."
        )


    if len(valid_data) < 20:

        raise ValueError(
            "Validation sample size < 20."
        )


    ###########################################################################
    ### 5.5 Survival objects ####
    ###########################################################################

    attr_labels = [
        "status",
        "time"
    ]


    train_model_data = train_data.drop(
        columns=["eid"],
        errors="ignore"
    )


    valid_model_data = valid_data.drop(
        columns=["eid"],
        errors="ignore"
    )


    x_train, y_train = get_x_y(
        train_model_data,
        attr_labels,
        1,
        True
    )


    x_valid, y_valid = get_x_y(
        valid_model_data,
        attr_labels,
        1,
        True
    )


    ###########################################################################
    ### 5.6 Identity feature map ####
    ###
    ### At this point all one-hot processing has already been completed.
    ### Therefore every selected Python feature is treated as one feature.
    ###########################################################################

    feature_map = {
        feature: [feature]
        for feature in usable_features
    }


    ###########################################################################
    ### 5.7 Fit CoxEM ####
    ###
    ### IMPORTANT:
    ###
    ### Outer fold validation data is NOT passed into model.fit().
    ###
    ### CoxEM performs its own internal OOB validation inside the OUTER
    ### training fold.
    ###
    ### The outer validation fold is used only for final fold evaluation.
    ###########################################################################

    stage2_task_id = (
        f"stage_parallel_"
        f"{c_type}_"
        f"{target}_"
        f"{view}_"
        f"{task_id}"
    )


    model = CoxEM(
        c_type=c_type,
        target=target,
        task_id=stage2_task_id,
        n_estimators=CoxEM_n_estimator,
        random_state=random_state + int(task_id),
        n_jobs=stage2_coxem_n_jobs,
        feature_map=feature_map,
        use_valid=use_valid,
        valid_metric=valid_metric,
        verbose=0
    )


    model.fit(
        x_train,
        y_train
    )

    coxem_selected_by = {
        feature: []
        for feature in usable_features
    }
    for estimator_id, estimator_features in enumerate(model.feature_indices):
        for feature in estimator_features:
            feature = str(feature)
            if feature in coxem_selected_by:
                coxem_selected_by[feature].append(estimator_id)

    coxem_feature_selection = [
        {
            "Micro_feature": feature,
            "Selected_base_learners": ";".join(
                str(i) for i in coxem_selected_by[feature]
            ),
            "N_base_learners_selected": len(coxem_selected_by[feature]),
            "Selection_frequency": (
                len(coxem_selected_by[feature]) / model.n_estimators
                if model.n_estimators > 0
                else np.nan
            ),
        }
        for feature in usable_features
    ]


    ###########################################################################
    ### 5.8 Predict OUTER validation fold ####
    ###########################################################################

    risks_valid = model.predict(
        x_valid
    )


    ###########################################################################
    ### 5.9 Time-dependent AUC ####
    ###########################################################################

    eval_times = generate_safe_eval_times(
        train_data["time"],
        valid_data["time"],
        365
    )


    mean_auc, n_auc_times = _calculate_stage2_mean_auc(
        y_train,
        y_valid,
        risks_valid,
        eval_times,
    )


    ###########################################################################
    ### 5.10 C-index ####
    ###########################################################################

    event_name, time_name = y_valid.dtype.names


    c_index = concordance_index_censored(
        y_valid[event_name],
        y_valid[time_name],
        risks_valid
    )[0]


    ###########################################################################
    ### 5.11 Clean memory ####
    ###########################################################################

    n_used_features = len(
        usable_features
    )

    # Preserve fold event counts before freeing structured survival arrays.
    # Previous code accessed y_train/y_valid AFTER deleting them, causing
    # UnboundLocalError for every Stage 2 CV fold.
    n_train_events = int(np.sum(y_train[y_train.dtype.names[0]]))
    n_valid_events = int(np.sum(y_valid[y_valid.dtype.names[0]]))

    del model
    del train_data
    del valid_data
    del x_train
    del x_valid
    del y_train
    del y_valid
    del risks_valid

    gc.collect()


    return {
        "task_id": int(task_id),
        "auc": float(mean_auc),
        "auc_evaluation_times": int(n_auc_times),
        "n_train_events": n_train_events,
        "n_valid_events": n_valid_events,
        "c-index": float(c_index),
        "n_requested_features": len(selected_features),
        "n_used_features": n_used_features,
        "unavailable_features": ";".join(unavailable_features),
        "constant_features_removed": ";".join(constant_features),
        "coxem_feature_selection": coxem_feature_selection,
    }


###############################################################################
### 6. Evaluate one Top-N candidate across internal CV folds ####
###############################################################################

def _run_fold_safe(
    c_type,
    target,
    view,
    selected_features,
    task_id
):
    """Run one parallel fold while returning failures as structured records."""
    try:
        perf = do_experiment_on_fold(
            c_type=c_type,
            target=target,
            view=view,
            selected_features=selected_features,
            task_id=task_id
        )
        if not np.isfinite(perf["auc"]) or not np.isfinite(perf["c-index"]):
            raise ValueError("non-finite fold performance")
        return {
            "ok": True,
            "task_id": int(task_id),
            "perf": perf
        }
    except Exception as e:
        return {
            "ok": False,
            "task_id": int(task_id),
            "error": repr(e)
        }


def evaluate_topn_internal(
    c_type,
    target,
    view,
    selected_features,
    LOGGER
):

    """Evaluate one Top-N candidate across the configured internal folds."""
    perfs = []


    total_tasks = (
        n_repeats * n_splits
        if fold_type != "tts"
        else n_repeats
    )

    n_jobs = min(
        int(stage2_parallel_folds),
        int(total_tasks)
    )

    LOGGER.info(
        f"Top {len(selected_features)}: running "
        f"{total_tasks} folds with {n_jobs} parallel workers; "
        f"inner CoxEM n_jobs={stage2_coxem_n_jobs}."
    )

    results = Parallel(
        n_jobs=n_jobs,
        backend="loky"
    )(
        delayed(_run_fold_safe)(
            c_type=c_type,
            target=target,
            view=view,
            selected_features=selected_features,
            task_id=task_id
        )
        for task_id in range(total_tasks)
    )

    for result in sorted(results, key=lambda x: x["task_id"]):

        task_id = result["task_id"]

        if result["ok"]:
            perf = result["perf"]
            perfs.append(perf)

            LOGGER.info(
                f"Top {len(selected_features)}, "
                f"fold {task_id}: "
                f"AUC={perf['auc']:.4f}, "
                f"C-index={perf['c-index']:.4f}, "
                f"used={perf['n_used_features']}"
            )
        else:
            LOGGER.info(
                f"Top {len(selected_features)}, "
                f"fold {task_id} failed: {result['error']}"
            )


    ###########################################################################
    ### Require enough successful folds ####
    ###########################################################################

    if (
        len(perfs) <
        stage2_min_successful_folds
    ):

        LOGGER.info(
            f"Top {len(selected_features)} rejected: "
            f"only {len(perfs)}/{total_tasks} folds succeeded."
        )

        return None


    ###########################################################################
    ### Fold-level statistics ####
    ###########################################################################

    auc_values = np.array(
        [
            p["auc"]
            for p in perfs
        ],
        dtype=float
    )


    cindex_values = np.array(
        [
            p["c-index"]
            for p in perfs
        ],
        dtype=float
    )


    n_success = len(
        perfs
    )


    auc_mean = np.mean(
        auc_values
    )


    auc_sd = np.std(
        auc_values,
        ddof=1
    )


    auc_se = (
        auc_sd /
        np.sqrt(n_success)
    )


    cindex_mean = np.mean(
        cindex_values
    )


    cindex_sd = np.std(
        cindex_values,
        ddof=1
    )


    cindex_se = (
        cindex_sd /
        np.sqrt(n_success)
    )


    mean_used_features = np.mean(
        [
            p["n_used_features"]
            for p in perfs
        ]
    )


    return {
        "auc": float(auc_mean),
        "auc_sd": float(auc_sd),
        "auc_se": float(auc_se),

        "c-index": float(cindex_mean),
        "c-index_sd": float(cindex_sd),
        "c-index_se": float(cindex_se),

        "n_successful_folds": int(n_success),
        "mean_used_features": float(mean_used_features),

        "fold_records": perfs
    }


###############################################################################
### 7. Build Top-N candidate grid ####
###############################################################################

def build_candidate_ns(
    n_features,
    step
):

    """Build the ordered grid of Top-N feature counts."""
    if n_features <= 0:

        return []


    if n_features <= step:

        return [
            n_features
        ]


    candidate_ns = list(
        range(
            step,
            n_features + 1,
            step
        )
    )


    if candidate_ns[-1] != n_features:

        candidate_ns.append(
            n_features
        )


    return candidate_ns


###############################################################################
### 8. 1-SE selection ####
###############################################################################

def select_n_by_one_se(
    perf_df,
    metric_name="auc"
):

    """Select the smallest eligible Top-N candidate under the one-standard-error rule."""
    if len(perf_df) == 0:

        raise ValueError(
            "perf_df is empty."
        )


    metric_se_col = (
        f"{metric_name}_se"
    )


    best_idx = perf_df[
        metric_name
    ].idxmax()


    best_n = int(
        perf_df.loc[
            best_idx,
            "num_features"
        ]
    )


    best_metric = float(
        perf_df.loc[
            best_idx,
            metric_name
        ]
    )


    best_se = float(
        perf_df.loc[
            best_idx,
            metric_se_col
        ]
    )


    threshold = (
        best_metric -
        best_se
    )


    eligible = perf_df[
        perf_df[
            metric_name
        ] >= threshold
    ].copy()


    selected_n = int(
        eligible[
            "num_features"
        ].min()
    )


    selected_row = eligible[
        eligible[
            "num_features"
        ] == selected_n
    ].iloc[0]


    return {
        "best_n_by_max": best_n,
        "best_metric": best_metric,
        "best_se": best_se,
        "one_se_threshold": threshold,

        "selected_n": selected_n,
        "selected_metric": float(
            selected_row[
                metric_name
            ]
        )
    }


###############################################################################
### 9. Main ####
###############################################################################

if __name__ == "__main__":

    LOGGER = get_logger(
        "Stage_parallel_CoxEM_TopN",
        log_dir
    )


    LOGGER.info(
        "Stage parallel starts: "
        "CoxEM internal CV + 1-SE parsimonious Top-N selection."
    )


    for c_type in comorbidity_types:

        for target in comorbidity_targets[
            c_type
        ]:

            for view in feat_name_dict:

                LOGGER.info(
                    "=================================================="
                )

                LOGGER.info(
                    f"Stage 2: "
                    f"{c_type}, "
                    f"{target}, "
                    f"{view}"
                )

                LOGGER.info(
                    "=================================================="
                )


                ################################################################
                ### 9.1 Paths ####
                ################################################################

                stage2_res_path = make_dir(
                    f"{res_dir}/stage_parallel/"
                    f"{c_type}_{target}_{view}"
                )


                ################################################################
                ### 9.2 Independent processed data ####
                ################################################################

                train_ind, test_ind = prepare_independent_features(
                    view,
                    c_type,
                    target
                )


                feature_names = [
                    column
                    for column in train_ind.columns
                    if column not in [
                        "eid",
                        "status",
                        "time"
                    ]
                ]


                ################################################################
                ### 9.3 Locate Stage 1 CoxEM results ####
                ################################################################

                if (
                    c_type == "comorbidity" and
                    target == "all"
                ):

                    stage1_path = (
                        f"{res_dir}/"
                        f"{c_type}_{target}_{view}/"
                        f"sample_ratio={sample_ratio} & "
                        f"event_censor_ratio=fold & "
                        f"fl_corr_type={fl_corr_type} & "
                        f"min_fl_c_corr={min_fl_c_corr} & "
                        f"n_fea_sel={n_fea_sel}"
                    )

                else:

                    stage1_path = (
                        f"{res_dir}/"
                        f"{c_type}_{target}_{view}"
                    )


                ################################################################
                ### 9.4 Aggregate Stage 1 feature importance ####
                ################################################################

                fi_dict = process_fi_from_stage1(
                    res_path=stage1_path,
                    feature_names=feature_names
                )


                fi_df = pd.DataFrame(
                    {
                        "Feature": list(
                            fi_dict.keys()
                        ),
                        "Importance": list(
                            fi_dict.values()
                        )
                    }
                )


                fi_df = fi_df.sort_values(
                    "Importance",
                    ascending=False
                ).reset_index(
                    drop=True
                )


                fi_df.to_csv(
                    f"{stage2_res_path}/"
                    f"feature_importance.csv",
                    index=False
                )

                signed_fi_dict = process_signed_fi_from_stage1(
                    res_path=stage1_path,
                    feature_names=feature_names
                )
                signed_fi_df = fi_df.copy()
                signed_fi_df["Signed_importance"] = (
                    signed_fi_df["Feature"].map(signed_fi_dict)
                )
                signed_fi_df.to_csv(
                    f"{stage2_res_path}/"
                    f"signed_feature_importance.csv",
                    index=False
                )


                ################################################################
                ### 9.5 Candidate features ####
                ###
                ### Only positive CoxEM importance features are candidates.
                ################################################################

                candidate_feature_df = fi_df[
                    np.isfinite(
                        fi_df[
                            "Importance"
                        ]
                    ) &
                    (
                        fi_df[
                            "Importance"
                        ] > 0
                    )
                ].copy()


                sorted_features = candidate_feature_df[
                    "Feature"
                ].tolist()


                if len(sorted_features) == 0:

                    LOGGER.info(
                        f"{c_type}, {target}, {view}: "
                        f"no positive-importance features, skip."
                    )
                    if SENS_STAGE2_ADAPT:
                        raise RuntimeError(
                            f"Sensitivity Stage 2 cannot continue: "
                            f"{c_type}/{target}/{view} has no candidate predictors. "
                            "Check Stage 1 feature evidence and the new source data."
                        )
                    continue


                LOGGER.info(
                    f"Positive CoxEM importance features: "
                    f"{len(sorted_features)}"
                )


                ################################################################
                ### 9.6 Top-N candidates ####
                ################################################################

                candidate_ns = build_candidate_ns(
                    n_features=len(
                        sorted_features
                    ),
                    step=num_features_per_step
                )


                LOGGER.info(
                    f"Top-N candidates: "
                    f"{candidate_ns}"
                )


                ################################################################
                ### 9.7 Internal CV ####
                ################################################################

                perf_records = []

                fold_perf_records = []

                internal_selection_records = []


                prev_metric = None

                no_improve_count = 0
                consecutive_initial_failures = 0


                metric_name = (
                    stage2_metric_to_select_n
                )


                for step_count in candidate_ns:

                    selected_features = (
                        sorted_features[
                            :step_count
                        ]
                    )


                    LOGGER.info(
                        f"Evaluating Top "
                        f"{step_count} "
                        f"with CoxEM..."
                    )


                    perf_s = evaluate_topn_internal(
                        c_type=c_type,
                        target=target,
                        view=view,
                        selected_features=selected_features,
                        LOGGER=LOGGER
                    )


                    if perf_s is None:

                        LOGGER.info(
                            f"Top {step_count} failed."
                        )
                        if SENS_STAGE2_ADAPT and not perf_records:
                            consecutive_initial_failures += 1
                            max_failures = int(SENS_STAGE2_FAST_FAIL)
                            if consecutive_initial_failures >= max_failures:
                                raise RuntimeError(
                                    f"Sensitivity Stage 2 stopped early: "
                                    f"{c_type}/{target}/{view} failed "
                                    f"{consecutive_initial_failures} initial Top-N candidates. "
                                    "Review validation event counts and AUC horizons "
                                    "before fitting larger models."
                                )
                        continue

                    consecutive_initial_failures = 0
                    current_metric = float(
                        perf_s[
                            metric_name
                        ]
                    )


                    improvement = np.nan


                    if prev_metric is not None:

                        improvement = (
                            current_metric -
                            prev_metric
                        )


                        if (
                            improvement <=
                            stage2_min_improvement
                        ):

                            no_improve_count += 1

                        else:

                            no_improve_count = 0


                    record = {
                        "num_features":
                            step_count,

                        "auc":
                            perf_s[
                                "auc"
                            ],

                        "auc_sd":
                            perf_s[
                                "auc_sd"
                            ],

                        "auc_se":
                            perf_s[
                                "auc_se"
                            ],

                        "c-index":
                            perf_s[
                                "c-index"
                            ],

                        "c-index_sd":
                            perf_s[
                                "c-index_sd"
                            ],

                        "c-index_se":
                            perf_s[
                                "c-index_se"
                            ],

                        "n_successful_folds":
                            perf_s[
                                "n_successful_folds"
                            ],

                        "mean_used_features":
                            perf_s[
                                "mean_used_features"
                            ],

                        "improvement":
                            improvement,

                        "no_improve_count":
                            no_improve_count
                    }


                    perf_records.append(
                        record
                    )


                    for fold_record in perf_s[
                        "fold_records"
                    ]:

                        fold_record_save = (
                            fold_record.copy()
                        )

                        fold_internal_records = fold_record_save.pop(
                            "coxem_feature_selection",
                            [],
                        )

                        for internal_record in fold_internal_records:
                            internal_selection_records.append({
                                "num_features": step_count,
                                "task_id": fold_record["task_id"],
                                **internal_record,
                            })

                        fold_record_save[
                            "num_features"
                        ] = step_count

                        fold_perf_records.append(
                            fold_record_save
                        )


                    LOGGER.info(
                        f"Top {step_count}: "
                        f"AUC={perf_s['auc']:.4f} "
                        f"(SE={perf_s['auc_se']:.4f}), "
                        f"C-index={perf_s['c-index']:.4f}, "
                        f"improvement="
                        f"{improvement}"
                    )


                    prev_metric = (
                        current_metric
                    )


                    ################################################################
                    ### Early stopping ####
                    ################################################################

                    if (
                        no_improve_count >=
                        stage2_patience
                    ):

                        LOGGER.info(
                            f"Early stopping at "
                            f"Top {step_count}: "
                            f"{stage2_patience} consecutive "
                            f"improvements <= "
                            f"{stage2_min_improvement}."
                        )

                        break


                ################################################################
                ### 9.8 No valid candidate ####
                ################################################################

                if len(perf_records) == 0:

                    LOGGER.info(
                        f"{c_type}, {target}, {view}: "
                        f"no valid Top-N result."
                    )
                    if SENS_STAGE2_ADAPT:
                        raise RuntimeError(
                            f"Sensitivity Stage 2 has no valid Top-N result for "
                            f"{c_type}/{target}/{view}; will not proceed to Stage 3."
                        )
                    continue


                perf_df = pd.DataFrame(
                    perf_records
                )


                fold_perf_df = pd.DataFrame(
                    fold_perf_records
                )


                ################################################################
                ### 9.9 1-SE rule ####
                ################################################################

                if (
                    stage2_selection_rule.lower()
                    == "1se"
                ):

                    selection = select_n_by_one_se(
                        perf_df=perf_df,
                        metric_name=metric_name
                    )


                    final_n = selection[
                        "selected_n"
                    ]


                else:

                    best_idx = perf_df[
                        metric_name
                    ].idxmax()


                    final_n = int(
                        perf_df.loc[
                            best_idx,
                            "num_features"
                        ]
                    )


                    selection = {
                        "best_n_by_max":
                            final_n,

                        "best_metric":
                            float(
                                perf_df.loc[
                                    best_idx,
                                    metric_name
                                ]
                            ),

                        "best_se":
                            float(
                                perf_df.loc[
                                    best_idx,
                                    f"{metric_name}_se"
                                ]
                            ),

                        "one_se_threshold":
                            np.nan,

                        "selected_n":
                            final_n,

                        "selected_metric":
                            float(
                                perf_df.loc[
                                    best_idx,
                                    metric_name
                                ]
                            )
                    }


                final_features = (
                    sorted_features[
                        :final_n
                    ]
                )


                ################################################################
                ### 9.10 Save performance records ####
                ################################################################

                perf_df.to_csv(
                    f"{stage2_res_path}/"
                    f"perf_record.csv",
                    index=False
                )


                fold_perf_df.to_csv(
                    f"{stage2_res_path}/"
                    f"perf_record_folds.csv",
                    index=False
                )


                selection_summary = pd.DataFrame(
                    [
                        {
                            "c_type":
                                c_type,

                            "target":
                                target,

                            "view":
                                view,

                            "selection_metric":
                                metric_name,

                            "selection_rule":
                                stage2_selection_rule,

                            "best_n_by_max":
                                selection[
                                    "best_n_by_max"
                                ],

                            "best_metric":
                                selection[
                                    "best_metric"
                                ],

                            "best_se":
                                selection[
                                    "best_se"
                                ],

                            "one_se_threshold":
                                selection[
                                    "one_se_threshold"
                                ],

                            "selected_n":
                                selection[
                                    "selected_n"
                                ],

                            "selected_metric":
                                selection[
                                    "selected_metric"
                                ],

                            "total_positive_importance_features":
                                len(
                                    sorted_features
                                )
                        }
                    ]
                )


                selection_summary.to_csv(
                    f"{stage2_res_path}/"
                    f"selection_summary.csv",
                    index=False
                )


                ################################################################
                ### 9.11 Save final feature list ####
                ################################################################

                final_feature_df = pd.DataFrame(
                    {
                        "Feature":
                            final_features,

                        "Rank":
                            np.arange(
                                1,
                                len(final_features) + 1
                            ),

                        "Importance":
                            [
                                fi_dict[
                                    feature
                                ]
                                for feature in final_features
                            ]
                    }
                )


                final_feature_df.to_csv(
                    f"{stage2_res_path}/"
                    f"feature_list.csv",
                    index=False
                )


                preprocess_lineage = read_feature_lineage(
                    f"{dat_dir}/{c_type}_{target}_{view}/"
                    f"feature_lineage_preprocess.csv"
                )


                stage2_trace = build_stage2_feature_selection_trace(
                    lineage=preprocess_lineage,
                    all_processed_features=feature_names,
                    feature_importance=fi_dict,
                    candidate_features=sorted_features,
                    final_features=final_features,
                    view=view,
                )


                stage2_trace.to_csv(
                    f"{stage2_res_path}/"
                    f"feature_selection_trace.csv",
                    index=False,
                )


                if internal_selection_records:
                    internal_selection_df = pd.DataFrame(
                        internal_selection_records
                    )
                    internal_features = (
                        internal_selection_df["Micro_feature"]
                        .astype(str)
                        .drop_duplicates()
                        .tolist()
                    )
                    internal_metadata = active_feature_lineage(
                        preprocess_lineage,
                        internal_features,
                        view=view,
                    )[[
                        "Micro_feature",
                        "View",
                        "Large_factor",
                        "Small_factor",
                        "Macro_feature",
                    ]]
                    internal_selection_df = internal_selection_df.merge(
                        internal_metadata,
                        on="Micro_feature",
                        how="left",
                    )
                    internal_selection_df.to_csv(
                        f"{stage2_res_path}/"
                        f"feature_selection_stage2_internal.csv",
                        index=False,
                    )


                ################################################################
                ### 9.12 Export final independent train/test subset ####
                ###
                ### IMPORTANT:
                ###
                ### Independent TEST has NOT been used for selecting N.
                ################################################################

                base_cols = [
                    column
                    for column in [
                        "eid",
                        "status",
                        "time"
                    ]
                    if column in
                    train_ind.columns
                ]


                train_keep = (
                    base_cols +
                    [
                        feature
                        for feature in
                        final_features
                        if feature in
                        train_ind.columns
                    ]
                )


                test_keep = (
                    base_cols +
                    [
                        feature
                        for feature in
                        final_features
                        if feature in
                        test_ind.columns
                    ]
                )


                train_out = train_ind[
                    train_keep
                ].copy()


                test_out = test_ind[
                    test_keep
                ].copy()


                train_out.to_csv(
                    f"{stage2_res_path}/"
                    f"train_data.csv",
                    index=False
                )


                test_out.to_csv(
                    f"{stage2_res_path}/"
                    f"test_data.csv",
                    index=False
                )


                ################################################################
                ### 9.13 Log final result ####
                ################################################################

                LOGGER.info(
                    f"{c_type}, {target}, {view}: "
                    f"best max-{metric_name} N="
                    f"{selection['best_n_by_max']}, "
                    f"best {metric_name}="
                    f"{selection['best_metric']:.4f}, "
                    f"SE="
                    f"{selection['best_se']:.4f}"
                )


                LOGGER.info(
                    f"{c_type}, {target}, {view}: "
                    f"1-SE threshold="
                    f"{selection['one_se_threshold']:.4f}, "
                    f"selected parsimonious N="
                    f"{selection['selected_n']}, "
                    f"selected {metric_name}="
                    f"{selection['selected_metric']:.4f}"
                )


                del train_ind
                del test_ind
                del fi_df
                del candidate_feature_df
                del perf_df
                del fold_perf_df

                gc.collect()


    LOGGER.info(
        "Stage parallel CoxEM Top-N selection completed."
    )

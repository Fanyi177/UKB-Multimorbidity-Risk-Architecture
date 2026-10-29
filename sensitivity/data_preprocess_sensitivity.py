"""Prepare jointly imputed development and held-out data for all views."""


import argparse
import copy
import os
import pickle
import time

import numpy as np
import pandas as pd
from sklearn.model_selection import (
    RepeatedKFold,
    RepeatedStratifiedKFold,
    train_test_split,
)

from config import (
    balance_event,
    comorbidity_labels,
    comorbidity_targets,
    comorbidity_types,
    couple_feature,
    dat_dir,
    desc_name,
    desc_name_new,
    feat_name_dict,
    fold_type,
    global_missing_threshold,
    independent_test_size,
    label_name,
    local_sample,
    log_dir,
    make_dir,
    n_process,
    n_repeats,
    n_sample,
    n_splits,
    random_state,
    spec_name,
    src_dir,
    use_enviro_2,
    top_k_survival_samples,
    primary_dat_dir,
    preserve_primary_split,
    sensitivity_name,
)
from utils import (
    _drop_high_missing_columns,
    build_preprocess_feature_lineage,
    couple_feature_func,
    get_logger,
    load_desc,
    load_feature_metadata,
    local_sample_func,
    one_hot_process_train_test,
    process_missing_value_v2,
)


JOINT_SEPARATOR = "::"
ID_COL = "eid"
LABEL_COLS = ["status", "time"]
BASE_C_TYPE = "comorbidity"
BASE_TARGET = "all"


def joint_column_name(view, feature):
    """Return a collision-free internal column name for one view feature."""
    return f"{view}{JOINT_SEPARATOR}{feature}"


def prefix_view_frame(frame, view):
    """Prefix one view's columns before concatenating the four views."""
    renamed = {
        feature: joint_column_name(view, feature)
        for feature in frame.columns
    }
    return frame.rename(columns=renamed), renamed


def split_joint_output(joint_data, view_spec):
    """Extract one view from a processed joint matrix and restore its names."""
    inverse_names = {
        joint_name: original_name
        for original_name, joint_name in view_spec["column_names"].items()
    }
    output = joint_data[
        [ID_COL] + LABEL_COLS + view_spec["joint_columns"]
    ].copy()
    return output.rename(columns=inverse_names)


def save_bundle_reference(path, joint_bundle_path, fit_scope, view):
    """Save a lightweight pointer to the shared all-view preprocessing model."""
    reference = {
        "fit_scope": fit_scope,
        "joint_all_view_imputation": True,
        "view": view,
        "joint_preprocess_bundle": joint_bundle_path,
    }
    with open(path, "wb") as file_obj:
        pickle.dump(reference, file_obj)


def save_view_outputs(
    joint_train,
    joint_test,
    view_specs,
    train_filename,
    test_filename,
    feature_map_filename,
    bundle_reference_filename,
    joint_bundle_path,
    fit_scope,
):
    """Split joint data and save backward-compatible files for every view."""
    for view, spec in view_specs.items():
        train_view = split_joint_output(joint_train, spec)
        test_view = split_joint_output(joint_test, spec)

        expected_columns = [ID_COL] + LABEL_COLS + spec["output_columns"]
        if list(train_view.columns) != expected_columns:
            raise ValueError(f"{view}: unexpected processed training columns")
        if list(test_view.columns) != expected_columns:
            raise ValueError(f"{view}: unexpected processed test columns")

        train_view.to_csv(
            os.path.join(spec["dat_path"], train_filename),
            index=False,
        )
        test_view.to_csv(
            os.path.join(spec["dat_path"], test_filename),
            index=False,
        )

        with open(
            os.path.join(spec["dat_path"], feature_map_filename),
            "wb",
        ) as file_obj:
            pickle.dump(spec["feature_map"], file_obj)

        save_bundle_reference(
            os.path.join(spec["dat_path"], bundle_reference_filename),
            joint_bundle_path=joint_bundle_path,
            fit_scope=fit_scope,
            view=view,
        )


def save_processed_fold(
    c_type,
    target,
    task_id,
    indice,
    view_specs,
    joint_bundle_path,
    logger,
):
    """Save fold row indices and metadata without duplicating feature CSVs."""
    start = time.time()
    for view, spec in view_specs.items():
        dat_path = spec["dat_path"]
        np.savez_compressed(
            os.path.join(dat_path, f"fold_indices_{fold_type}_{task_id}.npz"),
            train_idx=indice[0],
            valid_idx=indice[1],
        )
        with open(
            os.path.join(dat_path, f"feature_map_{fold_type}_{task_id}.pkl"),
            "wb",
        ) as file_obj:
            pickle.dump(spec["feature_map"], file_obj)
        save_bundle_reference(
            os.path.join(dat_path, f"preprocess_bundle_{fold_type}_{task_id}.pkl"),
            joint_bundle_path=joint_bundle_path,
            fit_scope="complete_development_reused_for_internal_cv",
            view=view,
        )

    logger.info(
        f"Comorbidity {c_type}, Target {target}, Task {task_id}: "
        "saved fold indices for the shared development-set preprocessing."
    )
    logger.info(
        f"Comorbidity {c_type}, Target {target}, Task {task_id}: "
        f"preprocessing cost {time.time() - start:.4f} seconds."
    )


def make_cv_indices(view_label_train, fixed_longest=False):
    """Create CV folds, optionally keeping longest follow-up in training."""
    if fixed_longest:
        fixed_indices = np.argsort(
            view_label_train["time"].to_numpy(), kind="stable"
        )[-top_k_survival_samples:]
        remaining_indices = np.setdiff1d(
            np.arange(len(view_label_train)), fixed_indices
        )
    else:
        fixed_indices = np.array([], dtype=int)
        remaining_indices = np.arange(len(view_label_train))
    remaining_labels = view_label_train.iloc[remaining_indices]

    if fold_type == "rkf":
        splitter = RepeatedKFold(
            n_splits=n_splits,
            n_repeats=n_repeats,
            random_state=random_state,
        )
        splits = splitter.split(remaining_labels)
    elif fold_type == "rskf":
        splitter = RepeatedStratifiedKFold(
            n_splits=n_splits,
            n_repeats=n_repeats,
            random_state=random_state,
        )
        splits = splitter.split(
            remaining_labels, remaining_labels["status"]
        )
    elif fold_type == "tts":
        splits = [
            train_test_split(
                np.arange(len(remaining_labels)),
                test_size=1.0 / n_splits,
                random_state=random_state + repeat_index,
            )
            for repeat_index in range(n_repeats)
        ]
    else:
        raise ValueError(f"Unknown fold_type: {fold_type}")

    return [
        (
            np.concatenate([remaining_indices[train_idx], fixed_indices]),
            remaining_indices[valid_idx],
        )
        for train_idx, valid_idx in splits
    ]


def _normalise_eid_index(frame):
    """Return a copy whose participant index uses a stable string dtype."""
    frame = frame.copy()
    frame.index = frame.index.astype(str)
    return frame


def _load_base_processed_views():
    """Load the shared all-participant development and test matrices."""
    views = {}
    reference_development = None
    reference_test = None

    for view in feat_name_dict:
        base_path = os.path.join(
            dat_dir,
            f"{BASE_C_TYPE}_{BASE_TARGET}_{view}",
        )
        train_path = os.path.join(
            base_path,
            "independent_train_processed.csv",
        )
        test_path = os.path.join(
            base_path,
            "independent_test_processed.csv",
        )
        feature_map_path = os.path.join(
            base_path,
            "feature_map_independent.pkl",
        )
        lineage_path = os.path.join(
            base_path,
            "feature_lineage_preprocess.csv",
        )
        required_paths = [
            train_path,
            test_path,
            feature_map_path,
            lineage_path,
        ]
        missing_paths = [path for path in required_paths if not os.path.exists(path)]
        if missing_paths:
            raise FileNotFoundError(
                f"Shared preprocessing output is incomplete for {view}: "
                f"{missing_paths}"
            )

        train = pd.read_csv(train_path)
        test = pd.read_csv(test_path)
        for name, frame in (("development", train), ("test", test)):
            if ID_COL not in frame.columns:
                raise ValueError(f"{view} {name} data do not contain {ID_COL}")
            if frame[ID_COL].duplicated().any():
                raise ValueError(f"{view} {name} data contain duplicated eid")
        if list(train.columns) != list(test.columns):
            raise ValueError(f"{view}: development and test columns differ")

        train[ID_COL] = train[ID_COL].astype(str)
        test[ID_COL] = test[ID_COL].astype(str)
        train = train.set_index(ID_COL, drop=False)
        test = test.set_index(ID_COL, drop=False)

        development_ids = list(train.index)
        test_ids = list(test.index)
        if set(development_ids) & set(test_ids):
            raise ValueError(f"{view}: shared development and test overlap")
        if reference_development is None:
            reference_development = development_ids
            reference_test = test_ids
        elif (
            development_ids != reference_development
            or test_ids != reference_test
        ):
            raise ValueError(
                f"{view}: participant order differs from the shared split"
            )

        with open(feature_map_path, "rb") as file_obj:
            feature_map = pickle.load(file_obj)
        lineage = pd.read_csv(lineage_path)
        views[view] = {
            "train": train,
            "test": test,
            "feature_map": feature_map,
            "lineage": lineage,
        }

    return views, reference_development, reference_test


def _filter_feature_map(feature_map, retained_features):
    """Remove target-specific constant micro features from a feature map."""
    retained = set(retained_features)
    filtered = {}
    mapped = set()
    for macro_feature, micro_features in feature_map.items():
        retained_micro = [
            feature for feature in micro_features if feature in retained
        ]
        if retained_micro:
            filtered[macro_feature] = retained_micro
            mapped.update(retained_micro)
    for feature in retained_features:
        if feature not in mapped:
            filtered[feature] = [feature]
    return filtered


def _mark_target_constants(lineage, constant_features):
    """Record target-specific constant removal in a copied lineage table."""
    lineage = lineage.copy()
    if "Micro_feature" not in lineage.columns:
        return lineage
    constant_features = set(constant_features)
    mask = lineage["Micro_feature"].astype(str).isin(constant_features)
    if "After_preprocess" in lineage.columns:
        lineage.loc[mask, "After_preprocess"] = False
    if "Drop_stage" in lineage.columns:
        lineage["Drop_stage"] = lineage["Drop_stage"].astype("object")
        lineage.loc[mask, "Drop_stage"] = "target_subset"
    if "Drop_reason" in lineage.columns:
        lineage["Drop_reason"] = lineage["Drop_reason"].astype("object")
        lineage.loc[mask, "Drop_reason"] = (
            "constant_in_target_development"
        )
    return lineage


def derive_targets_from_all():
    """Derive non-base tasks from the existing all-task preprocessing."""
    base_views, base_development_ids, base_test_ids = (
        _load_base_processed_views()
    )
    base_development_set = set(base_development_ids)
    base_test_set = set(base_test_ids)
    available_ids = base_development_set | base_test_set

    labels = _normalise_eid_index(
        pd.read_csv(os.path.join(src_dir, label_name), index_col=0)
    )
    sex = _normalise_eid_index(
        pd.read_csv(os.path.join(src_dir, spec_name), index_col=0)
    )

    shared_joint_bundle = os.path.join(
        dat_dir,
        f"{BASE_C_TYPE}_{BASE_TARGET}_joint",
        "preprocess_bundle.pkl",
    )
    if not os.path.exists(shared_joint_bundle):
        raise FileNotFoundError(
            f"Shared preprocessing bundle does not exist: {shared_joint_bundle}"
        )

    for c_type in comorbidity_types:
        for target in comorbidity_targets.get(c_type, []):
            if c_type == BASE_C_TYPE and target == BASE_TARGET:
                continue

            logger = get_logger(
                f"{c_type}_{target}_data_derivation",
                log_dir,
            )
            event_column, time_column = comorbidity_labels[c_type]
            target_labels = labels[[event_column, time_column]].rename(
                columns={event_column: "status", time_column: "time"}
            )
            target_labels = target_labels.dropna(subset=LABEL_COLS)
            target_labels = target_labels[target_labels["time"] <= 6100]

            if target == "male":
                target_labels = target_labels.loc[
                    target_labels.index.intersection(
                        sex.index[sex["sex"] == 1]
                    )
                ]
            elif target == "female":
                target_labels = target_labels.loc[
                    target_labels.index.intersection(
                        sex.index[sex["sex"] == 0]
                    )
                ]
            elif target != "all":
                raise ValueError(f"Unknown target: {target}")

            missing_ids = set(target_labels.index) - available_ids
            if missing_ids:
                raise ValueError(
                    f"{c_type}/{target}: {len(missing_ids)} eligible participants "
                    "are absent from the shared comorbidity/all preprocessing."
                )

            top_holdout = target_labels.nlargest(
                top_k_survival_samples,
                "time",
            )
            top_holdout_ids = set(top_holdout.index)
            development_ids = [
                eid
                for eid in base_development_ids
                if eid in target_labels.index
            ]
            development_ids.extend(
                eid
                for eid in base_test_ids
                if eid in top_holdout_ids
            )
            test_ids = [
                eid
                for eid in base_test_ids
                if eid in target_labels.index and eid not in top_holdout_ids
            ]

            development_labels = target_labels.loc[development_ids, LABEL_COLS]
            test_labels = target_labels.loc[test_ids, LABEL_COLS]
            if development_labels.empty or test_labels.empty:
                raise ValueError(f"{c_type}/{target}: empty development or test set")

            top_holdout.to_csv(
                os.path.join(dat_dir, f"{c_type}_{target}_holdout_label.csv"),
                index=True,
            )
            development_labels.to_csv(
                os.path.join(
                    dat_dir,
                    f"{c_type}_{target}_independent_train_label.csv",
                ),
                index=True,
            )
            test_labels.to_csv(
                os.path.join(
                    dat_dir,
                    f"{c_type}_{target}_independent_test_label.csv",
                ),
                index=True,
            )

            indices = make_cv_indices(
                development_labels, fixed_longest=True
            )
            if len(indices) != n_process:
                raise ValueError(
                    f"Configured n_process={n_process}, but generated "
                    f"{len(indices)} folds for {c_type}/{target}."
                )

            for view, base in base_views.items():
                output_path = make_dir(
                    os.path.join(dat_dir, f"{c_type}_{target}_{view}")
                )
                all_features = pd.concat(
                    [base["train"], base["test"]],
                    axis=0,
                )
                feature_columns = [
                    column
                    for column in all_features.columns
                    if column not in [ID_COL] + LABEL_COLS
                ]

                development = all_features.loc[
                    development_ids,
                    [ID_COL] + feature_columns,
                ].copy()
                test = all_features.loc[
                    test_ids,
                    [ID_COL] + feature_columns,
                ].copy()
                development.insert(
                    1,
                    "status",
                    development_labels.loc[development_ids, "status"].to_numpy(),
                )
                development.insert(
                    2,
                    "time",
                    development_labels.loc[development_ids, "time"].to_numpy(),
                )
                test.insert(
                    1,
                    "status",
                    test_labels.loc[test_ids, "status"].to_numpy(),
                )
                test.insert(
                    2,
                    "time",
                    test_labels.loc[test_ids, "time"].to_numpy(),
                )

                constant_features = [
                    column
                    for column in feature_columns
                    if development[column].nunique(dropna=False) <= 1
                ]
                if constant_features:
                    development = development.drop(columns=constant_features)
                    test = test.drop(columns=constant_features)
                retained_features = [
                    column
                    for column in feature_columns
                    if column not in set(constant_features)
                ]
                target_feature_map = _filter_feature_map(
                    base["feature_map"],
                    retained_features,
                )
                target_lineage = _mark_target_constants(
                    base["lineage"],
                    constant_features,
                )

                development.to_csv(
                    os.path.join(
                        output_path,
                        "independent_train_processed.csv",
                    ),
                    index=False,
                )
                test.to_csv(
                    os.path.join(
                        output_path,
                        "independent_test_processed.csv",
                    ),
                    index=False,
                )
                target_lineage.to_csv(
                    os.path.join(output_path, "feature_lineage_preprocess.csv"),
                    index=False,
                )
                pd.DataFrame({"Feature": constant_features}).to_csv(
                    os.path.join(output_path, "target_constant_features.csv"),
                    index=False,
                )
                with open(
                    os.path.join(output_path, "feature_map_independent.pkl"),
                    "wb",
                ) as file_obj:
                    pickle.dump(target_feature_map, file_obj)
                save_bundle_reference(
                    os.path.join(output_path, "preprocess_bundle.pkl"),
                    joint_bundle_path=shared_joint_bundle,
                    fit_scope=(
                        "comorbidity_all_development_reused_for_derived_task"
                    ),
                    view=view,
                )

                for task_id, indice in enumerate(indices):
                    np.savez_compressed(
                        os.path.join(
                            output_path,
                            f"fold_indices_{fold_type}_{task_id}.npz",
                        ),
                        train_idx=indice[0],
                        valid_idx=indice[1],
                    )
                    with open(
                        os.path.join(
                            output_path,
                            f"feature_map_{fold_type}_{task_id}.pkl",
                        ),
                        "wb",
                    ) as file_obj:
                        pickle.dump(target_feature_map, file_obj)
                    save_bundle_reference(
                        os.path.join(
                            output_path,
                            f"preprocess_bundle_{fold_type}_{task_id}.pkl",
                        ),
                        joint_bundle_path=shared_joint_bundle,
                        fit_scope=(
                            "comorbidity_all_development_reused_for_derived_task"
                        ),
                        view=view,
                    )

                logger.info(
                    f"Derived {view}: development={len(development)}, "
                    f"test={len(test)}, removed constants={len(constant_features)}."
                )

            logger.info(
                f"Derived {c_type}/{target} from shared comorbidity/all "
                "preprocessing without refitting transformers."
            )


def load_aligned_view_data(label_index, view, feature_filename):
    """Load one source view and align labelled participants by a normalized eid."""

    source = pd.read_csv(
        os.path.join(src_dir, feature_filename),
        index_col=0,
        low_memory=False,
    )

    # ------------------------------------------------------------
    # IMPORTANT:
    # All participant identifiers must use the same dtype.
    #
    # Primary development/held-out IDs are normalized to string in
    # load_primary_split_for_sensitivity(), so the feature-source
    # indices and requested label indices must also be strings.
    # Otherwise pandas.reindex() silently returns all-NA rows.
    # ------------------------------------------------------------
    source.index = source.index.astype(str)
    label_index = pd.Index(label_index).astype(str)

    if source.index.has_duplicates:
        raise ValueError(
            f"{view}: duplicated eid in {feature_filename}"
        )

    if source.columns.has_duplicates:
        raise ValueError(
            f"{view}: duplicated feature names in {feature_filename}"
        )

    # QA: do not allow an eid-type/alignment problem to silently
    # create a completely missing predictor matrix again.
    n_overlap = int(label_index.isin(source.index).sum())

    if n_overlap == 0:
        raise ValueError(
            f"{view}: zero eid overlap between labels and "
            f"{feature_filename}. "
            f"Check eid dtype/index alignment."
        )

    aligned = source.reindex(label_index)

    # A view may legitimately have incomplete participant coverage,
    # but it must not become completely empty after alignment.
    if aligned.notna().any(axis=1).sum() == 0:
        raise ValueError(
            f"{view}: all predictor values are missing after eid alignment."
        )

    return aligned


def build_availability_table(feature_data_by_view, sample_index):
    """Record whether each participant has measured data in every raw view."""
    table = pd.DataFrame({ID_COL: sample_index}, index=sample_index)
    for view, frame in feature_data_by_view.items():
        aligned = frame.reindex(sample_index)
        table[f"{view}_available"] = aligned.notna().any(axis=1).astype(int)
        table[f"{view}_observed_features"] = aligned.notna().sum(axis=1)
    return table.reset_index(drop=True)


def prepare_joint_features(
    task_name,
    label_data,
    all_train_label,
    test_label,
    variable2unit,
    variable2parent,
    feature_metadata,
    logger,
):
    """Load, filter and encode all views before fitting one joint imputer."""
    feature_data_by_view = {}
    for view, feature_filename in feat_name_dict.items():
        logger.info(f"Load and align {view} from {feature_filename}...")
        feature_data_by_view[view] = load_aligned_view_data(
            label_data.index,
            view,
            feature_filename,
        )

    train_prefixed_parts = []
    test_prefixed_parts = []
    joint_feature_map = {}
    categorical_groups = {}
    view_specs = {}

    for view, feature_data in feature_data_by_view.items():
        dat_path = make_dir(f"{dat_dir}/{task_name}_{view}")
        train_raw = feature_data.reindex(all_train_label.index).copy()
        test_raw = feature_data.reindex(test_label.index).copy()

        if view == "enviro" and couple_feature:
            train_raw = couple_feature_func(
                logger,
                train_raw,
                variable2unit,
                variable2parent,
            )
            test_raw = couple_feature_func(
                logger,
                test_raw,
                variable2unit,
                variable2parent,
            )

        raw_feature_columns = list(train_raw.columns)
        raw_missing_ratio = train_raw.isna().mean()
        raw_unique_counts = train_raw.nunique(dropna=False)

        train_raw, test_raw, dropped_macro_features = (
            _drop_high_missing_columns(
                logger,
                train_raw,
                test_raw,
                threshold=global_missing_threshold,
            )
        )

        categorical_macros = []
        if view == "enviro":
            categorical_macros = [
                column
                for column in train_raw.columns
                if pd.api.types.is_object_dtype(train_raw[column])
                or pd.api.types.is_categorical_dtype(train_raw[column])
                or pd.api.types.is_string_dtype(train_raw[column])
            ]
            train_encoded, test_encoded, feature_map = (
                one_hot_process_train_test(
                    train_raw,
                    test_raw,
                    drop_single=True,
                    preserve_missing=True,
                )
            )
            logger.info(
                f"After one-hot, {view} train shape: {train_encoded.shape}"
            )
            logger.info(
                f"After one-hot, {view} test shape: {test_encoded.shape}"
            )
        else:
            train_encoded = train_raw.copy()
            test_encoded = test_raw.copy()
            feature_map = {
                column: [column]
                for column in train_encoded.columns
            }

        one_hot_feature_map = copy.deepcopy(feature_map)
        train_prefixed, column_names = prefix_view_frame(train_encoded, view)
        test_prefixed = test_encoded.rename(columns=column_names)
        train_prefixed_parts.append(train_prefixed)
        test_prefixed_parts.append(test_prefixed)

        for macro_feature, micro_features in feature_map.items():
            joint_macro = joint_column_name(view, macro_feature)
            joint_feature_map[joint_macro] = [
                column_names[micro_feature]
                for micro_feature in micro_features
            ]

        for macro_feature in categorical_macros:
            if macro_feature not in feature_map:
                continue
            joint_macro = joint_column_name(view, macro_feature)
            categorical_groups[joint_macro] = [
                column_names[micro_feature]
                for micro_feature in feature_map[macro_feature]
            ]

        feature_lineage = build_preprocess_feature_lineage(
            view=view,
            raw_feature_columns=raw_feature_columns,
            feature_metadata=feature_metadata,
            missing_ratio=raw_missing_ratio,
            missing_threshold=global_missing_threshold,
            unique_counts=raw_unique_counts,
            dropped_missing_features=dropped_macro_features,
            one_hot_feature_map=one_hot_feature_map,
            final_feature_map=feature_map,
        )
        feature_lineage.to_csv(
            os.path.join(dat_path, "feature_lineage_preprocess.csv"),
            index=False,
        )

        view_specs[view] = {
            "dat_path": dat_path,
            "feature_map": feature_map,
            "column_names": column_names,
            "joint_columns": list(train_prefixed.columns),
            "output_columns": list(train_encoded.columns),
        }

    train_features = pd.concat(train_prefixed_parts, axis=1)
    test_features = pd.concat(test_prefixed_parts, axis=1)

    train_joint = pd.concat(
        [all_train_label[LABEL_COLS], train_features],
        axis=1,
    )
    test_joint = pd.concat(
        [test_label[LABEL_COLS], test_features],
        axis=1,
    )
    train_joint.insert(0, ID_COL, train_joint.index)
    test_joint.insert(0, ID_COL, test_joint.index)

    availability_train = build_availability_table(
        feature_data_by_view,
        all_train_label.index,
    )
    availability_test = build_availability_table(
        feature_data_by_view,
        test_label.index,
    )

    return (
        train_joint,
        test_joint,
        joint_feature_map,
        categorical_groups,
        view_specs,
        availability_train,
        availability_test,
    )


def load_primary_split_for_sensitivity(label_data, logger):
    """Restrict the sensitivity cohort to the original enviro-2 60/40 assignment.

    No primary development participant is moved into held-out.  No primary held-out
    participant is moved into development for the base overall outcome.  This keeps
    the main-analysis split as the reference assignment; the existing derived-task
    logic may still move its small longest-follow-up support set from held-out to
    development exactly as in the primary pipeline.
    """
    if not preserve_primary_split:
        raise RuntimeError(
            "Sensitivity analyses are configured to preserve the primary split."
        )

    train_file = os.path.join(
        primary_dat_dir,
        f"{BASE_C_TYPE}_{BASE_TARGET}_independent_train_label.csv",
    )
    test_file = os.path.join(
        primary_dat_dir,
        f"{BASE_C_TYPE}_{BASE_TARGET}_independent_test_label.csv",
    )
    if not os.path.exists(train_file) or not os.path.exists(test_file):
        raise FileNotFoundError(
            f"Primary enviro-2 split files are missing: {train_file} / {test_file}"
        )

    primary_train = _normalise_eid_index(pd.read_csv(train_file, index_col=0))
    primary_test = _normalise_eid_index(pd.read_csv(test_file, index_col=0))
    label_data = _normalise_eid_index(label_data)

    primary_train_ids = list(primary_train.index)
    primary_test_ids = list(primary_test.index)
    primary_all = set(primary_train_ids) | set(primary_test_ids)

    if set(primary_train_ids) & set(primary_test_ids):
        raise ValueError("Primary development and held-out IDs overlap.")

    eligible_ids = set(label_data.index)
    outside_primary = eligible_ids - primary_all
    if outside_primary:
        logger.info(
            f"{sensitivity_name}: drop {len(outside_primary)} sensitivity-eligible "
            "participants who were not in the primary enviro-2 analysis split "
            "(for example because primary overall follow-up exceeded 6100 days)."
        )

    development_ids = [eid for eid in primary_train_ids if eid in eligible_ids]
    test_ids = [eid for eid in primary_test_ids if eid in eligible_ids]

    if not development_ids or not test_ids:
        raise ValueError(
            f"{sensitivity_name}: empty development or held-out set after "
            "intersecting with the primary split."
        )

    all_train_label = label_data.loc[development_ids, LABEL_COLS].copy()
    test_label = label_data.loc[test_ids, LABEL_COLS].copy()

    split_audit = pd.DataFrame(
        [{
            "sensitivity": sensitivity_name,
            "primary_development_N": len(primary_train_ids),
            "primary_heldout_N": len(primary_test_ids),
            "eligible_after_sensitivity_and_time_cap_N": len(label_data),
            "development_N": len(all_train_label),
            "heldout_N": len(test_label),
            "eligible_not_in_primary_split_N": len(outside_primary),
            "development_events": int(all_train_label["status"].sum()),
            "heldout_events": int(test_label["status"].sum()),
            "primary_split_preserved": True,
        }]
    )
    split_audit.to_csv(
        os.path.join(dat_dir, "sensitivity_split_audit.csv"),
        index=False,
    )

    logger.info(
        f"Sensitivity split preserved from primary enviro-2: "
        f"development={len(all_train_label)}, held-out={len(test_label)}."
    )
    return all_train_label, test_label


def run_preprocessing():
    """Fit preprocessing once for the shared comorbidity/all population."""
    variable2unit, variable2parent = load_desc(
        src_dir,
        desc_name,
        desc_name_new,
        output_dir=dat_dir if use_enviro_2 else src_dir,
    )
    feature_metadata = load_feature_metadata(src_dir, desc_name)
    sex_data = _normalise_eid_index(
        pd.read_csv(
            os.path.join(src_dir, spec_name),
            index_col=0,
            low_memory=False,
        )
    )

    for c_type in comorbidity_types:
        if c_type != BASE_C_TYPE:
            continue
        label_data_source = _normalise_eid_index(
            pd.read_csv(
                os.path.join(src_dir, label_name),
                index_col=0,
                low_memory=False,
            )[comorbidity_labels[c_type]]
        )
        label_data_source.columns = LABEL_COLS

        for target in comorbidity_targets[c_type]:
            if target != BASE_TARGET:
                continue
            logger = get_logger(
                f"{c_type}_{target}_data_preprocess",
                log_dir,
            )
            np.random.seed(random_state)

            label_data = label_data_source[
                label_data_source["time"] <= 6100
            ].copy()

            if target == "male":
                label_data = label_data[sex_data["sex"] == 1]
            elif target == "female":
                label_data = label_data[sex_data["sex"] == 0]
            elif target != "all":
                raise ValueError(f"Unknown target: {target}")

            if local_sample:
                sampled_indices = local_sample_func(
                    dat_dir,
                    c_type,
                    target,
                    n_sample,
                    balance_event,
                    label_data,
                )
                label_data = label_data.iloc[sampled_indices]
                logger.info(
                    f"After local sample, data num: {label_data.shape[0]}"
                )

            # Sensitivity analyses start from the exact primary enviro-2 60/40
            # assignment rather than drawing a new random split.
            all_train_label, test_label = load_primary_split_for_sensitivity(
                label_data,
                logger,
            )

            # Retain a compatibility file.  These participants are already part of
            # the primary development set; no held-out participant is moved here.
            top_holdout = all_train_label.nlargest(
                min(top_k_survival_samples, len(all_train_label)),
                "time",
            )
            top_holdout.to_csv(
                f"{dat_dir}/{c_type}_{target}_holdout_label.csv",
                index=True,
            )
            all_train_label.to_csv(
                f"{dat_dir}/{c_type}_{target}_independent_train_label.csv",
                index=True,
            )
            test_label.to_csv(
                f"{dat_dir}/{c_type}_{target}_independent_test_label.csv",
                index=True,
            )

            task_name = f"{c_type}_{target}"
            joint_dat_path = make_dir(f"{dat_dir}/{task_name}_joint")

            (
                train_joint,
                test_joint,
                joint_feature_map,
                categorical_groups,
                view_specs,
                availability_train,
                availability_test,
            ) = prepare_joint_features(
                task_name,
                label_data,
                all_train_label,
                test_label,
                variable2unit,
                variable2parent,
                feature_metadata,
                logger,
            )

            availability_train.to_csv(
                os.path.join(
                    joint_dat_path,
                    "view_availability_independent_train.csv",
                ),
                index=False,
            )
            availability_test.to_csv(
                os.path.join(
                    joint_dat_path,
                    "view_availability_independent_test.csv",
                ),
                index=False,
            )

            train_processed, test_processed, _, preprocess_bundle = (
                process_missing_value_v2(
                    logger,
                    train_joint,
                    test_joint,
                    feature_map=joint_feature_map,
                    seed=random_state,
                    categorical_groups=categorical_groups,
                )
            )
            preprocess_bundle.update(
                {
                    "fit_scope": "complete_development",
                    "transform_scope": "held_out_test",
                    "joint_all_view_imputation": True,
                    "views": list(view_specs),
                    "n_fit_samples": int(train_processed.shape[0]),
                    "n_transform_samples": int(test_processed.shape[0]),
                }
            )

            joint_bundle_path = os.path.join(
                joint_dat_path,
                "preprocess_bundle.pkl",
            )
            with open(joint_bundle_path, "wb") as file_obj:
                pickle.dump(preprocess_bundle, file_obj)

            with open(
                os.path.join(joint_dat_path, "feature_map_joint.pkl"),
                "wb",
            ) as file_obj:
                pickle.dump(joint_feature_map, file_obj)

            save_view_outputs(
                train_processed,
                test_processed,
                view_specs,
                train_filename="independent_train_processed.csv",
                test_filename="independent_test_processed.csv",
                feature_map_filename="feature_map_independent.pkl",
                bundle_reference_filename="preprocess_bundle.pkl",
                joint_bundle_path=joint_bundle_path,
                fit_scope="complete_development",
            )

            indices = make_cv_indices(train_processed[LABEL_COLS])

            if len(indices) != n_process:
                raise ValueError(
                    f"Configured n_process={n_process}, but generated "
                    f"{len(indices)} folds."
                )

            task_args = [
                (
                    c_type,
                    target,
                    task_id,
                    indices[task_id],
                    view_specs,
                    joint_bundle_path,
                    logger,
                )
                for task_id in range(n_process)
            ]

            logger.info(
                "Split the once-preprocessed development set into CV folds..."
            )
            for args in task_args:
                save_processed_fold(*args)

            logger.info(
                f"Comorbidity {c_type}, Population {target}: "
                "joint all-view preprocessing finished."
            )


def parse_args():
    """Parse preprocessing mode from the command line."""
    parser = argparse.ArgumentParser(
        description=(
            "Fit shared preprocessing once and derive every target dataset."
        )
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--all-only",
        action="store_true",
        help="Fit and save only the shared comorbidity/all preprocessing.",
    )
    mode.add_argument(
        "--derive-only",
        action="store_true",
        help=(
            "Reuse existing comorbidity/all preprocessing and generate "
            "male, female and pattern datasets without changing all-task folds."
        ),
    )
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    if not args.derive_only:
        run_preprocessing()
    if not args.all_only:
        derive_targets_from_all()

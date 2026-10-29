"""Provide preprocessing, feature-lineage, logging, and evaluation helpers."""

import logging
import pickle
import sys
import os.path
import time
import warnings
from contextlib import redirect_stdout

import numpy as np
import pandas as pd
from config import (
    iterative_max_depth, iterative_max_iter, iterative_n_estimators,
    iterative_n_jobs, iterative_n_nearest_features,
    iterative_progress_every, iterative_skip_complete, iterative_tol,
    iterative_verbose,
    preprocess_add_missing_indicator, preprocess_impute_method,
    preprocess_scale_order,
)
from sklearn.experimental import enable_iterative_imputer  # noqa: F401
from sklearn.exceptions import ConvergenceWarning
from sklearn.impute import IterativeImputer, SimpleImputer
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.ensemble import RandomForestRegressor


def load_processed_fold(dat_path, fold_type, task_id):
    """Load a CV fold from saved row indices or legacy fold CSV files."""
    index_file = os.path.join(
        dat_path, f"fold_indices_{fold_type}_{task_id}.npz"
    )
    if os.path.exists(index_file):
        development = pd.read_csv(
            os.path.join(dat_path, "independent_train_processed.csv")
        )
        with np.load(index_file) as fold_indices:
            train_idx = fold_indices["train_idx"]
            valid_idx = fold_indices["valid_idx"]
        train = development.iloc[train_idx].reset_index(drop=True)
        valid = development.iloc[valid_idx].reset_index(drop=True)
        return train, valid

    train = pd.read_csv(
        os.path.join(dat_path, f"train_data_{fold_type}_{task_id}.csv")
    )
    valid = pd.read_csv(
        os.path.join(dat_path, f"test_data_{fold_type}_{task_id}.csv")
    )
    return train, valid


class _LoggerStream:
    """Forward line-oriented standard output to a project logger."""

    def __init__(self, logger):
        self.logger = logger
        self.buffer = ""

    def write(self, message):
        self.buffer += str(message)
        while "\n" in self.buffer:
            line, self.buffer = self.buffer.split("\n", 1)
            if line.strip():
                self.logger.info(line.rstrip())

    def flush(self):
        if self.buffer.strip():
            self.logger.info(self.buffer.rstrip())
        self.buffer = ""


class ProgressIterativeImputer(IterativeImputer):
    """Add variable-level progress logging to IterativeImputer."""

    def configure_progress(self, logger, every=10):
        """Configure the destination logger and reporting interval."""
        self._progress_logger_name = logger.name
        self._progress_every = max(1, int(every))
        return self

    def _start_progress(self, phase):
        """Reset progress counters for fitting or transformation."""
        self._progress_phase = phase
        self._progress_completed = 0
        self._progress_started = time.monotonic()

    def _finish_progress(self):
        """Disable progress reporting after the current operation."""
        self._progress_phase = None

    @staticmethod
    def _format_duration(seconds):
        """Format elapsed seconds as a compact hours-minutes-seconds string."""
        seconds = max(0, int(seconds))
        hours, remainder = divmod(seconds, 3600)
        minutes, seconds = divmod(remainder, 60)
        return f"{hours:02d}:{minutes:02d}:{seconds:02d}"

    def fit_transform(self, X, y=None, **fit_params):
        """Fit and transform while reporting completed target variables."""
        self._start_progress("fit")
        try:
            return super().fit_transform(X, y, **fit_params)
        finally:
            self._finish_progress()

    def transform(self, X):
        """Transform while reporting completed target variables."""
        self._start_progress("transform")
        try:
            return super().transform(X)
        finally:
            self._finish_progress()

    def _impute_one_feature(self, *args, **kwargs):
        """Delegate one feature update and emit periodic progress messages."""
        result = super()._impute_one_feature(*args, **kwargs)
        phase = getattr(self, "_progress_phase", None)
        logger_name = getattr(self, "_progress_logger_name", None)
        if phase is None or logger_name is None:
            return result

        self._progress_completed += 1
        per_round = int(getattr(self, "n_features_with_missing_", 0))
        if per_round <= 0:
            return result

        if phase == "fit":
            total = per_round * int(self.max_iter)
        else:
            total = len(getattr(self, "imputation_sequence_", []))
        if total <= 0:
            return result

        completed = self._progress_completed
        completed_in_round = ((completed - 1) % per_round) + 1
        round_index = ((completed - 1) // per_round) + 1
        should_report = (
            completed_in_round % self._progress_every == 0
            or completed_in_round == per_round
        )
        if should_report:
            elapsed = time.monotonic() - self._progress_started
            eta = elapsed / completed * max(total - completed, 0)
            logging.getLogger(logger_name).info(
                "[IterativeImputer] %s round %d: %d/%d variables "
                "completed (%.1f%% overall), elapsed %s, ETA %s",
                phase,
                round_index,
                completed_in_round,
                per_round,
                100.0 * completed / total,
                self._format_duration(elapsed),
                self._format_duration(eta),
            )
        return result



def get_logger(name, path):
    """Create a file-and-console logger without duplicate handlers."""
    logger = logging.getLogger(name)
    logger.setLevel(level=logging.INFO)
    logger.propagate = False

    # 防止反复创建同名 logger 时 handler 不断累积
    if logger.handlers:
        for handler in logger.handlers:
            try:
                handler.close()
            except Exception:
                pass
        logger.handlers.clear()

    formatter = logging.Formatter(
        '%(asctime)s - %(name)s -  %(message)s'
    )

    file_handler = logging.FileHandler(
        f"{path}/{name}.txt",
        mode='w'
    )
    file_handler.setLevel(logging.INFO)
    file_handler.setFormatter(formatter)

    console_handler = logging.StreamHandler(sys.stderr)
    console_handler.setLevel(logging.INFO)
    console_handler.setFormatter(formatter)

    logger.addHandler(file_handler)
    logger.addHandler(console_handler)

    return logger


def extract_logic_miss(row):
    """Parse a codebook row into its logical parent and activation value."""
    if pd.isna(row["是否逻辑缺失"]):  # 如果值为 None/NaN
        return pd.Series({"Parent": None, "Activate": None})
    else:
        # 匹配 "ID:Variables=value"，提取 Variables 和 value
        parts = row["是否逻辑缺失"].split(":")[1].split("=")
        parent = parts[0]
        activate = int(parts[1]) if ',' not in parts[1] else parts[1].split("[")[1].split("]")[0]
        return pd.Series({"Parent": parent, "Activate": activate})


def load_desc(src_dir, desc_name, desc_name_new, output_dir=None):
    # 读取数据描述
    """Load variable types and logical-dependency mappings from the codebook."""
    desc = pd.read_excel(f"{src_dir}/{desc_name}", sheet_name='enviroment_factor', engine='openpyxl')

    # 处理“是否逻辑缺失列
    desc[["Parent", "Activate"]] = desc.apply(extract_logic_miss, axis=1)
    output_dir = src_dir if output_dir is None else output_dir
    desc.to_csv(
        f"{output_dir}/{desc_name_new}",
        encoding="gbk",
    )

    # 定义两个字典，分别记录  特征ID->特征名称，特征名称->变量类型(连续or离散)
    variable2unit = dict(zip(desc['Variables'], desc['类型']))
    with open(f"{output_dir}/variable2unit.pkl", 'wb') as f:
        pickle.dump(variable2unit, f)

    # 第三个字典用于指明，对于每个特征名称（Variables列），它的逻辑关联特征名称（Parent列）是什么，当该关联特征取什么值（Activate列）时本特征有意义。
    # Parent列和Activate列是根据范师兄提供的Coding book手动添加上去的。
    # 2025/4/26更新，Parent和Activate列皆有范师兄提供的CodeBook
    variable2parent = {var: [parent, activate] for var, parent, activate in zip(desc['Variables'], desc['Parent'], desc['Activate']) if pd.notna(parent)}
    with open(f"{output_dir}/variable2parent.pkl", 'wb') as f:
        pickle.dump(variable2parent, f)

    return variable2unit, variable2parent


FEATURE_LINEAGE_COLUMNS = [
    "View",
    "Large_factor",
    "Small_factor",
    "Macro_feature",
    "Micro_feature",
    "One_hot_level",
    "Variable_type",
    "Logical_parent",
    "Activate",
    "Metadata_status",
    "Train_missing_ratio",
    "Missing_threshold",
    "Train_unique_count",
    "After_missing_filter",
    "After_one_hot",
    "After_preprocess",
    "Drop_stage",
    "Drop_reason",
]


def load_feature_metadata(src_dir, desc_name):
    """Load factor, type, and logical metadata for environmental variables."""
    desc = pd.read_excel(
        f"{src_dir}/{desc_name}",
        sheet_name="enviroment_factor",
        engine="openpyxl",
    )
    desc[["Parent", "Activate"]] = desc.apply(extract_logic_miss, axis=1)

    metadata = desc[[
        "Variables",
        "类型",
        "小维度",
        "大维度",
        "Parent",
        "Activate",
    ]].copy()
    metadata = metadata[metadata["Variables"].notna()].copy()
    metadata = metadata.drop_duplicates(subset=["Variables"], keep="first")
    metadata = metadata.rename(columns={
        "Variables": "Macro_feature",
        "类型": "Variable_type",
        "小维度": "Small_factor",
        "大维度": "Large_factor",
        "Parent": "Logical_parent",
    })
    return metadata.reset_index(drop=True)


def _one_hot_level(macro_feature, micro_feature):
    """Extract the encoded category level from a macro-to-micro feature name."""
    if not micro_feature or micro_feature == macro_feature:
        return np.nan
    prefix = f"{macro_feature}="
    if str(micro_feature).startswith(prefix):
        return str(micro_feature)[len(prefix):]
    if str(micro_feature) == f"{macro_feature}__missing":
        return "missing_indicator"
    return np.nan


def build_preprocess_feature_lineage(
    view,
    raw_feature_columns,
    feature_metadata,
    missing_ratio,
    missing_threshold,
    unique_counts,
    dropped_missing_features,
    one_hot_feature_map,
    final_feature_map,
):
    """Build a complete audit table for preprocessing retention and removal."""
    metadata_lookup = (
        feature_metadata.set_index("Macro_feature").to_dict("index")
        if feature_metadata is not None and not feature_metadata.empty
        else {}
    )
    dropped_missing = set(dropped_missing_features)
    final_micro_by_macro = {
        macro: set(micros)
        for macro, micros in final_feature_map.items()
    }

    rows = []
    for macro in raw_feature_columns:
        meta = metadata_lookup.get(macro, {}) if view == "enviro" else {}
        common = {
            "View": view,
            "Large_factor": meta.get("Large_factor", np.nan),
            "Small_factor": meta.get("Small_factor", np.nan),
            "Macro_feature": macro,
            "Variable_type": meta.get("Variable_type", np.nan),
            "Logical_parent": meta.get("Logical_parent", np.nan),
            "Activate": meta.get("Activate", np.nan),
            "Metadata_status": (
                "mapped" if view == "enviro" and macro in metadata_lookup
                else (
                    "unmapped_in_codebook"
                    if view == "enviro"
                    else "not_applicable_non_enviro"
                )
            ),
            "Train_missing_ratio": float(missing_ratio.get(macro, np.nan)),
            "Missing_threshold": float(missing_threshold),
            "Train_unique_count": int(unique_counts.get(macro, 0)),
        }

        if macro in dropped_missing:
            rows.append({
                **common,
                "Micro_feature": np.nan,
                "One_hot_level": np.nan,
                "After_missing_filter": False,
                "After_one_hot": False,
                "After_preprocess": False,
                "Drop_stage": "missing_filter",
                "Drop_reason": "missing_ratio_above_threshold",
            })
            continue

        if macro not in one_hot_feature_map:
            rows.append({
                **common,
                "Micro_feature": np.nan,
                "One_hot_level": np.nan,
                "After_missing_filter": True,
                "After_one_hot": False,
                "After_preprocess": False,
                "Drop_stage": "one_hot",
                "Drop_reason": "single_training_category",
            })
            continue

        for micro in one_hot_feature_map[macro]:
            retained = micro in final_micro_by_macro.get(macro, set())
            rows.append({
                **common,
                "Micro_feature": micro,
                "One_hot_level": _one_hot_level(macro, micro),
                "After_missing_filter": True,
                "After_one_hot": True,
                "After_preprocess": retained,
                "Drop_stage": np.nan if retained else "preprocess",
                "Drop_reason": np.nan if retained else "removed_during_preprocess",
            })

    mapped_final_micro = {
        micro
        for micros in final_micro_by_macro.values()
        for micro in micros
    }
    recorded_micro = {
        row["Micro_feature"]
        for row in rows
        if pd.notna(row["Micro_feature"])
    }
    for micro in sorted(mapped_final_micro - recorded_micro):
        macro = (
            str(micro)[:-len("__missing")]
            if str(micro).endswith("__missing")
            else str(micro)
        )
        meta = metadata_lookup.get(macro, {}) if view == "enviro" else {}
        rows.append({
            "View": view,
            "Large_factor": meta.get("Large_factor", np.nan),
            "Small_factor": meta.get("Small_factor", np.nan),
            "Macro_feature": macro,
            "Micro_feature": micro,
            "One_hot_level": _one_hot_level(macro, micro),
            "Variable_type": meta.get("Variable_type", np.nan),
            "Logical_parent": meta.get("Logical_parent", np.nan),
            "Activate": meta.get("Activate", np.nan),
            "Metadata_status": (
                "mapped" if view == "enviro" and macro in metadata_lookup
                else (
                    "unmapped_in_codebook"
                    if view == "enviro"
                    else "not_applicable_non_enviro"
                )
            ),
            "Train_missing_ratio": float(missing_ratio.get(macro, np.nan)),
            "Missing_threshold": float(missing_threshold),
            "Train_unique_count": int(unique_counts.get(macro, 0)),
            "After_missing_filter": True,
            "After_one_hot": False,
            "After_preprocess": True,
            "Drop_stage": np.nan,
            "Drop_reason": np.nan,
        })

    return pd.DataFrame(rows, columns=FEATURE_LINEAGE_COLUMNS)


def read_feature_lineage(path):
    """Read a lineage CSV or return an empty table with the expected schema."""
    if not os.path.exists(path):
        return pd.DataFrame(columns=FEATURE_LINEAGE_COLUMNS)
    return pd.read_csv(path)


def active_feature_lineage(lineage, features, view=None):
    """Return one active lineage row for every requested processed feature."""
    features = [str(feature) for feature in features]
    if lineage is None or lineage.empty:
        return pd.DataFrame({
            "View": view,
            "Large_factor": np.nan,
            "Small_factor": np.nan,
            "Macro_feature": features,
            "Micro_feature": features,
        })

    after_preprocess = lineage["After_preprocess"].map(
        lambda value: value is True or str(value).strip().lower() == "true"
    )
    active = lineage[
        after_preprocess & lineage["Micro_feature"].notna()
    ].copy()
    active["Micro_feature"] = active["Micro_feature"].astype(str)
    active = active.drop_duplicates(subset=["Micro_feature"], keep="first")
    active = active.set_index("Micro_feature", drop=False)

    rows = []
    for feature in features:
        if feature in active.index:
            rows.append(active.loc[feature].to_dict())
        else:
            rows.append({
                "View": view,
                "Large_factor": np.nan,
                "Small_factor": np.nan,
                "Macro_feature": feature,
                "Micro_feature": feature,
                "Metadata_status": "missing_from_lineage",
            })
    return pd.DataFrame(rows)


def build_stage1_feature_selection_trace(
    lineage,
    feature_names,
    feature_importances,
    feature_indices,
    view=None,
):
    """Summarize base-learner selections without modifying the fitted model."""
    feature_names = [str(feature) for feature in feature_names]
    trace = active_feature_lineage(lineage, feature_names, view=view)

    selected_by = {feature: [] for feature in feature_names}
    for estimator_id, selected_features in enumerate(feature_indices):
        for feature in selected_features:
            feature = str(feature)
            if feature in selected_by:
                selected_by[feature].append(estimator_id)

    importance_lookup = {
        feature: float(importance)
        for feature, importance in zip(feature_names, feature_importances)
    }
    n_estimators = len(feature_indices)
    trace["Stage1_selected"] = trace["Micro_feature"].map(
        lambda feature: len(selected_by.get(str(feature), [])) > 0
    )
    trace["Selected_base_learners"] = trace["Micro_feature"].map(
        lambda feature: ";".join(
            str(i) for i in selected_by.get(str(feature), [])
        )
    )
    trace["N_base_learners_selected"] = trace["Micro_feature"].map(
        lambda feature: len(selected_by.get(str(feature), []))
    )
    trace["Selection_frequency"] = (
        trace["N_base_learners_selected"] / n_estimators
        if n_estimators > 0
        else np.nan
    )
    trace["Importance"] = trace["Micro_feature"].map(
        lambda feature: importance_lookup.get(str(feature), np.nan)
    )
    trace["Stage1_exclusion_reason"] = trace["Stage1_selected"].map(
      lambda selected: pd.NA
      if selected
      else "not_selected_by_any_base_learner"
    )
    return trace


def _is_technical_lineage_feature(feature):
    """Identify technical missingness levels in a lineage feature name."""
    if pd.isna(feature):
        return False
    value = str(feature).strip()
    if value.lower().endswith("__missing"):
        return True
    if "=" not in value:
        return False
    level = value.rsplit("=", 1)[-1].strip().lower()
    return level in {
        "nan", "na", "<na>", "none", "null", "missing", "-1", "-1.0"
    }


def build_stage2_feature_selection_trace(
    lineage,
    all_processed_features,
    feature_importance,
    candidate_features,
    final_features,
    view=None,
):
    """Add Stage 2 candidacy, rank, selection, and exclusion information."""
    if lineage is None or lineage.empty:
        trace = active_feature_lineage(
            lineage,
            all_processed_features,
            view=view,
        )
        trace["After_preprocess"] = True
        trace["Drop_reason"] = np.nan
    else:
        trace = lineage.copy()

    importance_lookup = {
        str(feature): float(value)
        for feature, value in feature_importance.items()
    }
    candidate_rank = {
        str(feature): rank
        for rank, feature in enumerate(candidate_features, start=1)
    }
    final_rank = {
        str(feature): rank
        for rank, feature in enumerate(final_features, start=1)
    }

    micro_as_string = trace["Micro_feature"].map(
        lambda value: str(value) if pd.notna(value) else None
    )
    after_preprocess = trace["After_preprocess"].map(
        lambda value: value is True or str(value).strip().lower() == "true"
    )
    trace["Stage1_importance"] = micro_as_string.map(importance_lookup)
    trace["Is_technical_feature"] = trace["Micro_feature"].map(
        _is_technical_lineage_feature
    )
    trace["Positive_importance"] = (
        np.isfinite(trace["Stage1_importance"])
        & (trace["Stage1_importance"] > 0)
    )
    trace["Candidate_rank"] = micro_as_string.map(candidate_rank)
    trace["Stage2_candidate"] = micro_as_string.isin(candidate_rank)
    trace["Stage2_selected"] = micro_as_string.isin(final_rank)
    trace["Stage2_rank"] = micro_as_string.map(final_rank)

    reasons = []
    for row_index, row in trace.iterrows():
        if not after_preprocess.loc[row_index]:
            reasons.append(row.get("Drop_reason", "removed_before_stage2"))
        elif bool(row["Is_technical_feature"]):
            reasons.append("technical_feature")
        elif not bool(row["Positive_importance"]):
            reasons.append("non_positive_importance")
        elif not bool(row["Stage2_selected"]):
            reasons.append("not_in_selected_top_n")
        else:
            reasons.append(np.nan)
    trace["Stage2_exclusion_reason"] = reasons
    return trace


def summarize_feature_lineage(lineage_frames, features):
    """Collapse view-specific lineage into one row per processed feature."""
    features = [str(feature) for feature in features]
    if lineage_frames:
        lineage = pd.concat(lineage_frames, ignore_index=True, sort=False)
    else:
        lineage = pd.DataFrame()

    def joined_values(feature, column):
        """Join unique lineage values for one feature and metadata field."""
        if lineage.empty or column not in lineage.columns:
            return np.nan
        values = lineage.loc[
            lineage["Micro_feature"].astype(str) == feature,
            column,
        ].dropna()
        unique_values = []
        for value in values.astype(str):
            if value and value not in unique_values:
                unique_values.append(value)
        return ";".join(unique_values) if unique_values else np.nan

    return pd.DataFrame({
        "Feature": pd.Series(features, dtype="object"),
        "Macro_feature": [
            joined_values(feature, "Macro_feature") for feature in features
        ],
        "Micro_feature": pd.Series(features, dtype="object"),
        "Large_factor": [
            joined_values(feature, "Large_factor") for feature in features
        ],
        "Small_factor": [
            joined_values(feature, "Small_factor") for feature in features
        ],
    })


def local_sample_func(dat_path, c_type, target, n_sample, balance_event, label_data):
    """Sample participant indices for a reduced local experiment."""
    source_indices = np.arange(label_data.shape[0])
    if balance_event:
        event_counts = label_data["status"].value_counts(normalize=True)  # 计算status列中各个标签的比例
        sampled_indices = []
        # 对每个标签进行按比例采样      其实就是保持删失样本与未删失样本的比例
        for label, proportion in event_counts.items():
            target_indices = source_indices[label_data['status'] == label]  # 根据该标签筛选出对应的数据
            sample_size = int(proportion * n_sample)  # 计算该标签需要采样的数量
            sampled_indices.extend(list(np.random.choice(target_indices, sample_size, replace=False)))
        sampled_indices = np.array(sampled_indices)
    else:
        sampled_indices = np.random.choice(source_indices, n_sample, replace=False)  # 不放回随机抽样n_sample个样本
    np.savetxt(f"{dat_path}/{c_type}_{target}_sampled_indices.csv", sampled_indices, fmt='%s',
               delimiter=',')  # 将这些样本在原始数据中的索引号保存下来，便于复核
    return sampled_indices


def couple_feature_func(LOGGER, feature_data, variable2unit, variable2parent):
    # 是否将关联特征进行耦合处理
    # 2025/4/26 根据和范师兄开会的内容，对存在逻辑关联的变量，其中离散型变量作为-1，连续型均值填充。
    # 由于后面处理其他缺失特征时也有做均值填充，所以这里只处理了连续型变量。
    """Apply codebook-defined logical dependencies to related variables."""
    all_features = list(feature_data.columns)
    for column in all_features:
        # 补充了一个判断条件，避免Sex列不存在时出错
        if column in variable2parent.keys() and variable2parent[column][0] in all_features:        # 在variable2parent中有记录的特征，都是有逻辑关联的特征。
            parent = variable2parent[column][0]     # 取出该特征的关联特征名称
            activate = variable2parent[column][1]   # 取出该特征的关联特征的生效取值(范围)
            if isinstance(activate, int):
                activate = [activate]
            elif isinstance(activate, str):
                activate = [int(x) for x in activate.split(',')]
            else:
                raise TypeError(f"Unknown type of {activate}!")

            dtype = variable2unit[column]
            if dtype == 'Category':                 # 若该特征为离散型变量，则直接将该特征直接设置为-1
                feature_data.loc[~feature_data[parent].isin(activate), column] = -1
            # else:                                   # 若该特征为连续型变量，则将其设置为远离该特征正常取值范围的一个值（即  最小值-值域）
            #     # data.loc[~data[parent].isin(activate), column] = data[column].min() - 1
            #     data.loc[~data[parent].isin(activate), column] = 2 * data[column].min() - data[column].max()
    LOGGER.info("After couple feature, data shape: " + str(feature_data.shape))
    return feature_data

def _is_binary_01(series):
    """Return whether all observed values belong to the binary zero-one set."""
    vals = pd.Series(series).dropna().unique()
    return len(vals) > 0 and set(vals).issubset({0, 1, 0.0, 1.0, True, False})

def one_hot_process_train_test(
    train_data,
    test_data,
    drop_single=True,
    preserve_missing=True,
):
    """Fit categorical levels on training data and align encoded test columns.

    Ordinary categorical missingness is represented by a missing dummy block,
    rather than by a synthetic ``nan`` category, so that a downstream joint
    imputer can infer the category from all predictor views.
    """
    train_data = train_data.copy()
    test_data = test_data.copy()

    train_encoded_parts = []
    test_encoded_parts = []
    feature_map = {}

    for column in train_data.columns:
        col_train = train_data[column]
        col_test = test_data[column]

        is_categorical = (
            pd.api.types.is_object_dtype(col_train)
            or pd.api.types.is_categorical_dtype(col_train)
            or pd.api.types.is_string_dtype(col_train)
        )

        if not is_categorical:
            train_encoded_parts.append(train_data[[column]])
            test_encoded_parts.append(test_data[[column]])
            feature_map[column] = [column]
            continue

        train_col = col_train.astype("object")
        test_col = col_test.astype("object")

        # Only observed development-set values define the category space.
        train_categories = pd.Index(pd.unique(train_col.dropna()))

        if drop_single and len(train_categories) <= 1:
            continue

        train_missing = train_col.isna()
        # An unseen held-out category has no development-defined dummy column
        # and is therefore handled as an unknown value to be imputed.
        test_missing = test_col.isna() | ~test_col.isin(train_categories)
        test_col = test_col.where(~test_missing, np.nan)

        train_dummy = pd.get_dummies(train_col, prefix=None, prefix_sep=None, dummy_na=False, drop_first=False)
        test_dummy = pd.get_dummies(test_col, prefix=None, prefix_sep=None, dummy_na=False, drop_first=False)

        wanted_cols = [f"{column}={cat}" for cat in train_categories]

        train_dummy.columns = [f"{column}={c}" for c in train_dummy.columns]
        test_dummy.columns = [f"{column}={c}" for c in test_dummy.columns]

        for c in wanted_cols:
            if c not in train_dummy.columns:
                train_dummy[c] = 0
            if c not in test_dummy.columns:
                test_dummy[c] = 0

        train_dummy = train_dummy[wanted_cols].astype(float)
        test_dummy = test_dummy[wanted_cols].astype(float)

        if preserve_missing:
            train_dummy.loc[train_missing, :] = np.nan
            test_dummy.loc[test_missing, :] = np.nan

        train_encoded_parts.append(train_dummy)
        test_encoded_parts.append(test_dummy)
        feature_map[column] = wanted_cols

    train_out = pd.concat(train_encoded_parts, axis=1) if train_encoded_parts else pd.DataFrame(index=train_data.index)
    test_out = pd.concat(test_encoded_parts, axis=1) if test_encoded_parts else pd.DataFrame(index=test_data.index)

    return train_out, test_out, feature_map

def generate_safe_eval_times(y_train, y_test, interval=365):
    """Generate evaluation times inside the shared train-test follow-up range."""
    min_time = max(np.min(y_train), np.min(y_test))  # 取训练集和测试集的最小时间
    max_time = min(np.max(y_train), np.max(y_test))

    # 首先生成候选点
    candidate_times = np.arange(0, max_time, interval)

    # 筛选有效点（注意是闭区间）
    eval_times = candidate_times[
        (candidate_times >= min_time) &
        (candidate_times < max_time)
        ]

    # 如果没有满足条件的点（罕见情况）
    if len(eval_times) == 0:
        raise ValueError(f"测试数据的时间范围[{min_time}, {max_time}]内不存在可评估时间点！")

    return eval_times


# 输入micro的值（数组），以及转化为macro的值的处理方法
def operation(arr, op):
    """Reduce an array with the configured macro-feature aggregation operation."""
    if op == 'mean':
        return np.mean(arr)
    elif op == 'max':
        return np.max(arr)
    elif op == 'min':
        return np.min(arr)
    elif op == 'sum':
        return np.sum(arr)
    else:
        raise ValueError(f"Unknown op: {op}")


def _drop_high_missing_columns(LOGGER, train_df, test_df, threshold=0.8, label_keys=('status', 'time')):
    """Drop features exceeding the training-set missingness threshold."""
    train_df = train_df.copy()
    test_df = test_df.copy()

    feat_cols = [c for c in train_df.columns if c not in label_keys]
    miss_ratio = train_df[feat_cols].isna().mean()

    cols_drop = miss_ratio[miss_ratio > threshold].index.tolist()

    if cols_drop:
        LOGGER.info(f"Delete columns with missing ratio > {threshold:.0%}: {len(cols_drop)}")
        for c in cols_drop:
            LOGGER.info(f"  DROP {c}: missing ratio = {miss_ratio[c]:.2%}")
        train_df = train_df.drop(columns=cols_drop, errors='ignore')
        test_df = test_df.drop(columns=cols_drop, errors='ignore')
    else:
        LOGGER.info(f"No columns with missing ratio > {threshold:.0%}")

    return train_df, test_df, cols_drop

def process_missing_value_v2(
    LOGGER,
    train_data,
    test_data,
    feature_map,
    seed=42,
    column_groups=None,
    categorical_groups=None,
):
    """Impute and scale processed features while retaining macro-to-micro mapping."""
    train_data = train_data.copy()
    test_data = test_data.copy()

    # 识别标签列
    id_col = ['eid'] if 'eid' in train_data.columns else []
    label_cols = [c for c in ['status', 'time'] if c in train_data.columns]
    non_feature_cols = id_col + label_cols

    train_feature = train_data.drop(columns=non_feature_cols, errors='ignore')
    test_feature = test_data.drop(columns=non_feature_cols, errors='ignore')

    # 缺失值处理 + 缩放
    train_feature_processed, test_feature_processed, preprocess_bundle = new_process(
        LOGGER,
        train_feature,
        test_feature,
        seed=seed,
        preprocess_impute_method=preprocess_impute_method,
        preprocess_add_missing_indicator=preprocess_add_missing_indicator,
        preprocess_scale_order=preprocess_scale_order,
        iterative_max_iter=iterative_max_iter,
        iterative_n_estimators=iterative_n_estimators,
        iterative_max_depth=iterative_max_depth,
        iterative_n_jobs=iterative_n_jobs,
        iterative_tol=iterative_tol,
        column_groups=column_groups,
        categorical_groups=categorical_groups,
    )

    # 拼回 eid/time/status
    train_out = pd.concat(
        [train_data[id_col + label_cols], train_feature_processed],
        axis=1
    )
    test_out = pd.concat(
        [test_data[id_col + label_cols], test_feature_processed],
        axis=1
    )

    # 最终列顺序一致
    final_cols = id_col + label_cols + list(train_feature_processed.columns)
    train_out = train_out[final_cols]
    test_out = test_out[final_cols]

    # 保留 one-hot 阶段建立的 macro -> micro 映射，
    # 同时删除预处理过程中已经被移除的 micro 特征。
    processed_feature_set = set(train_feature_processed.columns)
    final_feature_map = {}
    mapped_features = set()
    for macro_feature, micro_features in feature_map.items():
        retained_micro_features = [
            micro_feature for micro_feature in micro_features if micro_feature in processed_feature_set
        ]
        if retained_micro_features:
            final_feature_map[macro_feature] = retained_micro_features
            mapped_features.update(retained_micro_features)
    # 正常情况下，原 feature_map 应覆盖所有处理后的特征。
    # 对未被覆盖的新增特征保留恒等映射。
    for feature in train_feature_processed.columns:
        if feature not in mapped_features:
            final_feature_map[feature] = [feature]

    return train_out, test_out, final_feature_map, preprocess_bundle


def new_process(
    LOGGER,
    train_data,
    test_data,
    seed=42,
    preprocess_impute_method="iterative_rf",
    preprocess_add_missing_indicator=True,
    preprocess_scale_order="minmax_then_zscore",
    iterative_max_iter=10,
    iterative_n_estimators=100,
    iterative_max_depth=8,
    iterative_n_jobs=-1,
    iterative_tol=0.0,
    column_groups=None,
    categorical_groups=None,
):
    
    """Fit train-derived imputers and scalers and transform train and test features."""
    train_data = train_data.copy()
    test_data = test_data.copy()

    # Reuse development-defined column groups during internal CV so that
    # every fold applies the same binary-versus-continuous treatment.
    if column_groups is None:
        binary_cols = [
            column
            for column in train_data.columns
            if _is_binary_01(train_data[column])
        ]
        other_cols = [
            column
            for column in train_data.columns
            if column not in binary_cols
        ]
    else:
        binary_cols = list(column_groups["binary_cols"])
        other_cols = list(column_groups["other_cols"])
        grouped_columns = binary_cols + other_cols
        if (
            len(grouped_columns) != len(set(grouped_columns)) or
            set(grouped_columns) != set(train_data.columns)
        ):
            raise ValueError(
                "column_groups must partition all preprocessing columns exactly once."
            )
    
    LOGGER.info(f"[new_process] train shape = {train_data.shape}, test shape = {test_data.shape}")
    LOGGER.info(f"[new_process] binary_cols = {len(binary_cols)}, other_cols = {len(other_cols)}")
    LOGGER.info("[new_process] start joint all-view imputation...")

    train_parts = []
    test_parts = []

    bundle = {
        "binary_cols": binary_cols,
        "other_cols": other_cols,
        "missing_indicator_cols": [],
        "imputer_bin": None,
        "imputer_num": None,
        "imputer_joint": None,
        "minmax_scaler": None,
        "zscore_scaler": None,
        "categorical_groups": categorical_groups or {},
    }

    # Every numeric predictor enters one joint imputation model. Complete
    # variables remain unchanged but can predict missing variables in any view.
    if train_data.shape[1] > 0:
        non_numeric = [
            column for column in train_data.columns
            if not pd.api.types.is_numeric_dtype(train_data[column])
        ]
        if non_numeric:
            raise TypeError(
                "Joint imputation requires numeric or one-hot encoded inputs; "
                f"non-numeric columns: {non_numeric[:10]}"
            )

        if preprocess_add_missing_indicator:
            train_miss = train_data.isna().astype(int)
            test_miss = test_data.isna().astype(int)
            train_miss.columns = [f"{c}__missing" for c in train_data.columns]
            test_miss.columns = [f"{c}__missing" for c in test_data.columns]
            train_parts.append(train_miss)
            test_parts.append(test_miss)
            bundle["missing_indicator_cols"] = list(train_miss.columns)

        if preprocess_impute_method == "iterative_rf":
            rf_estimator = RandomForestRegressor(
                n_estimators=iterative_n_estimators,
                max_depth=iterative_max_depth,
                random_state=seed,
                n_jobs=iterative_n_jobs,
            )
            imputer = ProgressIterativeImputer(
                estimator=rf_estimator,
                random_state=seed,
                max_iter=iterative_max_iter,
                tol=iterative_tol,
                initial_strategy="median",
                sample_posterior=False,
                n_nearest_features=iterative_n_nearest_features,
                skip_complete=iterative_skip_complete,
                verbose=iterative_verbose,
            ).configure_progress(
                LOGGER,
                every=iterative_progress_every,
            )
        elif preprocess_impute_method == "simple":
            imputer = SimpleImputer(strategy="median")
        else:
            raise ValueError(
                f"Unknown preprocess_impute_method: {preprocess_impute_method}"
            )

        n_missing_features = int(train_data.isna().any(axis=0).sum())
        LOGGER.info(
            "[new_process] features requiring iterative imputation: "
            f"{n_missing_features}/{train_data.shape[1]}; "
            f"configured rounds: {iterative_max_iter}."
        )
        logger_stream = _LoggerStream(LOGGER)
        LOGGER.info("[new_process] fit imputer on the development set...")
        with redirect_stdout(logger_stream), warnings.catch_warnings():
            # tol=0 deliberately requests the configured fixed iteration count.
            warnings.filterwarnings("ignore", category=ConvergenceWarning)
            train_imputed_array = imputer.fit_transform(train_data)
        logger_stream.flush()

        LOGGER.info("[new_process] transform the held-out test set...")
        with redirect_stdout(logger_stream):
            test_imputed_array = imputer.transform(test_data)
        logger_stream.flush()

        train_imputed = pd.DataFrame(
            train_imputed_array,
            index=train_data.index,
            columns=train_data.columns,
        )
        test_imputed = pd.DataFrame(
            test_imputed_array,
            index=test_data.index,
            columns=test_data.columns,
        )
        bundle["imputer_num"] = imputer
        bundle["imputer_joint"] = imputer
        bundle["imputation_iterations"] = int(
            getattr(imputer, "n_iter_", 0)
        )

        # Convert model predictions for binary variables back to valid 0/1
        # values. For a one-hot group, choose exactly one predicted category.
        train_bin = (train_imputed[binary_cols] >= 0.5).astype(float)
        test_bin = (test_imputed[binary_cols] >= 0.5).astype(float)

        for _, group_columns in (categorical_groups or {}).items():
            group_columns = [c for c in group_columns if c in binary_cols]
            if not group_columns:
                continue
            for raw, imputed, binary in (
                (train_data, train_imputed, train_bin),
                (test_data, test_imputed, test_bin),
            ):
                missing_rows = raw[group_columns].isna().any(axis=1)
                if not missing_rows.any():
                    continue
                scores = imputed.loc[missing_rows, group_columns].to_numpy()
                selected = np.argmax(scores, axis=1)
                binary.loc[missing_rows, group_columns] = 0.0
                missing_index = binary.index[missing_rows]
                for column_index, column in enumerate(group_columns):
                    selected_index = missing_index[selected == column_index]
                    binary.loc[selected_index, column] = 1.0

        if binary_cols:
            train_parts.append(train_bin)
            test_parts.append(test_bin)

        if other_cols:
            train_num = train_imputed[other_cols].copy()
            test_num = test_imputed[other_cols].copy()

            if preprocess_scale_order == "minmax_then_zscore":
                scaler_mm = MinMaxScaler()
                train_num = pd.DataFrame(
                    scaler_mm.fit_transform(train_num),
                    index=train_num.index,
                    columns=train_num.columns,
                )
                test_num = pd.DataFrame(
                    scaler_mm.transform(test_num),
                    index=test_num.index,
                    columns=test_num.columns,
                )
                bundle["minmax_scaler"] = scaler_mm

                scaler_z = StandardScaler()
                train_num = pd.DataFrame(
                    scaler_z.fit_transform(train_num),
                    index=train_num.index,
                    columns=train_num.columns,
                )
                test_num = pd.DataFrame(
                    scaler_z.transform(test_num),
                    index=test_num.index,
                    columns=test_num.columns,
                )
                bundle["zscore_scaler"] = scaler_z
            else:
                raise ValueError(
                    f"Unknown preprocess_scale_order: {preprocess_scale_order}"
                )

            train_parts.append(train_num)
            test_parts.append(test_num)

    train_out = pd.concat(train_parts, axis=1) if train_parts else pd.DataFrame(index=train_data.index)
    test_out = pd.concat(test_parts, axis=1) if test_parts else pd.DataFrame(index=test_data.index)

    # 保证顺序：原始特征列 + 缺失指示器列
    final_cols = list(train_data.columns) + bundle["missing_indicator_cols"]
    train_out = train_out[final_cols]
    test_out = test_out[final_cols]

    return train_out, test_out, bundle

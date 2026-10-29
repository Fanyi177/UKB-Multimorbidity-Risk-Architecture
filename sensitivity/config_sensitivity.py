"""Sensitivity-analysis configuration layered on top of the final enviro-2 config.

This file does not replace config.py. It imports the fixed modelling settings from
config.py and redirects only sensitivity data/results/runtime settings.
"""

import os

from config import *  # noqa: F401,F403

PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
SOURCE_ROOT = os.path.join(PROJECT_DIR, "Source")
PRIMARY_DATA_DIR = os.path.join(PROJECT_DIR, "Data_2")
PRIMARY_RESULT_DIR = os.path.join(PROJECT_DIR, "Result_2")

_ALLOWED = {
    "domain_availability",
    "disease_free",
    "landmark_2y",
    "simple_imputation",
}

sensitivity_name = os.environ.get("UKB_SENSITIVITY", "").strip()
if sensitivity_name not in _ALLOWED:
    raise RuntimeError(
        "UKB_SENSITIVITY must be one of: " + ", ".join(sorted(_ALLOWED))
    )

use_enviro_2 = True
experiment_suffix = "_2"
src_dir = SOURCE_ROOT

def _source_rel(*parts):
    return os.path.join(*parts)

if sensitivity_name == "domain_availability":
    source_subdir = "sensitivity_analysis"
    feat_name_dict = {
        "enviro": _source_rel(source_subdir, "final_environment_2.csv"),
        "metabolomics": _source_rel(source_subdir, "final_metabolomics.csv"),
        "biomarkers": _source_rel(source_subdir, "final_biomarkers.csv"),
        "gene": _source_rel(source_subdir, "final_genetics.csv"),
    }
    spec_name = _source_rel(source_subdir, "final_eid_sex.csv")
    label_name = _source_rel(source_subdir, "UKB_diseases_data_with_patterns.csv")

elif sensitivity_name == "disease_free":
    feat_name_dict = {
        "enviro": "final_environment_2.csv",
        "metabolomics": "final_metabolomics.csv",
        "biomarkers": "final_biomarkers.csv",
        "gene": "final_genetics.csv",
    }
    spec_name = "final_eid_sex.csv"
    label_name = _source_rel(
        "sensitivity_generated", "disease_free",
        "UKB_diseases_data_with_patterns.csv",
    )

elif sensitivity_name == "landmark_2y":
    feat_name_dict = {
        "enviro": "final_environment_2.csv",
        "metabolomics": "final_metabolomics.csv",
        "biomarkers": "final_biomarkers.csv",
        "gene": "final_genetics.csv",
    }
    spec_name = "final_eid_sex.csv"
    label_name = _source_rel(
        "sensitivity_generated", "landmark_2y",
        "UKB_diseases_data_with_patterns.csv",
    )

else:  # simple_imputation
    feat_name_dict = {
        "enviro": "final_environment_2.csv",
        "metabolomics": "final_metabolomics.csv",
        "biomarkers": "final_biomarkers.csv",
        "gene": "final_genetics.csv",
    }
    spec_name = "final_eid_sex.csv"
    label_name = "UKB_diseases_data_with_patterns.csv"

sensitivity_root = make_dir(
    os.path.join(PROJECT_DIR, "Sensitivity", sensitivity_name)
)
dat_dir = make_dir(os.path.join(sensitivity_root, "Data_2"))
log_dir = make_dir(os.path.join(sensitivity_root, "Log_2"))
res_dir = make_dir(os.path.join(sensitivity_root, "Result_2"))
ana_dir = make_dir(os.path.join(sensitivity_root, "Analysis_2"))
mod_dir = make_dir(os.path.join(sensitivity_root, "Model_2"))

# Prefer node-local scratch for high-frequency temporary CoxEM files when the
# pipeline provides SENS_LOCAL_TMP_ROOT. Fall back to the original Lustre path.
_local_tmp_root = os.environ.get("SENS_LOCAL_TMP_ROOT", "").strip()
if _local_tmp_root:
    tmp_dir = make_dir(os.path.join(_local_tmp_root, sensitivity_name, "Tmp_2"))
else:
    tmp_dir = make_dir(os.path.join(sensitivity_root, "Tmp_2"))

primary_dat_dir = PRIMARY_DATA_DIR
primary_result_dir = PRIMARY_RESULT_DIR
preserve_primary_split = True

if sensitivity_name == "simple_imputation":
    preprocess_impute_method = "simple"
else:
    preprocess_impute_method = "iterative_rf"

primary_stage2_result_folder = stage3_stage2_result_folder
stage3_stage2_result_folder = "stage_parallel"

# Runtime parallelism only; the fitted model definition is unchanged.
CoxEM_n_jobs = int(os.environ.get("SENS_STAGE1_COXEM_N_JOBS", "8"))
stage3_coxem_n_jobs = int(os.environ.get("SENS_STAGE3_COXEM_N_JOBS", "8"))

# ============================================================================
# Sensitivity-only safeguards for rare-event outcomes and internal-CV selection.
# Preserve 10-fold settings for the other three sensitivity analyses.
# For disease_free use 5-fold stratified CV so each validation fold carries
# more P1 events, and require >=4/5 successful folds rather than 8/10.
# A 5-fold strategy is a change in feature-selection CV only; it does not
# change the development / held-out participant assignment.
# ============================================================================
stage2_adaptive_auc_times = True
stage2_max_consecutive_initial_failures = 3
stage2_auc_min_cases = 2
stage2_auc_min_controls = 2
if sensitivity_name == "disease_free":
    n_splits = 5
    n_repeats = 1
    n_process = n_repeats * n_splits
    stage2_parallel_folds = min(stage2_parallel_folds, n_process)
    stage2_min_successful_folds = 4

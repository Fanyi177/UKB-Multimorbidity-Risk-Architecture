# UKB Multimorbidity Risk Architecture

Code accompanying a UK Biobank research project on incident multimorbidity, disease-pattern-specific risk factors, and multidomain survival prediction.

The analysis considers 36 chronic conditions and eight multimorbidity patterns (P1–P8). Predictors are organized into four domains: **E**, environmental and clinical characteristics; **P**, phenotypic characteristics and biomarkers; **M**, metabolomics; and **G**, genetics. The modelling workflow separates a development dataset (approximately 60%) from a held-out evaluation dataset (approximately 40%).

## Repository structure

```text
UKB-Multimorbidity-Risk-Architecture/
├── R/                 # UK Biobank processing, disease definitions, tables and figures
├── python/            # CoxEM prediction pipeline and model benchmarking
└── sensitivity/       # Sensitivity-analysis pipeline
```

### `R/` — data processing, results and visualization

| File | Main purpose |
| --- | --- |
| `1.process_data_UKB.R` | Prepare and process source UK Biobank variables. |
| `2.UKB_recoding.R`, `02_UKB_recoding.R` | Recode study variables for analysis. |
| `3.definition_of_diseases_and_merge_other_data.R` | Define chronic diseases and merge relevant datasets. |
| `5_definition_of_Multimorbidity_patterns_and_follow_time_.R` | Derive multimorbidity patterns and follow-up information. |
| `07_results_and_tables.R` | Summarize analysis results and generate tables. |
| `08_nature_figures.R` | Produce manuscript and supplementary figures. |
| `09_sensitivity_analysis.R` | Summarize and visualize sensitivity analyses. |

### `python/` — main survival-modelling pipeline

| File | Main purpose |
| --- | --- |
| `config.py` | Define file paths, predictors, outcomes and model settings. |
| `data_preprocess.py` | Prepare modelling datasets, including preprocessing and cross-validation inputs. |
| `disease_clustering_with_quality_metrics.py` | Identify and assess disease clusters. |
| `CoxEM.py` | Implement the Cox ensemble model (CoxEM). |
| `run_model.py` | Run Stage 1 CoxEM fitting and feature-ranking analyses. |
| `run_stage1b_equal_features.py` | Benchmark survival-learning models under a common input-feature specification. |
| `run_stage2.py`, `run_stage2_parallel_final.py` | Evaluate and select predictive feature sets in Stage 2. |
| `run_stage3.py` | Fit and evaluate final prediction models. |
| `run_stage3b.py` | Produce additional downstream model-evaluation outputs. |
| `run_pipeline_slurm_cn.sh` | Submit or coordinate the main pipeline on a SLURM cluster. |
| `utils.py` | Shared data-loading, evaluation and helper functions. |

### `sensitivity/` — sensitivity analyses

| File | Main purpose |
| --- | --- |
| `config_sensitivity.py` | Configure sensitivity-analysis scenarios. |
| `prepare_sensitivity_sources.py` | Prepare scenario-specific source datasets. |
| `data_preprocess_sensitivity.py`, `run_data_preprocess_sensitivity_safe.py` | Preprocess sensitivity-analysis datasets. |
| `CoxEM_sensitivity.py` | Sensitivity-analysis implementation of CoxEM. |
| `run_model_sensitivity.py`, `run_stage1_sensitivity.py` | Run sensitivity-model fitting and Stage 1 analyses. |
| `run_stage2_parallel_final.py` | Run Stage 2 feature-selection analyses for sensitivity scenarios. |
| `run_with_sensitivity_config.py` | Run scripts with sensitivity-specific configuration. |
| `collect_sensitivity_results.py` | Collect and summarize sensitivity-analysis outputs. |

The subdirectories also contain their own `README.md` files for additional implementation details.

## Analytical overview

1. **Data preparation:** Define chronic conditions, multimorbidity outcomes and E/P/M/G predictor datasets.
2. **Development-set modelling:** Use cross-validation for survival-model benchmarking, CoxEM feature ranking and parsimonious feature selection.
3. **Prediction:** Fit single-domain and multidomain survival models and evaluate performance in a held-out dataset.
4. **Risk characterization:** Examine domain contributions, risk scores, joint risk profiles and risk stratification.
5. **Sensitivity analyses:** Assess robustness under alternative analytical specifications.

## Data availability and use

UK Biobank individual-level data are subject to UK Biobank access and data-use requirements and are **not included** in this repository. The scripts may require authorized input data, project-specific paths, software dependencies and an HPC/SLURM environment. Consult the configuration files and subdirectory READMEs before running the pipeline.

## Citation

If you use this code, please cite the associated research article once its bibliographic information is available.

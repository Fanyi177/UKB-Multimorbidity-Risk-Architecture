#!/usr/bin/env Rscript

###############################################################################
### UKB multimorbidity FINAL 20261025：结果、表格、source data 与 QA ####
###############################################################################
# 目标：只整理当前已完成的 Stage 1/2/3 输出；不重新训练 CoxEM。
# 使用方法：在 RStudio 中按 Section 0 → 15 逐段运行并检查 cat() 与 QA CSV。
# 约定：正式展示名仍来自内部变量字典；最终补充表/图编号按本次 Excel/Word 顺序。
# 最终输出：Table S1–S3、S10–S26；各图 source CSV 维持原文件名供 Figure 4/5 复用。
# 08 正式输出：Figure S1–S4、S5a–S5d、S6（旧 S9 校准）、S7（旧 S10 特征重要性）。
# 本版保留原有统计模型与输出布局；仅更新九结局、E/P/M/G 和 20261025 S5-S8 权威变量字典。
# 20261025 修订：本地输出和 Stage2/3/3B 统一读取 Result，原始环境协变量统一读取
# source/final_environment.csv；Stage3B 系数另行审计实际训练来源，避免误把 _2 系数当作主分析。

###############################################################################
### Section 0. Packages / paths / output folders ####
###############################################################################
# Input:
#   项目根目录、Result、source、内部变量字典 S1–S4 Excel、本地 PPT。
# Output:
#   v8 输出目录、通用 helper 与路径对象。
# 这一段做什么：加载包、识别带/不带尾部空格的 Excel，并建立 v8 专属目录。

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readxl)
  library(openxlsx)
  library(survival)
  library(broom)
  library(officer)
})

options(stringsAsFactors = FALSE, scipen = 999)

project_dir <- "/Users/yifan2/Desktop/日常工作/UKB_multimorbidity"
result_dir <- file.path(project_dir, "Result")
stage2_dir <- file.path(result_dir, "Stage 2")
stage3_dir <- file.path(result_dir, "Stage 3")
stage3b_dir <- file.path(result_dir, "Stage 3B")
cluster_dir <- file.path(result_dir, "cluster_result")
# Figure 2 cluster network and Stage 2/3/3B models all use the same Result root.
raw_source_dir <- file.path(project_dir, "source")
analysis_dir <- file.path(project_dir, "R_final_analysis")

# -----------------------------------------------------------------------------
# MAIN disease source: single authoritative file + single-session cache
# -----------------------------------------------------------------------------
# MAIN cohort sample size is read from the current source (no fixed N).
#   source = source/UKB_diseases_data_with_patterns.csv
#
# Sections 7, 12 and 13 all reuse the same in-memory object. This avoids
# repeatedly reading the same ~309k-row disease file while keeping each section
# independently runnable after Section 0.
# -----------------------------------------------------------------------------

main_disease_file <- file.path(
  raw_source_dir,
  "UKB_diseases_data_with_patterns.csv"
)

disease_names_36 <- c(
  "Anaemia",
  "Angina",
  "Anxiety_or_panic_attacks",
  "Atrial_fibrillation",
  "Asthma",
  "Bronchiectasis",
  "Chronic_kidney_disease",
  "Chronic_obstructive_pulmonary_disease",
  "Chronic_sinusitis",
  "Cirrhosis",
  "Dementia",
  "Depression",
  "Diabetes",
  "Eczema_or_dermatitis",
  "Epilepsy",
  "Glaucoma",
  "Heart_failure",
  "Hepatitis",
  "Hypertension",
  "Inflammatory_bowel_disease",
  "Irritable_bowel_syndrome",
  "Meningitis",
  "Migraine",
  "Multiple_sclerosis",
  "Myocardial_infarction",
  "Osteoporosis",
  "Parkinson_disease",
  "Peripheral_vascular_disease",
  "Prostate_problem",
  "Rheumatoid_arthritis",
  "Schizophrenia",
  "Stroke",
  "Thyroid_problem",
  "Tuberculosis",
  "Vestibular_disorder",
  "overall_cancer"
)

disease_time_cols_36 <- paste0(
  disease_names_36,
  "_time"
)

disease_time_cols_36[
  disease_names_36 == "overall_cancer"
] <- "cancer_date"
#
pattern_baseline_count_cols <- paste0(
  "comorbidity_pattern_",
  1:8,
  "_baseline_disease_count"
)

pattern_incident_count_cols <- paste0(
  "comorbidity_pattern_",
  1:8,
  "_incident_disease_count"
)

#
main_disease_required_cols <- unique(
  c(
    "eid",
    "recruitment_date",
    "censor_date",
    "valid_followup",
    "baseline_disease_count",
    "incident_disease_count",
    "incident_multimorbidity",
    disease_names_36,
    disease_time_cols_36,
    pattern_baseline_count_cols,
    pattern_incident_count_cols
  )
)

get_main_disease_data <- function(force_reload = FALSE) {

  if (
    !force_reload &&
    exists(
      "main_disease_data",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  ) {
    
    cached_main <-
      get(
        "main_disease_data",
        envir = .GlobalEnv,
        inherits = FALSE
      )
    
    if (
      all(
        main_disease_required_cols %in%
        names(cached_main)
      )
    ) {
      return(cached_main)
    }
    
    rm(cached_main)
  }

  if (!file.exists(main_disease_file)) {
    stop("找不到 MAIN disease source：", main_disease_file)
  }

  main_header <- names(
    fread(
      main_disease_file,
      nrows = 0,
      showProgress = FALSE
    )
  )

  missing_main_cols <- setdiff(
    main_disease_required_cols,
    main_header
  )

  if (length(missing_main_cols)) {
    stop(
      "UKB_diseases_data_with_patterns.csv 缺少 MAIN analysis 需要的变量：",
      paste(
        missing_main_cols,
        collapse = "; "
      )
    )
  }

  z <- fread(
    main_disease_file,
    select = main_disease_required_cols,
    showProgress = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      eid = as.character(eid)
    ) %>%
    distinct(
      eid,
      .keep_all = TRUE
    )

  # 仅验证数据是否有效，不对总体样本量设置固定门槛。
  if (nrow(z) == 0L || anyNA(z$eid) || anyDuplicated(z$eid)) {
    stop("MAIN disease source invalid: empty cohort, missing eid or duplicate eid.")
  }

  assign(
    "main_disease_data",
    z,
    envir = .GlobalEnv
  )

  cat(
    "MAIN disease source loaded once: N = ",
    nrow(z),
    "; file = ",
    basename(main_disease_file),
    "\n",
    sep = ""
  )

  z
}

intermediate_dir <- file.path(analysis_dir, "intermediate")
figure_source_dir <- file.path(analysis_dir, "figure_source_data")
table_dir <- file.path(analysis_dir, "supplementary_tables")
main_table_dir <- file.path(analysis_dir, "main_tables")
log_dir <- file.path(analysis_dir, "logs")
caption_dir <- file.path(analysis_dir, "captions")
figure_dir <- file.path(analysis_dir, "figures")

for (p in c(
  intermediate_dir, figure_source_dir, table_dir, main_table_dir,
  log_dir, caption_dir, figure_dir
)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# 唯一权威变量字典：Manuscript 的最新版 Excel，Table S5-S8。
excel_authority_file <- file.path(
  project_dir, "Manuscript", "20261025 Supplementary Tables.xlsx"
)
if (!file.exists(excel_authority_file)) {
  stop("缺少 20261025 权威变量字典：", excel_authority_file,
       "；请把上传的 Excel 放在 Manuscript 文件夹，保持文件名不变。")
}

ppt_reference_file <- file.path(project_dir, "共病UKB汇报260911.pptx")
if (!file.exists(ppt_reference_file)) message("提示：未找到旧版参考 PPT，不影响 07 结果或图表 source 生成：", ppt_reference_file)

write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(as.data.table(x), path, bom = TRUE, na = "")
}
write_source <- function(x, filename) {
  write_csv(x, file.path(figure_source_dir, filename))
}
write_table <- function(x, filename) {
  write_csv(x, file.path(table_dir, filename))
}
write_log <- function(x, filename) {
  write_csv(x, file.path(log_dir, filename))
}
need_object <- function(object_name, csv_path) {
  if (!exists(object_name, envir = .GlobalEnv)) {
    if (!file.exists(csv_path)) {
      stop("缺少对象 ", object_name, "；需要先运行相应前置 Section。")
    }
    assign(
      object_name,
      fread(csv_path, showProgress = FALSE) %>% as_tibble(),
      envir = .GlobalEnv
    )
  }
}

# 本次最终编号版本的进度记录；每一段完成后覆盖更新，便于分段运行/中途继续。
checkpoint_path <- file.path(project_dir, "UKB_final_20261025_checkpoint.md")
update_progress_checkpoint <- function(current_stage, next_step) {
  writeLines(c(
    "# UKB multimorbidity final output checkpoint",
    paste0("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Current stage: ", current_stage),
    "MAIN cohort: dynamic N from source/UKB_diseases_data_with_patterns.csv",
    "Tables: S1–S3 and S10–S26 generated by 07; S4–S9 read/maintained externally (20261025 Excel)",
    "Figures: nine outcomes Overall/P1-P8; main Figure 2–5; supplementary S1–S13; domain E/P/M/G",
    paste0("Next step: ", next_step)
  ), checkpoint_path, useBytes = TRUE)
}

is_nan_level <- function(x) {
  str_detect(str_to_lower(str_trim(as.character(x))), "=nan$")
}
is_minus_one_level <- function(x) {
  str_detect(str_trim(as.character(x)), "=-1(?:\\.0)?$")
}
safe_sd <- function(x) {
  z <- sd(x, na.rm = TRUE)
  ifelse(is.finite(z) && z > 0, z, 1)
}
normalise_disease_key <- function(x) {
  x %>%
    str_replace_all("[’']", "") %>%
    str_replace_all("_", " ") %>%
    str_to_lower() %>%
    str_squish()
}

cat("完成：Section 0 packages / paths / v8 output folders\n")

update_progress_checkpoint("07 Section 0 completed", "从 07 Section 1 继续。")

###############################################################################
### Section 1. Master variable display dictionary ####
###############################################################################
# Input:
#   Manuscript/20261025 Supplementary Tables.xlsx (Table S5–S8)
#   Result/Stage 2/*/signed_feature_importance.csv（仅用于识别当前 model key）
# Output:
#   R_final_analysis/intermediate/variable_display_dictionary.csv
#   R_final_analysis/intermediate/variable_display_dictionary.xlsx
#   R_final_analysis/logs/variable_dictionary_QA.csv
# 这一段做什么：以 Excel 的 “Variable name in this study” 为唯一正式名称来源，
#   建立 analysis_variable / original_data_column 双通道匹配；不使用下划线替换作为正式名。

# Original model keys stay enviro/biomarkers/metabolomics/gene; labels are E/P/M/G.
sheet_spec <- tibble(
  sheet = paste0("Table S", 5:8),
  modality = c("enviro", "blood", "metabolites", "gene"),
  modality_label = c(
    "Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors"
  )
)

read_authority_sheet <- function(sheet, modality, modality_label) {
  x <- read_excel(excel_authority_file, sheet = sheet, skip = 2)
  get_col <- function(nm, default = NA_character_) {
    if (nm %in% names(x)) as.character(x[[nm]]) else rep(default, nrow(x))
  }
  tibble(
    modality = modality,
    modality_label = modality_label,
    subdomain = coalesce(
      get_col("Subdomain"),
      if (modality == "gene") "Genetic factors" else NA_character_
    ),
    analysis_variable = get_col("Variable name in the analysis"),
    canonical_variable = get_col("Variable name in the analysis"),
    ukb_description = get_col("Variable description in the UK Biobank"),
    display_name = get_col("Variable name in this study"),
    ukb_field_id = get_col("UK Biobank field ID"),
    original_data_column = get_col("Original data column"),
    parent_variable = get_col("Parent variable before one-hot"),
    one_hot_level = get_col("One-hot level"),
    variable_type = get_col("Variable type"),
    notes = get_col("Notes"),
    source_sheet = sheet
  )
}

variable_dictionary <- pmap_dfr(
  sheet_spec,
  function(sheet, modality, modality_label) {
    read_authority_sheet(sheet, modality, modality_label)
  }
) %>%
  filter(!is.na(analysis_variable), nzchar(str_trim(analysis_variable))) %>%
  mutate(
    across(
      c(
        subdomain, analysis_variable, canonical_variable, ukb_description,
        display_name, ukb_field_id, original_data_column, parent_variable,
        one_hot_level, variable_type, notes
      ),
      ~ na_if(str_trim(as.character(.x)), "")
    ),
    display_name = coalesce(display_name, ukb_description),
    display_name_short = if_else(
      str_length(display_name) > 65,
      str_c(str_sub(display_name, 1, 62), "…"),
      display_name
    ),
    technical_level = case_when(
      is_nan_level(analysis_variable) ~ "technical_missing_category",
      is_minus_one_level(analysis_variable) &
        str_detect(
          str_to_lower(coalesce(notes, "")),
          "missing-by-design|inapplicable|not applicable|not a substantive|unresolved mixture|project-coded inactive"
        ) ~ "structural_missing_or_unresolved",
      is_minus_one_level(analysis_variable) ~ "minus_one_review_required",
      TRUE ~ "substantive"
    ),
    show_in_manuscript = !technical_level %in% c(
      "technical_missing_category",
      "structural_missing_or_unresolved",
      "minus_one_review_required"
    )
  )

# 当前模型使用的 key 有时是 Excel 的 Original data column（如 gym），
# 有时是 Excel 的 Variable name in the analysis（如 accommodation_type=House）。
# 与 Section 2 使用同一组 9 outcomes x 4 Python views。
# 不再用 list.files() 返回的路径去猜数据是否齐全，因为找不到文件时
# purrr::map_dfr(character(0), ...) 会返回没有列名的空 tibble。
expected_signed_stage2 <- tidyr::crossing(
  dataset = c("comorbidity_all", paste0("comorbidity_pattern_", 1:8, "_all")),
  python_view = c("enviro", "biomarkers", "metabolomics", "gene")
) %>%
  mutate(
    modality = dplyr::case_when(
      python_view == "enviro" ~ "enviro",
      python_view == "biomarkers" ~ "blood",
      python_view == "metabolomics" ~ "metabolites",
      python_view == "gene" ~ "gene"
    ),
    stage2_folder = file.path(stage2_dir, paste0(dataset, "_", python_view)),
    signed_file = file.path(stage2_folder, "signed_feature_importance.csv"),
    folder_exists = dir.exists(stage2_folder),
    signed_exists = file.exists(signed_file)
  )

# 无论是否有缺失，都留下可读的诊断文件。
write_log(expected_signed_stage2, "Section1_stage2_signed_input_QA.csv")
if (!dir.exists(stage2_dir)) {
  stop(
    "Section 1：找不到 Stage 2 目录：", stage2_dir, "\n",
    "请确认 Mac 上的 Result/Stage 2 已同步（Stage 3B 存在不代表 Stage 2 已同步）。"
  )
}
if (!all(expected_signed_stage2$signed_exists)) {
  missing_stage2 <- expected_signed_stage2 %>% filter(!signed_exists)
  stop(
    "Section 1：Result/Stage 2 缺少 ", nrow(missing_stage2),
    " / ", nrow(expected_signed_stage2), " 份 signed_feature_importance.csv。\n",
    "例如：\n", paste(head(missing_stage2$signed_file, 8L), collapse = "\n"), "\n",
    "详见 R_final_analysis/logs/Section1_stage2_signed_input_QA.csv。\n",
    "请先确认文件的实际存放目录，不要将 Result 和 Result 混用。"
  )
}

model_feature_universe <- purrr::map_dfr(seq_len(nrow(expected_signed_stage2)), function(i) {
  spec <- expected_signed_stage2[i, ]
  hdr <- names(data.table::fread(spec$signed_file, nrows = 0L, showProgress = FALSE))
  if (!"Feature" %in% hdr) {
    stop("Section 1：文件缺少 Feature 列：", spec$signed_file)
  }
  vals <- data.table::fread(
    spec$signed_file, select = "Feature", showProgress = FALSE
  )$Feature
  tibble::tibble(
    modality = rep(spec$modality, length(vals)),
    model_variable = as.character(vals)
  )
}) %>%
  filter(!is.na(model_variable), nzchar(model_variable)) %>%
  distinct()

if (nrow(model_feature_universe) == 0L) {
  stop("Section 1：36份 signed_feature_importance.csv 均无有效 Feature，请检查 Stage 2 输出。")
}
cat(
  "Section 1 Stage 2 检查通过：", nrow(expected_signed_stage2),
  "份 signed 文件，", nrow(model_feature_universe),
  "个域内模型特征（去重后）。\n", sep = ""
)

variable_dictionary <- variable_dictionary %>%
  rowwise() %>%
  mutate(
    model_variable = case_when(
      analysis_variable %in% model_feature_universe$model_variable[
        model_feature_universe$modality == modality
      ] ~ analysis_variable,
      !is.na(original_data_column) &
        original_data_column %in% model_feature_universe$model_variable[
          model_feature_universe$modality == modality
        ] ~ original_data_column,
      TRUE ~ analysis_variable
    )
  ) %>%
  ungroup() %>%
  select(
    modality, modality_label, subdomain, analysis_variable,
    canonical_variable, model_variable, ukb_description, display_name,
    display_name_short, ukb_field_id, original_data_column,
    parent_variable, one_hot_level, variable_type, notes,
    technical_level, show_in_manuscript, source_sheet
  ) %>%
  arrange(match(modality, sheet_spec$modality), subdomain, display_name)
# Prevent accidental reuse of the 20260925 dictionary or the former 100%-missing code.
urban_20118_levels <- paste0("home_population_density=", c("Urban", "Town", "Rural"))
if (!all(urban_20118_levels %in% variable_dictionary$analysis_variable)) {
  stop("20261025 Table S5 缺少 UKB 20118 城乡分类的三个 one-hot 变量；",
       "核对 S5 第4列及 Python 的最终 feature lineage。")
}
domain_count_qa <- variable_dictionary %>% count(modality, modality_label, name="n_dictionary_rows")
write_csv(domain_count_qa, file.path(log_dir,"20261025_S5_S8_domain_dictionary_counts.csv"))

# 重建 alias（加入最终 model_variable 列），供所有后续 Section 统一调用。
dictionary_alias <- bind_rows(
  variable_dictionary %>%
    select(-model_variable) %>%
    mutate(model_variable = analysis_variable, alias_type = "analysis_variable") %>%
    select(modality, model_variable, alias_type, everything()),
  variable_dictionary %>%
    filter(!is.na(original_data_column)) %>%
    select(-model_variable) %>%
    mutate(
      model_variable = original_data_column,
      alias_type = "original_data_column"
    ) %>%
    select(modality, model_variable, alias_type, everything())
) %>%
  arrange(modality, model_variable, factor(alias_type,
    levels = c("analysis_variable", "original_data_column")
  )) %>%
  distinct(modality, model_variable, .keep_all = TRUE)

label_model_features <- function(x, variable_col = "model_variable") {
  join_key <- setNames(c("modality", "model_variable"), c("modality", variable_col))
  x %>%
    left_join(
      dictionary_alias %>%
        select(
          modality, model_variable, analysis_variable, canonical_variable,
          display_name, display_name_short, ukb_description, subdomain,
          ukb_field_id, original_data_column, parent_variable, one_hot_level,
          variable_type, notes, technical_level, show_in_manuscript
        ),
      by = join_key
    )
}

dict_csv <- file.path(intermediate_dir, "variable_display_dictionary.csv")
dict_xlsx <- file.path(intermediate_dir, "variable_display_dictionary.xlsx")
write_csv(variable_dictionary, dict_csv)
openxlsx::write.xlsx(
  list(variable_display_dictionary = as.data.frame(variable_dictionary)),
  dict_xlsx, overwrite = TRUE, asTable = TRUE
)

dict_unmatched_model <- model_feature_universe %>%
  anti_join(
    dictionary_alias %>% distinct(modality, model_variable),
    by = c("modality", "model_variable")
  )
dict_ambiguous_display <- variable_dictionary %>%
  count(modality, analysis_variable, display_name) %>%
  group_by(modality, analysis_variable) %>%
  summarise(n_display = n_distinct(display_name), .groups = "drop") %>%
  filter(n_display != 1)
variable_dictionary_qa <- tibble(
  check = c(
    "analysis_variable maps to exactly one display_name within modality",
    "all dictionary display_name values are non-missing",
    "all current Stage 2 model features match an authoritative Excel row",
    "=nan is never manuscript-visible",
    "review-required =-1 levels are not manuscript-visible"
  ),
  observed = c(
    as.character(nrow(dict_ambiguous_display)),
    as.character(sum(is.na(variable_dictionary$display_name))),
    as.character(nrow(dict_unmatched_model)),
    as.character(sum(
      is_nan_level(variable_dictionary$analysis_variable) &
        variable_dictionary$show_in_manuscript
    )),
    as.character(sum(
      is_minus_one_level(variable_dictionary$analysis_variable) &
        variable_dictionary$show_in_manuscript
    ))
  ),
  expected = rep("0", 5),
  passed = c(
    nrow(dict_ambiguous_display) == 0,
    !anyNA(variable_dictionary$display_name),
    nrow(dict_unmatched_model) == 0,
    !any(is_nan_level(variable_dictionary$analysis_variable) &
      variable_dictionary$show_in_manuscript),
    !any(is_minus_one_level(variable_dictionary$analysis_variable) &
      variable_dictionary$show_in_manuscript)
  ),
  note = c(
    "Authoritative key is modality × Excel analysis_variable",
    "Formal display names come from Variable name in this study",
    "Matching accepts Excel analysis_variable or original_data_column",
    "Technical missing category",
    "Current five -1 rows are documented as structural/inapplicable/unresolved"
  )
)
write_log(variable_dictionary_qa, "variable_dictionary_QA.csv")
write_log(dict_unmatched_model, "variable_dictionary_unmatched_model_features.csv")
if (!all(variable_dictionary_qa$passed)) {
  stop("变量字典 QA 未通过；请检查 variable_dictionary_QA.csv。")
}

cat("完成：Section 1 master variable display dictionary\n")

update_progress_checkpoint("07 Section 1 completed", "从 07 Section 2 继续。")

###############################################################################
### Section 2. Outcome / modality / pattern lookup ####
###############################################################################
# Input: 固定项目命名与 Result 文件夹结构。
# Output: outcome_lookup、modality_lookup、expected_stage2。
# 这一段做什么：固定正文/补充材料中的 outcome 与 modality 顺序。
# 需要先运行 Section 0。

outcome_lookup <- tibble(
  dataset = c(
    "comorbidity_all", paste0("comorbidity_pattern_", 1:8, "_all")
  ),
  outcome_label = c("Overall", paste0("P", 1:8)),
  outcome_order = seq_len(9)
)
modality_lookup <- tibble(
  modality = c("enviro", "blood", "metabolites", "gene"),
  python_view = c("enviro", "biomarkers", "metabolomics", "gene"),
  modality_label = c(
    "Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors"
  ),
  modality_order = seq_len(4)
)
expected_stage2 <- crossing(outcome_lookup, modality_lookup) %>%
  mutate(
    stage2_folder = file.path(stage2_dir, paste0(dataset, "_", python_view)),
    signed_file = file.path(stage2_folder, "signed_feature_importance.csv"),
    feature_list_file = file.path(stage2_folder, "feature_list.csv"),
    perf_folds_file = file.path(stage2_folder, "perf_record_folds.csv"),
    perf_file = file.path(stage2_folder, "perf_record.csv"),
    selection_summary_file = file.path(stage2_folder, "selection_summary.csv"),
    train_file = file.path(stage2_folder, "train_data.csv"),
    test_file = file.path(stage2_folder, "test_data.csv")
  )
write_log(
  expected_stage2 %>%
    transmute(
      dataset, outcome_label, modality, python_view,
      signed_exists = file.exists(signed_file),
      feature_list_exists = file.exists(feature_list_file),
      perf_exists = file.exists(perf_file),
      train_exists = file.exists(train_file),
      test_exists = file.exists(test_file)
    ),
  "stage2_file_inventory.csv"
)
cat("完成：Section 2 outcome / modality / pattern lookup\n")

update_progress_checkpoint("07 Section 2 completed", "从 07 Section 3 继续。")

###############################################################################
### Section 3. Stage 1 signed importance ####
###############################################################################
# Input: Result/Stage 2/<outcome>_<modality>/signed_feature_importance.csv
# Output:
#   intermediate/Stage1_signed_importance_complete.csv
#   figure_source_data/FigureS1_stage1_signed_top50.csv
# 这一段做什么：读取 44 个 signed 文件；top 50 按 abs(value) 选，显示保留正负。
# 需要先运行 Section 1–2。

if (!exists("variable_dictionary")) {
  variable_dictionary <- fread(dict_csv) %>% as_tibble()
}
if (!exists("dictionary_alias")) stop("需要先运行 Section 1。")
if (!exists("expected_stage2")) stop("需要先运行 Section 2。")

signed_file_inventory <- expected_stage2 %>%
  transmute(
    dataset, outcome_label, modality, python_view,
    file = signed_file,
    exists = file.exists(file),
    size_bytes = if_else(exists, as.numeric(file.info(file)$size), NA_real_)
  )
write_log(signed_file_inventory, "signed_importance_file_inventory.csv")
if (!all(signed_file_inventory$exists)) {
  stop("缺少 signed_feature_importance.csv；详见 signed_importance_file_inventory.csv。")
}

signed_importance_all <- map_dfr(seq_len(nrow(expected_stage2)), function(i) {
  spec <- expected_stage2[i, ]
  x <- fread(spec$signed_file, showProgress = FALSE) %>% as_tibble()
  tibble(
    dataset = spec$dataset,
    outcome_label = spec$outcome_label,
    outcome_order = spec$outcome_order,
    modality = spec$modality,
    modality_label = spec$modality_label,
    modality_order = spec$modality_order,
    model_variable = as.character(x$Feature),
    importance_magnitude = as.numeric(x$Importance),
    signed_importance = as.numeric(x$Signed_importance)
  )
}) %>%
  group_by(dataset, modality) %>%
  mutate(
    abs_signed_importance = abs(signed_importance),
    signed_rank_abs = min_rank(desc(abs_signed_importance)),
    signed_l1_sum = sum(abs_signed_importance, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  label_model_features() %>%
  arrange(outcome_order, modality_order, signed_rank_abs, model_variable)

write_csv(
  signed_importance_all,
  file.path(intermediate_dir, "Stage1_signed_importance_complete.csv")
)

figureS1_features <- signed_importance_all %>%
  filter(show_in_manuscript %in% TRUE) %>%
  group_by(modality, modality_label, modality_order, model_variable, display_name) %>%
  summarise(
    max_abs_signed = max(abs_signed_importance, na.rm = TRUE),
    mean_abs_signed = mean(abs_signed_importance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(modality) %>%
  arrange(desc(max_abs_signed), desc(mean_abs_signed), display_name, .by_group = TRUE) %>%
  slice_head(n = 50) %>%
  mutate(feature_rank_modality = row_number()) %>%
  ungroup()

figureS1_stage1_signed_top50 <- figureS1_features %>%
  select(
    modality, modality_label, modality_order, model_variable,
    display_name, feature_rank_modality
  ) %>%
  inner_join(
    signed_importance_all %>%
      select(
        dataset, outcome_label, outcome_order, modality, model_variable,
        signed_importance, abs_signed_importance
      ),
    by = c("modality", "model_variable")
  ) %>%
  mutate(
    selection_metric = "Top 50 within modality by maximum absolute signed importance across outcomes",
    display_metric = "Signed Stage 1 importance"
  ) %>%
  arrange(modality_order, feature_rank_modality, outcome_order)
write_source(figureS1_stage1_signed_top50, "FigureS1_stage1_signed_top50.csv")

signed_importance_direction_qa <- signed_importance_all %>%
  group_by(outcome_label, modality_label) %>%
  summarise(
    n = n(), n_positive = sum(signed_importance > 0, na.rm = TRUE),
    n_negative = sum(signed_importance < 0, na.rm = TRUE),
    direction_note = case_when(
      n_positive > 0 & n_negative > 0 ~ "both directions present",
      n_positive > 0 ~ "positive direction only in source",
      n_negative > 0 ~ "negative direction only in source",
      TRUE ~ "no non-zero direction"
    ),
    .groups = "drop"
  )
write_log(signed_importance_direction_qa, "signed_importance_direction_QA.csv")
cat("完成：Section 3 Stage 1 signed importance\n")

update_progress_checkpoint("07 Section 3 completed", "从 07 Section 4 继续。")

###############################################################################
### Section 4. Stage 2A 1-SE selection ####
###############################################################################
# Input: feature_list.csv、perf_record.csv、perf_record_folds.csv、selection_summary.csv
# Output:
#   intermediate/Stage2A_selected_features.csv
#   figure_source_data/FigureS2_stage2A_internal_CV_1SE.csv
#   figure_source_data/FigureS3_stage2A_selected_signed_importance.csv
# 这一段做什么：仅用 internal CV 和 1-SE 输出确认 Top-N；held-out 不参与筛选。
# 需要先运行 Section 2–3。

# Read each selection_summary.csv only once and reuse it in Section 4 + Table S19.
stage2a_selection_summary <- map_dfr(
  seq_len(
    nrow(
      expected_stage2
    )
  ),
  function(i) {

    spec <- expected_stage2[i, ]

    summary <- fread(
      spec$selection_summary_file,
      showProgress = FALSE
    ) %>%
      as_tibble()

    if (!nrow(summary)) {
      stop(
        "Empty selection_summary.csv: ",
        spec$selection_summary_file
      )
    }

    summary %>%
      mutate(
        dataset = spec$dataset,
        outcome_label = spec$outcome_label,
        outcome_order = spec$outcome_order,
        modality = spec$modality,
        modality_label = spec$modality_label,
        modality_order = spec$modality_order,
        python_view = spec$python_view,
        .before = 1
      )
  }
)

write_csv(
  stage2a_selection_summary,
  file.path(
    intermediate_dir,
    "Stage2A_selection_summary.csv"
  )
)

stage2a_selected <- map_dfr(
  seq_len(
    nrow(
      expected_stage2
    )
  ),
  function(i) {

    spec <- expected_stage2[i, ]

    selected <- fread(
      spec$feature_list_file,
      showProgress = FALSE
    ) %>%
      as_tibble()

    tibble(
      dataset = spec$dataset,
      outcome_label = spec$outcome_label,
      outcome_order = spec$outcome_order,
      modality = spec$modality,
      modality_label = spec$modality_label,
      modality_order = spec$modality_order,
      python_view = spec$python_view,
      model_variable = as.character(
        selected$Feature
      ),
      selected_rank = as.integer(
        selected$Rank
      ),
      stage1_importance_magnitude = as.numeric(
        selected$Importance
      )
    )
  }
) %>%
  left_join(
    stage2a_selection_summary %>%
      transmute(
        dataset,
        modality,
        selected_n_1se =
          as.integer(
            selected_n
          ),
        one_se_threshold =
          as.numeric(
            one_se_threshold
          ),
        best_n_by_max =
          as.integer(
            best_n_by_max
          ),
        selection_rule =
          as.character(
            selection_rule
          )
      ),
    by = c(
      "dataset",
      "modality"
    )
  ) %>%
  label_model_features() %>%
  left_join(
    signed_importance_all %>%
      select(
        dataset,
        modality,
        model_variable,
        signed_importance,
        abs_signed_importance
      ),
    by = c(
      "dataset",
      "modality",
      "model_variable"
    )
  ) %>%
  arrange(
    outcome_order,
    modality_order,
    selected_rank
  )

write_csv(
  stage2a_selected,
  file.path(intermediate_dir, "Stage2A_selected_features.csv")
)

stage2a_internal_cv <- map_dfr(
  seq_len(
    nrow(
      expected_stage2
    )
  ),
  function(i) {

    spec <- expected_stage2[i, ]

    perf <- fread(
      spec$perf_file,
      showProgress = FALSE
    ) %>%
      as_tibble()

    perf %>%
      transmute(
        dataset = spec$dataset,
        outcome_label = spec$outcome_label,
        outcome_order = spec$outcome_order,
        modality = spec$modality,
        modality_label = spec$modality_label,
        modality_order = spec$modality_order,
        n_features = as.integer(
          num_features
        ),
        mean_auc = as.numeric(
          auc
        ),
        se_auc = as.numeric(
          auc_se
        ),
        mean_c_index = as.numeric(
          `c-index`
        ),
        se_c_index = as.numeric(
          `c-index_se`
        ),
        n_folds = as.integer(
          n_successful_folds
        ),
        evaluation_set =
          "Development-set internal cross-validation only"
      )
  }
) %>%
  left_join(
    stage2a_selection_summary %>%
      transmute(
        dataset,
        modality,
        selected_n =
          as.integer(
            selected_n
          ),
        one_se_threshold =
          as.numeric(
            one_se_threshold
          ),
        best_n_by_max =
          as.integer(
            best_n_by_max
          )
      ),
    by = c(
      "dataset",
      "modality"
    )
  ) %>%
  mutate(
    selected_point =
      n_features ==
      selected_n
  )
write_source(stage2a_internal_cv, "FigureS2_stage2A_internal_CV_1SE.csv")

figureS3_feature_set <- stage2a_selected %>%
  filter(show_in_manuscript %in% TRUE) %>%
  group_by(modality, modality_label, modality_order, model_variable, display_name) %>%
  summarise(
    n_selected_outcomes = n_distinct(outcome_label),
    mean_abs_signed = mean(abs_signed_importance, na.rm = TRUE),
    best_selected_rank = min(selected_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(modality) %>%
  arrange(
    desc(n_selected_outcomes), desc(mean_abs_signed),
    best_selected_rank, display_name, .by_group = TRUE
  ) %>%
  slice_head(n = 10) %>%
  mutate(feature_rank_modality = row_number()) %>%
  ungroup()

figureS3_stage2a_selected_signed <- crossing(
  figureS3_feature_set,
  outcome_lookup %>% select(dataset, outcome_label, outcome_order)
) %>%
  left_join(
    stage2a_selected %>%
      select(dataset, modality, model_variable, selected_rank),
    by = c("dataset", "modality", "model_variable")
  ) %>%
  left_join(
    signed_importance_all %>%
      select(dataset, modality, model_variable, signed_importance),
    by = c("dataset", "modality", "model_variable")
  ) %>%
  mutate(
    selected_stage2a = !is.na(selected_rank),
    plot_signed_importance = if_else(selected_stage2a, signed_importance, NA_real_),
    metric_definition = "Stage 2A feature_list membership; cell value is signed Stage 1 importance"
  ) %>%
  arrange(modality_order, feature_rank_modality, outcome_order)
write_source(
  figureS3_stage2a_selected_signed,
  "FigureS3_stage2A_selected_signed_importance.csv"
)

stage2a_signed_match_qa <- stage2a_selected %>%
  group_by(outcome_label, modality_label) %>%
  summarise(
    n_selected = n(),
    n_signed_matched = sum(!is.na(signed_importance)),
    match_rate = n_signed_matched / n_selected,
    passed = n_selected == n_signed_matched,
    .groups = "drop"
  )
write_log(stage2a_signed_match_qa, "stage2A_to_signed_match_QA.csv")
if (!all(stage2a_signed_match_qa$passed)) {
  stop("Stage 2A feature_list 无法全部匹配 signed importance。")
}
cat("完成：Section 4 Stage 2A 1-SE selection\n")

update_progress_checkpoint("07 Section 4 completed", "从 07 Section 5 继续。")

###############################################################################
### Section 5. Stage 2B adjusted associations ####
###############################################################################
# Input:
#   Stage 2A feature_list.csv、同一 outcome × modality 的 test_data.csv；
#   source/final_environment.csv 中的核心协变量。
# Output:
#   intermediate/Stage2B_heldout_adjusted_associations_ethnicity_strata.csv
#   figure_source_data/FigureS4_nine_outcomes_stage2B_associations.csv（历史文件名，内容仅 Overall）
#   logs/stage2B_estimability_QA.csv
# 这一段做什么：在独立 held-out set 评估 Stage 2A 入选特征的调整后 HR；
#   ethnicity_group 保留为预设协变量，但通过 strata(ethnicity_group) 控制，
#   不估计 ethnicity 各水平的 HR，从而避免稀有 ethnicity level 造成不稳定系数。
#   不改变 Stage 1/2A 模型或 1-SE 选择。
# 需要先运行 Section 2 与 Section 4。

stage2b_output <- file.path(
  intermediate_dir,
  "Stage2B_heldout_adjusted_associations_20261025_9outcomes.csv"
)
REBUILD_STAGE2B <- isTRUE(getOption("ukb.v8.rebuild_stage2b", FALSE)) ||
  !file.exists(stage2b_output)

core_covariates <- c(
  "recruitment_age", "sex", "ethnicity_group", "recruitment_centre",
  "IMD_total", "BMI", "smoking_status"
)

covariate_file <- file.path(
  raw_source_dir,
  "final_environment.csv"
)

if (!file.exists(covariate_file)) {
  stop("缺少 Stage 2B 协变量源：", covariate_file)
}

# Single read reused by Section 5 and Section 6B.
stage2b_covariates <- fread(
  covariate_file,
  select = c("eid", core_covariates),
  showProgress = FALSE
) %>%
  as_tibble() %>%
  mutate(
    eid = as.character(eid),
    sex = factor(sex),
    ethnicity_group = factor(ethnicity_group),
    recruitment_centre = factor(recruitment_centre),
    smoking_status = factor(smoking_status)
  ) %>%
  rename_with(
    ~ paste0("cov_", .x),
    -eid
  ) %>%
  distinct(
    eid,
    .keep_all = TRUE
  )

if (REBUILD_STAGE2B) {

  fit_one_stage2b <- function(dat, model_variable, parent_variable, outcome_label) {
    empty_result <- function(reason, n = 0L, events = 0L,
                             adjustment_strategy = NA_character_,
                             cox_warning = NA_character_) {
      tibble(
        HR = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
        beta = NA_real_, SE = NA_real_, P = NA_real_,
        N = as.integer(n), events = as.integer(events),
        estimable = FALSE, failure_reason = reason,
        adjustment_strategy = adjustment_strategy,
        cox_warning = cox_warning
      )
    }
    if (!model_variable %in% names(dat)) {
      return(empty_result("feature_missing_from_test_data"))
    }

    # Ethnicity is controlled through Cox strata, not through a coefficient.
    parent_key <- dplyr::coalesce(parent_variable, model_variable)
    linear_covariates <- setdiff(core_covariates, "ethnicity_group")
    if (parent_key %in% linear_covariates) {
      linear_covariates <- setdiff(linear_covariates, parent_key)
    }
    use_ethnicity_strata <- !identical(parent_key, "ethnicity_group")

    linear_terms <- paste0("cov_", linear_covariates)
    strata_col <- if (use_ethnicity_strata) "cov_ethnicity_group" else character()
    required <- unique(c("time", "status", model_variable, linear_terms, strata_col))
    missing_required <- setdiff(required, names(dat))
    if (length(missing_required)) {
      return(empty_result(
        paste0("missing_columns: ", paste(missing_required, collapse = "; "))
      ))
    }

    fit_dat <- dat %>%
      transmute(
        time = as.numeric(time),
        status = as.integer(status),
        feature_value = as.numeric(.data[[model_variable]]),
        across(all_of(c(linear_terms, strata_col)))
      ) %>%
      filter(if_all(everything(), ~ !is.na(.))) %>%
      droplevels()

    n_fit <- nrow(fit_dat)
    n_event <- sum(fit_dat$status == 1L)
    if (n_fit < 20L || n_event < 5L || n_distinct(fit_dat$feature_value) < 2L) {
      return(empty_result("insufficient_N_events_or_variation", n_fit, n_event))
    }

    values <- unique(fit_dat$feature_value)
    if (all(values %in% c(0, 1))) {
      sparse <- fit_dat %>%
        group_by(feature_value) %>%
        summarise(n_group = n(), events_group = sum(status), .groups = "drop") %>%
        complete(
          feature_value = c(0, 1),
          fill = list(n_group = 0L, events_group = 0L)
        )
      if (any(sparse$n_group < 10L) || any(sparse$events_group < 5L)) {
        return(empty_result("binary_group_N<10_or_events<5", n_fit, n_event))
      }
    }

    rhs_terms <- c(
      "feature_value",
      linear_terms,
      if (use_ethnicity_strata) "strata(cov_ethnicity_group)" else character()
    )
    adjustment_strategy <- paste(
      c(
        linear_covariates,
        if (use_ethnicity_strata) "ethnicity_group [Cox strata]" else character()
      ),
      collapse = ", "
    )

    warning_messages <- character()
    fit <- tryCatch(
      withCallingHandlers(
        coxph(
          as.formula(paste("Surv(time, status) ~", paste(rhs_terms, collapse = " + "))),
          data = fit_dat, ties = "efron"
        ),
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      return(empty_result(
        conditionMessage(fit), n_fit, n_event, adjustment_strategy,
        paste(unique(warning_messages), collapse = " | ")
      ))
    }

    # If coxph specifically flags feature_value as potentially infinite,
    # do not expose that estimate as a valid association.
    flagged_feature <- FALSE
    if (length(warning_messages)) {
      coef_names <- names(coef(fit))
      for (msg in warning_messages) {
        hit <- stringr::str_match(msg, "variable\\s+([0-9,]+)")
        if (!is.na(hit[1, 2])) {
          idx <- suppressWarnings(as.integer(strsplit(hit[1, 2], ",", fixed = TRUE)[[1]]))
          idx <- idx[is.finite(idx) & idx >= 1L & idx <= length(coef_names)]
          if (length(idx) && "feature_value" %in% coef_names[idx] &&
              stringr::str_detect(msg, fixed("coefficient may be infinite"))) {
            flagged_feature <- TRUE
          }
        }
      }
    }

    z <- broom::tidy(fit, conf.int = TRUE) %>% filter(term == "feature_value")
    if (nrow(z) != 1L || !is.finite(z$estimate) || flagged_feature) {
      return(empty_result(
        ifelse(flagged_feature, "feature_coefficient_may_be_infinite", "coefficient_not_finite"),
        n_fit, n_event, adjustment_strategy,
        paste(unique(warning_messages), collapse = " | ")
      ))
    }

    tibble(
      HR = exp(z$estimate), CI_low = exp(z$conf.low),
      CI_high = exp(z$conf.high), beta = z$estimate,
      SE = z$std.error, P = z$p.value,
      N = n_fit, events = n_event, estimable = TRUE,
      failure_reason = NA_character_,
      adjustment_strategy = adjustment_strategy,
      cox_warning = if (length(warning_messages)) {
        paste(unique(warning_messages), collapse = " | ")
      } else NA_character_
    )
  }

  stage2b_targets <- stage2a_selected %>%
    filter(show_in_manuscript %in% TRUE) %>%
    distinct(
      dataset, outcome_label, outcome_order, modality, modality_label,
      modality_order, python_view, model_variable, analysis_variable,
      canonical_variable, parent_variable, display_name, selected_rank,
      .keep_all = TRUE
    )

  write_log(
    stage2a_selected %>%
      filter(!(show_in_manuscript %in% TRUE)) %>%
      select(dataset, outcome_label, modality, model_variable, display_name,
             technical_level, show_in_manuscript),
    "stage2B_excluded_technical_levels.csv"
  )

  stage2b_groups <- stage2b_targets %>%
    distinct(dataset, outcome_label, modality, python_view)
  stage2b_results_list <- vector("list", nrow(stage2b_groups))

  for (g in seq_len(nrow(stage2b_groups))) {
    spec <- stage2b_groups[g, ]
    targets <- stage2b_targets %>%
      filter(dataset == spec$dataset, modality == spec$modality)
    test_file <- file.path(
      stage2_dir, paste0(spec$dataset, "_", spec$python_view), "test_data.csv"
    )
    if (!file.exists(test_file)) {
      stage2b_results_list[[g]] <- targets %>%
        mutate(
          HR = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
          beta = NA_real_, SE = NA_real_, P = NA_real_, N = 0L,
          events = 0L, estimable = FALSE,
          failure_reason = "Stage 2 test_data.csv missing",
          adjustment_strategy = NA_character_, cox_warning = NA_character_
        )
      next
    }
    test_dat <- fread(test_file, showProgress = FALSE) %>%
      as_tibble() %>%
      mutate(eid = as.character(eid)) %>%
      left_join(stage2b_covariates, by = "eid")

    cat(
      "Section 5：", spec$outcome_label, " × ", spec$modality,
      "，held-out features = ", nrow(targets), "\n", sep = ""
    )

    stage2b_results_list[[g]] <- map_dfr(seq_len(nrow(targets)), function(j) {
      target <- targets[j, ]
      bind_cols(
        target,
        fit_one_stage2b(
          test_dat,
          target$model_variable,
          target$parent_variable,
          target$outcome_label
        )
      )
    })
    rm(test_dat)
    invisible(gc())
  }

  stage2b_associations <- bind_rows(stage2b_results_list) %>%
    group_by(dataset, modality) %>%
    mutate(
      FDR = if_else(estimable, p.adjust(P, method = "BH"), NA_real_),
      log_HR = log(HR), log2_HR = log2(HR),
      evaluation_set = "Independent held-out evaluation set",
      ethnicity_adjustment = "Cox baseline-hazard stratification"
    ) %>%
    ungroup()
  write_csv(stage2b_associations, stage2b_output)
} else {
  stage2b_associations <- fread(stage2b_output, showProgress = FALSE) %>%
    as_tibble()
  cat(
    "Section 5：读取已存在的 Stage2B v8 ethnicity-stratified cache；如需重算请设置 ",
    "options(ukb.v8.rebuild_stage2b = TRUE)。\n", sep = ""
  )
}

write_table(
  stage2b_associations,
  "Table_S20_Stage2B_heldout_adjusted_associations.csv"
)
stage2b_estimability_qa <- stage2b_associations %>%
  count(dataset, outcome_label, modality, estimable, failure_reason) %>%
  arrange(outcome_label, modality, desc(estimable))
write_log(stage2b_estimability_qa, "stage2B_estimability_QA.csv")

# Figure S4 (Overall only): increase to 40 objectively selected predictors.
# Domain quota: E 12, P 10, M 10, G 8. Order is determined exclusively by
# development-set Stage 1 absolute signed importance, never held-out HR/P/FDR.
# Table S20 retains all nine outcomes and ALL Stage 2B estimates unchanged.
figureS4_domain_quota <- c(enviro = 12L, blood = 10L, metabolites = 10L, gene = 8L)
select_figS4_features <- function(dat, quota = figureS4_domain_quota) {
  selected <- purrr::imap_dfr(quota, function(n_keep, domain_key) {
    dat %>%
      filter(modality == domain_key) %>%
      arrange(desc(abs_signed_importance), selected_rank, display_name) %>%
      slice_head(n = n_keep)
  })
  selected %>%
    arrange(modality_order, desc(abs_signed_importance), selected_rank, display_name)
}

figureS4_feature_set <- stage2a_selected %>%
  filter(outcome_label == "Overall", show_in_manuscript %in% TRUE) %>%
  group_by(outcome_label) %>%
  group_modify(~ select_figS4_features(.x)) %>%
  ungroup() %>%
  select(
    dataset, outcome_label, outcome_order, modality, modality_label,
    modality_order, model_variable, analysis_variable, display_name,
    signed_importance, abs_signed_importance, selected_rank
  )

figureS4_modality_balance_QA <- tibble(
  modality = names(figureS4_domain_quota),
  n_target = as.integer(figureS4_domain_quota)
) %>%
  left_join(
    figureS4_feature_set %>% count(modality, name = "n_displayed"),
    by = "modality"
  ) %>%
  mutate(n_displayed = coalesce(n_displayed, 0L),
         passed = n_displayed == n_target) %>%
  left_join(modality_lookup %>% select(modality, modality_label, modality_order),
            by = "modality") %>%
  arrange(modality_order)
write_log(figureS4_modality_balance_QA, "FigureS4_modality_balance_QA.csv")
if (any(!figureS4_modality_balance_QA$passed) ||
    nrow(figureS4_feature_set) != sum(figureS4_domain_quota)) {
  stop("Figure S4 selection quota QA failed: inspect FigureS4_modality_balance_QA.csv")
}

figureS4_associations <- figureS4_feature_set %>%
  left_join(
    stage2b_associations %>%
      select(
        dataset, modality, model_variable, HR, CI_low, CI_high,
        beta, SE, P, FDR, log2_HR, N, events, estimable, failure_reason,
        evaluation_set, adjustment_strategy, ethnicity_adjustment, cox_warning
      ),
    by = c("dataset", "modality", "model_variable")
  ) %>%
  arrange(outcome_order, desc(abs(signed_importance)), modality_order)

write_source(
  figureS4_associations,
  "FigureS4_nine_outcomes_stage2B_associations.csv"
)
cat("完成：Section 5 Stage 2B held-out adjusted associations（ethnicity 采用 Cox strata）\n")

update_progress_checkpoint("07 Section 5 completed", "从 07 Section 6 继续。")

###############################################################################
### Section 6. Stage 3 results + interpretable risk-oriented domain scores ####
###############################################################################
# Input:
#   Result/Stage 3、Result/Stage 3B 现有模型输出；
#   Stage 2 train/test matrices；Stage 2A selected features；
#   source/final_environment.csv 中的基础协变量。
# Output:
#   1) Stage 3 performance/final predictors，以及 Figure 4c 所需原始 partial-score Wald 结果；
#   2) Figure 5：从 Result/Stage 3B/<outcome>/coxem_coefficients_{E,B,M,G}.csv
#      读取60%开发集固定 Stage 2A 变量后重拟合的 CoxEM Weighted_beta。
#      - Python B 对应论文 P，只有显示名为 P，不改 Python 原始文件名；
#      - score = sum(Weighted_beta_j * Python Stage 2 特征值_j)；保护性系数保留负号；
#      - 不在 R 内重新拟合权重，不对原始特征随意二分类或第二次标准化；
#      - domain score 在开发集标准化，P/M/G 综合及四域综合均等权；
#      - 40% held-out 仅用于关联估计、联合风险分组、KM 和 HR；
#      - 开发集确定中位数及三分位数分组阈值。
#   3) Figure 5a–c held-out associations；Figure 5 不调整其他 domain scores。
# 重要：Figure 4c 继续解释 final Stage 3 model architecture；Figure 5 使用新的解释性风险评分，
#      二者不得混为一套 score。
# 需要先运行 Section 1–5。

if (!exists("stage2a_selected")) {
  stage2a_selected <- fread(
    file.path(intermediate_dir, "Stage2A_selected_features.csv"),
    showProgress = FALSE
  ) %>% as_tibble()
}

stage3_folder_info <- outcome_lookup %>%
  mutate(
    stage3_folder = file.path(stage3_dir, dataset),
    stage3b_folder = file.path(stage3b_dir, dataset),
    importance_file = file.path(stage3_folder, "feature_importance_and_coefficients.csv"),
    performance_file = file.path(stage3_folder, "performance.csv"),
    stage3b_performance_file = file.path(
      stage3b_folder, "stage3b_combination_performance.csv"
    )
  )


# Small Stage 3 coefficient files are cached once per dataset and reused by:
#   stage3_feature_importance, Figure 4c partial-score reconstruction,
#   Figure S11 environmental subdomains, and Figure S13 integrated score.
stage3_coef_cache <- new.env(
  parent = emptyenv()
)

get_stage3_coefficients <- function(dataset) {

  if (
    exists(
      dataset,
      envir = stage3_coef_cache,
      inherits = FALSE
    )
  ) {
    return(
      get(
        dataset,
        envir = stage3_coef_cache,
        inherits = FALSE
      )
    )
  }

  coef_file <- file.path(
    stage3_dir,
    dataset,
    "feature_importance_and_coefficients.csv"
  )

  if (!file.exists(coef_file)) {
    return(NULL)
  }

  z <- fread(
    coef_file,
    showProgress = FALSE
  )

  assign(
    dataset,
    z,
    envir = stage3_coef_cache
  )

  z
}

stage3b_performance <- map_dfr(seq_len(nrow(stage3_folder_info)), function(i) {
  spec <- stage3_folder_info[i, ]
  if (!file.exists(spec$stage3b_performance_file)) return(tibble())
  fread(spec$stage3b_performance_file, showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(
      dataset = spec$dataset,
      outcome_label = spec$outcome_label,
      outcome_order = spec$outcome_order,
      evaluation_set = "Independent held-out evaluation set"
    )
})

stage3_feature_importance <- map_dfr(seq_len(nrow(stage3_folder_info)), function(i) {
  spec <- stage3_folder_info[i, ]
  coef_i <- get_stage3_coefficients(spec$dataset)
  if (is.null(coef_i)) return(tibble())
  as_tibble(coef_i) %>%
    transmute(
      dataset = spec$dataset,
      outcome_label = spec$outcome_label,
      outcome_order = spec$outcome_order,
      model_variable = as.character(Feature),
      final_predictor_importance = as.numeric(Importance),
      weighted_beta = as.numeric(Weighted_beta),
      python_view = as.character(Views),
      final_importance_rank = as.integer(Importance_rank),
      large_factor = as.character(Large_factor),
      small_factor = as.character(Small_factor),
      modality = recode(
        python_view,
        enviro = "enviro", biomarkers = "blood",
        metabolomics = "metabolites", gene = "gene",
        .default = python_view
      )
    ) %>%
    left_join(modality_lookup, by = c("modality", "python_view")) %>%
    label_model_features()
}) %>%
  arrange(outcome_order, final_importance_rank)

write_csv(
  stage3b_performance,
  file.path(intermediate_dir, "Stage3B_combination_performance.csv")
)
write_csv(
  stage3_feature_importance,
  file.path(intermediate_dir, "Stage3_final_predictors.csv")
)

# -----------------------------------------------------------------------------
# 6A. 原始 Stage 3 partial scores：用于 Figure 4e / final-model architecture
# -----------------------------------------------------------------------------
view_map <- c(E = "enviro", P = "biomarkers", M = "metabolomics", G = "gene")
view_label <- c(
  E = "Environmental and clinical characteristics", P = "Phenotypic biomarkers",
  M = "Metabolomic biomarkers", G = "Genetic factors"
)
modality_code <- c(enviro = "E", blood = "P", metabolites = "M", gene = "G")

read_partial_split <- function(dataset, split = c("train", "test")) {

  split <- match.arg(split)

  coef_dat <- get_stage3_coefficients(
    dataset
  )

  if (is.null(coef_dat)) {
    return(NULL)
  }

  pieces <- list()

  for (short in names(view_map)) {

    py_view <- view_map[[short]]

    coef_view <- coef_dat[
      Views == py_view,
      .(
        Feature = as.character(Feature),
        Weighted_beta = as.numeric(Weighted_beta)
      )
    ]

    if (!nrow(coef_view)) {
      return(NULL)
    }

    if (anyDuplicated(coef_view$Feature)) {
      stop(
        "Stage 3 final coefficients contain duplicated features within view: ",
        dataset,
        " / ",
        py_view
      )
    }

    f <- file.path(
      stage2_dir,
      paste0(
        dataset,
        "_",
        py_view
      ),
      paste0(
        split,
        "_data.csv"
      )
    )

    if (!file.exists(f)) {
      return(NULL)
    }

    features <- coef_view$Feature

    header <- names(
      fread(
        f,
        nrows = 0,
        showProgress = FALSE
      )
    )

    if (any(!features %in% header)) {
      return(NULL)
    }

    z <- fread(
      f,
      select = c(
        "eid",
        "status",
        "time",
        features
      ),
      showProgress = FALSE
    )

    beta <- coef_view$Weighted_beta[
      match(
        features,
        coef_view$Feature
      )
    ]

    if (
      length(beta) != length(features) ||
        any(!is.finite(beta))
    ) {
      stop(
        "Invalid signed Weighted_beta for ",
        dataset,
        " / ",
        py_view
      )
    }

    z[
      ,
      paste0(
        short,
        "_raw"
      ) :=
        as.vector(
          as.matrix(.SD) %*% beta
        ),
      .SDcols = features
    ]

    pieces[[short]] <- z[
      ,
      c(
        "eid",
        "status",
        "time",
        paste0(
          short,
          "_raw"
        )
      ),
      with = FALSE
    ]
  }

  out <- pieces[[1]]

  for (short in names(pieces)[-1]) {

    out <- merge(
      out,
      pieces[[short]][
        ,
        c(
          "eid",
          paste0(
            short,
            "_raw"
          )
        ),
        with = FALSE
      ],
      by = "eid",
      all = FALSE
    )
  }

  out[
    ,
    eid :=
      as.character(
        eid
      )
  ]

  out
}

score_env_subdomains <- function(dataset, split = c("train", "test")) {
  split <- match.arg(split)

  coef_dat <- get_stage3_coefficients(
    dataset
  )

  f <- file.path(
    stage2_dir,
    paste0(dataset, "_enviro"),
    paste0(split, "_data.csv")
  )

  if (is.null(coef_dat) || !file.exists(f)) return(NULL)

  coef_env <- as_tibble(coef_dat) %>%
    filter(Views == "enviro") %>%
    transmute(model_variable = Feature, weighted_beta = Weighted_beta) %>%
    mutate(modality = "enviro") %>%
    label_model_features() %>%
    mutate(subdomain = coalesce(subdomain, "Other"))
  header <- names(fread(f, nrows = 0, showProgress = FALSE))
  if (nrow(coef_env) == 0L || any(!coef_env$model_variable %in% header)) return(NULL)
  z <- fread(
    f,
    select = c("eid", "status", "time", coef_env$model_variable),
    showProgress = FALSE
  )
  subdomains <- sort(unique(coef_env$subdomain))
  lookup <- tibble(
    subdomain = subdomains,
    score_term = paste0("ES", sprintf("%02d", seq_along(subdomains)))
  )
  for (k in seq_len(nrow(lookup))) {
    rows <- which(coef_env$subdomain == lookup$subdomain[[k]])
    features <- coef_env$model_variable[rows]
    beta <- coef_env$weighted_beta[rows]
    z[, (lookup$score_term[[k]]) := as.vector(as.matrix(.SD) %*% beta),
      .SDcols = features]
  }
  z[, eid := as.character(eid)]
  list(
    data = z[, c("eid", "status", "time", lookup$score_term), with = FALSE],
    lookup = lookup
  )
}

tidy_score_fit <- function(fit, terms, dataset, outcome_label) {
  broom::tidy(fit, conf.int = TRUE) %>%
    filter(term %in% terms) %>%
    transmute(
      dataset, outcome_label, term,
      beta = estimate, SE = std.error, HR = exp(estimate),
      CI_low = exp(conf.low), CI_high = exp(conf.high), P = p.value
    )
}

partial_score_qa_list <- list()
modality_assoc_list <- list()
wald_subdomain_list <- list()

for (i in seq_len(nrow(outcome_lookup))) {
  dataset_i <- outcome_lookup$dataset[[i]]
  outcome_i <- outcome_lookup$outcome_label[[i]]
  dev <- read_partial_split(dataset_i, "train")
  held <- read_partial_split(dataset_i, "test")
  if (is.null(dev) || is.null(held)) {
    partial_score_qa_list[[i]] <- tibble(
      dataset = dataset_i, outcome_label = outcome_i,
      stage2_matrices_available = FALSE,
      n_development = 0L, n_heldout = 0L,
      risk_score_residual_sd = NA_real_,
      risk_score_residual_range = NA_real_,
      exact_up_to_additive_constant = FALSE,
      note = "Required Stage 2 matrices missing; no substitute used"
    )
    next
  }

  raw_terms <- paste0(names(view_map), "_raw")
  for (term in raw_terms) {
    centre <- mean(dev[[term]], na.rm = TRUE)
    scale <- safe_sd(dev[[term]])
    zterm <- str_replace(term, "_raw$", "_z")
    dev[[zterm]] <- (dev[[term]] - centre) / scale
    held[[zterm]] <- (held[[term]] - centre) / scale
  }
  score_terms <- paste0(names(view_map), "_z")
  complete_held <- as_tibble(held) %>%
    filter(if_all(all_of(c("time", "status", score_terms)), is.finite))

  # Figure 4c：四个原始 final-model partial scores 同时进入 held-out Cox。
  fit_main <- coxph(
    Surv(time, status) ~ E_z + P_z + M_z + G_z,
    data = complete_held, ties = "efron"
  )
  assoc <- tidy_score_fit(fit_main, score_terms, dataset_i, outcome_i) %>%
    mutate(
      component_code = str_remove(term, "_z$"),
      component = unname(view_label[component_code]),
      wald_chisq = (beta / SE)^2,
      relative_wald_share = wald_chisq / sum(wald_chisq),
      N = nrow(complete_held), events = sum(complete_held$status),
      evaluation_set = "Independent held-out evaluation set",
      model_definition = "All four final-model partial scores entered simultaneously"
    )
  modality_assoc_list[[i]] <- assoc

  # 验证四个分量之和与 Python 保存总 risk score 仅相差加性常数。
  held_score_file <- file.path(stage3_dir, dataset_i, "heldout_risk_scores.csv")
  if (file.exists(held_score_file)) {
    held_score <- fread(
      held_score_file, select = c("eid", "risk_score"), showProgress = FALSE
    )
    held_score[, eid := as.character(eid)]
    score_check <- merge(
      held[, c("eid", raw_terms), with = FALSE], held_score, by = "eid", all = FALSE
    )
    score_check[, reconstructed := rowSums(.SD), .SDcols = raw_terms]
    residual <- score_check$reconstructed - score_check$risk_score
    residual_sd <- sd(residual)
    residual_range <- diff(range(residual))
  } else {
    residual_sd <- NA_real_
    residual_range <- NA_real_
  }

  # Figure S11：将 E partial score 按字典 subdomain 拆分。
  dev_env <- score_env_subdomains(dataset_i, "train")
  held_env <- score_env_subdomains(dataset_i, "test")
  if (!is.null(dev_env) && !is.null(held_env)) {
    env_lookup <- dev_env$lookup
    dev_wald <- merge(
      dev_env$data,
      dev[, c("eid", "P_raw", "M_raw", "G_raw"), with = FALSE],
      by = "eid", all = FALSE
    )
    held_wald <- merge(
      held_env$data,
      held[, c("eid", "P_raw", "M_raw", "G_raw"), with = FALSE],
      by = "eid", all = FALSE
    )
    wald_raw_terms <- c(env_lookup$score_term, "P_raw", "M_raw", "G_raw")
    wald_z_terms <- paste0(wald_raw_terms, "_z")
    for (k in seq_along(wald_raw_terms)) {
      centre <- mean(dev_wald[[wald_raw_terms[[k]]]], na.rm = TRUE)
      scale <- safe_sd(dev_wald[[wald_raw_terms[[k]]]])
      dev_wald[[wald_z_terms[[k]]]] <-
        (dev_wald[[wald_raw_terms[[k]]]] - centre) / scale
      held_wald[[wald_z_terms[[k]]]] <-
        (held_wald[[wald_raw_terms[[k]]]] - centre) / scale
    }
    wald_fit_dat <- as_tibble(held_wald) %>%
      filter(if_all(all_of(c("time", "status", wald_z_terms)), is.finite))
    wald_fit <- coxph(
      as.formula(paste(
        "Surv(time, status) ~", paste(wald_z_terms, collapse = " + ")
      )),
      data = wald_fit_dat, ties = "efron"
    )
    wald_subdomain_list[[i]] <- tidy_score_fit(
      wald_fit, wald_z_terms, dataset_i, outcome_i
    ) %>%
      mutate(raw_term = str_remove(term, "_z$")) %>%
      left_join(env_lookup, by = c("raw_term" = "score_term")) %>%
      mutate(
        component = case_when(
          !is.na(subdomain) ~ subdomain,
          raw_term == "P_raw" ~ "Phenotypic biomarkers",
          raw_term == "M_raw" ~ "Metabolomic biomarkers",
          raw_term == "G_raw" ~ "Genetic factors",
          TRUE ~ "Other"
        ),
        component_type = if_else(
          !is.na(subdomain), "Environmental and clinical characteristics subdomain", "Omics modality"
        ),
        wald_chisq = (beta / SE)^2,
        relative_wald_share = wald_chisq / sum(wald_chisq),
        N = nrow(wald_fit_dat), events = sum(wald_fit_dat$status)
      )
  }


  exact_score_match <-
    is.finite(
      residual_sd
    ) &&
    is.finite(
      residual_range
    ) &&
    residual_sd <
      1e-10 &&
    residual_range <
      1e-10

  partial_score_qa_list[[i]] <- tibble(
    dataset = dataset_i,
    outcome_label = outcome_i,
    stage2_matrices_available = TRUE,
    n_development = nrow(dev),
    n_heldout = nrow(held),
    risk_score_residual_sd = residual_sd,
    risk_score_residual_range = residual_range,
    exact_up_to_additive_constant = exact_score_match,
    note =
      if_else(
        exact_score_match,
        "R-reconstructed final-model score matches the saved Python score up to an additive constant",
        "R-reconstructed final-model score does not exactly match the saved Python score; Python score is retained only as a reconstruction QA comparator"
      )
  )
  cat(
    "Section 6A：", outcome_i, " final-model partial-score held-out n = ", nrow(held),
    "; residual SD = ", signif(residual_sd, 3), "\n", sep = ""
  )
}

partial_score_qa <- bind_rows(partial_score_qa_list)
modality_score_associations <- bind_rows(modality_assoc_list)
environment_subdomain_wald <- bind_rows(wald_subdomain_list)

write_log(partial_score_qa, "stage3_partial_score_reconstruction_QA.csv")
write_csv(
  modality_score_associations,
  file.path(intermediate_dir, "Stage3_modality_score_associations.csv")
)
write_csv(
  environment_subdomain_wald,
  file.path(intermediate_dir, "Stage3_environment_subdomain_wald.csv")
)

# -----------------------------------------------------------------------------
# 6B. Figure 5：development-derived, risk-oriented weighted domain scores
# -----------------------------------------------------------------------------
# Final v8 Figure 5 contract:
#   1) Figure 5 uses Stage 2A-selected features surviving documented Stage 3B technical
#      filters, including any selected age/sex/ethnicity/centre covariates.
#   2) Direction and weight come DIRECTLY from development-fixed-feature Stage 3B
#      single-domain CoxEM Weighted_beta, NOT Stage 1 signed importance.
#      R does NOT refit a weighting Cox model or flip weights using held-out outcomes.
#   3) Continuous Stage 2 feature columns are used exactly as stored after Python
#      preprocessing; no second feature-level standardization is done in R.
#   4) Domain raw score = sum(Weighted_beta_j * X_j). Signed weights already encode
#      risk/protective direction. No extra reverse coding or weight renormalization is
#      required in R; a common scaling constant would disappear after score z-scaling.
#   5) Each domain score is standardized using development-set mean/SD only.
#   6) Figure 5a uses the continuous standardized domain score (HR per 1 SD).
#   7) Figure 5b/c use DEVELOPMENT-DEFINED median binary splits for joint profiles:
#      Lower risk vs Higher risk; the same cut points are applied unchanged to held-out.
#   8) Combined omics = equal-weighted mean of P/M/G domain risk z-scores (1/3 each),
#      followed by development-set re-standardization.
#   9) Figure 5 includes Overall and P1–P8.
#  10) Held-out Figure 5 Cox associations are UNADJUSTED by default. To inspect the
#      covariate-adjusted version, set options(ukb.v8.figure5_adjust_association = TRUE)
#      BEFORE running Section 6B. When adjustment is enabled, a covariate already
#      represented by a score feature (or its parent variable) is not adjusted again.

figure5_base_covariates <- c(
  "recruitment_age", "sex", "ethnicity_group", "recruitment_centre"
)

if (!exists("stage2b_covariates")) {
  stop(
    "Section 6B 需要先运行 Section 5，以复用已经读取的 final_environment 协变量。"
  )
}

required_figure5_covariates <- paste0(
  "cov_",
  figure5_base_covariates
)

missing_figure5_covariates <- setdiff(
  c(
    "eid",
    required_figure5_covariates
  ),
  names(stage2b_covariates)
)

if (length(missing_figure5_covariates)) {
  stop(
    "Section 5 cached covariates 缺少 Figure 5 需要的变量：",
    paste(
      missing_figure5_covariates,
      collapse = "; "
    )
  )
}

figure5_covariates <- stage2b_covariates %>%
  select(
    eid,
    all_of(
      required_figure5_covariates
    )
  )

split_parent_keys <- function(x) {
  x <- unique(na.omit(as.character(x)))
  if (!length(x)) return(character())
  x <- unlist(str_split(x, ";"), use.names = FALSE)
  unique(str_trim(x[nzchar(str_trim(x))]))
}

# Identify conventional baseline covariates (including one-hot descendants) for QA
# and for optional held-out association adjustment. IMPORTANT: this is an AUDIT flag
# only. Stage 2A-selected baseline/demographic features remain in the Figure 5 score.
mark_baseline_adjustment_features <- function(meta) {
  if (!nrow(meta)) return(meta %>% mutate(.baseline_adjustment_feature = logical()))
  meta %>%
    rowwise() %>%
    mutate(
      .baseline_adjustment_feature = {
        keys <- unique(c(
          as.character(model_variable),
          as.character(analysis_variable),
          as.character(canonical_variable),
          split_parent_keys(parent_variable)
        ))
        keys <- str_trim(keys)
        keys <- keys[!is.na(keys) & nzchar(keys)]
        any(keys %in% figure5_base_covariates)
      }
    ) %>%
    ungroup()
}

figure5_adjustment_spec <- function(feature_meta, outcome_label) {
  covs <- figure5_base_covariates

  overlap_keys <- unique(c(
    as.character(feature_meta$model_variable),
    as.character(feature_meta$analysis_variable),
    as.character(feature_meta$canonical_variable),
    split_parent_keys(feature_meta$parent_variable)
  ))
  overlap_keys <- str_trim(overlap_keys)
  overlap_keys <- overlap_keys[!is.na(overlap_keys) & nzchar(overlap_keys)]

  overlap <- intersect(covs, overlap_keys)
  controlled <- setdiff(covs, overlap)

  use_ethnicity_strata <- "ethnicity_group" %in% controlled
  linear_covariates <- setdiff(controlled, "ethnicity_group")
  linear_terms <- paste0("cov_", linear_covariates)
  strata_col <- if (use_ethnicity_strata) "cov_ethnicity_group" else NA_character_
  strata_formula_term <- if (use_ethnicity_strata) {
    "strata(cov_ethnicity_group)"
  } else character()

  adjustment_label <- paste(
    c(
      linear_covariates,
      if (use_ethnicity_strata) "ethnicity_group [Cox strata]" else character()
    ),
    collapse = ", "
  )

  list(
    covariates = controlled,
    linear_covariates = linear_covariates,
    terms = linear_terms,
    use_ethnicity_strata = use_ethnicity_strata,
    strata_col = strata_col,
    strata_formula_term = strata_formula_term,
    excluded_overlap = overlap,
    adjustment_label = adjustment_label
  )
}

# Figure 5 association-adjustment switch. Default = FALSE, as requested.
# To obtain the adjusted sensitivity version, run before Section 6B:
# options(ukb.v8.figure5_adjust_association = TRUE)
figure5_adjust_association <- isTRUE(
  getOption("ukb.v8.figure5_adjust_association", FALSE)
)

figure5_unadjusted_spec <- function() {
  list(
    covariates = character(),
    linear_covariates = character(),
    terms = character(),
    use_ethnicity_strata = FALSE,
    strata_col = NA_character_,
    strata_formula_term = character(),
    excluded_overlap = character(),
    adjustment_label = "Unadjusted",
    association_adjusted = FALSE
  )
}

figure5_association_adjustment_spec <- function(feature_meta, outcome_label) {
  if (!isTRUE(figure5_adjust_association)) {
    return(figure5_unadjusted_spec())
  }
  z <- figure5_adjustment_spec(feature_meta, outcome_label)
  z$association_adjusted <- TRUE
  if (!nzchar(z$adjustment_label)) z$adjustment_label <- "Adjusted; no additional baseline covariates after overlap removal"
  z
}

cat(
  "Figure 5 held-out association model: ",
  ifelse(figure5_adjust_association, "ADJUSTED sensitivity mode", "UNADJUSTED primary mode"),
  "\n", sep = ""
)

validate_adjustment_spec <- function(dat, adj, context = "Figure 5") {
  required <- c(
    adj$terms,
    if (isTRUE(adj$use_ethnicity_strata)) adj$strata_col else character()
  )
  required <- unique(required[!is.na(required) & nzchar(required)])
  missing <- setdiff(required, names(dat))
  if (length(missing)) {
    stop(
      context, " 缺少应调整的协变量列：", paste(missing, collapse = "; "),
      ". 可用 cov_ 列：",
      paste(grep("^cov_", names(dat), value = TRUE), collapse = "; ")
    )
  }
  invisible(adj)
}

cox_rhs <- function(main_terms, adj) {
  c(main_terms, adj$terms, adj$strata_formula_term)
}

cox_required_columns <- function(main_columns, adj) {
  unique(c(
    main_columns,
    adj$terms,
    if (isTRUE(adj$use_ethnicity_strata)) adj$strata_col else character()
  ))
}

capture_coxph <- function(formula, data, context,
                             singular.ok = TRUE, iter.max = 100L) {
  warning_messages <- character()
  fit <- tryCatch(
    withCallingHandlers(
      coxph(
        formula, data = data, ties = "efron", singular.ok = singular.ok,
        control = coxph.control(iter.max = iter.max)
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      fit = fit,
      error = conditionMessage(fit),
      warnings = tibble(
        context = context, warning_message = NA_character_,
        coefficient_index = NA_integer_, coefficient_term = NA_character_,
        infinite_coefficient_warning = FALSE
      )[0, ],
      flagged_terms = character(),
      has_unmapped_warning = FALSE
    ))
  }

  coef_names <- names(stats::coef(fit))
  warning_detail <- if (!length(warning_messages)) {
    tibble(
      context = character(), warning_message = character(),
      coefficient_index = integer(), coefficient_term = character(),
      infinite_coefficient_warning = logical()
    )
  } else {
    map_dfr(seq_along(warning_messages), function(ii) {
      msg <- warning_messages[[ii]]
      hit <- str_match(msg, "variable\\s+([0-9,]+)")
      idx <- integer()
      if (!is.na(hit[1, 2])) {
        idx <- suppressWarnings(as.integer(str_split(hit[1, 2], ",", simplify = FALSE)[[1]]))
        idx <- idx[is.finite(idx)]
      }
      infinite_flag <- str_detect(msg, fixed("coefficient may be infinite"))
      if (!length(idx)) {
        return(tibble(
          context = context,
          warning_message = msg,
          coefficient_index = NA_integer_,
          coefficient_term = NA_character_,
          infinite_coefficient_warning = infinite_flag
        ))
      }
      tibble(
        context = context,
        warning_message = msg,
        coefficient_index = idx,
        coefficient_term = ifelse(
          idx >= 1L & idx <= length(coef_names), coef_names[idx], NA_character_
        ),
        infinite_coefficient_warning = infinite_flag
      )
    })
  }

  list(
    fit = fit,
    error = NULL,
    warnings = warning_detail,
    flagged_terms = unique(na.omit(
      warning_detail$coefficient_term[warning_detail$infinite_coefficient_warning]
    )),
    has_unmapped_warning = any(
      warning_detail$infinite_coefficient_warning & is.na(warning_detail$coefficient_term)
    )
  )
}

joint_group_event_qa <- function(dat, expected_levels, dataset, outcome_label,
                                    analysis, omic_label = NA_character_) {
  dat %>%
    mutate(joint_group = factor(as.character(joint_group), levels = expected_levels)) %>%
    group_by(joint_group, .drop = FALSE) %>%
    summarise(
      N = n(),
      events = sum(status == 1L, na.rm = TRUE),
      censored = sum(status == 0L, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dataset = dataset,
      outcome_label = outcome_label,
      analysis = analysis,
      omic_label = omic_label,
      empty_group = N == 0L,
      zero_events = events == 0L,
      events_lt5 = events < 5L,
      N_lt10 = N < 10L,
      sparse_or_separation_risk = empty_group | zero_events | events_lt5 | N_lt10,
      .before = 1
    )
}

# Median split without arbitrarily separating participants with exactly the same score.
# If the empirical median lies on a large tie, choose the boundary between adjacent
# unique score values whose development-set cumulative proportion is closest to 50%.
development_median_cutpoint <- function(x, context = "risk score") {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) stop(context, ": no finite development-set values.")
  tab <- table(x)
  vals <- as.numeric(names(tab))
  ord <- order(vals)
  vals <- vals[ord]
  counts <- as.numeric(tab)[ord]
  if (length(vals) < 2L) {
    stop(context, ": fewer than 2 unique development-set score values.")
  }
  cumprop <- cumsum(counts) / sum(counts)
  candidate <- seq_len(length(vals) - 1L)
  idx <- candidate[which.min(abs(cumprop[candidate] - 0.5))]
  cutpoint <- (vals[idx] + vals[idx + 1L]) / 2
  if (!is.finite(cutpoint)) stop(context, ": non-finite median split cut point.")
  tibble(
    cutpoint = cutpoint,
    development_prop_lower = mean(x <= cutpoint),
    development_prop_higher = mean(x > cutpoint),
    target_quantile = 0.5,
    split_rule = "Boundary nearest 50% without splitting identical score values"
  )
}

level_binary_risk <- function(x, cutpoint) {
  cut(
    x,
    breaks = c(-Inf, cutpoint, Inf),
    labels = c("Lower risk", "Higher risk"),
    include.lowest = TRUE, right = TRUE
  )
}

# Build a clean design matrix with generic X001... terms so model variables with
# symbols such as '=' never break an R formula. Input feature values are NOT re-scaled.
prepare_domain_design <- function(train_raw, test_raw, meta) {
  mapping <- meta %>%
    distinct(model_variable, .keep_all = TRUE) %>%
    mutate(term = paste0("X", sprintf("%03d", row_number())))

  train_design <- train_raw %>% select(eid, time, status)
  test_design <- test_raw %>% select(eid, time, status)
  map_rows <- vector("list", nrow(mapping))

  for (j in seq_len(nrow(mapping))) {
    var <- mapping$model_variable[[j]]
    term <- mapping$term[[j]]
    xtr <- suppressWarnings(as.numeric(train_raw[[var]]))
    xte <- suppressWarnings(as.numeric(test_raw[[var]]))
    train_design[[term]] <- xtr
    test_design[[term]] <- xte
    uniq <- sort(unique(xtr[is.finite(xtr)]))
    usable <- length(uniq) >= 2L
    is_binary <- usable && all(uniq %in% c(0, 1))
    map_rows[[j]] <- mapping[j, ] %>%
      mutate(
        feature_type_score = if_else(
          is_binary, "binary_dummy", "continuous_python_preprocessed"
        ),
        input_scaling = if_else(
          is_binary,
          "0/1 value retained from Stage 2 matrix",
          "Python Stage 2 value used unchanged; no second feature-level standardization in R"
        ),
        dev_empirical_mean = mean(xtr, na.rm = TRUE),
        dev_empirical_sd = sd(xtr, na.rm = TRUE),
        usable = usable
      )
  }

  list(
    train = train_design,
    test = test_design,
    mapping = bind_rows(map_rows)
  )
}

build_domain_risk_score <- function(dataset_i, outcome_i, modality_i) {
  # Complete Stage 2A-selected feature set: do NOT exclude demographic/baseline features.
  meta <- stage2a_selected %>%
    filter(
      dataset == dataset_i,
      modality == modality_i
    ) %>%
    arrange(selected_rank) %>%
    distinct(model_variable, .keep_all = TRUE) %>%
    mark_baseline_adjustment_features()

  if (!nrow(meta)) {
    return(list(
      passed = FALSE,
      qa = tibble(
        dataset = dataset_i, outcome_label = outcome_i, modality = modality_i,
        passed = FALSE, reason = "no_stage2A_selected_features"
      ),
      cox_warning = tibble()
    ))
  }

  # 使用 Python 在60%开发集固定 Stage 2A 特征后，独立拟合的单域 CoxEM。
  # 不使用 Stage 1 Signed_importance，亦不按 held-out HR 改变权重方向。
  python_domain <- unname(c(enviro="E", blood="B", metabolites="M", gene="G")[[modality_i]])
  coef_file <- file.path(
    stage3b_dir, dataset_i, paste0("coxem_coefficients_", python_domain, ".csv")
  )
  if (!file.exists(coef_file)) {
    return(list(passed=FALSE, qa=tibble(
      dataset=dataset_i, outcome_label=outcome_i, modality=modality_i,
      passed=FALSE, reason=paste0("missing_stage3b_coxem_coefficients: ", coef_file)
    ), cox_warning=tibble()))
  }
  coef_df <- fread(coef_file, showProgress=FALSE) %>% as_tibble()
  if (!all(c("Feature","Weighted_beta") %in% names(coef_df))) {
    stop("Figure5 系数文件缺少 Feature/Weighted_beta: ", coef_file)
  }
  coef_df <- coef_df %>% mutate(Feature=as.character(Feature),
                                Weighted_beta=as.numeric(Weighted_beta))
  if (nrow(coef_df)==0L || anyDuplicated(coef_df$Feature) ||
      anyNA(coef_df$Feature) || any(!is.finite(coef_df$Weighted_beta))) {
    stop("Figure5 Stage3B 系数重复/缺失/非有限：", coef_file)
  }
  extra_coef_features <- setdiff(coef_df$Feature, meta$model_variable)
  if (length(extra_coef_features)) {
    stop("Stage3B 单域系数包含非 Stage2A 入选变量：", dataset_i, "/", python_domain,
         " => ", paste(extra_coef_features,collapse="; "))
  }
  dropped_after_stage2a <- setdiff(meta$model_variable,coef_df$Feature)
  # Stage3B 原生 build_combo_data() 会按缺失、非数值、非有限、开发集常量
  # 技术性排除列。只对实际训练进入模型的特征赋予系数，不将缺失者赋0冒充。
  n_selected_before_technical_qa <- nrow(meta)
  meta <- meta %>% inner_join(
    coef_df %>% select(Feature,Weighted_beta,
                       any_of(c("Importance","Base_learner_beta_SD",
                                "Base_learner_sign_agreement","N_development",
                                "Events_development","Environmental_source"))),
    by=c("model_variable"="Feature")
  ) %>% arrange(selected_rank)
  if (nrow(meta)!=nrow(coef_df)) stop("Figure5 系数匹配行数不一致：",coef_file)
  # IMPORTANT: the exporter used for enviro-2 explicitly wrote
  # Environmental_source=final_environment_2.csv. Re-locating those files into
  # Result/Stage 3B does NOT turn their weights into main-environment weights.
  # Permit preliminary R plot generation while marking source mismatch in QA;
  # set options(ukb.v8.strict_environment_provenance=TRUE) to fail closed.
  expected_environment_source <- "final_environment.csv"
  reported_environment_source <- if ("Environmental_source" %in% names(meta)) {
    paste(sort(unique(na.omit(as.character(meta$Environmental_source)))), collapse="; ")
  } else {
    NA_character_
  }
  environment_provenance_passed <- !is.na(reported_environment_source) &&
    nzchar(reported_environment_source) &&
    identical(reported_environment_source, expected_environment_source)
  if (!environment_provenance_passed &&
      isTRUE(getOption("ukb.v8.strict_environment_provenance", FALSE))) {
    stop(
      "Figure 5 系数来源与当前 final_environment.csv 不一致。
",
      "系数标记：", reported_environment_source, "；期望：", expected_environment_source,
      "
文件：", coef_file,
      "
请用 Result/Stage 2 的原始环境版本重新导出 CoxEM Weighted_beta。"
    )
  }

  spec <- expected_stage2 %>%
    filter(dataset == dataset_i, modality == modality_i) %>%
    slice(1)
  if (nrow(spec) != 1L || !all(file.exists(c(spec$train_file, spec$test_file)))) {
    return(list(
      passed = FALSE,
      qa = tibble(
        dataset = dataset_i, outcome_label = outcome_i, modality = modality_i,
        passed = FALSE, reason = "stage2_train_or_test_missing"
      ),
      cox_warning = tibble()
    ))
  }

  header_tr <- names(fread(spec$train_file, nrows = 0, showProgress = FALSE))
  header_te <- names(fread(spec$test_file, nrows = 0, showProgress = FALSE))
  requested_features <- meta$model_variable
  missing_features <- requested_features[
    !(requested_features %in% header_tr & requested_features %in% header_te)
  ]

  # Do not silently alter the Stage 2A model by dropping selected predictors.
  if (length(missing_features)) {
    return(list(
      passed = FALSE,
      qa = tibble(
        dataset = dataset_i, outcome_label = outcome_i, modality = modality_i,
        passed = FALSE,
        reason = paste0(
          "stage2A_selected_feature_missing_from_matrix: ",
          paste(missing_features, collapse = "; ")
        ),
        n_stage2A_selected = nrow(meta),
        n_missing_matrix = length(missing_features)
      ),
      cox_warning = tibble()
    ))
  }

  train_raw <- fread(
    spec$train_file,
    select = c("eid", "time", "status", requested_features),
    showProgress = FALSE
  ) %>% as_tibble() %>% mutate(eid = as.character(eid))
  test_raw <- fread(
    spec$test_file,
    select = c("eid", "time", "status", requested_features),
    showProgress = FALSE
  ) %>% as_tibble() %>% mutate(eid = as.character(eid))

  # Feature-level diagnostics are audit-only. No feature is standardized again in R.
  diag_rows <- vector("list", nrow(meta))
  for (j in seq_len(nrow(meta))) {
    var <- meta$model_variable[[j]]
    xtr <- suppressWarnings(as.numeric(train_raw[[var]]))
    finite_tr <- xtr[is.finite(xtr)]
    if (!length(finite_tr)) {
      return(list(
        passed = FALSE,
        qa = tibble(
          dataset = dataset_i, outcome_label = outcome_i, modality = modality_i,
          passed = FALSE, reason = paste0("all_nonfinite_development_feature: ", var)
        ),
        cox_warning = tibble()
      ))
    }
    uniq <- unique(finite_tr)
    is_binary <- all(uniq %in% c(0, 1))
    diag_rows[[j]] <- tibble(
      model_variable = var,
      feature_type_score = if_else(
        is_binary, "binary_dummy", "continuous_python_preprocessed"
      ),
      input_scaling = if_else(
        is_binary,
        "Stage 2 matrix value used unchanged",
        "Python Stage 2 value used unchanged; no second feature-level standardization in R"
      ),
      dev_empirical_mean = mean(finite_tr),
      dev_empirical_sd = sd(finite_tr)
    )
  }
  feature_diag <- bind_rows(diag_rows)

  if ("N_development" %in% names(meta) &&
      any(meta$N_development != nrow(train_raw), na.rm=TRUE)) {
    stop("Figure5 系数的训练 N 与 Stage 2 train_data.csv 不一致：",coef_file)
  }
  meta_weight <- meta %>%
    left_join(feature_diag, by = "model_variable") %>%
    mutate(
      signed_importance_source = as.numeric(signed_importance), # 仅用于对照审计
      weighted_beta_source = as.numeric(Weighted_beta),
      score_weight = weighted_beta_source,
      abs_score_weight = abs(score_weight),
      score_direction = case_when(
        score_weight > 0 ~ "Risk-increasing",
        score_weight < 0 ~ "Protective",
        TRUE ~ "Zero"
      ),
      # The flag below is descriptive only; selected demographic features stay in score.
      is_baseline_adjustment_feature = .baseline_adjustment_feature,
      weight_source = paste0(
        "60% development fixed-feature single-domain Stage 3B CoxEM Weighted_beta"
      ),
      score_formula = "sum(Stage3B CoxEM Weighted_beta_j * Python Stage2 feature_j)",
      heldout_used_for_weight_or_orientation = FALSE
    )

  # Memory-safe score calculation: accumulate one selected feature at a time.
  calculate_signed_score <- function(dat, weight_meta) {
    score <- rep(0, nrow(dat))
    complete <- rep(TRUE, nrow(dat))
    for (j in seq_len(nrow(weight_meta))) {
      x <- suppressWarnings(as.numeric(dat[[weight_meta$model_variable[[j]]]]))
      ok <- is.finite(x)
      complete <- complete & ok
      x[!ok] <- 0
      score <- score + weight_meta$score_weight[[j]] * x
    }
    score[!complete] <- NA_real_
    score
  }

  raw_tr <- calculate_signed_score(train_raw, meta_weight)
  raw_te <- calculate_signed_score(test_raw, meta_weight)
  score_mean <- mean(raw_tr, na.rm = TRUE)
  score_sd <- sd(raw_tr, na.rm = TRUE)
  if (!is.finite(score_mean) || !is.finite(score_sd) || score_sd <= 0) {
    return(list(
      passed = FALSE,
      qa = tibble(
        dataset = dataset_i, outcome_label = outcome_i, modality = modality_i,
        passed = FALSE, reason = "nonfinite_or_zero_development_score_sd"
      ),
      cox_warning = tibble()
    ))
  }

  z_tr <- (raw_tr - score_mean) / score_sd
  z_te <- (raw_te - score_mean) / score_sd
  med_ref <- development_median_cutpoint(
    z_tr,
    paste0("Section 6B ", outcome_i, " × ", modality_i, " domain risk score")
  )

  code <- unname(c(enviro = "E", blood = "P", metabolites = "M", gene = "G")[[modality_i]])
  modality_label_i <- modality_lookup$modality_label[
    match(modality_i, modality_lookup$modality)
  ]

  # Identify whether conventional covariates are genuinely Stage 2A-selected score inputs.
  baseline_selected <- meta_weight %>% filter(is_baseline_adjustment_feature)

  weights <- meta_weight %>%
    mutate(
      dataset = dataset_i,
      outcome_label = outcome_i,
      modality = modality_i,
      modality_label = modality_label_i,
      selected_abs_weight_sum = sum(abs_score_weight, na.rm = TRUE),
      selected_signed_weight_sum = sum(score_weight, na.rm = TRUE),
      direct_weight_match = abs(score_weight - weighted_beta_source) < 1e-15,
      association_adjustment_default = "Unadjusted; optional switch available"
    ) %>%
    select(
      dataset, outcome_label, modality, modality_label,
      selected_rank, model_variable, display_name, parent_variable,
      any_of(c("analysis_variable", "canonical_variable", "show_in_manuscript")),
      feature_type_score, input_scaling, dev_empirical_mean, dev_empirical_sd,
      stage1_importance_magnitude, signed_importance_source,
      weighted_beta_source, any_of(c("Base_learner_beta_SD",
                                   "Base_learner_sign_agreement")),
      score_weight, abs_score_weight, score_direction,
      selected_abs_weight_sum, selected_signed_weight_sum,
      direct_weight_match, is_baseline_adjustment_feature,
      weight_source, score_formula, heldout_used_for_weight_or_orientation,
      association_adjustment_default
    )

  reference <- tibble(
    dataset = dataset_i, outcome_label = outcome_i,
    modality = modality_i, modality_label = modality_label_i,
    score_mean_development = score_mean,
    score_sd_development = score_sd,
    median_split_cutpoint = med_ref$cutpoint,
    development_prop_lower = med_ref$development_prop_lower,
    development_prop_higher = med_ref$development_prop_higher,
    split_rule = med_ref$split_rule,
    n_features_stage2A_selected = n_selected_before_technical_qa,
    n_baseline_or_demographic_features_selected = nrow(baseline_selected),
    baseline_or_demographic_features_selected = paste(
      baseline_selected$model_variable, collapse = "; "
    ),
    selected_abs_signed_importance_sum = sum(abs(meta_weight$score_weight)), # legacy CSV column name
    score_weight_source = "Single-domain Stage 3B CoxEM Weighted_beta, development only",
    n_stage2a_selected_before_technical_exclusions = n_selected_before_technical_qa,
    n_technical_exclusions_stage3b = length(dropped_after_stage2a),
    technical_exclusions = paste(dropped_after_stage2a,collapse="; "),
    coefficient_file = coef_file,
    coefficient_reported_environment_source = reported_environment_source,
    expected_environment_source = expected_environment_source,
    environment_provenance_passed = environment_provenance_passed,
    score_weight_refit_in_R = FALSE,
    heldout_used_for_score_construction = FALSE,
    orientation_source = "Development-fitted Stage3B CoxEM signed Weighted_beta; no held-out reorientation"
  )

  dev_score <- tibble(
    eid = train_raw$eid, time = train_raw$time, status = train_raw$status,
    score_raw = raw_tr, score_z = z_tr
  )
  held_score <- tibble(
    eid = test_raw$eid, time = test_raw$time, status = test_raw$status,
    score_raw = raw_te, score_z = z_te
  )
  names(dev_score)[names(dev_score) %in% c("score_raw", "score_z")] <-
    paste0(code, c("_risk_raw", "_risk_z"))
  names(held_score)[names(held_score) %in% c("score_raw", "score_z")] <-
    paste0(code, c("_risk_raw", "_risk_z"))

  qa <- tibble(
    dataset = dataset_i, outcome_label = outcome_i,
    modality = modality_i, modality_label = modality_label_i,
    passed = TRUE, reason = "ok",
    n_stage2A_selected = n_selected_before_technical_qa,
    n_features_used = nrow(meta_weight),
    n_baseline_or_demographic_features_selected = nrow(baseline_selected),
    abs_signed_importance_sum_selected = sum(abs(meta_weight$score_weight)), # legacy column name
    max_abs_direct_weight_difference = max(
      abs(meta_weight$score_weight - meta_weight$weighted_beta_source), na.rm = TRUE
    ),
    coefficient_file = coef_file,
    coefficient_reported_environment_source = reported_environment_source,
    expected_environment_source = expected_environment_source,
    environment_provenance_passed = environment_provenance_passed,
    n_stage2a_selected_before_technical_exclusions = n_selected_before_technical_qa,
    n_technical_exclusions_stage3b = length(dropped_after_stage2a),
    n_positive_weights = sum(meta_weight$score_weight > 0),
    n_negative_weights = sum(meta_weight$score_weight < 0),
    n_zero_weights = sum(meta_weight$score_weight == 0),
    n_development_score_nonmissing = sum(is.finite(z_tr)),
    n_heldout_score_nonmissing = sum(is.finite(z_te)),
    score_weight_refit_in_R = FALSE,
    heldout_used_for_weight_or_orientation = FALSE,
    all_stage2A_selected_features_used = nrow(meta_weight) == n_selected_before_technical_qa
  )

  # Kept as an empty object only for backward compatibility with previous QA filenames.
  no_weighting_cox_warning <- tibble(
    context = character(), warning_message = character(),
    coefficient_index = integer(), coefficient_term = character(),
    infinite_coefficient_warning = logical(), model_variable = character(),
    display_name = character()
  )

  list(
    passed = TRUE,
    dev = dev_score,
    held = held_score,
    weights = weights,
    reference = reference,
    qa = qa,
    adjustment = NULL,
    cox_warning = no_weighting_cox_warning
  )
}

# Stage 3B 单域系数清单：9 × 4 = 36。按输出实际存在性检查，防止误读 Result。
figure5_coefficients_inventory <- tidyr::crossing(
  outcome_lookup %>% select(dataset,outcome_label),
  tibble(modality=c("enviro","blood","metabolites","gene"),
         python_code=c("E","B","M","G"))
) %>% mutate(
  coefficient_file=file.path(stage3b_dir,dataset,
                             paste0("coxem_coefficients_",python_code,".csv")),
  exists=file.exists(coefficient_file)
)
write_log(figure5_coefficients_inventory,"Figure5_Stage3B_coefficient_inventory_QA.csv")
if (nrow(figure5_coefficients_inventory)!=36L || any(!figure5_coefficients_inventory$exists)) {
  stop("Figure 5 缺少 Result/Stage 3B 的36份 CoxEM 单域系数。请核查 logs/Figure5_Stage3B_coefficient_inventory_QA.csv")
}

# All 36 files must be checked for their true training-data provenance.
figure5_coefficients_source_audit <- purrr::map_dfr(
  seq_len(nrow(figure5_coefficients_inventory)), function(i) {
    xx <- figure5_coefficients_inventory[i, ]
    header <- names(data.table::fread(xx$coefficient_file, nrows=0L, showProgress=FALSE))
    source <- if ("Environmental_source" %in% header) {
      unique(as.character(data.table::fread(
        xx$coefficient_file, select="Environmental_source", showProgress=FALSE
      )$Environmental_source))
    } else NA_character_
    tibble::tibble(
      dataset=xx$dataset, modality=xx$modality, file=xx$coefficient_file,
      reported_environment_source=paste(sort(unique(stats::na.omit(source))),collapse="; "),
      expected_environment_source="final_environment.csv"
    )
  }
) %>% dplyr::mutate(
  environment_provenance_passed =
    reported_environment_source == expected_environment_source
)
write_log(figure5_coefficients_source_audit, "Figure5_Stage3B_environment_provenance_QA.csv")
if (any(!figure5_coefficients_source_audit$environment_provenance_passed)) {
  bad <- figure5_coefficients_source_audit %>% dplyr::filter(!environment_provenance_passed)
  warning(
    "Figure 5 数据来源核查警告：", nrow(bad), "/36 份系数未标记为 final_environment.csv；",
    "详见 logs/Figure5_Stage3B_environment_provenance_QA.csv。",
    "当前脚本允许临时计算和检查图表，但不能把环境来源不一致的结果当作最终论文结果。",
    "如需禁止继续运行，请在 Section 6B 前设置 ",
    "options(ukb.v8.strict_environment_provenance=TRUE)。",
    call.=FALSE
  )
  if (isTRUE(getOption("ukb.v8.strict_environment_provenance", FALSE))) {
    stop("Figure 5 存在训练来源不一致的 Stage3B Weighted_beta，请重跑原环境版本。")
  }
}

figure5_score_results <- list()
figure5_weights_list <- list()
figure5_reference_list <- list()
figure5_score_qa_list <- list()
figure5_weight_cox_warning_list <- list()

# All nine outcomes are scored for Figure 5.
figure5_targets <- crossing(
  outcome_lookup %>% select(dataset, outcome_label, outcome_order),
  modality_lookup %>% select(modality, modality_label, modality_order)
)

for (k in seq_len(nrow(figure5_targets))) {
  z <- figure5_targets[k, ]
  key <- paste(z$dataset, z$modality, sep = "||")
  res <- build_domain_risk_score(z$dataset, z$outcome_label, z$modality)
  figure5_score_results[[key]] <- res
  figure5_score_qa_list[[key]] <- res$qa
  if (!is.null(res$cox_warning) && nrow(res$cox_warning)) {
    figure5_weight_cox_warning_list[[key]] <- res$cox_warning %>%
      mutate(
        dataset = z$dataset, outcome_label = z$outcome_label,
        modality = z$modality, .before = 1
      )
  }
  if (isTRUE(res$passed)) {
    figure5_weights_list[[key]] <- res$weights
    figure5_reference_list[[key]] <- res$reference
  }
  cat(
    "Section 6B：", z$outcome_label, " × ", z$modality_label,
    " risk score = ", ifelse(isTRUE(res$passed), "PASS", "FAIL"), "\n", sep = ""
  )
  rm(res)
  invisible(gc(full = TRUE))
}

figure5_risk_score_weights <- bind_rows(figure5_weights_list)
figure5_risk_score_reference <- bind_rows(figure5_reference_list)
figure5_risk_score_qa <- bind_rows(figure5_score_qa_list)
figure5_weight_cox_warning <- bind_rows(figure5_weight_cox_warning_list)
if (ncol(figure5_weight_cox_warning) == 0L) {
  figure5_weight_cox_warning <- tibble(
    dataset = character(), outcome_label = character(), modality = character(),
    context = character(), warning_message = character(),
    coefficient_index = integer(), coefficient_term = character(),
    infinite_coefficient_warning = logical(), model_variable = character(),
    display_name = character()
  )
}

figure5_python_scaling_diagnostic <- figure5_risk_score_weights %>%
  filter(feature_type_score == "continuous_python_preprocessed") %>%
  group_by(dataset, outcome_label, modality, modality_label) %>%
  summarise(
    n_continuous_features = n(),
    median_abs_development_mean = median(abs(dev_empirical_mean), na.rm = TRUE),
    median_development_sd = median(dev_empirical_sd, na.rm = TRUE),
    max_abs_development_mean = max(abs(dev_empirical_mean), na.rm = TRUE),
    max_abs_sd_minus_1 = max(abs(dev_empirical_sd - 1), na.rm = TRUE),
    diagnostic_note = paste0(
      "Continuous feature columns are used exactly as stored in the Python Stage 2 ",
      "matrices; empirical means/SDs are audit-only and do not trigger a second ",
      "feature-level standardization in R."
    ),
    .groups = "drop"
  )

write_csv(
  figure5_risk_score_weights,
  file.path(intermediate_dir, "Figure5_domain_risk_score_weights.csv")
)
write_csv(
  figure5_risk_score_reference,
  file.path(intermediate_dir, "Figure5_domain_risk_score_reference.csv")
)
write_log(figure5_risk_score_qa, "Figure5_domain_risk_score_QA.csv")
write_log(
  figure5_python_scaling_diagnostic,
  "Figure5_python_standardization_diagnostic.csv"
)
write_log(
  figure5_weight_cox_warning,
  "Figure5_domain_weighting_Cox_warning_log.csv" # expected empty: no weighting Cox refit in final v8
)

if (nrow(figure5_risk_score_qa) != nrow(outcome_lookup) * nrow(modality_lookup) ||
    any(!figure5_risk_score_qa$passed)) {
  stop("Figure 5 CoxEM Weighted_beta score QA failed: logs/Figure5_domain_risk_score_QA.csv")
}

combine_domain_scores <- function(dataset_i, split = c("dev", "held")) {
  split <- match.arg(split)
  pieces <- lapply(c("enviro", "blood", "metabolites", "gene"), function(m) {
    key <- paste(dataset_i, m, sep = "||")
    res <- figure5_score_results[[key]]
    if (is.null(res) || !isTRUE(res$passed)) return(NULL)
    res[[split]]
  })
  if (any(vapply(pieces, is.null, logical(1)))) {
    stop("Figure 5 score missing for dataset: ", dataset_i)
  }
  out <- pieces[[1]]
  for (j in 2:4) {
    add_cols <- setdiff(names(pieces[[j]]), c("time", "status"))
    out <- inner_join(out, pieces[[j]][, add_cols], by = "eid")
  }
  out
}

fit_score_association <- function(dat, score_term, adj, context) {
  validate_adjustment_spec(dat, adj, context)
  required <- cox_required_columns(c("time", "status", score_term), adj)
  fit_dat <- dat %>%
    filter(if_all(all_of(required), ~ !is.na(.))) %>%
    droplevels()
  rhs <- cox_rhs(score_term, adj)
  cap <- capture_coxph(
    as.formula(paste("Surv(time, status) ~", paste(rhs, collapse = " + "))),
    data = fit_dat, context = context
  )
  if (!is.null(cap$error)) stop(context, ": Cox model failed: ", cap$error)
  z <- broom::tidy(cap$fit, conf.int = TRUE) %>% filter(term == score_term)
  coef_warning <- score_term %in% cap$flagged_terms
  estimable <- nrow(z) == 1L && is.finite(z$estimate) && is.finite(z$std.error) &&
    is.finite(z$conf.low) && is.finite(z$conf.high) &&
    is.finite(exp(z$estimate)) && !coef_warning
  result <- tibble(
    beta_raw = if (nrow(z)) z$estimate[[1]] else NA_real_,
    SE_raw = if (nrow(z)) z$std.error[[1]] else NA_real_,
    HR_raw = if (nrow(z)) exp(z$estimate[[1]]) else NA_real_,
    CI_low_raw = if (nrow(z)) exp(z$conf.low[[1]]) else NA_real_,
    CI_high_raw = if (nrow(z)) exp(z$conf.high[[1]]) else NA_real_,
    P_raw = if (nrow(z)) z$p.value[[1]] else NA_real_,
    beta = if (estimable) z$estimate[[1]] else NA_real_,
    SE = if (estimable) z$std.error[[1]] else NA_real_,
    HR = if (estimable) exp(z$estimate[[1]]) else NA_real_,
    CI_low = if (estimable) exp(z$conf.low[[1]]) else NA_real_,
    CI_high = if (estimable) exp(z$conf.high[[1]]) else NA_real_,
    P = if (estimable) z$p.value[[1]] else NA_real_,
    N = nrow(fit_dat), events = sum(fit_dat$status),
    model_warning = nrow(cap$warnings) > 0L,
    coefficient_warning = coef_warning,
    estimable = estimable,
    adjustment = adj$adjustment_label,
    association_adjusted = isTRUE(adj$association_adjusted),
    ethnicity_adjustment = if_else(
      adj$use_ethnicity_strata,
      "Cox baseline-hazard stratification",
      "Not used"
    )
  )
  list(result = result, warning_log = cap$warnings, fit_data = fit_dat)
}

# 评分方向诊断：九个结局 × 四域 × 开发/held-out，报告 HR/95% CI。
# QA 不用于筛选权重或在 held-out 事后翻转系数。
figure5_domain_orientation_rows <- list()
for (ii in seq_len(nrow(outcome_lookup))) {
  dat_id <- outcome_lookup$dataset[[ii]]
  dat_lab <- outcome_lookup$outcome_label[[ii]]
  for (ss in c("dev","held")) {
    dat_score <- combine_domain_scores(dat_id,ss)
    for (cc in c("E","P","M","G")) {
      term <- paste0(cc,"_risk_z")
      dd <- dat_score %>% select(time,status,all_of(term)) %>%
        filter(if_all(everything(),is.finite))
      ff <- survival::coxph(
        as.formula(paste0("Surv(time,status)~",term)),data=dd,ties="efron"
      )
      tt <- broom::tidy(ff,conf.int=TRUE) %>% filter(.data$term==!!term)
      figure5_domain_orientation_rows[[paste(dat_id,ss,cc,sep="|")]] <- tibble(
        dataset=dat_id,outcome_label=dat_lab,split=ss,domain=cc,
        N=nrow(dd),events=sum(dd$status==1L),
        beta=tt$estimate[[1]],HR=exp(tt$estimate[[1]]),
        CI_low=exp(tt$conf.low[[1]]),CI_high=exp(tt$conf.high[[1]]),
        P=tt$p.value[[1]],direction_higher_score_higher_risk=tt$estimate[[1]]>0
      )
    }
  }
}
figure5_domain_orientation_QA <- bind_rows(figure5_domain_orientation_rows)
write_log(figure5_domain_orientation_QA,
          "Figure5_WeightedBeta_domain_direction_dev_heldout_95CI_QA.csv")
if (nrow(figure5_domain_orientation_QA)!=72L) {
  stop("Figure5 方向核查行数不是72：",nrow(figure5_domain_orientation_QA))
}

figure5a_list <- list()
figure5b_list <- list()
figure5b_full_list <- list()
figure5c_list <- list()
figure5_combined_omics_reference_list <- list()
interaction_list <- list()
figure5_score_wide_qa <- list()
figure5_joint_group_qa_list <- list()
figure5_association_cox_warning_list <- list()

outcomes_f5 <- outcome_lookup %>% arrange(outcome_order)

for (i in seq_len(nrow(outcomes_f5))) {
  spec_out <- outcomes_f5[i, ]
  dataset_i <- spec_out$dataset
  outcome_i <- spec_out$outcome_label
  outcome_order_i <- spec_out$outcome_order

  dev_risk <- combine_domain_scores(dataset_i, "dev")
  held_risk <- combine_domain_scores(dataset_i, "held") %>%
    left_join(figure5_covariates, by = "eid")

  figure5_score_wide_qa[[i]] <- tibble(
    dataset = dataset_i, outcome_label = outcome_i, outcome_order = outcome_order_i,
    n_development_common = nrow(dev_risk),
    n_heldout_common = nrow(held_risk),
    all_four_scores_development_complete = all(
      complete.cases(dev_risk[, paste0(c("E", "P", "M", "G"), "_risk_z")])
    ),
    all_four_scores_heldout_complete = all(
      complete.cases(held_risk[, paste0(c("E", "P", "M", "G"), "_risk_z")])
    )
  )

  # ---------------------------------------------------------------------------
  # Figure 5a: each continuous domain score separately + baseline covariates.
  # Other domain scores are NOT included.
  # ---------------------------------------------------------------------------
  for (m in c("enviro", "blood", "metabolites", "gene")) {
    code <- c(enviro = "E", blood = "P", metabolites = "M", gene = "G")[[m]]
    score_term <- paste0(code, "_risk_z")
    meta_m <- stage2a_selected %>%
      filter(dataset == dataset_i, modality == m) %>%
      distinct(model_variable, .keep_all = TRUE)
    adj <- figure5_association_adjustment_spec(meta_m, outcome_i)
    fit_context <- paste0("Figure5a ", outcome_i, " × ", m)
    zfit_obj <- fit_score_association(held_risk, score_term, adj, fit_context)
    if (nrow(zfit_obj$warning_log)) {
      figure5_association_cox_warning_list[[paste0("5a|", i, "|", m)]] <-
        zfit_obj$warning_log %>%
        mutate(
          dataset = dataset_i, outcome_label = outcome_i,
          analysis = "Figure5a", modality = m, .before = 1
        )
    }
    figure5a_list[[paste(i, m)]] <- zfit_obj$result %>%
      mutate(
        dataset = dataset_i,
        outcome_label = outcome_i,
        outcome_order = outcome_order_i,
        modality = m,
        component = modality_lookup$modality_label[
          match(m, modality_lookup$modality)
        ],
        other_domain_scores_adjusted = FALSE,
        score_definition = paste0(
          "Stage 3B CoxEM Weighted_beta weighted domain score; ",
          "weights fixed in development and applied unchanged to held-out; ",
          "HR per 1 development-SD higher score"
        ),
        evaluation_set = "Independent held-out evaluation set"
      )
  }

  # ---------------------------------------------------------------------------
  # Figure 5b: Environmental score × each individual omic score.
  # Development-defined median binary split; held-out 2×2 joint profiles.
  # ---------------------------------------------------------------------------
  for (m in c("blood", "metabolites", "gene")) {
    code <- c(blood = "P", metabolites = "M", gene = "G")[[m]]
    e_ref <- development_median_cutpoint(
      dev_risk$E_risk_z,
      paste0("Figure 5b ", outcome_i, " environmental score")
    )
    o_ref <- development_median_cutpoint(
      dev_risk[[paste0(code, "_risk_z")]],
      paste0("Figure 5b ", outcome_i, " × ", m, " score")
    )

    dat_joint <- held_risk %>%
      mutate(
        environment_level = level_binary_risk(E_risk_z, e_ref$cutpoint),
        omic_level = level_binary_risk(
          .data[[paste0(code, "_risk_z")]], o_ref$cutpoint
        ),
        joint_group = factor(
          paste(environment_level, omic_level, sep = " / "),
          levels = c(
            "Lower risk / Lower risk",
            "Lower risk / Higher risk",
            "Higher risk / Lower risk",
            "Higher risk / Higher risk"
          )
        )
      )

    meta_joint <- stage2a_selected %>%
      filter(dataset == dataset_i, modality %in% c("enviro", m)) %>%
      distinct(modality, model_variable, .keep_all = TRUE)
    adj <- figure5_association_adjustment_spec(meta_joint, outcome_i)
    validate_adjustment_spec(dat_joint, adj, paste0("Figure 5b ", outcome_i, " × ", m))
    required <- cox_required_columns(c("time", "status", "joint_group"), adj)
    fit_dat <- dat_joint %>%
      filter(if_all(all_of(required), ~ !is.na(.))) %>%
      droplevels()

    joint_levels <- levels(dat_joint$joint_group)
    figure5_joint_group_qa_list[[paste0("5b|", i, "|", m)]] <-
      joint_group_event_qa(
        fit_dat, joint_levels, dataset_i, outcome_i,
        analysis = "Figure5b", omic_label = m
      )

    fit_context <- paste0("Figure5b ", outcome_i, " × ", m)
    rhs <- cox_rhs("joint_group", adj)
    cap_joint <- capture_coxph(
      as.formula(paste("Surv(time, status) ~", paste(rhs, collapse = " + "))),
      data = fit_dat, context = fit_context
    )
    if (!is.null(cap_joint$error)) stop(fit_context, ": Cox model failed: ", cap_joint$error)
    if (nrow(cap_joint$warnings)) {
      figure5_association_cox_warning_list[[paste0("5b|", i, "|", m)]] <-
        cap_joint$warnings %>%
        mutate(
          dataset = dataset_i, outcome_label = outcome_i,
          analysis = "Figure5b", modality = m, .before = 1
        )
    }

    z <- broom::tidy(cap_joint$fit, conf.int = TRUE) %>%
      filter(str_detect(term, "^joint_group")) %>%
      mutate(
        group = str_remove(term, "^joint_group"),
        coefficient_warning = term %in% cap_joint$flagged_terms,
        estimable = is.finite(estimate) & is.finite(std.error) &
          is.finite(conf.low) & is.finite(conf.high) &
          is.finite(exp(estimate)) & !coefficient_warning
      )
    ref <- tibble(
      term = NA_character_, estimate = 0, std.error = 0,
      conf.low = 0, conf.high = 0, p.value = NA_real_,
      group = "Lower risk / Lower risk",
      coefficient_warning = FALSE, estimable = TRUE
    )

    full <- bind_rows(ref, z) %>%
      transmute(
        dataset = dataset_i, outcome_label = outcome_i,
        outcome_order = outcome_order_i,
        modality = m,
        omic_label = modality_lookup$modality_label[
          match(m, modality_lookup$modality)
        ],
        joint_group = group,
        environment_level = str_trim(str_extract(group, "^[^/]+")),
        omic_level = str_trim(str_extract(group, "[^/]+$")),
        beta_raw = estimate, SE_raw = std.error,
        HR_raw = exp(estimate), CI_low_raw = exp(conf.low), CI_high_raw = exp(conf.high),
        P_raw = p.value,
        HR = if_else(estimable, exp(estimate), NA_real_),
        CI_low = if_else(estimable, exp(conf.low), NA_real_),
        CI_high = if_else(estimable, exp(conf.high), NA_real_),
        beta = if_else(estimable, estimate, NA_real_),
        SE = if_else(estimable, std.error, NA_real_),
        P = if_else(estimable, p.value, NA_real_),
        model_warning = nrow(cap_joint$warnings) > 0L,
        coefficient_warning = coefficient_warning,
        estimable = estimable,
        N = nrow(fit_dat), events = sum(fit_dat$status),
        environmental_cutpoint_development = e_ref$cutpoint,
        omic_cutpoint_development = o_ref$cutpoint,
        cutpoint_source = "Development-set median binary split",
        reference = "Lower-risk environment / Lower-risk omic",
        adjustment = adj$adjustment_label,
        association_adjusted = isTRUE(adj$association_adjusted),
        ethnicity_adjustment = if_else(
          adj$use_ethnicity_strata,
          "Cox baseline-hazard stratification",
          "Not used"
        ),
        other_domain_scores_adjusted = FALSE,
        evaluation_set = "Independent held-out evaluation set"
      )

    figure5b_full_list[[paste(i, m)]] <- full
    figure5b_list[[paste(i, m)]] <- full %>%
      filter(
        environment_level == "Higher risk",
        omic_level == "Higher risk"
      ) %>%
      mutate(comparison = "Higher-risk environment / Higher-risk omic")

    # Continuous interaction is separate from the categorical joint-profile contrast.
    int_dat <- held_risk %>%
      mutate(
        E_score = E_risk_z,
        O_score = .data[[paste0(code, "_risk_z")]]
      )
    required_i <- cox_required_columns(
      c("time", "status", "E_score", "O_score"), adj
    )
    int_dat <- int_dat %>%
      filter(if_all(all_of(required_i), ~ !is.na(.))) %>%
      droplevels()
    int_context <- paste0("Figure5 interaction ", outcome_i, " × ", m)
    rhs_i <- cox_rhs(c("E_score", "O_score", "E_score:O_score"), adj)
    cap_int <- capture_coxph(
      as.formula(paste("Surv(time, status) ~", paste(rhs_i, collapse = " + "))),
      data = int_dat, context = int_context
    )
    if (!is.null(cap_int$error)) stop(int_context, ": Cox model failed: ", cap_int$error)
    if (nrow(cap_int$warnings)) {
      figure5_association_cox_warning_list[[paste0("int|", i, "|", m)]] <-
        cap_int$warnings %>%
        mutate(
          dataset = dataset_i, outcome_label = outcome_i,
          analysis = "Figure5_interaction", modality = m, .before = 1
        )
    }
    zi <- broom::tidy(cap_int$fit, conf.int = TRUE) %>%
      filter(term == "E_score:O_score")
    int_warning <- "E_score:O_score" %in% cap_int$flagged_terms
    int_estimable <- nrow(zi) == 1L && is.finite(zi$estimate) &&
      is.finite(zi$std.error) && is.finite(zi$conf.low) &&
      is.finite(zi$conf.high) && is.finite(exp(zi$estimate)) && !int_warning

    interaction_list[[paste(i, m)]] <- tibble(
      dataset = dataset_i, outcome_label = outcome_i,
      outcome_order = outcome_order_i,
      modality = m,
      omic_label = modality_lookup$modality_label[
        match(m, modality_lookup$modality)
      ],
      beta_interaction_raw = if (nrow(zi)) zi$estimate[[1]] else NA_real_,
      HR_interaction_raw = if (nrow(zi)) exp(zi$estimate[[1]]) else NA_real_,
      beta_interaction = if (int_estimable) zi$estimate[[1]] else NA_real_,
      HR_interaction = if (int_estimable) exp(zi$estimate[[1]]) else NA_real_,
      CI_low = if (int_estimable) exp(zi$conf.low[[1]]) else NA_real_,
      CI_high = if (int_estimable) exp(zi$conf.high[[1]]) else NA_real_,
      P = if (int_estimable) zi$p.value[[1]] else NA_real_,
      model_warning = nrow(cap_int$warnings) > 0L,
      coefficient_warning = int_warning,
      estimable = int_estimable,
      N = nrow(int_dat), events = sum(int_dat$status),
      adjustment = adj$adjustment_label,
      association_adjusted = isTRUE(adj$association_adjusted),
      ethnicity_adjustment = if_else(
        adj$use_ethnicity_strata,
        "Cox baseline-hazard stratification",
        "Not used"
      ),
      other_domain_scores_adjusted = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Figure 5c: Environmental × equal-weighted combined omics, 2×2 median split.
  # ---------------------------------------------------------------------------
  dev_risk <- dev_risk %>%
    mutate(O_risk_raw = rowMeans(across(c(P_risk_z, M_risk_z, G_risk_z))))
  held_risk <- held_risk %>%
    mutate(O_risk_raw = rowMeans(across(c(P_risk_z, M_risk_z, G_risk_z))))

  o_mean <- mean(dev_risk$O_risk_raw, na.rm = TRUE)
  o_sd <- safe_sd(dev_risk$O_risk_raw)
  dev_risk <- dev_risk %>% mutate(O_risk_z = (O_risk_raw - o_mean) / o_sd)
  held_risk <- held_risk %>% mutate(O_risk_z = (O_risk_raw - o_mean) / o_sd)

  e_ref <- development_median_cutpoint(
    dev_risk$E_risk_z, paste0("Figure 5c ", outcome_i, " environmental score")
  )
  o_ref <- development_median_cutpoint(
    dev_risk$O_risk_z, paste0("Figure 5c ", outcome_i, " combined-omics score")
  )

  figure5_combined_omics_reference_list[[i]] <- tibble(
    dataset = dataset_i,
    outcome_label = outcome_i,
    outcome_order = outcome_order_i,
    biomarker_weight = 1 / 3,
    metabolomics_weight = 1 / 3,
    genetics_weight = 1 / 3,
    raw_formula = "(P_risk_z + M_risk_z + G_risk_z) / 3",
    cross_domain_weighting = "Equal weighting; no second outcome-driven weighting",
    combined_raw_mean_development = o_mean,
    combined_raw_sd_development = o_sd,
    environmental_median_cutpoint = e_ref$cutpoint,
    combined_omics_median_cutpoint = o_ref$cutpoint,
    environmental_prop_lower_development = e_ref$development_prop_lower,
    environmental_prop_higher_development = e_ref$development_prop_higher,
    combined_omics_prop_lower_development = o_ref$development_prop_lower,
    combined_omics_prop_higher_development = o_ref$development_prop_higher,
    n_development_common = sum(is.finite(dev_risk$O_risk_z)),
    n_heldout_common = sum(is.finite(held_risk$O_risk_z)),
    scaling_rule = paste0(
      "P/M/G domain risk scores are first standardized using development-set ",
      "parameters; their equal-weighted mean is re-standardized using the development ",
      "mean/SD; development median split boundaries are applied unchanged to held-out."
    )
  )

  joint4 <- held_risk %>%
    mutate(
      environment_level = level_binary_risk(E_risk_z, e_ref$cutpoint),
      omics_level = level_binary_risk(O_risk_z, o_ref$cutpoint),
      joint_group = factor(
        paste(environment_level, omics_level, sep = " / "),
        levels = c(
          "Lower risk / Lower risk",
          "Lower risk / Higher risk",
          "Higher risk / Lower risk",
          "Higher risk / Higher risk"
        )
      )
    )

  meta_all <- stage2a_selected %>%
    filter(dataset == dataset_i) %>%
    distinct(modality, model_variable, .keep_all = TRUE)
  adj_all <- figure5_association_adjustment_spec(meta_all, outcome_i)
  validate_adjustment_spec(joint4, adj_all, paste0("Figure 5c ", outcome_i))
  required4 <- cox_required_columns(c("time", "status", "joint_group"), adj_all)
  fit4_dat <- joint4 %>%
    filter(if_all(all_of(required4), ~ !is.na(.))) %>%
    droplevels()

  joint4_levels <- levels(joint4$joint_group)
  figure5_joint_group_qa_list[[paste0("5c|", i)]] <-
    joint_group_event_qa(
      fit4_dat, joint4_levels, dataset_i, outcome_i,
      analysis = "Figure5c", omic_label = "Equal-weighted combined omics"
    )

  fit4_context <- paste0("Figure5c ", outcome_i)
  rhs4 <- cox_rhs("joint_group", adj_all)
  cap4 <- capture_coxph(
    as.formula(paste("Surv(time, status) ~", paste(rhs4, collapse = " + "))),
    data = fit4_dat, context = fit4_context
  )
  if (!is.null(cap4$error)) stop(fit4_context, ": Cox model failed: ", cap4$error)
  if (nrow(cap4$warnings)) {
    figure5_association_cox_warning_list[[paste0("5c|", i)]] <-
      cap4$warnings %>%
      mutate(
        dataset = dataset_i, outcome_label = outcome_i,
        analysis = "Figure5c", modality = "combined_omics", .before = 1
      )
  }

  z4 <- broom::tidy(cap4$fit, conf.int = TRUE) %>%
    filter(str_detect(term, "^joint_group")) %>%
    mutate(
      group = str_remove(term, "^joint_group"),
      coefficient_warning = term %in% cap4$flagged_terms,
      estimable = is.finite(estimate) & is.finite(std.error) &
        is.finite(conf.low) & is.finite(conf.high) &
        is.finite(exp(estimate)) & !coefficient_warning
    )
  reference4 <- tibble(
    term = NA_character_, estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0, p.value = NA_real_,
    group = "Lower risk / Lower risk",
    coefficient_warning = FALSE, estimable = TRUE
  )

  figure5c_list[[i]] <- bind_rows(reference4, z4) %>%
    transmute(
      dataset = dataset_i,
      outcome_label = outcome_i,
      outcome_order = outcome_order_i,
      joint_group = group,
      environment_level = str_trim(str_extract(group, "^[^/]+")),
      omics_level = str_trim(str_extract(group, "[^/]+$")),
      beta_raw = estimate, SE_raw = std.error,
      HR_raw = exp(estimate), CI_low_raw = exp(conf.low), CI_high_raw = exp(conf.high),
      P_raw = p.value,
      HR = if_else(estimable, exp(estimate), NA_real_),
      CI_low = if_else(estimable, exp(conf.low), NA_real_),
      CI_high = if_else(estimable, exp(conf.high), NA_real_),
      beta = if_else(estimable, estimate, NA_real_),
      SE = if_else(estimable, std.error, NA_real_),
      P = if_else(estimable, p.value, NA_real_),
      model_warning = nrow(cap4$warnings) > 0L,
      coefficient_warning = coefficient_warning,
      estimable = estimable,
      N = nrow(fit4_dat), events = sum(fit4_dat$status),
      environmental_cutpoint_development = e_ref$cutpoint,
      combined_omics_cutpoint_development = o_ref$cutpoint,
      cutpoint_source = "Development-set median binary split",
      reference = "Lower-risk environment / Lower-risk combined omics",
      adjustment = adj_all$adjustment_label,
      association_adjusted = isTRUE(adj_all$association_adjusted),
      ethnicity_adjustment = if_else(
        adj_all$use_ethnicity_strata,
        "Cox baseline-hazard stratification",
        "Not used"
      ),
      other_domain_scores_adjusted = FALSE,
      score_definition = paste0(
        "Risk-oriented weighted domain scores; combined omics is the equal-weighted ",
        "mean of P/M/G domain risk z-scores (1/3 each), re-standardized using ",
        "development-set mean/SD; no second outcome-driven weighting across domains"
      ),
      evaluation_set = "Independent held-out evaluation set"
    )
}

figure5a <- bind_rows(figure5a_list) %>%
  group_by(dataset) %>%
  mutate(FDR = if_else(is.na(P), NA_real_, p.adjust(P, method = "BH"))) %>%
  ungroup()
figure5b <- bind_rows(figure5b_list) %>%
  group_by(dataset) %>%
  mutate(FDR = if_else(is.na(P), NA_real_, p.adjust(P, method = "BH"))) %>%
  ungroup()
figure5b_full <- bind_rows(figure5b_full_list)
figure5c <- bind_rows(figure5c_list) %>%
  group_by(dataset) %>%
  mutate(FDR = if_else(is.na(P), NA_real_, p.adjust(P, method = "BH"))) %>%
  ungroup()
score_interactions <- bind_rows(interaction_list) %>%
  group_by(dataset) %>%
  mutate(FDR = if_else(is.na(P), NA_real_, p.adjust(P, method = "BH"))) %>%
  ungroup()
figure5_combined_omics_reference <- bind_rows(
  figure5_combined_omics_reference_list
)
figure5_score_wide_qa <- bind_rows(figure5_score_wide_qa)
figure5_joint_group_event_qa <- bind_rows(figure5_joint_group_qa_list)
figure5_association_cox_warning <- bind_rows(figure5_association_cox_warning_list)
if (ncol(figure5_association_cox_warning) == 0L) {
  figure5_association_cox_warning <- tibble(
    dataset = character(), outcome_label = character(), analysis = character(),
    modality = character(), context = character(), warning_message = character(),
    coefficient_index = integer(), coefficient_term = character(),
    infinite_coefficient_warning = logical()
  )
}

write_log(
  figure5_score_wide_qa,
  "Figure5_common_score_population_QA.csv"
)
write_log(
  figure5_joint_group_event_qa,
  "Figure5_joint_group_event_QA.csv"
)
write_log(
  figure5_association_cox_warning,
  "Figure5_association_Cox_warning_log.csv"
)
write_csv(
  score_interactions,
  file.path(intermediate_dir, "Figure5_environment_omics_interactions.csv")
)
write_source(
  figure5b_full,
  "Figure5b_full_environment_individual_omics_profiles.csv"
)

cat(
  "完成：Section 6B Figure 5 risk-oriented scores + median binary joint profiles ",
  "for Overall/P1-P8（score 直接使用 Stage 3B development-fitted CoxEM Weighted_beta；",
  ifelse(figure5_adjust_association, "held-out Cox=adjusted", "held-out Cox=unadjusted"),
  "）\n",
  sep = ""
)

#figure 2####
update_progress_checkpoint("07 Section 6 completed", "从 07 Section 7 继续。")

###############################################################################
### Section 7. Main Figure 2 source ####
###############################################################################
# Input: Result/cluster_result 与 source/UKB_diseases_data_with_patterns.csv。
# Output: Figure2_nodes/edges/pattern_counts/burden.csv。
# 这一段做什么：以当前 cluster_result 为唯一权威来源，重建 36 diseases 与 P1–P8。
# 需要先运行 Section 0–2。

cluster_nodes_file <- file.path(
  cluster_dir, "undirected_disease_graph_spe_corr_n_cluster=8_nodes.csv"
)
cluster_edges_file <- file.path(
  cluster_dir, "undirected_disease_graph_spe_corr_n_cluster=8_edges.csv"
)
cluster_membership_file <- file.path(
  cluster_dir, "disease_cluster_spe_corr_n_cluster=8_disease.csv"
)

required_cluster_files <- c(
  cluster_nodes_file,
  cluster_edges_file,
  cluster_membership_file
)
if (any(!file.exists(required_cluster_files))) {
  stop("cluster_result 缺少文件：", paste(
    required_cluster_files[!file.exists(required_cluster_files)],
    collapse = "; "
  ))
}

cluster_nodes_raw <- fread(cluster_nodes_file) %>% as_tibble()
cluster_edges_raw <- fread(cluster_edges_file) %>% as_tibble()
cluster_membership_raw <- fread(cluster_membership_file) %>% as_tibble()

pattern_label_file <- file.path(
  raw_source_dir, "comorbidity_pattern_followup_summary_K8_labeled.csv"
)
pattern_label_lookup <- if (file.exists(pattern_label_file)) {
  fread(pattern_label_file) %>%
    as_tibble() %>%
    select(any_of(c("pattern_id", "pattern_label", "events", "N", "event_percent")))
} else {
  tibble(
    pattern_id = paste0("P", 1:8), pattern_label = paste("Pattern", 1:8),
    events = NA_integer_, N = NA_integer_, event_percent = NA_real_
  )
}

figure2_nodes <- cluster_nodes_raw %>%
  transmute(
    disease = as.character(disease),
    disease_display = str_replace_all(as.character(disease), "_", " "),
    node_id = as.integer(node_id) + 1L,
    cluster_id_zero_based = as.integer(cluster),
    pattern_id = paste0("P", as.integer(cluster) + 1L),
    prevalence_proportion = as.numeric(frequency),
    prevalence_percent = 100 * as.numeric(frequency),
    source_color = as.character(color),
    x = as.numeric(x), y = as.numeric(y),
    source_file = basename(cluster_nodes_file)
  ) %>%
  left_join(pattern_label_lookup, by = "pattern_id") %>%
  arrange(cluster_id_zero_based, disease)

figure2_edges <- cluster_edges_raw %>%
  transmute(
    source = as.character(source), target = as.character(target),
    correlation = as.numeric(effect), layout_weight = as.numeric(layout_weight),
    edge_width = as.numeric(edge_width),
    source_file = basename(cluster_edges_file)
  )
# ============================================================================
# Pattern participant burden
# ============================================================================

disease_cohort_file <- main_disease_file
main_disease_data <- get_main_disease_data()

# MAIN cohort denominator: reuse the single cached disease object.
main_pattern_cohort_n <- n_distinct(
  main_disease_data$eid
)


cat(
  "\nMAIN cohort N used for pattern burden = ",
  main_pattern_cohort_n,
  "\n",
  sep = ""
)


# 总人数以本次加载数据为准，不再要求固定样本量。

# ============================================================================
# Pattern participant burden
# ============================================================================
# 不再使用 cluster_result combination 文件中的第3/4列作为 participant counts。
#
# participant burden 直接根据正式的
# UKB_diseases_data_with_patterns.csv
# 中每个 pattern 的 baseline + incident disease count 重新计算。
#
# 定义：
#   >=2 diseases:
#       pattern 内截至 valid follow-up 结束时至少有2种疾病
#
#   all diseases:
#       pattern 内截至 valid follow-up 结束时拥有该 pattern 的全部疾病
#
# MAIN cohort denominator = the current source cohort size
# ============================================================================



# --------------------------------------------------------------------------
# 每个 pattern 的疾病数完全由 Figure 2 nodes 决定
# --------------------------------------------------------------------------

pattern_definition_for_counts <-
  figure2_nodes %>%
  
  group_by(
    pattern_id
  ) %>%
  
  summarise(
    
    n_diseases =
      n(),
    
    diseases =
      paste(
        disease,
        collapse = "; "
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    pattern_number =
      as.integer(
        str_remove(
          pattern_id,
          "^P"
        )
      )
  ) %>%
  
  arrange(
    pattern_number
  )


stopifnot(
  nrow(
    pattern_definition_for_counts
  ) ==
    8L,
  
  setequal(
    pattern_definition_for_counts$pattern_id,
    paste0(
      "P",
      1:8
    )
  )
)


# --------------------------------------------------------------------------
# 从正式 pattern-specific baseline / incident counts 重新计算
# --------------------------------------------------------------------------

figure2_pattern_counts_list <-
  vector(
    "list",
    8L
  )


for (
  i in 1:8
) {
  
  pattern_id_i <-
    paste0(
      "P",
      i
    )
  
  baseline_col <-
    paste0(
      "comorbidity_pattern_",
      i,
      "_baseline_disease_count"
    )
  
  incident_col <-
    paste0(
      "comorbidity_pattern_",
      i,
      "_incident_disease_count"
    )
  
  
  required_cols_i <-
    c(
      baseline_col,
      incident_col
    )
  
  
  if (
    any(
      !required_cols_i %in%
      names(
        main_disease_data
      )
    )
  ) {
    
    stop(
      "缺少 pattern-specific disease-count columns for ",
      pattern_id_i,
      ": ",
      paste(
        setdiff(
          required_cols_i,
          names(
            main_disease_data
          )
        ),
        collapse = ", "
      )
    )
  }
  
  
  n_diseases_i <-
    pattern_definition_for_counts$n_diseases[
      pattern_definition_for_counts$pattern_id ==
        pattern_id_i
    ]
  
  
  if (
    length(
      n_diseases_i
    ) !=
    1L
  ) {
    
    stop(
      "无法确定 ",
      pattern_id_i,
      " 的疾病数。"
    )
  }
  
  
  pattern_disease_count_end <-
    as.integer(
      main_disease_data[[baseline_col]]
    ) +
    as.integer(
      main_disease_data[[incident_col]]
    )
  
  
  if (
    any(
      is.na(
        pattern_disease_count_end
      )
    )
  ) {
    
    stop(
      pattern_id_i,
      " 的 pattern disease count 出现 NA。"
    )
  }
  
  
  if (
    any(
      pattern_disease_count_end <
      0L |
      pattern_disease_count_end >
      n_diseases_i
    )
  ) {
    
    stop(
      pattern_id_i,
      " 的 pattern disease count 超出 0-",
      n_diseases_i,
      "。"
    )
  }
  
  
  n_at_least_two <-
    sum(
      pattern_disease_count_end >=
        2L
    )
  
  
  n_all <-
    sum(
      pattern_disease_count_end ==
        n_diseases_i
    )
  
  
  figure2_pattern_counts_list[[i]] <-
    tibble(
      
      pattern_id =
        pattern_id_i,
      
      pattern_number =
        i,
      
      n_diseases =
        n_diseases_i,
      
      participants_with_at_least_two_diseases_n =
        n_at_least_two,
      
      participants_with_at_least_two_diseases_percent =
        100 *
        n_at_least_two /
        main_pattern_cohort_n,
      
      participants_with_all_diseases_n =
        n_all
    )
}


figure2_pattern_counts <-
  bind_rows(
    figure2_pattern_counts_list
  ) %>%
  
  left_join(
    pattern_label_lookup,
    by =
      "pattern_id"
  ) %>%
  
  left_join(
    
    pattern_definition_for_counts %>%
      select(
        pattern_id,
        diseases
      ),
    
    by =
      "pattern_id"
  ) %>%
  
  arrange(
    pattern_number
  )

# ============================================================================
# Pattern burden QA
# ============================================================================

stopifnot(
  
  nrow(
    figure2_pattern_counts
  ) ==
    8L,
  
  all(
    figure2_pattern_counts$
      participants_with_at_least_two_diseases_n >=
      figure2_pattern_counts$
      participants_with_all_diseases_n
  ),
  
  all(
    figure2_pattern_counts$
      participants_with_at_least_two_diseases_n <=
      main_pattern_cohort_n
  ),
  
  all(
    figure2_pattern_counts$
      participants_with_all_diseases_n <=
      main_pattern_cohort_n
  )
)


# 对只有2种疾病的 pattern：
# >=2 diseases 必须严格等于 all diseases
two_disease_patterns <-
  figure2_pattern_counts %>%
  filter(
    n_diseases ==
      2L
  )


stopifnot(
  all(
    two_disease_patterns$
      participants_with_at_least_two_diseases_n ==
      two_disease_patterns$
      participants_with_all_diseases_n
  )
)


cat(
  "\nTwo-disease pattern QA:\n"
)

print(
  two_disease_patterns %>%
    select(
      pattern_id,
      n_diseases,
      participants_with_at_least_two_diseases_n,
      participants_with_all_diseases_n
    )
)

disease_names <- figure2_nodes$disease
disease_header <- names(main_disease_data)
if (any(!disease_names %in% disease_header)) {
  stop("36-disease cohort 缺少列：", paste(
    setdiff(disease_names, disease_header), collapse = "; "
  ))
}
# ============================================================================
# Figure 2a:
# chronic disease burden by the end of valid follow-up
# ============================================================================

disease_cohort_burden <- main_disease_data %>%
  select(
    eid,
    baseline_disease_count,
    incident_disease_count
  ) %>%
  mutate(
    
    disease_count_end_followup =
      baseline_disease_count +
      incident_disease_count
  )


# QA
if (
  any(
    disease_cohort_burden$
    disease_count_end_followup < 0 |
    disease_cohort_burden$
    disease_count_end_followup > 36,
    na.rm = TRUE
  )
) {
  
  stop(
    "Figure 2a disease burden 超出 0-36。"
  )
}


if (
  any(
    disease_cohort_burden$
    baseline_disease_count > 1,
    na.rm = TRUE
  )
) {
  
  stop(
    "Figure 2a cohort 中发现 baseline_disease_count > 1。"
  )
}


figure2_burden <- disease_cohort_burden %>%
  
  count(
    disease_count_end_followup,
    name = "participants"
  ) %>%
  
  mutate(
    
    total_participants =
      sum(
        participants
      ),
    
    percent =
      100 *
      participants /
      total_participants,
    
    burden_group =
      case_when(
        
        disease_count_end_followup <= 1 ~
          "0-1 disease",
        
        disease_count_end_followup >= 2 ~
          "Multimorbidity (>=2 diseases)"
      )
  ) %>%
  
  arrange(
    disease_count_end_followup
  )
write_source(figure2_nodes, "Figure2_nodes.csv")
write_source(figure2_edges, "Figure2_edges.csv")
write_source(figure2_pattern_counts, "Figure2_pattern_counts.csv")
write_source(figure2_burden, "Figure2_burden.csv")

membership_crosscheck <- cluster_membership_raw %>%
  transmute(
    disease = as.character(.data[[names(cluster_membership_raw)[[1]]]]),
    cluster_membership = as.integer(.data[[names(cluster_membership_raw)[[2]]]])
  ) %>%
  full_join(
    figure2_nodes %>% select(disease, cluster_nodes = cluster_id_zero_based),
    by = "disease"
  )
figure2_source_qa <- tibble(
  check = c(
    "exactly 36 diseases",
    "diseases unique",
    "exactly P1-P8",
    "membership and node clusters agree",
    "two-disease patterns have identical >=2 and all-disease counts",
    "all edge endpoints exist",
    "Figure 2 burden cohort N matches MAIN cohort"
  ),
  
  observed = c(
    as.character(
      nrow(figure2_nodes)
    ),
    
    as.character(
      n_distinct(figure2_nodes$disease)
    ),
    
    paste(
      sort(
        unique(
          figure2_nodes$pattern_id
        )
      ),
      collapse = ";"
    ),
    
    as.character(
      all(
        membership_crosscheck$cluster_membership ==
          membership_crosscheck$cluster_nodes
      )
    ),
    
    as.character(
      all(
        two_disease_patterns$
          participants_with_at_least_two_diseases_n ==
          two_disease_patterns$
          participants_with_all_diseases_n
      )
    ),
    
    as.character(
      all(
        figure2_edges$source %in%
          figure2_nodes$disease &
          figure2_edges$target %in%
          figure2_nodes$disease
      )
    ),
    
    as.character(
      nrow(disease_cohort_burden) ==
        main_pattern_cohort_n
    )
  ),
  
  expected = c(
    "36",
    "36",
    "P1;P2;P3;P4;P5;P6;P7;P8",
    "TRUE",
    "TRUE",
    "TRUE",
    "TRUE"
  )
) %>%
  
  mutate(
    passed =
      observed ==
      expected
  )
write_log(figure2_source_qa, "figure2_source_QA.csv")
cat("完成：Section 7 Main Figure 2 source\n")

#figure 3####
update_progress_checkpoint("07 Section 7 completed", "从 07 Section 8 继续。")

###############################################################################
### Section 8. Main Figure 3 source ####
###############################################################################
# Input: Stage2A_selected_features、signed importance、Stage2B associations。
# Output: Figure3a/3b/3c、row annotations 与 Figure S5a–d 四域共用 membership source。
# 这一段做什么：
#   1) sharedness 只由 P1–P8 Stage 2A selection 定义；
#   2) 正文 3b/c 不再按四个 view 分 facet；
#   3) 先按 Shared → Multi-pattern → Pattern-specific 分组，再在每组选择代表性强的特征；
#   4) 3b/c 使用完全相同 feature key 与 row order；
#   5) side annotation 同时标注 sharedness 和 modality。
# 需要先运行 Section 3–5。

pattern_selected <- stage2a_selected %>%
  filter(outcome_label %in% paste0("P", 1:8), show_in_manuscript %in% TRUE)

figure3_membership <- pattern_selected %>%
  group_by(
    modality, modality_label, modality_order, model_variable,
    analysis_variable, canonical_variable, display_name, subdomain
  ) %>%
  summarise(
    n_selected_patterns = n_distinct(outcome_label),
    selected_patterns = paste(
      paste0("P", sort(as.integer(str_remove(unique(outcome_label), "P")))),
      collapse = ";"
    ),
    mean_abs_signed_importance = mean(abs_signed_importance, na.rm = TRUE),
    max_abs_signed_importance = max(abs_signed_importance, na.rm = TRUE),
    best_stage2a_rank = min(selected_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sharedness = case_when(
      n_selected_patterns >= 4L ~ "Shared",
      n_selected_patterns %in% 2:3 ~ "Multi-pattern",
      n_selected_patterns == 1L ~ "Pattern-specific"
    ),
    sharedness_definition = case_when(
      sharedness == "Shared" ~ "Selected in >=4 patterns",
      sharedness == "Multi-pattern" ~ "Selected in 2-3 patterns",
      TRUE ~ "Selected in exactly 1 pattern"
    ),
    group_order = match(
      sharedness, c("Shared", "Multi-pattern", "Pattern-specific")
    ),
    specific_pattern_order = if_else(
      sharedness == "Pattern-specific",
      as.integer(str_extract(selected_patterns, "(?<=P)\\d+")),
      NA_integer_
    )
  )

figure3a_architecture <- figure3_membership %>%
  count(
    modality, modality_label, modality_order, sharedness,
    sharedness_definition, group_order, name = "n_unique_features"
  ) %>%
  group_by(modality) %>%
  mutate(
    modality_total = sum(n_unique_features),
    proportion_within_modality = n_unique_features / modality_total,
    sharedness_source = "Stage 2A feature_list across P1-P8 only"
  ) %>%
  ungroup() %>%
  arrange(modality_order, group_order)

# Display-name contract: Table S5!F65 = "Chest wheeze (past year)".
# This SHORT, publication-friendly label travels through the shared dictionary.
# Do not modify D65/H65 or the model key chest_weeze_last_1yr.
# Any discrepancy is reported here; please correct the authority Excel before export.
if (any(figure3_membership$model_variable == "chest_weeze_last_1yr" &
        figure3_membership$display_name == "Chest weeze last 1yr")) {
  warning("Figure 3: update Table S5!F65 to 'Chest wheeze (past year)' in authority Excel; retain D65/H65 original model keys")
}

# Figure 3b/c 共用同一套代表性特征与顺序：
# Shared / Multi-pattern / Pattern-specific × E/P/M/G，每格最多 4 个（旧版 3 个）。
# 新增的特征仍必须来自 Stage 2A 已入选的变量；优先入选跨模式重复且重要的特征。
# Pattern-specific 额外保障 P1–P8 的可用模式各至少展示 1 个。
# 若个别交叉格少于 4 个，保留实际数量，不填充虚构变量。
figure3_per_class_domain <- 4L
figure3_rank_features <- function(dat) {
  dat %>%
    arrange(
      desc(n_selected_patterns),
      desc(mean_abs_signed_importance),
      desc(max_abs_signed_importance),
      best_stage2a_rank,
      modality_order, display_name
    ) %>%
    mutate(figure3_global_rank = row_number(),
           figure3_feature_key = paste(modality, model_variable, sep = "||"))
}
figure3_ranked <- figure3_membership %>%
  group_by(sharedness) %>%
  group_modify(~figure3_rank_features(.x)) %>%
  ungroup()

# Clinical/public-health examples are anchored by explicitly documented variable keys,
# but ONLY where those features were actually selected by Stage 2A for P1-P8.
# The quota stays 4 per sharedness x domain; remaining slots follow original
# development-only importance ranking. No held-out effect estimate guides selection.
figure3_priority_keys <- tibble::tribble(
  ~modality, ~model_variable, ~figure3_priority,
  "enviro", "BMI",                    1L,
  "enviro", "cycle_commute",          2L,
  "enviro", "smoking_status",         1L,
  "enviro", "hand_grip_strength_mean",1L,
  "enviro", "nerves",                 2L,
  "enviro", "worrier",                3L,
  "enviro", "chest_weeze_last_1yr",   4L,
  "blood", "cystatin_C",             1L,
  "blood", "creatinine",             2L,
  "blood", "mean_corpuscular_volume",3L,
  "blood", "hbA1c",                 4L,
  "blood", "albumin",               1L,
  "blood", "total_protein",         1L
)
# Verify priorities originate from actual Stage 2A selections; fail instead of
# silently plotting a predictor that did not enter a pattern-specific model.
figure3_priority_QA <- figure3_priority_keys %>%
  left_join(figure3_ranked %>%
              select(modality, model_variable, sharedness, selected_patterns),
            by = c("modality", "model_variable")) %>%
  mutate(passed = !is.na(sharedness))
write_log(figure3_priority_QA, "Figure3_priority_selected_features_QA.csv")
if (any(!figure3_priority_QA$passed)) {
  stop("Figure 3 prioritized features not present in Stage2A memberships; inspect Figure3_priority_selected_features_QA.csv")
}

figure3_top_by_class_domain <- figure3_ranked %>%
  left_join(figure3_priority_keys, by = c("modality", "model_variable")) %>%
  mutate(figure3_priority = coalesce(figure3_priority, 999L)) %>%
  group_by(sharedness, modality) %>%
  arrange(figure3_priority, figure3_global_rank, .by_group = TRUE) %>%
  slice_head(n = figure3_per_class_domain) %>%
  ungroup()

# Add missing pattern representatives only when the full candidate pool contains them.
figure3_specific_candidates <- figure3_ranked %>%
  filter(sharedness == "Pattern-specific")
figure3_specific_pool_patterns <- unique(figure3_specific_candidates$selected_patterns)
figure3_selected_specific_patterns <- unique(
  figure3_top_by_class_domain$selected_patterns[
    figure3_top_by_class_domain$sharedness == "Pattern-specific"
  ]
)
figure3_missing_specific_patterns <- setdiff(
  intersect(paste0("P", 1:8), figure3_specific_pool_patterns),
  figure3_selected_specific_patterns
)
figure3_add_specific <- figure3_specific_candidates %>%
  filter(selected_patterns %in% figure3_missing_specific_patterns) %>%
  group_by(selected_patterns) %>%
  slice_min(figure3_global_rank, n = 1, with_ties = FALSE) %>%
  ungroup()

figure3_representative <- bind_rows(
  figure3_top_by_class_domain, figure3_add_specific
) %>%
  distinct(figure3_feature_key, .keep_all = TRUE) %>%
  mutate(
    forced_environmental_display = modality == "enviro" &
      !is.na(figure3_priority) & figure3_priority < 999L,
    figure3_selection_rule = case_when(
      figure3_feature_key %in% figure3_add_specific$figure3_feature_key ~
        "Additional P1-P8 pattern-specific coverage",
      !is.na(figure3_priority) & figure3_priority < 999L ~
        "Clinically interpretable representative chosen for display from Stage2A-selected features",
      TRUE ~ paste0("Top ", figure3_per_class_domain,
                     " within sharedness x domain by development importance")
    )
  ) %>%
  arrange(
    group_order, modality_order,
    if_else(sharedness == "Pattern-specific", specific_pattern_order, 0L),
    figure3_global_rank, display_name
  ) %>%
  group_by(sharedness) %>%
  mutate(feature_order_within_group = row_number()) %>%
  ungroup() %>%
  mutate(feature_order = row_number())

# All prioritized predictor keys must be present in the representative figure.
figure3_priority_selected_QA <- figure3_priority_QA %>%
  mutate(in_main_figure = paste(modality, model_variable, sep = "||") %in%
           figure3_representative$figure3_feature_key)
write_log(figure3_priority_selected_QA, "Figure3_priority_visible_QA.csv")
if (any(!figure3_priority_selected_QA$in_main_figure)) {
  stop("Figure 3 clinical-priority variables were displaced; inspect Figure3_priority_visible_QA.csv")
}

figure3_representative_balance_qa <- figure3_ranked %>%
  count(sharedness, modality, name = "n_available") %>%
  left_join(
    figure3_representative %>%
      count(sharedness, modality, name = "n_displayed"),
    by = c("sharedness", "modality")
  ) %>%
  mutate(
    n_displayed = coalesce(n_displayed, 0L),
    n_target = pmin(figure3_per_class_domain, n_available),
    passed = n_displayed >= n_target
  ) %>%
  arrange(match(sharedness, c("Shared", "Multi-pattern", "Pattern-specific")),
          match(modality, c("enviro", "blood", "metabolites", "gene")))
figure3_specific_coverage_qa <- tibble(
  pattern = paste0("P", 1:8),
  n_available = vapply(paste0("P", 1:8), function(p)
    sum(figure3_specific_candidates$selected_patterns == p), integer(1)),
  n_displayed = vapply(paste0("P", 1:8), function(p)
    sum(figure3_representative$sharedness == "Pattern-specific" &
          figure3_representative$selected_patterns == p), integer(1))
) %>%
  mutate(passed = n_available == 0L | n_displayed >= 1L)
write_log(figure3_representative_balance_qa,
          "Figure3_representative_modality_balance_QA.csv")
write_log(figure3_specific_coverage_qa,
          "Figure3_pattern_specific_coverage_QA.csv")
if (any(!figure3_representative_balance_qa$passed) ||
    any(!figure3_specific_coverage_qa$passed) ||
    anyDuplicated(figure3_representative$figure3_feature_key)) {
  stop("Figure 3 representative feature selection QA failed; inspect Figure3_*_QA.csv")
}

figure3_row_annotation <- figure3_representative %>%
  transmute(
    feature_order, feature_order_within_group,
    model_variable, analysis_variable, canonical_variable, display_name,
    sharedness, group_order,
    modality, modality_label, modality_order,
    n_selected_patterns, selected_patterns,
    figure3_global_rank, forced_environmental_display,
    figure3_selection_rule
  )

figure3_grid <- crossing(
  figure3_representative %>%
    select(
      modality, modality_label, modality_order, model_variable,
      analysis_variable, canonical_variable, display_name, subdomain,
      n_selected_patterns, selected_patterns, sharedness,
      sharedness_definition, group_order, specific_pattern_order,
      feature_order
    ),
  outcome_label = paste0("P", 1:8)
) %>%
  left_join(
    outcome_lookup %>% select(outcome_label, dataset, outcome_order),
    by = "outcome_label"
  ) %>%
  left_join(
    pattern_selected %>%
      select(outcome_label, modality, model_variable, selected_rank),
    by = c("outcome_label", "modality", "model_variable")
  ) %>%
  mutate(selected_stage2a = !is.na(selected_rank))

figure3b_signed <- figure3_grid %>%
  left_join(
    signed_importance_all %>%
      filter(outcome_label %in% paste0("P", 1:8)) %>%
      select(
        outcome_label, modality, model_variable,
        signed_importance, abs_signed_importance
      ),
    by = c("outcome_label", "modality", "model_variable")
  ) %>%
  mutate(
    selected_but_signed_missing = selected_stage2a & is.na(signed_importance),
    plot_signed_importance = if_else(selected_stage2a, signed_importance, NA_real_),
    source_stage = "Stage 1 signed importance after Stage 2A selection"
  ) %>%
  arrange(feature_order, outcome_order)

figure3c_association <- figure3_grid %>%
  left_join(
    stage2b_associations %>%
      filter(outcome_label %in% paste0("P", 1:8)) %>%
      select(
        outcome_label, modality, model_variable,
        HR, CI_low, CI_high, beta, SE, P, FDR,
        log2_HR, N, events, estimable, failure_reason,
        evaluation_set, adjustment = adjustment_strategy
      ),
    by = c("outcome_label", "modality", "model_variable")
  ) %>%
  mutate(
    selected_but_not_estimable = selected_stage2a &
      (is.na(estimable) | !estimable),
    plot_log2_HR = if_else(
      selected_stage2a & estimable %in% TRUE, log2_HR, NA_real_
    ),
    source_stage = "Stage 2B adjusted association in independent held-out set"
  ) %>%
  arrange(feature_order, outcome_order)

# Figure S5a–d 与 Table S21：展示全部 P1-P8 Stage 2A 入选特征，而不是正文 Figure 3
# 的 30 个 representative features。排序固定为：
# Environmental/phenotypic -> Biomarkers -> Metabolomics -> Genetics，
# 每个 view 内再按 Shared -> Multi-pattern -> Pattern-specific、
# 入选 pattern 数、importance 和 Stage 2A rank 排序。
figureS5_feature_order <- figure3_membership %>%
  arrange(
    modality_order,
    group_order,
    if_else(sharedness == "Pattern-specific", specific_pattern_order, 0L),
    desc(n_selected_patterns),
    desc(mean_abs_signed_importance),
    desc(max_abs_signed_importance),
    best_stage2a_rank,
    display_name
  ) %>%
  mutate(
    feature_order_all = row_number(),
    feature_key_all = paste(modality, model_variable, sep = "||")
  )

figureS5_membership <- crossing(
  figureS5_feature_order,
  outcome_label = paste0("P", 1:8)
) %>%
  mutate(
    selected_stage2a = str_detect(
      selected_patterns,
      paste0("(^|;)", outcome_label, "($|;)")
    ),
    outcome_order = as.integer(str_remove(outcome_label, "P"))
  ) %>%
  arrange(feature_order_all, outcome_order)

# Table S21 使用全部特征，并把 P1-P8 membership 展开成独立列，便于正文/补充图逐行核对。
table_s21_membership_wide <- figureS5_membership %>%
  select(
    feature_order_all,
    modality, modality_label, modality_order,
    model_variable, analysis_variable, canonical_variable,
    display_name, subdomain,
    n_selected_patterns, selected_patterns,
    sharedness, sharedness_definition,
    mean_abs_signed_importance, max_abs_signed_importance,
    best_stage2a_rank,
    outcome_label, selected_stage2a
  ) %>%
  distinct() %>%
  mutate(
    outcome_label = factor(outcome_label, levels = paste0("P", 1:8))
  ) %>%
  pivot_wider(
    names_from = outcome_label,
    values_from = selected_stage2a,
    names_prefix = "selected_",
    values_fill = FALSE
  ) %>%
  arrange(feature_order_all)

figureS5_source_QA <- tibble(
  check = c(
    "all Figure S5 features retained",
    "eight pattern columns represented",
    "all four modalities present",
    "all three sharedness classes present",
    "Figure S5 rows equal all-feature rows x 8",
    "Table S21 contains same complete feature set"
  ),
  observed = c(
    as.character(n_distinct(figureS5_membership$feature_key_all)),
    as.character(n_distinct(figureS5_membership$outcome_label)),
    paste(sort(unique(as.character(figureS5_membership$modality))), collapse = ";"),
    paste(sort(unique(as.character(figureS5_membership$sharedness))), collapse = ";"),
    as.character(
      nrow(figureS5_membership) ==
        8L * nrow(figureS5_feature_order)
    ),
    as.character(
      nrow(table_s21_membership_wide) ==
        nrow(figureS5_feature_order)
    )
  ),
  expected = c(
    as.character(nrow(figureS5_feature_order)),
    "8",
    paste(sort(c("enviro", "blood", "metabolites", "gene")), collapse = ";"),
    paste(sort(c("Shared", "Multi-pattern", "Pattern-specific")), collapse = ";"),
    "TRUE",
    "TRUE"
  )
) %>%
  mutate(passed = observed == expected)

write_log(figureS5_source_QA, "FigureS5_source_QA.csv")
if (!all(figureS5_source_QA$passed)) {
  stop("Figure S5 / Table S21 all-feature source QA failed; inspect FigureS5_source_QA.csv")
}

write_source(figure3a_architecture, "Figure3a_sharedness_architecture.csv")
write_source(figure3_row_annotation, "Figure3_row_annotation.csv")
write_source(figure3_representative, "Figure3_representative_features.csv")
write_source(figure3b_signed, "Figure3b_signed_importance.csv")
write_source(figure3c_association, "Figure3c_heldout_associations.csv")
write_source(figureS5_membership, "FigureS5_exact_figure3_membership.csv")
write_table(
  table_s21_membership_wide,
  "Table_S21_complete_P1_P8_feature_membership.csv"
)
cat("完成：Section 8 Main Figure 3 source + Figure S5/Table S21 all-feature membership\n")

#figure 4####
update_progress_checkpoint("07 Section 8 completed", "从 07 Section 9 继续。")

###############################################################################
### Section 9. Main Figure 4 source ####
###############################################################################
# Input: Stage 3B performance 与 Section 6A 的 final-model partial-score Wald 结果。
# Output: Figure4a/4b/4c source。
# 这一段做什么：
#   a) 与 E-only model 比较的增量 discrimination，单位统一为 percentage points；
#      正文只保留 E+Biomarkers、E+Metabolomics、E+Genetics、Integrated 四组，匹配旧版正文布局；
#   b) final integrated model 的 C-index + mean time-dependent AUC；
#   c) 仍基于四个 final-model partial scores 同时进入 held-out Cox 后的 relative Wald chi-square。
# 需要先运行 Section 6。

stage3b_baseline <- stage3b_performance %>%
  filter(combination == "E") %>%
  select(
    dataset,
    c_index_E = c_index,
    mean_time_dependent_auc_E = mean_time_dependent_auc
  )

figure4a <- stage3b_performance %>%
  left_join(stage3b_baseline, by = "dataset") %>%
  filter(
    combination %in% c("E+B", "E+M", "E+G", "E+B+M+G")
  ) %>%
  mutate(
    combination_display = recode(
      combination,
      `E+B` = "E + Phenotypic biomarkers",
      `E+M` = "E + Metabolomics",
      `E+G` = "E + Genetics",
      `E+B+M+G` = "Integrated"
    ),
    delta_c_index_vs_E = c_index - c_index_E,
    delta_mean_time_dependent_auc_vs_E =
      mean_time_dependent_auc - mean_time_dependent_auc_E,
    delta_c_index_pp = 100 * delta_c_index_vs_E,
    delta_mean_time_dependent_auc_pp =
      100 * delta_mean_time_dependent_auc_vs_E
  ) %>%
  select(
    dataset, outcome_label, outcome_order,
    combination, combination_display, combination_slug,
    n_features, c_index, mean_time_dependent_auc,
    delta_c_index_vs_E, delta_mean_time_dependent_auc_vs_E,
    delta_c_index_pp, delta_mean_time_dependent_auc_pp,
    n_development, n_heldout, evaluation_set
  )

figure4b <- stage3b_performance %>%
  filter(combination == "E+B+M+G") %>%
  select(
    dataset, outcome_label, outcome_order, c_index,
    mean_time_dependent_auc, auc_3y, auc_5y, auc_10y, auc_15y,
    n_development, n_heldout, evaluation_set
  )

# Each outcome must have four finite and non-negative Wald chi-squared components.
# The bars must sum to 100% across E/P/M/G, not within each domain.
figure4c <- modality_score_associations %>%
  left_join(outcome_lookup %>% select(dataset, outcome_order), by = "dataset") %>%
  group_by(dataset, outcome_label) %>%
  mutate(
    wald_domain_count = n_distinct(component_code[is.finite(wald_chisq)]),
    wald_total = sum(wald_chisq, na.rm = TRUE),
    relative_wald_share = if_else(
      wald_domain_count == 4L & is.finite(wald_total) & wald_total > 0,
      wald_chisq / wald_total,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  arrange(outcome_order, match(component_code, c("E", "P", "M", "G")))
figure4e_wald_QA <- figure4c %>%
  group_by(dataset, outcome_label) %>%
  summarise(
    n_domains = n_distinct(component_code),
    n_rows = n(),
    n_finite = sum(is.finite(wald_chisq)),
    wald_total = first(wald_total),
    summed_share = sum(relative_wald_share, na.rm = TRUE),
    passed = n_rows == 4L & n_domains == 4L & n_finite == 4L &
      is.finite(wald_total) & wald_total > 0 &
      abs(summed_share - 1) < 1e-8,
    .groups = "drop"
  )
write_log(figure4e_wald_QA, "Figure4e_four_domain_100pct_QA.csv")
if (nrow(figure4e_wald_QA) != nrow(outcome_lookup) ||
    any(!figure4e_wald_QA$passed)) {
  stop("Figure 4e 不是全部 4 个有效 Domain 的 100% 贡献率。请检查 logs/Figure4e_four_domain_100pct_QA.csv；不允许把缺失贡献强行填满。")
}

write_source(figure4a, "Figure4a_incremental_prediction.csv")
write_source(figure4b, "Figure4b_final_discrimination.csv")
write_source(figure4c, "Figure4c_modality_wald_contribution.csv")
cat("完成：Section 9 Main Figure 4 source\n")

#figure 5####
update_progress_checkpoint("07 Section 9 completed", "从 07 Section 10 继续。")

###############################################################################
### Section 10. Main Figure 5 source ####
###############################################################################
# Input: Section 6B 的 Stage 3B CoxEM Weighted_beta weighted domain scores。
# Output: Figure5a/5b/5c source。
# 最终定义：
#   a) 9 个 outcome（Overall、P1–P8）的四领域连续风险评分关联；
#      每个 score 单独进入 held-out Cox，不调整另外三个 domain scores。
#   b) Environment × individual omic：development median 二分，展示
#      Higher-risk/Higher-risk vs Lower-risk/Lower-risk。
#   c) Environment × equal-weighted combined omics：P/M/G 各 1/3，
#      development re-standardization 后再用 development median 二分，展示完整 2×2 profile。
#   评分严格保留 Stage 2A selected features；若 ethnicity/age/sex/centre 被 Stage 2A 选中，
#   也作为评分特征进入。Figure 5 held-out Cox 默认不调整；可通过 option 开启调整敏感性分析。

figure5a <- figure5a %>%
  left_join(
    outcome_lookup %>% select(dataset, outcome_order_lookup = outcome_order),
    by = "dataset"
  ) %>%
  mutate(
    outcome_order = coalesce(outcome_order, outcome_order_lookup),
    component = factor(
      component,
      levels = c(
        "Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors"
      )
    )
  ) %>%
  select(-outcome_order_lookup) %>%
  arrange(outcome_order, component)

figure5b <- figure5b %>%
  left_join(
    outcome_lookup %>% select(dataset, outcome_order_lookup = outcome_order),
    by = "dataset"
  ) %>%
  mutate(
    outcome_order = coalesce(outcome_order, outcome_order_lookup),
    omic_label = factor(
      omic_label,
      levels = c("Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors")
    )
  ) %>%
  select(-outcome_order_lookup) %>%
  arrange(outcome_order, omic_label)

figure5c <- figure5c %>%
  left_join(
    outcome_lookup %>% select(dataset, outcome_order_lookup = outcome_order),
    by = "dataset"
  ) %>%
  mutate(
    outcome_order = coalesce(outcome_order, outcome_order_lookup),
    environment_order = match(environment_level, c("Lower risk", "Higher risk")),
    omics_order = match(omics_level, c("Lower risk", "Higher risk"))
  ) %>%
  select(-outcome_order_lookup) %>%
  arrange(outcome_order, environment_order, omics_order)

write_source(
  figure5a,
  "Figure5a_risk_oriented_domain_score_associations.csv"
)
write_source(
  figure5b,
  "Figure5b_higher_risk_joint_contrasts.csv"
)
write_source(
  figure5c,
  "Figure5c_environment_combined_omics.csv"
)
write_source(
  figure5_risk_score_weights,
  "Figure5_domain_risk_score_weights.csv"
)
write_source(
  figure5_risk_score_reference,
  "Figure5_domain_risk_score_reference.csv"
)
write_source(
  figure5_combined_omics_reference,
  "Figure5_combined_omics_reference.csv"
)
cat("完成：Section 10 Main Figure 5 source（9 outcomes + median binary joint profiles）\n")

#figure S1–S13####
update_progress_checkpoint("07 Section 10 completed", "从 07 Section 11 继续。")

###############################################################################
### Section 11. Supplementary Figure source data ####
###############################################################################
# Input: Sections 3–10 与指定 PPT 页对应的当前 Stage 2/3 输出。
#   保留全部 source CSV（包括旧名 FigureS6/S7/S8/S11/S12/S13），
#   因为正文 Figure 4/5 仍引用这些文件；08 不再导出重复的补充 PDF。
#   校准数据（旧 S9）和 final predictors（旧 S10）由 08 依次绘制为 S6/S7。
# 这一段做什么：按 v8 新顺序整理，不沿用旧 S 编号。
# 需要先运行 Section 3–10。

# S6：PPT slide 15 的单 modality held-out prediction。
figureS6 <- stage3b_performance %>%
  filter(combination %in% c("E", "B", "M", "G")) %>%
  mutate(
    modality_label = recode(
      combination,
      E = "Environmental and clinical characteristics", B = "Phenotypic biomarkers",
      M = "Metabolomic biomarkers", G = "Genetic factors"
    )
  )
write_source(figureS6, "FigureS6_single_modality_heldout_prediction.csv")

# S7：PPT slide 23，所有 E-based multimodal combinations 与预设 horizon。
figureS7 <- stage3b_performance %>%
  filter(contains_environment) %>%
  arrange(outcome_order, n_omics_added_to_environment, combination)
write_source(figureS7, "FigureS7_incremental_multimodal_prediction.csv")

# S8：PPT slide 24，使用保存的 final risk score 在 held-out set 计算
# 5/10-year cumulative/dynamic IPCW ROC；不重拟合模型。
if (!requireNamespace("timeROC", quietly = TRUE)) {
  stop("Figure S8 需要 R package timeROC；不允许改用其他后端。")
}
figureS8_list <- list()
figureS8_auc_qa_list <- list()
for (i in seq_len(nrow(outcome_lookup))) {
  dataset_i <- outcome_lookup$dataset[[i]]
  outcome_i <- outcome_lookup$outcome_label[[i]]
  f <- file.path(stage3_dir, dataset_i, "heldout_risk_scores.csv")
  fixed_f <- file.path(stage3_dir, dataset_i, "fixed_horizon_performance.csv")
  if (!file.exists(f)) next
  dat <- fread(
    f, select = c("time", "status", "risk_score"), showProgress = FALSE
  )
  roc <- timeROC::timeROC(
    T = dat$time, delta = dat$status, marker = dat$risk_score,
    cause = 1, weighting = "marginal", times = c(1825, 3650),
    iid = FALSE
  )
  for (k in seq_along(roc$times)) {
    figureS8_list[[paste(i, k)]] <- tibble(
      dataset = dataset_i, outcome_label = outcome_i,
      outcome_order = outcome_lookup$outcome_order[[i]],
      horizon_days = as.numeric(roc$times[[k]]),
      horizon_years = as.numeric(roc$times[[k]]) / 365,
      false_positive_rate = as.numeric(roc$FP[, k]),
      sensitivity = as.numeric(roc$TP[, k]),
      auc = as.numeric(roc$AUC[[k]]),
      evaluation_set = "Independent held-out evaluation set",
      estimator = "Cumulative/dynamic IPCW ROC (marginal weighting)"
    )
  }
  if (file.exists(fixed_f)) {
    fixed <- fread(fixed_f) %>% as_tibble()
    figureS8_auc_qa_list[[i]] <- tibble(
      dataset = dataset_i, outcome_label = outcome_i,
      horizon_days = roc$times,
      timeROC_auc = as.numeric(roc$AUC),
      saved_auc = fixed$time_dependent_auc[
        match(roc$times, fixed$horizon_days)
      ],
      absolute_difference = abs(timeROC_auc - saved_auc)
    )
  }
}
figureS8 <- bind_rows(figureS8_list)
figureS8_auc_qa <- bind_rows(figureS8_auc_qa_list) %>%
  mutate(passed_within_0_02 = absolute_difference <= 0.02)
write_source(figureS8, "FigureS8_time_dependent_ROC.csv")
write_log(figureS8_auc_qa, "FigureS8_timeROC_AUC_QA.csv")

# S9a/S9b：Stage 3 保存的 held-out calibration。
read_stage3_across_outcomes <- function(filename) {
  map_dfr(seq_len(nrow(outcome_lookup)), function(i) {
    f <- file.path(stage3_dir, outcome_lookup$dataset[[i]], filename)
    if (!file.exists(f)) return(tibble())
    fread(f, showProgress = FALSE) %>%
      as_tibble() %>%
      mutate(
        dataset = outcome_lookup$dataset[[i]],
        outcome_label = outcome_lookup$outcome_label[[i]],
        outcome_order = outcome_lookup$outcome_order[[i]],
        evaluation_set = "Independent held-out evaluation set"
      )
  })
}
calibration_5year <- read_stage3_across_outcomes("calibration_5y.csv")
calibration_10year <- read_stage3_across_outcomes("calibration_10y.csv")
write_source(calibration_5year, "FigureS9_5year_calibration.csv")
write_source(calibration_10year, "FigureS9_10year_calibration.csv")

# S10：final Stage 3 CoxEM importance magnitude；不解释为有方向的效应。
# 仍按每个 outcome 的全局 top 15，不人为强塞某一 modality；若 top 15 中无 Genetics，
# 由 QA 明确记录。08 中 legend 固定显示四个 modality，以免“无 legend”被误解为数据缺失。
figureS10 <- stage3_feature_importance %>%
  group_by(dataset) %>%
  arrange(final_importance_rank, .by_group = TRUE) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  mutate(metric_definition = paste0(
    "Final Stage 3 CoxEM feature importance magnitude; ",
    "direction is not inferred from this value"
  ))
write_source(figureS10, "FigureS10_final_integrated_feature_importance.csv")

figureS10_top15_modality_QA <- figureS10 %>%
  count(outcome_label, modality_label, name = "n_in_top15") %>%
  complete(
    outcome_label = c("Overall", paste0("P", 1:8)),
    modality_label = c("Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors"),
    fill = list(n_in_top15 = 0L)
  ) %>%
  arrange(match(outcome_label, c("Overall", paste0("P", 1:8))),
          match(modality_label, c("Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors")))
write_log(figureS10_top15_modality_QA, "FigureS10_top15_modality_QA.csv")

figureS10_full_modality_QA <- stage3_feature_importance %>%
  count(outcome_label, modality_label, name = "n_in_final_model") %>%
  complete(
    outcome_label = c("Overall", paste0("P", 1:8)),
    modality_label = c("Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors"),
    fill = list(n_in_final_model = 0L)
  ) %>%
  arrange(match(outcome_label, c("Overall", paste0("P", 1:8))),
          match(modality_label, c("Environmental and clinical characteristics", "Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors")))
write_log(figureS10_full_modality_QA, "FigureS10_full_final_model_modality_QA.csv")

# S11：仅展示 Environmental/phenotypic 内部 subdomain 的相对 Wald 架构。
# v7 图中出现的“无文字色块”不是未知环境域，而是 B/M/G 三个 omics 行的
# subdomain=NA 被误带入了 fill=subdomain。v8 明确排除 omics 行，并在环境域内部
# 重新归一化至 100%。同时输出 feature-level QA，确认是否真的存在未分域环境特征。
figureS11_feature_domain_QA <- stage3_feature_importance %>%
  filter(modality == "enviro") %>%
  transmute(
    dataset, outcome_label, outcome_order,
    model_variable, analysis_variable, display_name,
    subdomain_raw = subdomain,
    subdomain_plot = if_else(
      is.na(subdomain) | !nzchar(str_trim(as.character(subdomain))),
      "Other / unclassified",
      as.character(subdomain)
    ),
    dictionary_domain_missing = is.na(subdomain) |
      !nzchar(str_trim(as.character(subdomain))),
    final_predictor_importance, weighted_beta, final_importance_rank
  ) %>%
  arrange(outcome_order, subdomain_plot, final_importance_rank)
write_log(
  figureS11_feature_domain_QA,
  "FigureS11_environment_feature_domain_QA.csv"
)
write_log(
  figureS11_feature_domain_QA %>% filter(dictionary_domain_missing),
  "FigureS11_unclassified_environment_features.csv"
)
figureS11_domain_QA_summary <- figureS11_feature_domain_QA %>%
  group_by(outcome_label, subdomain_plot, dictionary_domain_missing) %>%
  summarise(n_features = n(), .groups = "drop") %>%
  arrange(match(outcome_label, c("Overall", paste0("P", 1:8))),
          dictionary_domain_missing, subdomain_plot)
write_log(
  figureS11_domain_QA_summary,
  "FigureS11_environment_subdomain_count_QA.csv"
)

figureS11 <- environment_subdomain_wald %>%
  filter(component_type == "Environmental and clinical characteristics subdomain") %>%
  left_join(
    outcome_lookup %>% select(dataset, outcome_order),
    by = "dataset"
  ) %>%
  mutate(
    subdomain = if_else(
      is.na(subdomain) | !nzchar(str_trim(as.character(subdomain))),
      "Other / unclassified",
      as.character(subdomain)
    )
  ) %>%
  group_by(dataset, outcome_label) %>%
  mutate(
    relative_wald_share_full_integrated_model = relative_wald_share,
    relative_wald_share = {
      denom <- sum(wald_chisq, na.rm = TRUE)
      if (is.finite(denom) && denom > 0) {
        wald_chisq / denom
      } else {
        rep(NA_real_, dplyr::n())
      }
    }
  ) %>%
  ungroup() %>%
  arrange(outcome_order, subdomain)
write_source(figureS11, "FigureS11_environmental_subdomain_wald.csv")

# S12：与 Figure 5b 同源，显示 E × individual omic 在 9 个 outcome 中的完整 2×2 结果。
write_source(figure5b_full, "FigureS12_environment_individual_omics_joint_risk.csv")

# S13 / main Figure 5d–e: 统一使用 Stage 3B 单域 Weighted_beta 综合评分。
# E/P/M/G 评分来自开发集拟合的 CoxEM Weighted_beta，随后用开发集均值/SD标准化。
# The integrated epidemiologic risk score is the unweighted mean of FOUR
# standardized domain scores, then standardized using DEVELOPMENT mean/SD.
# This is a descriptive risk stratification tool, NOT a refitted Stage 3 model.
# Tertile cutpoints are selected only in development and held unchanged in held-out.
# No held-out HR/direction is used to choose weights, signs or cutpoints.
# ----------------------------------------------------------------------------
if (!exists("combine_domain_scores") || !exists("figure5_score_results")) {
  stop("Figure 5d/e 必须先运行 07 Section 6B，获得 Stage 3B Weighted_beta domain score objects。")
}
figureS13_km_list <- list()
figureS13_hr_list <- list()
figureS13_cutpoint_list <- list()
figureS13_group_qa_list <- list()
figureS13_orientation_list <- list()

for (i in seq_len(nrow(outcome_lookup))) {
  dataset_i <- outcome_lookup$dataset[[i]]
  outcome_i <- outcome_lookup$outcome_label[[i]]
  score_terms <- paste0(c("E", "P", "M", "G"), "_risk_z")
  dev_score <- combine_domain_scores(dataset_i, "dev")
  held_score <- combine_domain_scores(dataset_i, "held")
  for (split_i in c("dev_score", "held_score")) {
    z <- get(split_i)
    required_cols <- c("eid", "time", "status", score_terms)
    if (!all(required_cols %in% names(z))) {
      stop("Figure 5 integrated risk score missing columns in ", split_i,
           " / ", outcome_i, ": ",
           paste(setdiff(required_cols, names(z)), collapse = ", "))
    }
    z <- z %>%
      mutate(eid = as.character(eid), time = as.numeric(time),
             status = as.integer(status)) %>%
      filter(is.finite(time), status %in% c(0L,1L),
             if_all(all_of(score_terms), is.finite))
    if (anyDuplicated(z$eid)) stop("Duplicate eid in Figure 5 ", split_i,
                                  " / ", outcome_i)
    z$integrated_risk_score_raw <- rowMeans(as.matrix(z[, score_terms]))
    assign(split_i, z)
  }
  mu <- mean(dev_score$integrated_risk_score_raw)
  sigma <- sd(dev_score$integrated_risk_score_raw)
  if (!is.finite(sigma) || sigma <= 0) {
    stop("Invalid development composite score SD for ", outcome_i)
  }
  dev_score$integrated_risk_score <- (dev_score$integrated_risk_score_raw-mu)/sigma
  held_score$integrated_risk_score <- (held_score$integrated_risk_score_raw-mu)/sigma
  cuts <- as.numeric(quantile(dev_score$integrated_risk_score,
                       probs = c(1/3, 2/3), type = 7, names = FALSE))
  if (any(!is.finite(cuts)) || cuts[1] >= cuts[2]) {
    stop("Invalid development tertiles for ", outcome_i)
  }
  make_risk_group <- function(x) factor(
    cut(x, breaks=c(-Inf,cuts,Inf), labels=c("Low","Intermediate","High"),
        include.lowest=TRUE), levels=c("Low","Intermediate","High"))
  dev_score$risk_group <- make_risk_group(dev_score$integrated_risk_score)
  held_score$risk_group <- make_risk_group(held_score$integrated_risk_score)
  if (n_distinct(held_score$risk_group) != 3L) {
    stop("Held-out Figure 5 risk groups incomplete for ", outcome_i)
  }
  event_count <- sum(held_score$status==1L)
  figureS13_group_qa_list[[i]] <- bind_rows(
    dev_score %>% count(risk_group, name="N") %>%
      left_join(dev_score %>% group_by(risk_group) %>%
                  summarise(events=sum(status==1L),.groups="drop"),
                by="risk_group") %>% mutate(split="Development"),
    held_score %>% count(risk_group, name="N") %>%
      left_join(held_score %>% group_by(risk_group) %>%
                  summarise(events=sum(status==1L),.groups="drop"),
                by="risk_group") %>% mutate(split="Held-out")
  ) %>% mutate(dataset=dataset_i,outcome_label=outcome_i,
              outcome_order=outcome_lookup$outcome_order[[i]],
              p33_development=cuts[1],p67_development=cuts[2],.before=1)
  sf <- survival::survfit(Surv(time,status)~risk_group,data=held_score)
  logrank_fit <- survival::survdiff(Surv(time,status)~risk_group,data=held_score)
  logrank_df <- length(logrank_fit$n)-1L
  logrank_p <- stats::pchisq(logrank_fit$chisq,df=logrank_df,lower.tail=FALSE)
  km_i <- broom::tidy(sf) %>%
    transmute(time=as.numeric(time), time_years=as.numeric(time)/365.25,
              survival=as.numeric(estimate), std_error=as.numeric(std.error),
              conf_low=as.numeric(conf.low), conf_high=as.numeric(conf.high),
              strata=as.character(strata),
              risk_group=str_remove(as.character(strata),"^risk_group="),
              logrank_chisq=as.numeric(logrank_fit$chisq),
              logrank_df=as.integer(logrank_df), logrank_p=as.numeric(logrank_p),
              dataset=dataset_i,outcome_label=outcome_i,
              outcome_order=outcome_lookup$outcome_order[[i]],
              score_source="Stage 3B fixed-feature CoxEM Weighted_beta; equal-weight E/P/M/G z-scores",
              evaluation_set="Independent held-out evaluation set")
  km_zero_i <- tibble(time=0,time_years=0,survival=1,std_error=0,
        conf_low=1,conf_high=1,
        strata=paste0("risk_group=",c("Low","Intermediate","High")),
        risk_group=c("Low","Intermediate","High"),
        logrank_chisq=as.numeric(logrank_fit$chisq),
        logrank_df=as.integer(logrank_df),logrank_p=as.numeric(logrank_p),
        dataset=dataset_i,outcome_label=outcome_i,
        outcome_order=outcome_lookup$outcome_order[[i]],
        score_source="Stage 3B fixed-feature CoxEM Weighted_beta; equal-weight E/P/M/G z-scores",
        evaluation_set="Independent held-out evaluation set")
  figureS13_km_list[[i]] <- bind_rows(km_zero_i,km_i) %>%
    arrange(risk_group,time)
  fit_hr <- survival::coxph(Surv(time,status)~risk_group,data=held_score,ties="efron")
  hr_est <- broom::tidy(fit_hr,conf.int=TRUE) %>%
    transmute(risk_group=str_remove(term,"^risk_group"),
              HR=exp(estimate),CI_low=exp(conf.low),CI_high=exp(conf.high),
              beta=estimate, SE=std.error, P=p.value)
  figureS13_hr_list[[i]] <- bind_rows(
    tibble(risk_group="Low",HR=1,CI_low=1,CI_high=1,
           beta=0,SE=0,P=NA_real_),hr_est
  ) %>% mutate(dataset=dataset_i,outcome_label=outcome_i,
      outcome_order=outcome_lookup$outcome_order[[i]],
      p33_development=cuts[1],p67_development=cuts[2],
      n_development=nrow(dev_score),N=nrow(held_score),events=event_count,
      reference="Low integrated epidemiologic risk-score tertile",
      cutpoint_source="Development-set tertiles, held-out unchanged",
      score_definition="equal-weight mean of 4 development-z Stage 3B Weighted_beta domain scores, then development-z",
      python_saved_risk_score_used=FALSE,
      evaluation_set="Independent held-out evaluation set")
  figureS13_cutpoint_list[[i]] <- tibble(
      dataset=dataset_i,outcome_label=outcome_i,
      outcome_order=outcome_lookup$outcome_order[[i]],
      p33_development=cuts[1],p67_development=cuts[2],
      development_score_mean=mu,development_score_sd=sigma,
      n_development=nrow(dev_score),n_heldout=nrow(held_score),
      events_heldout=event_count,
      score_definition="equal-weight E/P/M/G Stage 3B Weighted_beta risk z-scores",
      python_saved_risk_score_used=FALSE)
  # Hold-out direction audit only. Do not flip score signs based on its result.
  fit_cont <- survival::coxph(Surv(time,status)~integrated_risk_score,
                             data=held_score,ties="efron")
  hr_cont <- exp(as.numeric(coef(fit_cont)[1]))
  figureS13_orientation_list[[i]] <- tibble(
    dataset=dataset_i,outcome_label=outcome_i,
    heldout_hr_per_1sd=hr_cont,
    direction_higher_is_higher_risk=is.finite(hr_cont) & hr_cont>1,
    note="Audit only: fixed development Stage 3B CoxEM Weighted_beta; no held-out sign flipping.")
}
figureS13_km <- bind_rows(figureS13_km_list)
figureS13_hr <- bind_rows(figureS13_hr_list)
figureS13_cutpoints <- bind_rows(figureS13_cutpoint_list)
figureS13_group_qa <- bind_rows(figureS13_group_qa_list)
figureS13_orientation_QA <- bind_rows(figureS13_orientation_list)
write_source(figureS13_km,"FigureS13_integrated_score_KM.csv")
write_source(figureS13_hr,"FigureS13_integrated_score_HR.csv")
write_log(figureS13_cutpoints,"FigureS13_integrated_score_cutpoint_QA.csv")
write_log(figureS13_group_qa,"FigureS13_integrated_score_group_QA.csv")
write_log(figureS13_orientation_QA,"Figure5_integrated_risk_direction_QA.csv")
write_csv(figureS13_hr,
          file.path(intermediate_dir,"Stage3B_integrated_coxem_risk_stratification.csv"))
cat("完成：Figure 5d/e 和 Figure S13 使用同一套 Stage3B Weighted_beta 风险导向评分；四个领域等权综合，开发集三分位数，独立 held-out 评估。\n")

# 最终补充图顺序：与 260927 Supplementary Figure(1).docx 一致。
# S9 是同一张图的 a/b 两个校准面板；S13 同时使用 KM 和 HR source。
supp_manifest <- tibble(
  new_figure_number = paste0("Figure S", 1:13),
  ppt_slide_reference = c(NA, NA, 14, 16, 19, 15, 23, 24, 20, 26, 25, 28, 30),
  figure_title = c(
    "Stage 1 signed feature importance across four data domains",
    "Internal cross-validation and 1-SE feature selection in Stage 2A",
    "Stage 2A-selected predictors and development-derived signed importance",
    "Adjusted held-out associations of selected predictors for overall multimorbidity and eight patterns",
    "Complete cross-pattern membership of Stage 2A-selected features",
    "Single-domain predictive discrimination in the independent held-out set",
    "Incremental 10-year predictive gain across environment-based multimodal combinations",
    "Five- and ten-year time-dependent ROC curves for the final integrated models",
    "Calibration of the final integrated multimodal models",
    "Leading predictors in the final integrated multimodal models",
    "Environmental and clinical subdomain contributions to prognostic information",
    "Joint held-out risk profiles for environmental and individual biomarker scores",
    "Held-out clinical risk stratification using the integrated Stage 3B CoxEM-derived risk score"
  ),
  source_file = c(
    "FigureS1_stage1_signed_top50.csv",
    "FigureS2_stage2A_internal_CV_1SE.csv",
    "FigureS3_stage2A_selected_signed_importance.csv",
    "FigureS4_nine_outcomes_stage2B_associations.csv",
    "FigureS5_exact_figure3_membership.csv",
    "FigureS6_single_modality_heldout_prediction.csv",
    "FigureS7_incremental_multimodal_prediction.csv",
    "FigureS8_time_dependent_ROC.csv",
    "FigureS9_5year_calibration.csv",
    "FigureS10_final_integrated_feature_importance.csv",
    "FigureS11_environmental_subdomain_wald.csv",
    "FigureS12_environment_individual_omics_joint_risk.csv",
    "FigureS13_integrated_score_KM.csv"
  ),
  secondary_source_file = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "FigureS9_10year_calibration.csv",
    "",
    "",
    "",
    "FigureS13_integrated_score_HR.csv"
  )
) %>%
  mutate(
    source_path = file.path(figure_source_dir, source_file),
    secondary_source_path = if_else(
      nzchar(secondary_source_file),
      file.path(figure_source_dir, secondary_source_file), NA_character_
    ),
    primary_source_exists = file.exists(source_path),
    secondary_source_exists = if_else(
      nzchar(secondary_source_file), file.exists(secondary_source_path), TRUE
    ),
    source_exists = primary_source_exists & secondary_source_exists,
    status = if_else(source_exists, "source_ready", "missing_source")
  )
write_log(supp_manifest, "supplementary_figure_manifest.csv")
write_log(
  supp_manifest %>% select(
    new_figure_number, source_file, secondary_source_file, source_exists, status
  ),
  "supplementary_source_inventory.csv"
)
if (!all(supp_manifest$source_exists)) {
  stop("补充图 source 不完整；检查 supplementary_source_inventory.csv。")
}
cat("完成：Section 11 Supplementary Figure source data（S1–S13；S9 含 a/b 校准）\n")

update_progress_checkpoint("07 Section 11 completed", "从 07 Section 12 继续。")

###############################################################################
### Section 12. Main Table 1 + descriptive helpers ####
###############################################################################
# Input:
#   source/final_environment.csv
#   source/final_biomarkers.csv
#   source/UKB_diseases_data_with_patterns.csv
# Output:
#   main_tables/Table_1_baseline_characteristics.csv/.xlsx
#   logs/Table1_source_QA.csv
#
# 说明：
#   1) Table 1 按 No incident multimorbidity / Incident multimorbidity / Overall 展示；
#   2) sex 作为 Table 1 的基线特征之一，而不是再单独按男女分列；
#   3) 连续变量为 Mean (SD)；分类变量为 n (%)；
#   4) 正文 Table 1 不单列 Missing 或 -1 structural missing；
#   5) 本 Section 同时定义 S10-S17 会复用的描述性统计 helper。
###############################################################################

need_object(
  "variable_dictionary",
  file.path(intermediate_dir, "variable_display_dictionary.csv")
)

# -----------------------------------------------------------------------------
# 12.1 Descriptive helpers
# -----------------------------------------------------------------------------

normalise_category <- function(x) {
  str_to_lower(
    str_squish(
      str_replace_all(
        as.character(x),
        "_",
        " "
      )
    )
  )
}

format_mean_sd <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  sprintf("%.2f (%.2f)", mean(x), sd(x))
}

format_count_pct <- function(n, den) {
  if (!is.finite(den) || den <= 0) return(NA_character_)
  sprintf(
    "%s (%.1f%%)",
    format(as.integer(n), big.mark = ",", scientific = FALSE),
    100 * n / den
  )
}

is_raw_missing <- function(x) {
  raw <- as.character(x)
  trimmed <- str_trim(raw)
  is.na(raw) |
    !nzchar(trimmed) |
    str_to_lower(trimmed) == "nan"
}

is_raw_structural_missing <- function(x) {
  raw <- as.character(x)
  trimmed <- str_trim(raw)
  !is_raw_missing(x) &
    trimmed %in% c("-1", "-1.0")
}

classify_missing_category <- function(x) {
  out <- str_trim(as.character(x))
  out[is_raw_missing(x)] <- "Missing"
  out[is_raw_structural_missing(x)] <-
    "Not applicable / missing by design (-1)"
  out
}

valid_nonmissing_mask <- function(x) {
  !is_raw_missing(x) &
    !is_raw_structural_missing(x)
}

order_category_levels <- function(
    levels_in_data,
    preferred = NULL
) {
  
  levels_in_data <-
    unique(
      as.character(
        levels_in_data
      )
    )
  
  levels_in_data <-
    levels_in_data[
      !is.na(levels_in_data) &
        nzchar(
          str_trim(
            levels_in_data
          )
        )
    ]
  
  if (!length(levels_in_data)) {
    return(character())
  }
  
  
  # Missing / structural missing 永远放最后
  special <- c(
    "Not applicable / missing by design (-1)",
    "Missing"
  )
  
  ordinary <-
    setdiff(
      levels_in_data,
      special
    )
  
  
  # ==========================================================
  # 1. 如果这个变量有人工指定顺序，则优先按指定顺序
  # ==========================================================
  
  if (
    !is.null(preferred) &&
    length(preferred)
  ) {
    
    preferred_norm <-
      normalise_category(
        preferred
      )
    
    ordinary_norm <-
      normalise_category(
        ordinary
      )
    
    matched_index <-
      match(
        preferred_norm,
        ordinary_norm,
        nomatch = 0L
      )
    
    matched_index <-
      matched_index[
        matched_index > 0L
      ]
    
    ordered <-
      ordinary[
        matched_index
      ]
    
    remaining <-
      ordinary[
        !ordinary %in%
          ordered
      ]
    
    
    # 未在 preferred 中明确指定的剩余水平：
    # 如果是纯数值，则继续按数值升序
    if (length(remaining) > 0L) {
      
      remaining_numeric <-
        suppressWarnings(
          as.numeric(
            str_trim(
              remaining
            )
          )
        )
      
      if (
        all(
          is.finite(
            remaining_numeric
          )
        )
      ) {
        
        remaining <-
          remaining[
            order(
              remaining_numeric
            )
          ]
      }
    }
    
    
    ordinary <-
      c(
        ordered,
        remaining
      )
    
    
  } else {
    
    # ========================================================
    # 2. 没有人工指定顺序
    #
    # 纯数字类别严格按照数值升序：
    #   1,0,2      -> 0,1,2
    #   2,10,1     -> 1,2,10
    #   1.0,0.0,2  -> 0.0,1.0,2
    # ========================================================
    
    numeric_levels <-
      suppressWarnings(
        as.numeric(
          str_trim(
            ordinary
          )
        )
      )
    
    
    if (
      length(ordinary) > 0L &&
      all(
        is.finite(
          numeric_levels
        )
      )
    ) {
      
      ordinary <-
        ordinary[
          order(
            numeric_levels
          )
        ]
      
      
    } else {
      
      # ======================================================
      # 3. 如果类别是：
      #      "0 = Never"
      #      "1 = Sometimes"
      #      "2 = Usually"
      #
      # 也按开头的数字顺序排
      # ======================================================
      
      leading_numeric <-
        suppressWarnings(
          as.numeric(
            str_extract(
              str_trim(
                ordinary
              ),
              "^-?[0-9]+(?:\\.[0-9]+)?"
            )
          )
        )
      
      
      if (
        length(ordinary) > 0L &&
        all(
          is.finite(
            leading_numeric
          )
        )
      ) {
        
        ordinary <-
          ordinary[
            order(
              leading_numeric
            )
          ]
        
      } else {
        
        # 真正无顺序的 nominal variable 才按字符排序
        ordinary <-
          sort(
            ordinary
          )
      }
    }
  }
  
  
  # structural missing / Missing 始终在最后
  c(
    ordinary,
    
    intersect(
      "Not applicable / missing by design (-1)",
      levels_in_data
    ),
    
    intersect(
      "Missing",
      levels_in_data
    )
  )
}

infer_descriptive_kind <- function(
    variable_type,
    x
) {
  
  type_lower <-
    str_to_lower(
      coalesce(
        as.character(variable_type),
        ""
      )
    )
  
  valid <-
    valid_nonmissing_mask(
      x
    )
  
  xv <-
    x[
      valid
    ]
  
  if (!length(xv)) {
    return("continuous")
  }
  
  
  # ==========================================================
  # Data-based diagnostics
  # ==========================================================
  
  num <-
    suppressWarnings(
      as.numeric(
        as.character(
          xv
        )
      )
    )
  
  numeric_ok <-
    length(num) > 0L &&
    all(
      is.finite(
        num
      )
    )
  
  n_unique <-
    n_distinct(
      xv
    )
  
  
  # ==========================================================
  # 1. Metadata says binary / dummy
  #
  # 不能直接相信 metadata：
  #   heel_sound_speed 在当前 Table S5 被误标成 Binary，
  #   但实际是大量不同数值的连续变量。
  # ==========================================================
  
  if (
    str_detect(
      type_lower,
      "binary|dummy|indicator"
    )
  ) {
    
    # 真正的 0/1 binary
    if (
      numeric_ok &&
      all(
        num %in%
        c(
          0,
          1
        )
      )
    ) {
      return("binary")
    }
    
    
    # 明显的连续数值变量：
    # metadata 即使误标 binary，也按数据纠正
    if (
      numeric_ok &&
      n_unique >
      20L
    ) {
      return("continuous")
    }
    
    
    # one-hot collapse 回原始 parent 后，
    # parent 本身可能是多分类字符变量
    if (
      !numeric_ok &&
      n_unique >
      2L
    ) {
      return("categorical")
    }
    
    
    return("binary")
  }
  
  
  # ==========================================================
  # 2. Explicit continuous metadata
  # ==========================================================
  
  if (
    str_detect(
      type_lower,
      "continuous|numeric|numerical|quantitative|double|float|real"
    )
  ) {
    return("continuous")
  }
  
  
  # ==========================================================
  # 3. Explicit categorical metadata
  # ==========================================================
  
  if (
    str_detect(
      type_lower,
      "ordinal|categorical|factor|nominal"
    )
  ) {
    return("categorical")
  }
  
  
  # ==========================================================
  # 4. Fallback from observed data
  # ==========================================================
  
  if (numeric_ok) {
    
    if (
      n_unique <=
      2L &&
      all(
        num %in%
        c(
          0,
          1
        )
      )
    ) {
      return("binary")
    }
    
    return("continuous")
  }
  
  
  "categorical"
}

category_vector_for_descriptive <- function(
    x,
    kind = c("categorical", "binary"),
    target_level = NA_character_,
    derived_from_parent = FALSE
) {

  kind <-
    match.arg(kind)

  out <-
    classify_missing_category(x)

  valid <-
    valid_nonmissing_mask(x)

  if (!any(valid)) {
    return(out)
  }

  if (kind == "binary") {

    target_ok <-
      !is.na(target_level) &&
      nzchar(
        str_trim(
          target_level
        )
      )

    if (
      isTRUE(
        derived_from_parent
      ) &&
      target_ok
    ) {

      hit <-
        normalise_category(
          x[valid]
        ) ==
        normalise_category(
          target_level
        )

      out[valid] <-
        ifelse(
          hit,
          as.character(target_level),
          "Other / not this level"
        )

    } else {

      num <-
        suppressWarnings(
          as.numeric(
            as.character(
              x[valid]
            )
          )
        )

      binary_numeric <-
        length(num) > 0L &&
        all(
          is.finite(
            num
          )
        ) &&
        all(
          num %in%
            c(
              0,
              1
            )
        )

      if (binary_numeric) {

        if (target_ok) {

          out[valid] <-
            ifelse(
              num == 1,
              as.character(target_level),
              "Other / not this level"
            )

        } else {

          out[valid] <-
            ifelse(
              num == 1,
              "Yes / present",
              "No / absent"
            )
        }

      } else {

        out[valid] <-
          str_trim(
            as.character(
              x[valid]
            )
          )
      }
    }

  } else {

    out[valid] <-
      str_trim(
        as.character(
          x[valid]
        )
      )
  }

  out
}

summarise_variable_rows <- function(
    x,
    groups,
    kind = c("continuous", "categorical", "binary"),
    include_missing_rows = TRUE,
    target_level = NA_character_,
    derived_from_parent = FALSE,
    preferred_levels = NULL
) {

  kind <-
    match.arg(kind)

  if (kind == "continuous") {

    row <- tibble(
      category = NA_character_,
      category_order = 1L,
      statistic = "Mean (SD)"
    )

    for (g in names(groups)) {

      row[[g]] <-
        format_mean_sd(
          x[
            groups[[g]]
          ]
        )
    }

    return(row)
  }

  category_x <-
    category_vector_for_descriptive(
      x = x,
      kind = kind,
      target_level = target_level,
      derived_from_parent = derived_from_parent
    )

  valid <-
    valid_nonmissing_mask(x)

  valid_levels <-
    unique(
      category_x[
        valid
      ]
    )

  valid_levels <-
    order_category_levels(
      valid_levels,
      preferred = preferred_levels
    )

  levels_to_show <-
    valid_levels

  if (
    isTRUE(
      include_missing_rows
    )
  ) {

    if (
      any(
        is_raw_structural_missing(
          x
        )
      )
    ) {

      levels_to_show <- c(
        levels_to_show,
        "Not applicable / missing by design (-1)"
      )
    }

    # 补充描述表：Missing 始终单独展示，即使 n=0。
    levels_to_show <- c(
      levels_to_show,
      "Missing"
    )
  }

  levels_to_show <-
    unique(
      levels_to_show
    )

  map_dfr(
    seq_along(
      levels_to_show
    ),
    function(k) {

      lev <-
        levels_to_show[[k]]

      row <- tibble(
        category = lev,
        category_order = as.integer(k),
        statistic = "n (%)"
      )

      for (g in names(groups)) {

        idx <-
          groups[[g]]

        if (
          isTRUE(
            include_missing_rows
          )
        ) {

          den <-
            length(idx)

        } else {

          # 正文 Table 1 不展示 Missing，因此分类变量百分比分母为 non-missing。
          den <-
            sum(
              valid[idx],
              na.rm = TRUE
            )
        }

        n <-
          sum(
            category_x[idx] ==
              lev,
            na.rm = TRUE
          )

        row[[g]] <-
          format_count_pct(
            n,
            den
          )
      }

      row
    }
  )
}

resolve_source_column <- function(meta, header) {
  
  # ==========================================================
  # 1. 显式 parent variable
  # ==========================================================
  
  parent_candidates <-
    unlist(
      str_split(
        coalesce(
          meta$parent_variable,
          ""
        ),
        ";"
      )
    )
  
  parent_candidates <-
    str_trim(
      parent_candidates
    )
  
  parent_candidates <-
    parent_candidates[
      !is.na(parent_candidates) &
        nzchar(parent_candidates)
    ]
  
  
  hit <-
    parent_candidates[
      parent_candidates %in%
        header
    ]
  
  if (length(hit)) {
    
    return(
      list(
        column =
          hit[[1]],
        derived_from_parent =
          TRUE,
        source_rule =
          "explicit_parent"
      )
    )
  }
  
  
  # ==========================================================
  # 2. 从 variable==level 自动识别原始 parent
  #
  # 例如：
  # reason_reducing_alcohol==Doctor's advice
  #            ↓
  # reason_reducing_alcohol
  # ==========================================================
  
  encoded_candidates <-
    unique(
      na.omit(
        c(
          meta$analysis_variable,
          meta$model_variable
        )
      )
    )
  
  encoded_parent <-
    encoded_candidates[
      str_detect(
        encoded_candidates,
        fixed("==")
      )
    ]
  
  if (length(encoded_parent)) {
    
    encoded_parent <-
      str_trim(
        str_remove(
          encoded_parent,
          "==.*$"
        )
      )
    
    hit <-
      encoded_parent[
        encoded_parent %in%
          header
      ]
    
    if (length(hit)) {
      
      return(
        list(
          column =
            hit[[1]],
          derived_from_parent =
            TRUE,
          source_rule =
            "parent_inferred_from_encoded_name"
        )
      )
    }
  }
  
  
  # ==========================================================
  # 3. 非 one-hot 的原始列
  # ==========================================================
  
  direct_candidates <-
    unique(
      na.omit(
        c(
          meta$original_data_column,
          meta$analysis_variable,
          meta$model_variable
        )
      )
    )
  
  direct_candidates <-
    direct_candidates[
      nzchar(
        str_trim(
          direct_candidates
        )
      ) &
        !str_detect(
          direct_candidates,
          fixed("==")
        )
    ]
  
  hit <-
    direct_candidates[
      direct_candidates %in%
        header
    ]
  
  if (length(hit)) {
    
    return(
      list(
        column =
          hit[[1]],
        derived_from_parent =
          FALSE,
        source_rule =
          "direct_raw_column"
      )
    )
  }
  
  
  list(
    column =
      NA_character_,
    derived_from_parent =
      FALSE,
    source_rule =
      "unavailable"
  )
}

prepare_view_dictionary <- function(
    modality_i,
    raw_file_i
) {
  
  # ==========================================================
  # Raw file header
  # ==========================================================
  
  header_i <-
    names(
      fread(
        raw_file_i,
        nrows = 0,
        showProgress = FALSE
      )
    )
  
  
  # ==========================================================
  # Analytic dictionary
  # ==========================================================
  
  dict_analytic <-
    variable_dictionary %>%
    filter(
      modality ==
        modality_i,
      show_in_manuscript %in%
        TRUE
    ) %>%
    mutate(
      dictionary_order_analytic =
        row_number()
    )
  
  
  # ==========================================================
  # Resolve every analytic row back to its raw source
  # ==========================================================
  
  source_map_i <-
    map_dfr(
      seq_len(
        nrow(
          dict_analytic
        )
      ),
      function(j) {
        
        z <-
          resolve_source_column(
            dict_analytic[j, ],
            header_i
          )
        
        tibble(
          dictionary_order_analytic =
            j,
          
          source_column =
            z$column,
          
          derived_from_parent =
            z$derived_from_parent,
          
          source_rule =
            z$source_rule
        )
      }
    )
  
  
  mapped <-
    dict_analytic %>%
    left_join(
      source_map_i,
      by =
        "dictionary_order_analytic"
    )
  
  
  # ==========================================================
  # QA:
  # analytic variables that cannot be represented in the
  # raw/pre-one-hot descriptive table
  #
  # IMPORTANT:
  # these remain valid analytic predictors elsewhere;
  # they are simply not printed as "Source unavailable".
  # ==========================================================
  
  unavailable <-
    mapped %>%
    filter(
      is.na(
        source_column
      )
    )
  
  
  if (
    nrow(
      unavailable
    ) >
    0L
  ) {
    
    write_log(
      unavailable %>%
        select(
          modality,
          analysis_variable,
          model_variable,
          display_name,
          original_data_column,
          parent_variable,
          one_hot_level,
          source_rule
        ),
      
      paste0(
        "Table_descriptive_",
        modality_i,
        "_analytic_rows_without_raw_source.csv"
      )
    )
  }
  
  
  mapped_available <-
    mapped %>%
    filter(
      !is.na(
        source_column
      )
    )
  
  
  # ==========================================================
  # Collapse multiple one-hot / encoded rows back to ONE
  # original raw variable.
  #
  # Example:
  #
  # Reason reducing alcohol: Doctor's advice
  # Reason reducing alcohol: Financial reasons
  # Reason reducing alcohol: Health precaution
  #
  #            ↓
  #
  # Reason reducing alcohol
  # ==========================================================
  
  raw_dictionary <-
    mapped_available %>%
    
    group_by(
      modality,
      source_column
    ) %>%
    
    group_modify(
      function(.x, .y) {
        
        ukb_description_values <-
          unique(
            na.omit(
              as.character(
                .x$ukb_description
              )
            )
          )
        # ------------------------------------------------------
        # Display name
        # ------------------------------------------------------
        
        display_values <-
          unique(
            na.omit(
              as.character(
                .x$display_name
              )
            )
          )
        
        
        display_base <-
          if (
            nrow(.x) >
            1L &&
            length(
              ukb_description_values
            ) ==
            1L
          ) {
            
            ukb_description_values[[1]]
            
          } else if (
            length(
              display_values
            ) >
            0L
          ) {
            
            display_values[[1]]
            
          } else {
            
            str_replace_all(
              .y$source_column[[1]],
              "_",
              " "
            )
          }
        
        
        # ------------------------------------------------------
        # Variable type metadata
        # ------------------------------------------------------
        
        variable_type_values <-
          unique(
            na.omit(
              as.character(
                .x$variable_type
              )
            )
          )
        
        
        tibble(
          dictionary_order =
            min(
              .x$dictionary_order_analytic
            ),
          
          modality_label =
            dplyr::first(
              na.omit(
                .x$modality_label
              )
            ),
          
          subdomain =
            dplyr::first(
              na.omit(
                .x$subdomain
              )
            ),
          
          analysis_variable =
            .y$source_column[[1]],
          
          display_name =
            display_base,
          
          variable_type =
            if (
              length(
                variable_type_values
              )
            ) {
              paste(
                variable_type_values,
                collapse = "; "
              )
            } else {
              NA_character_
            },
          
          original_data_column =
            .y$source_column[[1]],
          
          parent_variable =
            .y$source_column[[1]],
          
          one_hot_level =
            NA_character_,
          
          technical_level =
            "raw_descriptive_variable",
          
          
          derived_from_parent =
            any(
              .x$derived_from_parent %in%
                TRUE
            ),
          
          n_analytic_rows_collapsed =
            nrow(
              .x
            )
        )
      }
    ) %>%
    
    ungroup() %>%
    
    arrange(
      dictionary_order
    )
  
  
  raw_dictionary
}

# -----------------------------------------------------------------------------
# 12.2 Authoritative incident multimorbidity status
# -----------------------------------------------------------------------------
# Reuse the single cached MAIN disease object from Section 0/7.
# No second fread() of UKB_diseases_data_with_patterns.csv is performed here.
# -----------------------------------------------------------------------------

main_disease_data <- get_main_disease_data()

multimorbidity_status <-
  main_disease_data %>%
  transmute(
    eid =
      as.character(
        eid
      ),
    incident_multimorbidity =
      as.integer(
        incident_multimorbidity
      ),
    multimorbidity_status =
      factor(
        if_else(
          incident_multimorbidity ==
            1L,
          "Incident multimorbidity",
          "No incident multimorbidity"
        ),
        levels = c(
          "No incident multimorbidity",
          "Incident multimorbidity"
        )
      )
  ) %>%
  distinct(
    eid,
    .keep_all = TRUE
  )

main_cohort_n <-
  nrow(
    multimorbidity_status
  )

stopifnot(
  !anyDuplicated(
    multimorbidity_status$eid
  ),
  !anyNA(
    multimorbidity_status$incident_multimorbidity
  ),
  all(
    multimorbidity_status$incident_multimorbidity %in%
      c(
        0L,
        1L
      )
  )
)

write_log(
  multimorbidity_status %>%
    count(
      multimorbidity_status,
      name = "N"
    ) %>%
    mutate(
      percent =
        100 *
        N /
        sum(
          N
        )
    ),
  "incident_multimorbidity_status_distribution_QA.csv"
)

# -----------------------------------------------------------------------------
# 12.3 Main Table 1
# -----------------------------------------------------------------------------

table1_variable_spec <- tribble(
  ~variable, ~source_view, ~kind, ~display_name,
  "recruitment_age", "enviro", "continuous", "Recruitment age",
  "sex", "enviro", "categorical", "Sex",
  "recruitment_centre", "enviro", "categorical", "Recruitment centre",
  "ethnicity_group", "enviro", "categorical", "Ethnicity",
  "smoking_status", "enviro", "categorical", "Smoking status",
  "alcohol_status", "enviro", "categorical", "Alcohol status",
  "sleep_difficulty", "enviro", "categorical", "Sleep difficulty",
  "diet_change_5yrs", "enviro", "categorical",
    "Major dietary changes in the last 5 years",
  "adopted", "enviro", "categorical", "Adopted",
  "breastfed", "enviro", "categorical", "Breastfed as a baby",
  "waist_circumference", "enviro", "continuous",
    "Waist circumference (cm)",
  "systolic_bp1", "enviro", "continuous",
    "Systolic blood pressure (mmHg)",
  "townsend_deprivation_index", "enviro", "continuous",
    "Townsend deprivation index",
  "MET_mins_moderate_activity", "enviro", "continuous",
    "MET minutes per week for moderate activity",
  "cholesterol", "blood", "continuous", "Cholesterol",
  "triglycerides", "blood", "continuous", "Triglycerides",
  "creatinine", "blood", "continuous", "Creatinine"
) %>%
  mutate(
    variable_order =
      row_number(),
    .before = 1
  )

table1_preferred_levels <- list(
  sex = c(
    "Female",
    "Male"
  ),
  recruitment_centre = c(
    "England",
    "Scotland",
    "Wales"
  ),
  ethnicity_group = c(
    "White",
    "Chinese",
    "South Asian",
    "Black",
    "Mixed",
    "Other"
  ),
  smoking_status = c(
    "Never",
    "Previous",
    "Current"
  ),
  alcohol_status = c(
    "Never",
    "Previous",
    "Current"
  ),
  sleep_difficulty = c(
    "Never/rarely",
    "Sometimes",
    "Usually"
  ),
  diet_change_5yrs = c(
    "No",
    "Yes, because of illness",
    "Yes, because of other reasons"
  ),
  adopted = c(
    "No",
    "Yes"
  ),
  breastfed = c(
    "No",
    "Yes"
  )
)

table1_env_file <-
  file.path(
    raw_source_dir,
    "final_environment.csv"
  )

table1_blood_file <-
  file.path(
    raw_source_dir,
    "final_biomarkers.csv"
  )

if (!file.exists(table1_env_file)) {
  stop(
    "Table 1 缺少 environment source：",
    table1_env_file
  )
}

if (!file.exists(table1_blood_file)) {
  stop(
    "Table 1 缺少 biomarker source：",
    table1_blood_file
  )
}

table1_env_vars <-
  table1_variable_spec %>%
  filter(
    source_view ==
      "enviro"
  ) %>%
  pull(
    variable
  )

table1_blood_vars <-
  table1_variable_spec %>%
  filter(
    source_view ==
      "blood"
  ) %>%
  pull(
    variable
  )

env_header <-
  names(
    fread(
      table1_env_file,
      nrows = 0,
      showProgress = FALSE
    )
  )

blood_header <-
  names(
    fread(
      table1_blood_file,
      nrows = 0,
      showProgress = FALSE
    )
  )

missing_table1_env <-
  setdiff(
    c(
      "eid",
      table1_env_vars
    ),
    env_header
  )

missing_table1_blood <-
  setdiff(
    c(
      "eid",
      table1_blood_vars
    ),
    blood_header
  )

if (length(missing_table1_env)) {
  stop(
    "Table 1 environment source 缺少：",
    paste(
      missing_table1_env,
      collapse = "; "
    )
  )
}

if (length(missing_table1_blood)) {
  stop(
    "Table 1 biomarker source 缺少：",
    paste(
      missing_table1_blood,
      collapse = "; "
    )
  )
}

table1_env_raw <-
  fread(
    table1_env_file,
    select = unique(
      c(
        "eid",
        table1_env_vars
      )
    ),
    showProgress = FALSE
  ) %>%
  as_tibble() %>%
  mutate(
    eid =
      as.character(
        eid
      )
  ) %>%
  distinct(
    eid,
    .keep_all = TRUE
  )

table1_blood_raw <-
  fread(
    table1_blood_file,
    select = unique(
      c(
        "eid",
        table1_blood_vars
      )
    ),
    showProgress = FALSE
  ) %>%
  as_tibble() %>%
  mutate(
    eid =
      as.character(
        eid
      )
  ) %>%
  distinct(
    eid,
    .keep_all = TRUE
  )

# 以 MAIN cohort eid 为唯一母集，保证分母不因某个 view 完全缺失而缩小。
table1_raw <-
  multimorbidity_status %>%
  select(
    eid,
    incident_multimorbidity,
    multimorbidity_status
  ) %>%
  left_join(
    table1_env_raw,
    by = "eid"
  ) %>%
  left_join(
    table1_blood_raw,
    by = "eid"
  ) %>%
  mutate(
    sex_raw =
      str_to_lower(
        str_trim(
          as.character(
            sex
          )
        )
      ),
    sex =
      case_when(
        sex_raw %in%
          c(
            "female",
            "f",
            "0",
            "0.0",
            "woman",
            "women"
          ) ~
          "Female",
        sex_raw %in%
          c(
            "male",
            "m",
            "1",
            "1.0",
            "man",
            "men"
          ) ~
          "Male",
        TRUE ~
          NA_character_
      )
  )

stopifnot(
  nrow(
    table1_raw
  ) ==
    main_cohort_n
)

table1_sex_qa <-
  table1_raw %>%
  count(
    sex_raw,
    sex,
    name = "N"
  ) %>%
  arrange(
    desc(
      N
    )
  )

write_log(
  table1_sex_qa,
  "Table1_sex_mapping_QA.csv"
)

build_core_characteristics_table <- function(
    dat,
    groups,
    include_missing_rows = FALSE
) {

  map_dfr(
    seq_len(
      nrow(
        table1_variable_spec
      )
    ),
    function(i) {

      spec <-
        table1_variable_spec[i, ]

      v <-
        spec$variable[[1]]

      preferred <-
        table1_preferred_levels[[v]]

      if (is.null(preferred)) {
        preferred <- NULL
      }

      rows <-
        summarise_variable_rows(
          x = dat[[v]],
          groups = groups,
          kind = spec$kind[[1]],
          include_missing_rows = include_missing_rows,
          preferred_levels = preferred
        )

      bind_cols(
        spec[
          rep(
            1,
            nrow(
              rows
            )
          ),
          ,
          drop = FALSE
        ],
        rows
      )
    }
  ) %>%
  arrange(
    variable_order,
    category_order
  )
}

table1_groups <- list(
  `No incident multimorbidity` =
    which(
      table1_raw$incident_multimorbidity ==
        0L
    ),
  `Incident multimorbidity` =
    which(
      table1_raw$incident_multimorbidity ==
        1L
    ),
  Overall =
    seq_len(
      nrow(
        table1_raw
      )
    )
)

table1_n_no <-
  length(
    table1_groups[["No incident multimorbidity"]]
  )

table1_n_event <-
  length(
    table1_groups[["Incident multimorbidity"]]
  )

table1_n_total <-
  length(
    table1_groups[["Overall"]]
  )

stopifnot(
  table1_n_no + table1_n_event == table1_n_total
)

cat(
  "\nTable 1 group sizes:\n",
  "No incident multimorbidity = ", table1_n_no, "\n",
  "Incident multimorbidity = ", table1_n_event, "\n",
  "Overall = ", table1_n_total, "\n",
  sep = ""
)
table1_main <-
  build_core_characteristics_table(
    dat = table1_raw,
    groups = table1_groups,
    include_missing_rows = FALSE
  ) %>%
  select(
    variable_order,
    variable,
    Characteristic = display_name,
    Category = category,
    statistic,
    `No incident multimorbidity`,
    `Incident multimorbidity`,
    Overall
  )

write_csv(
  table1_main,
  file.path(
    intermediate_dir,
    "Table_1_baseline_characteristics_source.csv"
  )
)

table1_main_display <-
  table1_main %>%
  select(
    Characteristic,
    Category,
    `No incident multimorbidity`,
    `Incident multimorbidity`,
    Overall
  )

names(table1_main_display)[
  names(table1_main_display) ==
    "Incident multimorbidity"
] <-
  paste0(
    "Incident multimorbidity (N=",
    format(
      table1_n_event,
      big.mark = ",",
      scientific = FALSE
    ),
    ")"
  )


names(table1_main_display)[
  names(table1_main_display) ==
    "Overall"
] <-
  paste0(
    "Overall (N=",
    format(
      table1_n_total,
      big.mark = ",",
      scientific = FALSE
    ),
    ")"
  )

table1_main_csv <-
  file.path(
    main_table_dir,
    "Table_1_baseline_characteristics.csv"
  )

write_csv(
  table1_main_display,
  table1_main_csv
)

table1_main_xlsx <-
  file.path(
    main_table_dir,
    "Table_1_baseline_characteristics.xlsx"
  )

wb_table1 <-
  createWorkbook()

addWorksheet(
  wb_table1,
  "Table 1",
  gridLines = FALSE
)

writeData(
  wb_table1,
  "Table 1",
  paste0(
    "Table 1. Baseline characteristics of the main analytical cohort ",
    "by incident multimorbidity status"
  ),
  startRow = 1,
  startCol = 1
)

writeData(
  wb_table1,
  "Table 1",
  table1_main_display,
  startRow = 3,
  withFilter = FALSE
)

table1_footnote_row <-
  4 +
  nrow(
    table1_main_display
  )

writeData(
  wb_table1,
  "Table 1",
  paste0(
    "Footnotes: Continuous variables are presented as mean (standard deviation), ",
    "and categorical variables as number (percentage). Missing values and values ",
    "coded as not applicable / missing by design are not presented as separate ",
    "categories in the main-text Table 1. Percentages for categorical variables ",
    "use the corresponding non-missing denominator."
  ),
  startRow = table1_footnote_row,
  startCol = 1
)

setColWidths(
  wb_table1,
  "Table 1",
  cols =
    seq_len(
      ncol(
        table1_main_display
      )
    ),
  widths = "auto"
)

freezePane(
  wb_table1,
  "Table 1",
  firstActiveRow = 4
)

saveWorkbook(
  wb_table1,
  table1_main_xlsx,
  overwrite = TRUE
)

table1_source_qa <- tibble(
  check = c(
    "all 17 requested Table 1 variables present",
    "Table 1 contains no Missing rows",
    "Table 1 contains no structural-missing rows",
    "group Ns sum to Overall N",
    "Overall N equals MAIN cohort N",
    "environment source selected",
    "biomarker source selected"
  ),
  observed = c(
    as.character(
      n_distinct(
        table1_main$variable
      )
    ),
    as.character(
      any(
        table1_main$Category ==
          "Missing",
        na.rm = TRUE
      )
    ),
    as.character(
      any(
        table1_main$Category ==
          "Not applicable / missing by design (-1)",
        na.rm = TRUE
      )
    ),
    as.character(
      table1_n_no +
        table1_n_event ==
        table1_n_total
    ),
    as.character(
      table1_n_total ==
        main_cohort_n
    ),
    basename(
      table1_env_file
    ),
    basename(
      table1_blood_file
    )
  ),
  expected = c(
    "17",
    "FALSE",
    "FALSE",
    "TRUE",
    "TRUE",
    "final_environment.csv",
    "final_biomarkers.csv"
  )
) %>%
  mutate(
    passed =
      observed ==
      expected
  )

write_log(
  table1_source_qa,
  "Table1_source_QA.csv"
)

if (
  !all(
    table1_source_qa$passed
  )
) {
  stop(
    "Table 1 QA failed; inspect Table1_source_QA.csv"
  )
}

cat(
  "完成：Section 12 Main Table 1（incident multimorbidity status + Overall）\n"
)

update_progress_checkpoint("07 Section 12 completed", "从 07 Section 13 继续。")

###############################################################################
### Section 13. Supplementary Tables S1-S3 ####
###############################################################################
# Final contract:
#   Table S1. Definitions + current baseline/incident counts for 36 diseases
#   Table S2. Chronic disease burden by end of valid follow-up
#   Table S3. Eight multimorbidity patterns
#
# Table S4-S9 are maintained separately and are not generated or overwritten here.
#
# Important:
#   1) Table S1 no longer reads 20260928 Supplementary materials.xlsx at runtime.
#   2) The static Field ID -> ICD / diagnosis / source dictionary below was extracted
#      once from the user-provided 20260928 Table S1 and is now embedded in this script.
#   3) Disease membership and Field IDs remain determined by the CURRENT Step 03 logic.
#   4) Baseline/incident counts are recalculated from the current MAIN cohort:
#      source/UKB_diseases_data_with_patterns.csv, N is derived at runtime.
#   5) The MAIN disease file is reused from the single Section 0 cache.
###############################################################################

# -----------------------------------------------------------------------------
# 13.1 Current Step 03 disease -> Field ID definitions
# -----------------------------------------------------------------------------

current_disease_fields <- list(
  
  Anaemia = c(
    "130622", "130624", "130626", "130628", "130630",
    "130632", "130634", "130636", "130638", "130640",
    "130642", "130644", "130646", "130648", "130650",
    "130652", "130654", "130656", "130658"
  ),
  
  Angina = c(
    "131296"
  ),
  
  Anxiety_or_panic_attacks = c(
    "130904", "130906"
  ),
  
  Atrial_fibrillation = c(
    "131350"
  ),
  
  Asthma = c(
    "131494", "131496"
  ),
  
  Bronchiectasis = c(
    "131498"
  ),
  
  Chronic_kidney_disease = c(
    "131290", "131292",
    "132004", "132006", "132008", "132010",
    "132014", "132018", "132020", "132022",
    "132024", "132026", "132028", "132032",
    "132034", "132036", "132038"
  ),
  
  Chronic_obstructive_pulmonary_disease = c(
    "131484", "131486", "131488", "131490", "131492"
  ),
  
  Chronic_sinusitis = c(
    "131468"
  ),
  
  Cirrhosis = c(
    "131666"
  ),
  
  Dementia = c(
    "130836", "130838", "130840",
    "130842", "130846",
    "131036", "131038"
  ),
  
  Depression = c(
    "130894", "130896"
  ),
  
  Diabetes = c(
    "130706", "130708", "130710",
    "130712", "130714", "132202"
  ),
  
  Eczema_or_dermatitis = c(
    "131720", "131722", "131724", "131726",
    "131728", "131730", "131732", "131734",
    "131736", "131738", "131740"
  ),
  
  Epilepsy = c(
    "131048"
  ),
  
  Glaucoma = c(
    "131186", "131188"
  ),
  
  Heart_failure = c(
    "131354"
  ),
  
  Hepatitis = c(
    "130194", "130196", "130198", "130200", "130202",
    "131658", "131660", "131662", "131664",
    "131666", "131668", "131670", "131672"
  ),
  
  Hypertension = c(
    "131286", "131294"
  ),
  
  Inflammatory_bowel_disease = c(
    "131626", "131628", "131630"
  ),
  
  Irritable_bowel_syndrome = c(
    "131638"
  ),
  
  Meningitis = c(
    "130024", "130052", "130066", "130148",
    "130992", "130994", "130996", "130998",
    "131000", "131002"
  ),
  
  Migraine = c(
    "131052"
  ),
  
  Multiple_sclerosis = c(
    "131042"
  ),
  
  Myocardial_infarction = c(
    "131298", "131300", "131302",
    "131304", "131306"
  ),
  
  Osteoporosis = c(
    "131962", "131964", "131966",
    "131968", "131970", "131972"
  ),
  
  Parkinson_disease = c(
    "131022"
  ),
  
  Peripheral_vascular_disease = c(
    "131380", "131382", "131384", "131386",
    "131388", "131390", "131392", "131394",
    "131396", "131398", "131400", "131402",
    "131404", "131406", "131408", "131410"
  ),
  
  Prostate_problem = c(
    "132072", "132074", "132076"
  ),
  
  Rheumatoid_arthritis = c(
    "131848", "131850"
  ),
  
  Schizophrenia = c(
    "130874", "130876", "130878", "130880",
    "130882", "130884", "130886", "130888"
  ),
  
  Stroke = c(
    "131360", "131362", "131364",
    "131366", "131368"
  ),
  
  Thyroid_problem = c(
    "130692", "130694", "130696",
    "130698", "130700", "130702",
    "130704", "130744", "130746",
    "130748"
  ),
  
  Tuberculosis = c(
    "130020", "130022", "130024",
    "130026", "130028"
  ),
  
  Vestibular_disorder = c(
    "131252"
  )
)

stopifnot(
  length(current_disease_fields) == 35L,
  identical(
    names(current_disease_fields),
    disease_names_36[
      disease_names_36 != "overall_cancer"
    ]
  )
)

current_field_registry <- tibble(
  disease =
    names(
      current_disease_fields
    ),
  field_id =
    unname(
      current_disease_fields
    )
) %>%
  tidyr::unnest_longer(
    field_id
  ) %>%
  group_by(
    disease
  ) %>%
  mutate(
    field_order =
      row_number()
  ) %>%
  ungroup() %>%
  mutate(
    field_id =
      as.character(
        field_id
      )
  )

# -----------------------------------------------------------------------------
# 13.2 Embedded static Field ID metadata for Table S1
# -----------------------------------------------------------------------------
# This is metadata only. No historical baseline/incident counts are embedded.
# -----------------------------------------------------------------------------

table_s1_field_dictionary <- tribble(
  ~field_id, ~icd_code, ~diagnosis_definition, ~ascertainment_source,
  "130622", "D50", "Nutritional anaemias", "First occurrence data",
  "130624", "D51", "", "First occurrence data",
  "130626", "D52", "", "First occurrence data",
  "130628", "D53", "", "First occurrence data",
  "130630", "D55", "Haemolytic anaemias", "First occurrence data",
  "130632", "D56", "", "First occurrence data",
  "130634", "D57", "", "First occurrence data",
  "130636", "D58", "", "First occurrence data",
  "130638", "D59", "", "First occurrence data",
  "130640", "D60", "Aplastic and other anaemias", "First occurrence data",
  "130642", "D61", "", "First occurrence data",
  "130644", "D62", "", "First occurrence data",
  "130646", "D63", "", "First occurrence data",
  "130648", "D64", "", "First occurrence data",
  "130650", "D65", "Coagulation defects, purpura and other haemorrhagic conditions", "First occurrence data",
  "130652", "D66", "", "First occurrence data",
  "130654", "D67", "", "First occurrence data",
  "130656", "D68", "", "First occurrence data",
  "130658", "D69", "", "First occurrence data",
  "131296", "I20", "angina pectoris", "First occurrence data",
  "130904", "F40", "Phobic anxiety disorders", "First occurrence data",
  "130906", "F41", "Other anxiety disorders", "First occurrence data",
  "131350", "I48", "atrial fibrillation and flutter", "First occurrence data",
  "131494", "J45", "Asthma", "First occurrence data",
  "131496", "J46", "Status asthmaticus", "First occurrence data",
  "131498", "J47", "Bronchiectasis", "First occurrence data",
  "131290", "I12", "Hypertensive renal disease with renal failure", "First occurrence data",
  "131292", "I13", "hypertensive heart and renal disease", "First occurrence data",
  "132004", "N03", "Chronic nephritic syndrome", "First occurrence data",
  "132006", "N04", "Nephrotic syndrome", "First occurrence data",
  "132008", "N05", "Unspecified nephritic syndrome", "First occurrence data",
  "132010", "N06", "Isolated proteinuria with specified morphological lesion", "First occurrence data",
  "132014", "N08", "Glomerular disorders in diseases classified elsewhere", "First occurrence data",
  "132018", "N11", "Chronic tubulo-interstitial nephritis", "First occurrence data",
  "132020", "N12", "Tubulo-interstitial nephritis, not specified as acute or chronic", "First occurrence data",
  "132022", "N13", "Obstructive and reflux uropathy", "First occurrence data",
  "132024", "N14", "Drug- and heavy-metal-induced tubulo-interstitial and tubular conditions", "First occurrence data",
  "132026", "N15", "Other renal tubulo-interstitial diseases", "First occurrence data",
  "132028", "N16", "Renal tubulo-interstitial disorders in diseases classified elsewhere", "First occurrence data",
  "132032", "N18", "Chronic renal failure", "First occurrence data",
  "132034", "N19", "Unspecified renal failure", "First occurrence data",
  "132036", "N20", "Calculus of kidney and ureter", "First occurrence data",
  "132038", "N21", "Calculus of lower urinary tract", "First occurrence data",
  "131484", "J40", "Bronchitis, not specified as acute or chronic", "First occurrence data",
  "131486", "J41", "Simple and mucopurulent chronic bronchitis", "First occurrence data",
  "131488", "J42", "Unspecified chronic bronchitis", "First occurrence data",
  "131490", "J43", "Emphysema", "First occurrence data",
  "131492", "J44", "Other chronic obstructive pulmonary disease", "First occurrence data",
  "131468", "J32", "Chronic sinusitis", "First occurrence data",
  "131666", "K74", "Fibrosis and cirrhosis of liver", "First occurrence data",
  "130836", "F00", "Dementia in Alzheimer's disease", "First occurrence data",
  "130838", "F01", "Vascular dementia", "First occurrence data",
  "130840", "F02", "Dementia in other diseases classified elsewhere", "First occurrence data",
  "130842", "F03", "Unspecified dementia", "First occurrence data",
  "130846", "F05", "Delirium superimposed on dementia", "First occurrence data",
  "131036", "G30", "Alzheimer's disease", "First occurrence data",
  "131038", "G31", "Other specified degenerative diseases of nervous system", "First occurrence data",
  "130894", "F32", "Depressive episode", "First occurrence data",
  "130896", "F33", "Recurrent depressive disorder", "First occurrence data",
  "130706", "E10", "insulin-dependent diabetes mellitus", "First occurrence data",
  "130708", "E11", "non-insulin-dependent diabetes mellitus", "First occurrence data",
  "130710", "E12", "malnutrition-related diabetes mellitus", "First occurrence data",
  "130712", "E13", "other specified diabetes mellitus", "First occurrence data",
  "130714", "E14", "unspecified diabetes mellitus", "First occurrence data",
  "132202", "O24", "diabetes mellitus in pregnancy", "First occurrence data",
  "131720", "L20", "Atopic dermatitis", "First occurrence data",
  "131722", "L21", "Seborrhoeic dermatitis", "First occurrence data",
  "131724", "L22", "Diaper [napkin] dermatitis", "First occurrence data",
  "131726", "L23", "Allergic contact dermatitis", "First occurrence data",
  "131728", "L24", "Irritant contact dermatitis", "First occurrence data",
  "131730", "L25", "Unspecified contact dermatitis", "First occurrence data",
  "131732", "L26", "Exfoliative dermatitis", "First occurrence data",
  "131734", "L27", "Dermatitis due to substances taken internally", "First occurrence data",
  "131736", "L28", "Lichen simplex chronicus and prurigo", "First occurrence data",
  "131738", "L29", "Pruritus", "First occurrence data",
  "131740", "L30", "Other dermatitis", "First occurrence data",
  "131048", "G40", "epilepsy", "First occurrence data",
  "131186", "H40", "Glaucoma", "First occurrence data",
  "131188", "H42", "Glaucoma in diseases classified elsewhere", "First occurrence data",
  "131354", "I50", "Heart failure", "First occurrence data",
  "130194", "B15", "Acute hepatitis A", "First occurrence data",
  "130196", "B16", "Acute hepatitis B", "First occurrence data",
  "130198", "B17", "Other acute viral hepatitis", "First occurrence data",
  "130200", "B18", "Chronic viral hepatitis", "First occurrence data",
  "130202", "B19", "Unspecified viral hepatitis", "First occurrence data",
  "131658", "K70", "Alcoholic liver disease", "First occurrence data",
  "131660", "K71", "Toxic liver disease", "First occurrence data",
  "131662", "K72", "Hepatic failure, not elsewhere classified", "First occurrence data",
  "131664", "K73", "Chronic hepatitis, not elsewhere classified", "First occurrence data",
  "131668", "K75", "Other inflammatory liver diseases", "First occurrence data",
  "131670", "K76", "Other diseases of liver", "First occurrence data",
  "131672", "K77", "Liver disorders in diseases classified elsewhere", "First occurrence data",
  "131286", "I10", "Essential (primary) hypertension1", "First occurrence data",
  "131294", "I15", "Secondary hypertension", "First occurrence data",
  "131626", "K50", "Crohn's disease [regional enteritis]", "First occurrence data",
  "131628", "K51", "Ulcerative colitis", "First occurrence data",
  "131630", "K52", "Other non-infective gastro-enteritis and colitis", "First occurrence data",
  "131638", "K58", "Irritable bowel syndrome", "First occurrence data",
  "130024", "A17", "Tuberculous meningitis; tuberculosis of nervous system", "First occurrence data",
  "130052", "A32", "Listerial meningitis and meningoencephalitis", "First occurrence data",
  "130066", "A39", "Meningococcal infection", "First occurrence data",
  "130148", "A87", "Viral meningitis", "First occurrence data",
  "130992", "G00", "Bacterial meningitis, not elsewhere classified", "First occurrence data",
  "130994", "G01", "Meningitis in bacterial diseases classified elsewhere", "First occurrence data",
  "130996", "G02", "Meningitis in other infectious and parasitic diseases classified elsewhere", "First occurrence data",
  "130998", "G03", "Meningitis due to other and unspecified causes", "First occurrence data",
  "131000", "G04", "Encephalitis, myelitis and encephalomyelitis", "First occurrence data",
  "131002", "G05", "Encephalitis, myelitis and encephalomyelitis in diseases classified elsewhere", "First occurrence data",
  "131052", "G43", "Migraine", "First occurrence data",
  "131042", "G35", "Multiple sclerosis", "First occurrence data",
  "131298", "I21", "Acute myocardial infarction", "First occurrence data",
  "131300", "I22", "Subsequent myocardial infarction", "First occurrence data",
  "131302", "I23", "Certain current complications following acute myocardial infarction", "First occurrence data",
  "131304", "I24", "Other acute ischaemic heart diseases", "First occurrence data",
  "131306", "I25", "Chronic ischaemic heart disease", "First occurrence data",
  "131962", "M80", "osteoporosis with pathological fracture", "First occurrence data",
  "131964", "M81", "osteoporosis without pathological fracture", "First occurrence data",
  "131966", "M82", "osteoporosis in diseases classified elsewhere", "First occurrence data",
  "131968", "M83", "adult osteomalacia", "First occurrence data",
  "131970", "M84", "disorders of continuity of bone", "First occurrence data",
  "131972", "M85", "other disorders of bone density and structure", "First occurrence data",
  "131022", "G20", "Parkinson’s disease", "First occurrence data",
  "131380", "I70", "Atherosclerosis", "First occurrence data",
  "131382", "I71", "Aortic aneurysm and dissection", "First occurrence data",
  "131384", "I72", "Other aneurysm", "First occurrence data",
  "131386", "I73", "Other peripheral vascular diseases", "First occurrence data",
  "131388", "I74", "Arterial embolism and thrombosis", "First occurrence data",
  "131390", "I77", "Other disorders of arteries and arterioles", "First occurrence data",
  "131392", "I78", "Diseases of capillaries", "First occurrence data",
  "131394", "I79", "Disorders of arteries, arterioles and capillaries in diseases classified elsewhere", "First occurrence data",
  "131396", "I80", "Phlebitis and thrombophlebitis", "First occurrence data",
  "131398", "I81", "Portal vein thrombosis", "First occurrence data",
  "131400", "I82", "Other venous embolism and thrombosis", "First occurrence data",
  "131402", "I83", "Varicose veins of lower extremities", "First occurrence data",
  "131404", "I84", "Haemorrhoids", "First occurrence data",
  "131406", "I85", "Oesophageal varices", "First occurrence data",
  "131408", "I86", "Varicose veins of other sites", "First occurrence data",
  "131410", "I87", "Other disorders of veins", "First occurrence data",
  "132072", "N40", "Hyperplasia of prostate", "First occurrence data",
  "132074", "N41", "Inflammatory diseases of prostate", "First occurrence data",
  "132076", "N42", "Other disorders of prostate", "First occurrence data",
  "131848", "M05", "seropositive rheumatoid arthritis", "First occurrence data",
  "131850", "M06", "other rheumatoid arthritis", "First occurrence data",
  "130874", "F20", "Schizophrenia, schizotypal and delusional disorders", "First occurrence data",
  "130876", "F21", "schizotypal disorder", "First occurrence data",
  "130878", "F22", "persistent delusional disorders", "First occurrence data",
  "130880", "F23", "acute and transient psychotic disorders", "First occurrence data",
  "130882", "F24", "induced delusional disorder", "First occurrence data",
  "130884", "F25", "schizoaffective disorders", "First occurrence data",
  "130886", "F28", "other nonorganic psychotic disorders", "First occurrence data",
  "130888", "F29", "unspecified nonorganic psychosis", "First occurrence data",
  "131360", "I60", "Subarachnoid haemorrhage", "First occurrence data",
  "131362", "I61", "Intracerebral haemorrhage", "First occurrence data",
  "131364", "I62", "Other nontraumatic intracranial haemorrhage", "First occurrence data",
  "131366", "I63", "Cerebral infarction", "First occurrence data",
  "131368", "I64", "Stroke, not specified as haemorrhage or infarction", "First occurrence data",
  "130692", "E01", "iodine-deficiency-related thyroid disorders and allied conditions", "First occurrence data",
  "130694", "E02", "subclinical iodine-deficiency hypothyroidism", "First occurrence data",
  "130696", "E03", "other hypothyroidism", "First occurrence data",
  "130698", "E04", "other non-toxic goitre", "First occurrence data",
  "130700", "E05", "thyrotoxicosis [hyperthyroidism", "First occurrence data",
  "130702", "E06", "thyroiditis", "First occurrence data",
  "130704", "E07", "other disorders of thyroid", "First occurrence data",
  "130744", "E032", "diseases of thymus", "First occurrence data",
  "130746", "E034", "other endocrine disorders", "First occurrence data",
  "130748", "E035", "disorders of endocrine glands in diseases classified elsewhere", "First occurrence data",
  "130020", "A15", "Tuberculosis", "First occurrence data",
  "130022", "A16", "respiratory tuberculosis, not confirmed bacteriologically or histologically", "First occurrence data",
  "130026", "A18", "tuberculosis of other organs", "First occurrence data",
  "130028", "A19", "miliary tuberculosis", "First occurrence data",
  "131252", "H81", "disorders of vestibular function", "First occurrence data",
  "40005", NA_character_, "Date of cancer diagnosis", "Cancer registry",
  "40006", "C00-C97", "Malignant neoplasms, cancer registry ICD-10", "Cancer registry",
  "40013", "140-208", "Malignant neoplasms, cancer registry ICD-9", "Cancer registry",
  "41270", "C00-C97", "Malignant neoplasms, hospital inpatient ICD-10", "Hospital inpatient records",
  "41280", NA_character_, "Date of first inpatient diagnosis", "Hospital inpatient records",
  "40000", NA_character_, "Date of death", "Death register",
  "40001", "C00-C97", "Malignant neoplasms, primary cause of death", "Death register",
  "40002", "C00-C97", "Malignant neoplasms, contributory cause of death", "Death register"
) %>%
  mutate(
    field_id =
      as.character(
        field_id
      ),
    icd_code =
      na_if(
        str_squish(
          as.character(
            icd_code
          )
        ),
        ""
      ),
    diagnosis_definition =
      na_if(
        str_squish(
          as.character(
            diagnosis_definition
          )
        ),
        ""
      ),
    ascertainment_source =
      na_if(
        str_squish(
          as.character(
            ascertainment_source
          )
        ),
        ""
      )
  ) %>%
  distinct(
    field_id,
    .keep_all = TRUE
  )

stopifnot(
  nrow(
    table_s1_field_dictionary
  ) == 179L,
  !anyDuplicated(
    table_s1_field_dictionary$field_id
  )
)

write_csv(
  table_s1_field_dictionary,
  file.path(
    intermediate_dir,
    "Table_S1_field_dictionary_static.csv"
  )
)

# Current 35 non-cancer fields must all map to the embedded metadata dictionary.
current_field_icd <- current_field_registry %>%
  left_join(
    table_s1_field_dictionary,
    by = "field_id"
  ) %>%
  mutate(
    disease_order =
      match(
        disease,
        disease_names_36
      )
  ) %>%
  arrange(
    disease_order,
    field_order
  )

missing_current_mapping <- current_field_icd %>%
  filter(
    is.na(icd_code) |
      !nzchar(
        str_trim(
          icd_code
        )
      ) |
      is.na(ascertainment_source) |
      !nzchar(
        str_trim(
          ascertainment_source
        )
      )
  )

if (nrow(missing_current_mapping) > 0L) {
  print(
    missing_current_mapping,
    n = Inf,
    width = Inf
  )
  stop(
    "当前 Step 03 non-cancer Field ID 无法在内嵌 Table S1 metadata 完整匹配：",
    paste(
      unique(
        missing_current_mapping$field_id
      ),
      collapse = ", "
    )
  )
}

# Overall cancer uses the current Step 03 cancer sources.
overall_cancer_fields <- c(
  "40005",
  "40006",
  "40013",
  "41270",
  "41280",
  "40000",
  "40001",
  "40002"
)

overall_cancer_definition <- table_s1_field_dictionary %>%
  filter(
    field_id %in%
      overall_cancer_fields
  ) %>%
  mutate(
    disease =
      "overall_cancer",
    disease_order =
      match(
        "overall_cancer",
        disease_names_36
      ),
    field_order =
      match(
        field_id,
        overall_cancer_fields
      )
  ) %>%
  arrange(
    field_order
  ) %>%
  select(
    disease,
    icd_code,
    diagnosis_definition,
    field_id,
    ascertainment_source,
    disease_order,
    field_order
  )

if (nrow(overall_cancer_definition) != 8L) {
  stop(
    "Overall cancer Table S1 metadata should contain 8 source fields."
  )
}

non_cancer_definition <- current_field_icd %>%
  select(
    disease,
    icd_code,
    diagnosis_definition,
    field_id,
    ascertainment_source,
    disease_order,
    field_order
  ) %>%
  distinct()

current_disease_definition <- bind_rows(
  non_cancer_definition,
  overall_cancer_definition
) %>%
  arrange(
    disease_order,
    field_order
  )

stopifnot(
  n_distinct(
    current_disease_definition$disease
  ) == 36L,
  setequal(
    current_disease_definition$disease,
    disease_names_36
  )
)

# Exact display labels retained from the finalized Table S1 structure.
disease_display_lookup <- tibble(
  disease = disease_names_36,
  display_name = c(
  "Anaemia",
  "Angina",
  "Anxiety or panic attacks",
  "Atrial fibrillation",
  "Asthma",
  "Bronchiectasis",
  "Chronic kidney disease",
  "Chronic obstructive pulmonary disease",
  "Chronic sinusitis",
  "Cirrhosis",
  "Dementia",
  "Depression",
  "Diabetes",
  "Eczema or dermatitis",
  "Epilepsy",
  "Glaucoma",
  "Heart failure",
  "Hepatitis",
  "Hypertension",
  "Inflammatory bowel disease",
  "Irritable bowel syndrome",
  "Meningitis",
  "Migraine",
  "Multiple sclerosis",
  "Myocardial infarction",
  "Osteoporosis",
  "Parkinson’s disease",
  "Peripheral vascular disease",
  "Prostate problem",
  "Rheumatoid arthritis",
  "Schizophrenia",
  "Stroke",
  "Thyroid problem",
  "Tuberculosis",
  "Vestibular disorder",
  "Overall cancer"
  )
)


# -----------------------------------------------------------------------------
# 13.3 Reuse MAIN disease data and recalculate baseline / incident counts
# -----------------------------------------------------------------------------

disease_main_supp <- get_main_disease_data() %>%
  mutate(
    recruitment_date =
      as.Date(
        recruitment_date
      ),
    censor_date =
      as.Date(
        censor_date
      )
  )

for (v in disease_time_cols_36) {
  disease_main_supp[[v]] <-
    as.Date(
      disease_main_supp[[v]]
    )
}

if (
  any(
    !as.logical(
      disease_main_supp$valid_followup
    ),
    na.rm = TRUE
  )
) {
  stop(
    "MAIN cohort 中仍存在 valid_followup == FALSE。"
  )
}

if (
  any(
    disease_main_supp$censor_date <=
      disease_main_supp$recruitment_date,
    na.rm = TRUE
  )
) {
  stop(
    "MAIN cohort 中存在 censor_date <= recruitment_date。"
  )
}

stopifnot(
  !anyDuplicated(
    disease_main_supp$eid
  )
)

disease_epidemiology_list <-
  vector(
    "list",
    length(
      disease_names_36
    )
  )

for (j in seq_along(disease_names_36)) {

  disease_j <-
    disease_names_36[[j]]

  time_j <-
    disease_time_cols_36[[j]]

  disease_status <-
    as.integer(
      disease_main_supp[[disease_j]]
    )

  onset_date <-
    disease_main_supp[[time_j]]

  unknown_timing_n <-
    sum(
      disease_status == 1L &
        is.na(
          onset_date
        ),
      na.rm = TRUE
    )

  if (unknown_timing_n > 0L) {
    stop(
      disease_j,
      ": MAIN cohort 中仍有 ",
      unknown_timing_n,
      " 个 disease=1 但 onset time 缺失。"
    )
  }

  baseline_flag <-
    disease_status == 1L &
      !is.na(
        onset_date
      ) &
      onset_date <=
        disease_main_supp$recruitment_date

  baseline_flag[
    is.na(
      baseline_flag
    )
  ] <- FALSE

  incident_flag <-
    !baseline_flag &
      disease_status == 1L &
      !is.na(
        onset_date
      ) &
      onset_date >
        disease_main_supp$recruitment_date &
      onset_date <=
        disease_main_supp$censor_date

  incident_flag[
    is.na(
      incident_flag
    )
  ] <- FALSE

  disease_epidemiology_list[[j]] <-
    tibble(
      disease =
        disease_j,
      baseline_cases_n =
        sum(
          baseline_flag
        ),
      incident_cases_n =
        sum(
          incident_flag
        )
    )
}

disease_epidemiology_36 <-
  bind_rows(
    disease_epidemiology_list
  )

stopifnot(
  nrow(
    disease_epidemiology_36
  ) == 36L,
  !anyDuplicated(
    disease_epidemiology_36$disease
  )
)

# -----------------------------------------------------------------------------
# 13.4 Table S1
# -----------------------------------------------------------------------------

table_s1 <- current_disease_definition %>%
  left_join(
    disease_epidemiology_36,
    by = "disease"
  ) %>%
  left_join(
    disease_display_lookup,
    by = "disease"
  ) %>%
  group_by(
    disease
  ) %>%
  mutate(
    disease_row =
      row_number(),
    `No.` =
      if_else(
        disease_row == 1L,
        as.character(
          disease_order
        ),
        NA_character_
      ),
    `Outcome name in this study` =
      if_else(
        disease_row == 1L,
        display_name,
        NA_character_
      ),
    `Baseline cases, n` =
      if_else(
        disease_row == 1L,
        as.character(
          baseline_cases_n
        ),
        NA_character_
      ),
    `Incident cases, n` =
      if_else(
        disease_row == 1L,
        as.character(
          incident_cases_n
        ),
        NA_character_
      )
  ) %>%
  ungroup() %>%
  arrange(
    disease_order,
    disease_row
  ) %>%
  transmute(
    `No.`,
    `Outcome name in this study`,
    `ICD code` =
      icd_code,
    `Diagnosis definition` =
      diagnosis_definition,
    `UKB Field ID` =
      field_id,
    `Ascertainment / data source` =
      ascertainment_source,
    `Baseline cases, n`,
    `Incident cases, n`
  )

write_table(
  table_s1,
  "Table_S1_36_chronic_diseases.csv"
)

write_log(
  tibble(
    check = c(
      "MAIN cohort N",
      "36 diseases",
      "all current non-cancer Field IDs mapped",
      "overall cancer source fields"
    ),
    observed = c(
      as.character(
        nrow(
          disease_main_supp
        )
      ),
      as.character(
        n_distinct(
          current_disease_definition$disease
        )
      ),
      as.character(
        nrow(
          missing_current_mapping
        )
      ),
      as.character(
        nrow(
          overall_cancer_definition
        )
      )
    ),
    expected = c(
      as.character(
        nrow(main_disease_data)
      ),
      "36",
      "0",
      "8"
    )
  ) %>%
    mutate(
      passed =
        observed ==
        expected
    ),
  "Table_S1_source_QA.csv"
)

# -----------------------------------------------------------------------------
# 13.5 Table S2
# -----------------------------------------------------------------------------

disease_burden_end_followup <-
  disease_main_supp %>%
  transmute(
    eid,
    baseline_disease_count =
      as.integer(
        baseline_disease_count
      ),
    incident_disease_count =
      as.integer(
        incident_disease_count
      ),
    disease_count_end_followup =
      baseline_disease_count +
      incident_disease_count
  )

if (
  any(
    disease_burden_end_followup$disease_count_end_followup <
      0L |
      disease_burden_end_followup$disease_count_end_followup >
      36L,
    na.rm = TRUE
  )
) {
  stop(
    "Table S2 disease_count_end_followup 超出 0-36。"
  )
}

if (
  any(
    disease_burden_end_followup$baseline_disease_count >
      1L,
    na.rm = TRUE
  )
) {
  stop(
    "Table S2 MAIN cohort 中仍存在 baseline_disease_count > 1。"
  )
}

table_s2 <-
  disease_burden_end_followup %>%
  count(
    disease_count_end_followup,
    name = "participants_n"
  ) %>%
  mutate(
    participants_percent =
      round(
        100 *
          participants_n /
          sum(
            participants_n
          ),
        2
      ),
    burden_group =
      case_when(
        disease_count_end_followup <=
          1L ~
          "0-1 disease",
        disease_count_end_followup >=
          2L ~
          "Multimorbidity (>=2 diseases)"
      )
  ) %>%
  arrange(
    disease_count_end_followup
  ) %>%
  transmute(
    `Number of chronic diseases by end of valid follow-up` =
      disease_count_end_followup,
    `Participants, n` =
      participants_n,
    `Participants, %` =
      participants_percent,
    `Burden group` =
      burden_group
  )

stopifnot(
  sum(
    table_s2[["Participants, n"]],
    na.rm = TRUE
  ) == nrow(disease_main_supp)
)

write_table(
  table_s2,
  "Table_S2_chronic_disease_burden_end_followup.csv"
)

# -----------------------------------------------------------------------------
# 13.6 Table S3
# -----------------------------------------------------------------------------
# Final display:
#   - participants with >=2 diseases in the pattern, n
#   - participants with >=2 diseases in the pattern, %
#   - participants with all diseases in the pattern, n
# -----------------------------------------------------------------------------

need_object(
  "figure2_nodes",
  file.path(
    figure_source_dir,
    "Figure2_nodes.csv"
  )
)

need_object(
  "figure2_pattern_counts",
  file.path(
    figure_source_dir,
    "Figure2_pattern_counts.csv"
  )
)

table_s3 <-
  figure2_nodes %>%
  group_by(
    pattern_id,
    pattern_label
  ) %>%
  summarise(
    `Number of diseases` =
      n(),
    `Disease outcomes included in the pattern` =
      paste(
        disease_display,
        collapse = "; "
      ),
    .groups = "drop"
  ) %>%
  left_join(
    figure2_pattern_counts %>%
      transmute(
        pattern_id,
        
        `Participants with >=2 diseases in the pattern, n` =
          participants_with_at_least_two_diseases_n,
        
        `Participants with >=2 diseases in the pattern, %` =
          round(
            participants_with_at_least_two_diseases_percent,
            2
          ),
        
        `Participants with all diseases in the pattern, n` =
          participants_with_all_diseases_n
      ),
    by = "pattern_id"
  ) %>%
  arrange(
    as.integer(
      str_remove(
        pattern_id,
        "P"
      )
    )
  ) %>%
  rename(
    Pattern =
      pattern_id,
    `Pattern label` =
      pattern_label
  )

write_table(
  table_s3,
  "Table_S3_eight_multimorbidity_patterns.csv"
)

cat(
  "完成：Section 13 Supplementary Tables S1-S3；",
  "Table S1 使用内嵌静态 Field metadata + 当前 MAIN cohort 自动更新病例数；",
  "Table S4-S9 不在本脚本生成。\n",
  sep = ""
)

update_progress_checkpoint("07 Section 13 completed", "从 07 Section 14 继续。")

###############################################################################
### Section 14. Supplementary Tables S10-S17 ####
###############################################################################
# 需要先运行 Section 12（使用描述性 helper、MAIN cohort eid 和 incident status）。
#
# 顺序：
#   S10 Environment：Overall only
#   S11 Phenotype：Overall only
#   S12 Metabolomics：Overall only
#   S13 Genetics：Overall only
#
#   S14 Environment/phenotypic：No incident multimorbidity / Incident multimorbidity
#   S15 Biomarkers：No incident multimorbidity / Incident multimorbidity
#   S16 Metabolomics：No incident multimorbidity / Incident multimorbidity
#   S17 Genetics：No incident multimorbidity / Incident multimorbidity
#
# 注意：
#   1) S10-S13 仅统计 Overall，不再分男女；
#   2) S14-S17 仅分 incident status，不按男女分层；
#   3) 连续变量 Mean (SD)；
#   4) categorical/binary 每个类别独立一行；
#   5) Missing 单独一行；-1 structural missing 单独一行；
#   6) 以 MAIN cohort eid 为母集，不因某个 view 完全缺失而缩小分母。
###############################################################################

if (
  !exists("multimorbidity_status") ||
  !exists("main_cohort_n") ||
  !exists("prepare_view_dictionary") ||
  !exists("summarise_variable_rows")
) {
  stop("Section 14 需要先运行 Section 12。")
}

descriptive_spec <- tribble(
  ~number_sex, ~number_status, ~modality, ~raw_file,
  "S10", "S14", "enviro",
    file.path(raw_source_dir, "final_environment.csv"),
  "S11", "S15", "blood",
    file.path(raw_source_dir, "final_biomarkers.csv"),
  "S12", "S16", "metabolites",
    file.path(raw_source_dir, "final_metabolomics.csv"),
  "S13", "S17", "gene",
    file.path(raw_source_dir, "final_genetics.csv")
)

if (
  any(
    !file.exists(
      descriptive_spec$raw_file
    )
  )
) {
  stop(
    "缺少 descriptive statistics 原始 view：",
    paste(
      descriptive_spec$raw_file[
        !file.exists(
          descriptive_spec$raw_file
        )
      ],
      collapse = "; "
    )
  )
}

# -----------------------------------------------------------------------------
# 14.1 Sex-stratified descriptive output disabled.
# S10-S13 retain the legacy CSV names for existing manuscript links, but now
# contain only Overall columns. No final_eid_sex.csv is required.

# -----------------------------------------------------------------------------
# 14.2 Common view loader
# -----------------------------------------------------------------------------

load_descriptive_view <- function(
    modality_i,
    raw_file_i
) {

  dict_i <-
    prepare_view_dictionary(
      modality_i,
      raw_file_i
    )

  selected_cols_i <-
    unique(
      c(
        "eid",
        na.omit(
          dict_i$source_column
        )
      )
    )

  raw_source_i <-
    fread(
      raw_file_i,
      select = selected_cols_i,
      showProgress = FALSE
    ) %>%
    as_tibble() %>%
    mutate(
      eid =
        as.character(
          eid
        )
    ) %>%
    distinct(
      eid,
      .keep_all = TRUE
    )

  # MAIN cohort 为母集：整个 view 缺失的人仍保留，变量值变为 NA。
  raw_i <-
    multimorbidity_status %>%
    select(
      eid,
      incident_multimorbidity,
      multimorbidity_status
    ) %>%
    left_join(
      raw_source_i,
      by = "eid"
    )

  if (
    nrow(
      raw_i
    ) !=
      main_cohort_n
  ) {
    stop(
      modality_i,
      " descriptive cohort N != MAIN cohort N."
    )
  }

  list(
    dictionary = dict_i,
    data = raw_i
  )
}

summarise_view_groups <- function(
    modality_i,
    raw_file_i,
    groups_fun,
    loaded_view = NULL
) {

  loaded <-
    if (is.null(loaded_view)) {
      load_descriptive_view(
        modality_i,
        raw_file_i
      )
    } else {
      loaded_view
    }

  dict_i <-
    loaded$dictionary

  raw_i <-
    loaded$data

  groups <-
    groups_fun(
      raw_i
    )

  result_i <-
    map_dfr(
      seq_len(
        nrow(
          dict_i
        )
      ),
      function(j) {

        meta <-
          dict_i[j, ]

        base_meta <-
          meta %>%
          select(
            dictionary_order,
            modality,
            modality_label,
            subdomain,
            analysis_variable,
            display_name,
            variable_type,
            original_data_column,
            parent_variable,
            one_hot_level,
            technical_level
          )

        if (
          is.na(
            meta$source_column
          ) ||
          !meta$source_column %in%
          names(
            raw_i
          )
        ) {
          
          stop(
            "Section 14 internal error: raw descriptive source missing for ",
            meta$display_name,
            " [",
            meta$source_column,
            "]"
          )
        }

        x <-
          raw_i[[
              meta$source_column
            ]]

        kind <-
          infer_descriptive_kind(
            meta$variable_type,
            x
          )

        rows <-
          summarise_variable_rows(
            x = x,
            groups = groups,
            kind = kind,
            include_missing_rows = TRUE,
            target_level = NA_character_,
            derived_from_parent = FALSE
          )

        bind_cols(
          base_meta[
            rep(
              1,
              nrow(
                rows
              )
            ),
          ],
          tibble(
            source_column =
              rep(
                meta$source_column,
                nrow(
                  rows
                )
              ),
            derived_from_parent =
              rep(
                meta$derived_from_parent %in%
                  TRUE,
                nrow(
                  rows
                )
              )
          ),
          rows,
          tibble(
            descriptive_kind =
              rep(
                kind,
                nrow(
                  rows
                )
              ),
            N_main_cohort =
              rep(
                main_cohort_n,
                nrow(
                  rows
                )
              ),
            N_nonmissing_overall =
              rep(
                as.integer(
                  sum(
                    valid_nonmissing_mask(
                      x
                    )
                  )
                ),
                nrow(
                  rows
                )
              )
          )
        )
      }
    ) %>%
    arrange(
      dictionary_order,
      category_order
    )

  rm(
    loaded,
    raw_i
  )

  invisible(
    gc()
  )

  result_i
}

# -----------------------------------------------------------------------------
# 14.3-14.4 S10-S17: load each raw view at most once per rebuild
# -----------------------------------------------------------------------------

groups_overall_only <- function(dat) {
  list(Overall = seq_len(nrow(dat)))
}

groups_incident_status <- function(dat) {
  list(
    `No incident multimorbidity` =
      which(
        dat$incident_multimorbidity ==
          0L
      ),
    `Incident multimorbidity` =
      which(
        dat$incident_multimorbidity ==
          1L
      )
  )
}

descriptive_sex_tables <- list()
descriptive_status_tables <- list()

# v4 cache: forces regeneration after the ordered-category fix and the
# one-read-per-view refactor.
descriptive_cache_dir <-
  file.path(
    intermediate_dir,
    "descriptive_tables_raw_parent_v20261025_9outcomes"
  )

dir.create(
  descriptive_cache_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

rebuild_descriptive <-
  isTRUE(
    getOption(
      "ukb.v8.rebuild_descriptive",
      FALSE
    )
  )

for (
  i in seq_len(
    nrow(
      descriptive_spec
    )
  )
) {

  spec_i <-
    descriptive_spec[i, ]

  slug_i <-
    recode(
      spec_i$modality,
      enviro = "environment",
      blood = "biomarkers",
      metabolites = "metabolomics",
      gene = "genetics"
    )

  sex_cache_i <-
    file.path(
      descriptive_cache_dir,
      paste0(
        "Table_",
        spec_i$number_sex,
        "_",
        slug_i,
        "_overall_only.csv"
      )
    )

  status_cache_i <-
    file.path(
      descriptive_cache_dir,
      paste0(
        "Table_",
        spec_i$number_status,
        "_",
        slug_i,
        "_by_incident_multimorbidity.csv"
      )
    )

  need_sex <-
    rebuild_descriptive ||
      !file.exists(
        sex_cache_i
      )

  need_status <-
    rebuild_descriptive ||
      !file.exists(
        status_cache_i
      )

  loaded_i <- NULL

  # If either table needs recalculation, read this raw modality ONCE and reuse it
  # for both the sex-stratified and incident-status summaries.
  if (need_sex || need_status) {

    cat(
      "Section 14：读取一次 ",
      spec_i$modality,
      " raw view，并复用于 ",
      spec_i$number_sex,
      " / ",
      spec_i$number_status,
      "\n",
      sep = ""
    )

    loaded_i <-
      load_descriptive_view(
        spec_i$modality,
        spec_i$raw_file
      )
  }

  if (need_sex) {

    tab_sex_i <-
      summarise_view_groups(
        spec_i$modality,
        spec_i$raw_file,
        groups_overall_only,
        loaded_view = loaded_i
      )

    write_csv(
      tab_sex_i,
      sex_cache_i
    )

  } else {

    cat(
      "Section 14：读取 ",
      spec_i$number_sex,
      " cache\n",
      sep = ""
    )

    tab_sex_i <-
      fread(
        sex_cache_i,
        showProgress = FALSE
      ) %>%
      as_tibble()
  }

  if (need_status) {

    tab_status_i <-
      summarise_view_groups(
        spec_i$modality,
        spec_i$raw_file,
        groups_incident_status,
        loaded_view = loaded_i
      )

    write_csv(
      tab_status_i,
      status_cache_i
    )

  } else {

    cat(
      "Section 14：读取 ",
      spec_i$number_status,
      " cache\n",
      sep = ""
    )

    tab_status_i <-
      fread(
        status_cache_i,
        showProgress = FALSE
      ) %>%
      as_tibble()
  }

  descriptive_sex_tables[[
      spec_i$number_sex
    ]] <-
    tab_sex_i

  descriptive_status_tables[[
      spec_i$number_status
    ]] <-
    tab_status_i

  rm(
    loaded_i,
    tab_sex_i,
    tab_status_i
  )

  invisible(
    gc()
  )
}

clean_overall_sex_table <- function(x) {
  x %>% transmute(
    Variable = display_name,
    Category = category,
    Statistic = statistic,
    Overall
  )
}

clean_incident_status_table <- function(x) {
  x %>%
    transmute(
      Variable =
        display_name,
      Category =
        category,
      Statistic =
        statistic,
      `No incident multimorbidity`,
      `Incident multimorbidity`
    )
}

table_s10 <-
  clean_overall_sex_table(
    descriptive_sex_tables[["S10"]]
  )

table_s11 <-
  clean_overall_sex_table(
    descriptive_sex_tables[["S11"]]
   
  )

table_s12 <-
  clean_overall_sex_table(
    descriptive_sex_tables[["S12"]]
    
  )

table_s13 <-
  clean_overall_sex_table(
    descriptive_sex_tables[["S13"]]
    
  )

table_s14 <-
  clean_incident_status_table(
    descriptive_status_tables[["S14"]]
    
  )

table_s15 <-
  clean_incident_status_table(
    descriptive_status_tables[["S15"]]
    
  )

table_s16 <-
  clean_incident_status_table(
    descriptive_status_tables[["S16"]]
    
  )

table_s17 <-
  clean_incident_status_table(
    descriptive_status_tables[["S17"]]
    
  )
# =====================================================================
# S10-S17 描述性表格类别标签转换 | 基于 1.process_data_UKB + 2.UKB_recoding
# 作用范围：仅输出 CSV 中的 Category；不改原始数据/模型/统计人数。
# 将此文件 source() 在 07 的 table_s10 ... table_s17 创建之后、write_table() 之前。
# 20261026：结合 01 原始回答频数 (S10_77variables_original_levels_QA.csv) 和 02
# 的缺失处理/有序分类代码，将 73 个原先保留的数字标签补成有证据的英文。
# 02 在编码前将 Do not know/Prefer not to answer/空字符串置为 NA；
# 其中仅有两个末端代码将多个有意义回答合并，以下另行报告供方法学复核：
# - Mobile phone weekly usage: 4-6 hours + More than 6 hours -> code 4
# - Job satisfaction (已修改的 02): Extremely happy -> 5；I am not employed -> NA。
# 07 仅展示修正后的类别标签，不改变模型或已有数据；必须先重跑上游。
# =====================================================================

s10_17_explicit_category_map <- list(
  "Body size 10yrs old" = c("0" = "Thinner", "1" = "About average", "2" = "Plumper"),
  "Childhood doctor" = c("0" = "Never true", "1" = "Rarely true", "2" = "Sometimes true", "3" = "Often", "4" = "Very often true"),
  "Childhood hate" = c("0" = "Never true", "1" = "Rarely true", "2" = "Sometimes true", "3" = "Often", "4" = "Very often true"),
  "Childhood love" = c("0" = "Never true", "1" = "Rarely true", "2" = "Sometimes true", "3" = "Often", "4" = "Very often true"),
  "Childhood physical abuse" = c("0" = "Never true", "1" = "Rarely true", "2" = "Sometimes true", "3" = "Often", "4" = "Very often true"),
  "Childhood sex abuse" = c("0" = "Never true", "1" = "Rarely true", "2" = "Sometimes true", "3" = "Often", "4" = "Very often true"),
  "Height 10yrs old" = c("0" = "Shorter", "1" = "About average", "2" = "Taller"),
  "Falls last 1yr" = c("0" = "No falls", "1" = "Only one fall", "2" = "More than one fall"),
  "Hearing difficulty" = c("0" = "No", "1" = "Yes", "2" = "I am completely deaf"),
  "Loud music exposure frequency" = c("0" = "No", "1" = "Yes, for less than a year", "2" = "Yes, for around 1-5 years", "3" = "Yes, for more than 5 years"),
  "Overall health rating" = c("0" = "Poor", "1" = "Fair", "2" = "Good", "3" = "Excellent"),
  "Tinnitus" = c("0" = "No, never", "1" = "Yes, but not now, but have in the past", "2" = "Yes, now some of the time", "3" = "Yes, now a lot of the time", "4" = "Yes, now most or all of the time"),
  "Weight change last 1yr" = c("0" = "Yes - lost weight", "1" = "No - weigh about the same", "2" = "Yes - gained weight"),
  "Alcohol 10yrs" = c("0" = "Less nowadays", "1" = "About the same", "2" = "More nowadays"),
  "Alcohol frequency" = c("0" = "Never", "1" = "Special occasions only", "2" = "One to three times a month", "3" = "Once or twice a week", "4" = "Three or four times a week", "5" = "Daily or almost daily"),
  "Alcohol status" = c("0" = "Never", "1" = "Previous", "2" = "Current"),
  "Beef" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Cheese" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Computer games" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Often"),
  "Diet variation" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Often"),
  "Easy wake" = c("0" = "Not at all easy", "1" = "Not very easy", "2" = "Fairly easy", "3" = "Very easy"),
  "Facial aging" = c("0" = "Younger than you are", "1" = "About your age", "2" = "Older than you are"),
  "Fast driving" = c("0" = "Do not drive on the motorway", "1" = "Never/rarely", "2" = "Sometimes", "3" = "Often", "4" = "Most of the time"),
  "Heavy DIY duration" = c("0" = "Less than 15 minutes", "1" = "Between 15 and 30 minutes", "2" = "Between 30 minutes and 1 hour", "3" = "Between 1 and 1.5 hours", "4" = "Between 1.5 and 2 hours", "5" = "Between 2 and 3 hours", "6" = "Over 3 hours"),
  "Heavy DIY frequency 4wks" = c("0" = "Once in the last 4 weeks", "1" = "2-3 times in the last 4 weeks", "2" = "Once a week", "3" = "2-3 times a week", "4" = "4-5 times a week", "5" = "Every day"),
  "Hot drink temp" = c("0" = "Do not drink hot drinks", "1" = "Warm", "2" = "Hot", "3" = "Very hot"),
  "Household smokers" = c("0" = "No", "1" = "Yes, one household member smokes", "2" = "Yes, more than one household member smokes"),
  "IPAQ activity group" = c("0" = "low", "1" = "moderate", "2" = "high"),
  "Lamb" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Light DIY duration" = c("0" = "Less than 15 minutes", "1" = "Between 15 and 30 minutes", "2" = "Between 30 minutes and 1 hour", "3" = "Between 1 and 1.5 hours", "4" = "Between 1.5 and 2 hours", "5" = "Between 2 and 3 hours", "6" = "Over 3 hours"),
  "Light DIY frequency 4wks" = c("0" = "Once in the last 4 weeks", "1" = "2-3 times in the last 4 weeks", "2" = "Once a week", "3" = "2-3 times a week", "4" = "4-5 times a week", "5" = "Every day"),
  "Mobile phone 2yrs" = c("0" = "No", "1" = "Yes, use is now less frequent", "2" = "Yes, use is now more frequent"),
  "Mobile phone duration" = c("0" = "Never used mobile phone at least once per week", "1" = "One year or less", "2" = "Two to four years", "3" = "Five to eight years", "4" = "More than eight years"),
  "Mobile phone weekly usage" = c("0" = "Less than 5mins", "1" = "5-29 mins", "2" = "30-59 mins", "3" = "1-3 hours", "4" = "4-6 hours / More than 6 hours (combined in 02)"),
  "Nap" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Usually"),
  "Narcolepsy" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Often", "3" = "All of the time"),
  "Non oily fish" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Oily fish" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Other exercise duration" = c("0" = "Less than 15 minutes", "1" = "Between 15 and 30 minutes", "2" = "Between 30 minutes and 1 hour", "3" = "Between 1 and 1.5 hours", "4" = "Between 1.5 and 2 hours", "5" = "Between 2 and 3 hours", "6" = "Over 3 hours"),
  "Other exercise frequency 4wks" = c("0" = "Once in the last 4 weeks", "1" = "2-3 times in the last 4 weeks", "2" = "Once a week", "3" = "2-3 times a week", "4" = "4-5 times a week", "5" = "Every day"),
  "Past tobacco" = c("0" = "I have never smoked", "1" = "Just tried once or twice", "2" = "Smoked occasionally", "3" = "Smoked on most or all days"),
  "Pleasure walks duration" = c("0" = "Less than 15 minutes", "1" = "Between 15 and 30 minutes", "2" = "Between 30 minutes and 1 hour", "3" = "Between 1 and 1.5 hours", "4" = "Between 1.5 and 2 hours", "5" = "Between 2 and 3 hours", "6" = "Over 3 hours"),
  "Pleasure walks frequency 4wks" = c("0" = "Once in the last 4 weeks", "1" = "2-3 times in the last 4 weeks", "2" = "Once a week", "3" = "2-3 times a week", "4" = "4-5 times a week", "5" = "Every day"),
  "Pork" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Poultry" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Processed meat" = c("0" = "Never", "1" = "Less than once a week", "2" = "Once a week", "3" = "2-4 times a week", "4" = "5-6 times a week", "5" = "Once or more daily"),
  "Resume smoking likelihood" = c("0" = "No, definitely not", "1" = "No, probably not", "2" = "Yes, probably", "3" = "Yes, definitely"),
  "Salt" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Usually", "3" = "Always"),
  "Skin tan ease" = c("0" = "Never tan, only burn", "1" = "Get mildly or occasionally tanned", "2" = "Get moderately tanned", "3" = "Get very tanned"),
  "Sleep difficulty" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Usually"),
  "Smoking status" = c("0" = "Never", "1" = "Previous", "2" = "Current"),
  "Speakerphone" = c("0" = "Never or almost never", "1" = "Less than half the time", "2" = "About half the time", "3" = "More than half the time", "4" = "Always or almost always"),
  "Stair climbing frequency 4wks" = c("0" = "None", "1" = "1-5 times a day", "2" = "6-10 times a day", "3" = "11-15 times a day", "4" = "16-20 times a day", "5" = "More than 20 times a day"),
  "Sun protection use" = c("0" = "Do not go out in sunshine", "1" = "Never/rarely", "2" = "Sometimes", "3" = "Most of the time", "4" = "Always"),
  "Tobacco" = c("0" = "No", "1" = "Only occasionally", "2" = "Yes, on most or all days"),
  "Usual walking pace" = c("0" = "Slow pace", "1" = "Steady average pace", "2" = "Brisk pace"),
  "Confide frequency" = c("0" = "Never or almost never", "1" = "Once every few months", "2" = "About once a month", "3" = "About once a week", "4" = "2-4 times a week", "5" = "Almost daily"),
  "Depressed mood frequency" = c("0" = "Not at all", "1" = "Several days", "2" = "More than half the days", "3" = "Nearly every day"),
  "Family satisfaction" = c("0" = "Extremely unhappy", "1" = "Very unhappy", "2" = "Moderately unhappy", "3" = "Moderately happy", "4" = "Very happy", "5" = "Extremely happy"),
  "Family visit frequency" = c("0" = "No friends/family outside household", "1" = "Never or almost never", "2" = "Once every few months", "3" = "About once a month", "4" = "About once a week", "5" = "2-4 times a week", "6" = "Almost daily"),
  "Financial satisfaction" = c("0" = "Extremely unhappy", "1" = "Very unhappy", "2" = "Moderately unhappy", "3" = "Moderately happy", "4" = "Very happy", "5" = "Extremely happy"),
  "Friends satisfaction" = c("0" = "Extremely unhappy", "1" = "Very unhappy", "2" = "Moderately unhappy", "3" = "Moderately happy", "4" = "Very happy", "5" = "Extremely happy"),
  "Happiness" = c("0" = "Extremely unhappy", "1" = "Very unhappy", "2" = "Moderately unhappy", "3" = "Moderately happy", "4" = "Very happy", "5" = "Extremely happy"),
  "Health satisfaction" = c("0" = "Extremely unhappy", "1" = "Very unhappy", "2" = "Moderately unhappy", "3" = "Moderately happy", "4" = "Very happy", "5" = "Extremely happy"),
  "Job satisfaction" = c("0" = "Extremely unhappy", "1" = "Very unhappy", "2" = "Moderately unhappy", "3" = "Moderately happy", "4" = "Very happy", "5" = "Extremely happy"),
  "Tenseness frequency" = c("0" = "Not at all", "1" = "Several days", "2" = "More than half the days", "3" = "Nearly every day"),
  "Tiredness frequency" = c("0" = "Not at all", "1" = "Several days", "2" = "More than half the days", "3" = "Nearly every day"),
  "Unenthusiasm frequency" = c("0" = "Not at all", "1" = "Several days", "2" = "More than half the days", "3" = "Nearly every day"),
  "Age facial hair" = c("0" = "Younger than average", "1" = "About average age", "2" = "Older than average"),
  "Age voice" = c("0" = "Younger than average", "1" = "About average age", "2" = "Older than average"),
  "Employment heavy manual" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Usually", "3" = "Always"),
  "Employment standing" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Usually", "3" = "Always"),
  "Household income" = c("0" = "Less than 18,000", "1" = "18,000 to 30,999", "2" = "31,000 to 51,999", "3" = "52,000 to 100,000", "4" = "Greater than 100,000"),
  "Household vehicles" = c("0" = "None", "1" = "One", "2" = "Two", "3" = "Three", "4" = "Four or more"),
  "Noisy workplace" = c("0" = "No", "1" = "Yes, for less than a year", "2" = "Yes, for around 1-5 years", "3" = "Yes, for more than 5 years"),
  "Private healthcare" = c("0" = "No, never", "1" = "Yes, sometimes", "2" = "Yes, most of the time", "3" = "Yes, all of the time"),
  "Shift work" = c("0" = "Never/rarely", "1" = "Sometimes", "2" = "Usually", "3" = "Always")
)

relabel_S10_S17 <- function(tab, tab_name = "S10") {
  stopifnot(all(c("Variable", "Category") %in% names(tab)))
  out <- tab
  qa <- list()
  for (i in seq_len(nrow(out))) {
    var <- as.character(out$Variable[i])
    val <- as.character(out$Category[i])
    if (is.na(val) || !nzchar(val) || val == "Missing") next
    if (identical(var, "Sex")) {
      if (val %in% c("No / absent", "0")) out$Category[i] <- "Female"
      else if (val %in% c("Yes / present", "1")) out$Category[i] <- "Male"
      else if (!(val %in% c("Female", "Male"))) {
        qa[[length(qa) + 1L]] <- data.frame(table=tab_name, Variable=var, Category=val, reason="Unexpected sex value")
      }
      next
    }
    m <- s10_17_explicit_category_map[[var]]
    if (is.null(m)) next
    matched <- unname(m[val])
    if (!is.na(matched)) out$Category[i] <- matched
    else if (grepl("^[0-9]+$", val)) {
      qa[[length(qa) + 1L]] <- data.frame(table=tab_name, Variable=var, Category=val, reason="02 final else has no explicit source label")
    }
  }
  list(table=out,qa=if(length(qa)) dplyr::bind_rows(qa) else data.frame(table=character(),Variable=character(),Category=character(),reason=character()))
}

# 在 07 正常生成 table_s10 ... table_s17 后，直接运行：
qa_s10_s17 <- list()
for (k in 10:17) {
  name <- paste0("table_s",k)
  if (!exists(name, inherits=TRUE)) stop("Expected table object does not exist: ",name)
  res <- relabel_S10_S17(get(name,inherits=TRUE),paste0("S",k))
  assign(name,res$table,envir=.GlobalEnv)
  qa_s10_s17[[paste0("S",k)]] <- res$qa
}
qa_labels <- dplyr::bind_rows(qa_s10_s17)
# 当前 07 代码通常已定义 output_dir / intermediate_dir，如有其它目录请按运行环境调整
qa_path <- file.path(intermediate_dir, "S10_S17_02_default_category_QA.csv")
utils::write.csv(qa_labels,qa_path,row.names=FALSE,fileEncoding="UTF-8")
# 所有原先的 S10/S14 有序数字类别都应被明确映射：没有映射则中断，避免带错表继续发布。
if (nrow(qa_labels) > 0L) {
  stop("S10-S17 仍存在未映射编码；请检查：", qa_path)
}
# 02 当前保留的末端合并：Mobile phone weekly usage 的最后一档包含两个时长。
# Job satisfaction 已在修订版 02 修正：未就业设为 NA，代码 5 仅指 Extremely happy。
# 以下原始 N 来自 01 的全体源数据，并非 MAIN cohort 的分析人数。
source_merged_levels_qa <- data.frame(
  Variable = "Mobile phone weekly usage",
  source_variable = "mobile_phone_weekly_usage",
  recoded_category = "4",
  original_answers_merged_by_02 =
    "4-6 hours [N=17315] | More than 6 hours [N=17953]",
  reason = "Two duration categories pooled at code 4 by upstream 02",
  analysis_impact = "Label reports both original source categories",
  stringsAsFactors = FALSE
)
write_log(source_merged_levels_qa, "S10_S14_02_merged_ordinal_categories_QA.csv")
job_satisfaction_qa <- data.frame(
  Variable = "Job satisfaction",
  source_variable = "job_satisfaction",
  raw_answer = c("Extremely happy", "I am not employed"),
  corrected_02_code = c("5", "NA"),
  s10_s14_display = c("Extremely happy", "Missing"),
  important_note = rep(
    "Requires rerun of corrected 02 and all downstream source/model outputs; 07 cannot repair stale model inputs",
    2L
  ),
  stringsAsFactors = FALSE
)
write_log(job_satisfaction_qa, "S10_S14_job_satisfaction_4537_rule_QA.csv")
message(
  "S10-S17 category mapping complete: job_satisfaction=5 is labelled Extremely happy; ",
  "I am not employed must be NA in regenerated 02 and downstream files. ",
  "Unmapped-category QA: ", qa_path
)

# -----------------------------------------------------------------------------
# 14.5 Save S10-S17 + QA
# -----------------------------------------------------------------------------

write_table(
  table_s10,
  "Table_S10_environment_overall_women_men.csv"
)

write_table(
  table_s11,
  "Table_S11_biomarkers_overall_women_men.csv"
)

write_table(
  table_s12,
  "Table_S12_metabolomics_overall_women_men.csv"
)

write_table(
  table_s13,
  "Table_S13_genetics_overall_women_men.csv"
)

write_table(
  table_s14,
  "Table_S14_environment_by_incident_multimorbidity.csv"
)

write_table(
  table_s15,
  "Table_S15_biomarkers_by_incident_multimorbidity.csv"
)

write_table(
  table_s16,
  "Table_S16_metabolomics_by_incident_multimorbidity.csv"
)

write_table(
  table_s17,
  "Table_S17_genetics_by_incident_multimorbidity.csv"
)

descriptive_missing_qa <-
  bind_rows(
    imap_dfr(
      descriptive_sex_tables,
      ~ .x %>%
        filter(
          descriptive_kind %in%
            c(
              "categorical",
              "binary"
            )
        ) %>%
        group_by(
          analysis_variable
        ) %>%
        summarise(
          has_missing_row =
            any(
              category ==
                "Missing",
              na.rm = TRUE
            ),
          .groups = "drop"
        ) %>%
        mutate(
          table =
            .y,
          table_type =
            "Overall only",
          .before = 1
        )
    ),
    imap_dfr(
      descriptive_status_tables,
      ~ .x %>%
        filter(
          descriptive_kind %in%
            c(
              "categorical",
              "binary"
            )
        ) %>%
        group_by(
          analysis_variable
        ) %>%
        summarise(
          has_missing_row =
            any(
              category ==
                "Missing",
              na.rm = TRUE
            ),
          .groups = "drop"
        ) %>%
        mutate(
          table =
            .y,
          table_type =
            "Incident multimorbidity status",
          .before = 1
        )
    )
  )

write_log(
  descriptive_missing_qa,
  "Table_S10_S17_missing_row_QA.csv"
)

if (
  nrow(
    descriptive_missing_qa
  ) &&
  any(
    !descriptive_missing_qa$has_missing_row
  )
) {
  stop(
    "S10-S17 categorical Missing-row QA failed; inspect Table_S10_S17_missing_row_QA.csv"
  )
}

cat(
  "完成：Section 14 Supplementary Tables S10-S17（S10-S13 Overall/P1–P8；S14-S17 incident status only）\n"
)

update_progress_checkpoint("07 Section 14 completed", "从 07 Section 15 继续。")

###############################################################################
### Section 15. Supplementary Tables S18-S26 + final table manifest ####
###############################################################################
# 先运行 Section 0–2；S1–S3、S10–S17 来自 Section 13–14。
# 本段可读取前面已经保存的 source/cache，不要求所有对象一直保留在环境中。
# S4–S9 不在本 07 的输出范围内。
# 最终顺序：S18 Stage 1；S19 Stage 2A；S20 held-out 关联；S21 pattern membership；
#           S22 performance；S23 final predictors；S24 环境子域+生物域 Wald；
#           S25 E×单独生物域完整四组；S26 E×综合生物评分完整四组。

# -----------------------------------------------------------------------------
# 15.1 Read required objects / completed caches
# -----------------------------------------------------------------------------
generated_table_csv_names_descriptive <- c(
  "1" = "Table_S1_36_chronic_diseases.csv",
  "2" = "Table_S2_chronic_disease_burden_end_followup.csv",
  "3" = "Table_S3_eight_multimorbidity_patterns.csv",
  "10" = "Table_S10_environment_overall_women_men.csv",
  "11" = "Table_S11_biomarkers_overall_women_men.csv",
  "12" = "Table_S12_metabolomics_overall_women_men.csv",
  "13" = "Table_S13_genetics_overall_women_men.csv",
  "14" = "Table_S14_environment_by_incident_multimorbidity.csv",
  "15" = "Table_S15_biomarkers_by_incident_multimorbidity.csv",
  "16" = "Table_S16_metabolomics_by_incident_multimorbidity.csv",
  "17" = "Table_S17_genetics_by_incident_multimorbidity.csv"
)

for (n in c(1:3, 10:17)) {
  need_object(
    paste0("table_s", n),
    file.path(table_dir, generated_table_csv_names_descriptive[[as.character(n)]])
  )
}
need_object("signed_importance_all",
            file.path(intermediate_dir, "Stage1_signed_importance_complete.csv"))
need_object("stage2a_selected",
            file.path(intermediate_dir, "Stage2A_selected_features.csv"))
need_object("stage2b_associations",
            file.path(intermediate_dir, "Stage2B_heldout_adjusted_associations_20261025_9outcomes.csv"))
need_object("table_s21_membership_wide",
            file.path(table_dir, "Table_S21_complete_P1_P8_feature_membership.csv"))
need_object("stage3b_performance",
            file.path(intermediate_dir, "Stage3B_combination_performance.csv"))
need_object("stage3_feature_importance",
            file.path(intermediate_dir, "Stage3_final_predictors.csv"))
need_object("environment_subdomain_wald",
            file.path(intermediate_dir, "Stage3_environment_subdomain_wald.csv"))
need_object("figure5b_full",
            file.path(figure_source_dir, "Figure5b_full_environment_individual_omics_profiles.csv"))
need_object("figure5c",
            file.path(figure_source_dir, "Figure5c_environment_combined_omics.csv"))

# -----------------------------------------------------------------------------
# 15.2 Final S18–S26 objects
# -----------------------------------------------------------------------------
table_s18 <- signed_importance_all
table_s19 <- stage2a_selected
table_s20 <- stage2b_associations
table_s21 <- table_s21_membership_wide
table_s22 <- stage3b_performance %>%
  mutate(combination_display_EPMG = gsub("B", "P", combination, fixed = TRUE))
table_s23 <- stage3_feature_importance
# 保留环境子域和 B/M/G 的共同模型结果；不使用 S11 图内再次归一化的环境百分比。
table_s24 <- environment_subdomain_wald

# S25：使用完整 figure5b_full；每个 outcome × 生物域保留 L/L、L/H、H/L、H/H。
# HR、CI、P 和 development cutpoints 原样来自已完成的联合 Cox 模型。
# L/L 为参照组（HR=1）；缺失/不可估计组保留行，不人为生成估计值。
# 因展示的对比由 3 个 H/H 扩展为 9 个非参照对比，S25 的 BH-FDR
# 按 outcome 对全部有效非参照对比重新计算；正文 Figure 5b 不改变。
joint_profile_levels <- c(
  "Lower risk / Lower risk", "Lower risk / Higher risk",
  "Higher risk / Lower risk", "Higher risk / Higher risk"
)
s25_expected <- crossing(
  outcome_lookup %>% select(dataset, outcome_label, outcome_order),
  modality_lookup %>%
    filter(modality %in% c("blood", "metabolites", "gene")) %>%
    select(modality, omic_label = modality_label),
  joint_group = joint_profile_levels
)
if (anyDuplicated(figure5b_full[, c("dataset", "modality", "joint_group")])) {
  stop("S25 full-profile source contains duplicated outcome/domain/group rows.")
}
table_s25 <- s25_expected %>%
  left_join(
    figure5b_full %>% select(-outcome_label, -outcome_order, -omic_label),
    by = c("dataset", "modality", "joint_group")
  ) %>%
  mutate(
    environment_level = str_trim(str_extract(joint_group, "^[^/]+")),
    omic_level = str_trim(str_extract(joint_group, "[^/]+$")),
    is_reference = joint_group == joint_profile_levels[[1]],
    estimable = coalesce(estimable, FALSE),
    HR_95_CI = case_when(
      is_reference & is.finite(HR) & HR == 1 ~ "1.00 (reference)",
      is.finite(HR) & is.finite(CI_low) & is.finite(CI_high) ~
        sprintf("%.2f (%.2f, %.2f)", HR, CI_low, CI_high),
      TRUE ~ "Not estimable"
    )
  ) %>%
  group_by(dataset) %>%
  mutate(
    FDR = {
      eligible <- !is_reference & estimable %in% TRUE & is.finite(P)
      adjusted <- rep(NA_real_, n())
      adjusted[eligible] <- p.adjust(P[eligible], method = "BH")
      adjusted
    },
    FDR_family = "All estimable non-reference individual-omics joint contrasts within outcome"
  ) %>%
  ungroup() %>%
  arrange(
    outcome_order, match(omic_label, c("Phenotypic biomarkers", "Metabolomic biomarkers", "Genetic factors")),
    match(joint_group, joint_profile_levels)
  ) %>%
  select(
    outcome_label, omic_label, joint_group, environment_level, omic_level,
    HR, CI_low, CI_high, HR_95_CI, P, FDR, everything()
  )
table_s26 <- figure5c

s25_group_qa <- table_s25 %>%
  group_by(dataset, outcome_label, modality, omic_label) %>%
  summarise(
    n_groups = n(),
    n_unique_groups = n_distinct(joint_group),
    complete_four_groups = setequal(joint_group, joint_profile_levels),
    n_reference = sum(is_reference),
    reference_HR_is_one = all(HR[is_reference] == 1 & !is.na(HR[is_reference])),
    n_estimable_nonreference = sum(!is_reference & estimable %in% TRUE),
    .groups = "drop"
  ) %>%
  mutate(passed = n_groups == 4L & n_unique_groups == 4L &
           complete_four_groups & n_reference == 1L & reference_HR_is_one)
write_log(s25_group_qa, "Table_S25_complete_joint_profile_QA.csv")
if (nrow(table_s25) != nrow(outcome_lookup) * 3L * 4L ||
    nrow(s25_group_qa) != nrow(outcome_lookup) * 3L ||
    !all(s25_group_qa$passed)) {
  stop("S25 complete joint-profile QA failed; inspect Table_S25_complete_joint_profile_QA.csv.")
}

# -----------------------------------------------------------------------------
# 15.3 Final order and save
# -----------------------------------------------------------------------------
supplementary_table_manifest <- tribble(
  ~number, ~title_en, ~generated_in_07,
  "Table S1", "Definitions and epidemiological characteristics of the 36 chronic diseases included in the multimorbidity analysis", TRUE,
  "Table S2", "Distribution of chronic disease burden by the end of valid follow-up in the main analytical cohort", TRUE,
  "Table S3", "Disease composition and participant burden of the eight multimorbidity patterns", TRUE,
  "Table S4", "Candidate predictor variables excluded before model development and reasons for exclusion", FALSE,
  "Table S5", "Environmental and clinical predictor characteristics included in the analysis", FALSE,
  "Table S6", "Phenotypic blood and urine biomarker predictor variables included in the analysis", FALSE,
  "Table S7", "Metabolomic biomarker predictor variables included in the analysis", FALSE,
  "Table S8", "Genetic predictor variables included in the analysis", FALSE,
  "Table S9", "Benchmark comparison of candidate survival-learning models for incident multimorbidity prediction", FALSE,
  "Table S10", "Environmental and clinical characteristics in the overall cohort", TRUE,
  "Table S11", "Phenotypic blood and urine biomarker characteristics in the overall cohort", TRUE,
  "Table S12", "Metabolomic biomarker characteristics in the overall cohort", TRUE,
  "Table S13", "Genetic risk factor characteristics in the overall cohort", TRUE,
  "Table S14", "Environmental and clinical characteristics according to incident multimorbidity status", TRUE,
  "Table S15", "Biomarker characteristics according to incident multimorbidity status", TRUE,
  "Table S16", "Metabolomic characteristics according to incident multimorbidity status", TRUE,
  "Table S17", "Genetic characteristics according to incident multimorbidity status", TRUE,
  "Table S18", "Complete Stage 1 signed feature importance across incident multimorbidity outcomes", TRUE,
  "Table S19", "Stage 2A predictors selected for incident multimorbidity outcomes using development-set cross-validation and the 1-SE rule", TRUE,
  "Table S20", "Associations of Stage 2A-selected predictors with incident multimorbidity outcomes in the independent held-out set", TRUE,
  "Table S21", "Complete cross-pattern membership of Stage 2A-selected predictors across multimorbidity patterns P1–P8", TRUE,
  "Table S22", "Predictive performance of single-domain and multimodal models for overall and eight pattern-specific incident multimorbidity outcomes in the independent held-out set", TRUE,
  "Table S23", "Predictor importance in final integrated models for overall and eight pattern-specific incident multimorbidity outcomes", TRUE,
  "Table S24", "Relative prognostic contributions of environmental and clinical subdomains and biological data domains in final integrated models", TRUE,
  "Table S25", "Joint associations of environmental and clinical risk scores and individual biological-domain risk scores with incident multimorbidity", TRUE,
  "Table S26", "Joint associations of environmental and clinical risk scores and combined biological risk scores with incident multimorbidity", TRUE
) %>%
  mutate(
    table_order = as.integer(str_remove(number, fixed("Table S"))),
    source = if_else(generated_in_07, "Generated in 07_results_and_tables", "Prepared separately")
  ) %>% arrange(table_order)
write_csv(
  supplementary_table_manifest,
  file.path(intermediate_dir, "supplementary_table_manifest_S1_S26.csv")
)

generated_supplementary_tables <- list(
  `Table S1` = table_s1,
  `Table S2` = table_s2,
  `Table S3` = table_s3,
  `Table S10` = table_s10,
  `Table S11` = table_s11,
  `Table S12` = table_s12,
  `Table S13` = table_s13,
  `Table S14` = table_s14,
  `Table S15` = table_s15,
  `Table S16` = table_s16,
  `Table S17` = table_s17,
  `Table S18` = table_s18,
  `Table S19` = table_s19,
  `Table S20` = table_s20,
  `Table S21` = table_s21,
  `Table S22` = table_s22,
  `Table S23` = table_s23,
  `Table S24` = table_s24,
  `Table S25` = table_s25,
  `Table S26` = table_s26
)

generated_table_csv_names <- c(
  `Table S1` = "Table_S1_36_chronic_diseases.csv",
  `Table S2` = "Table_S2_chronic_disease_burden_end_followup.csv",
  `Table S3` = "Table_S3_eight_multimorbidity_patterns.csv",
  `Table S10` = "Table_S10_environment_overall_women_men.csv",
  `Table S11` = "Table_S11_biomarkers_overall_women_men.csv",
  `Table S12` = "Table_S12_metabolomics_overall_women_men.csv",
  `Table S13` = "Table_S13_genetics_overall_women_men.csv",
  `Table S14` = "Table_S14_environment_by_incident_multimorbidity.csv",
  `Table S15` = "Table_S15_biomarkers_by_incident_multimorbidity.csv",
  `Table S16` = "Table_S16_metabolomics_by_incident_multimorbidity.csv",
  `Table S17` = "Table_S17_genetics_by_incident_multimorbidity.csv",
  `Table S18` = "Table_S18_complete_signed_importance.csv",
  `Table S19` = "Table_S19_Stage2A_selected_features.csv",
  `Table S20` = "Table_S20_Stage2B_heldout_adjusted_associations.csv",
  `Table S21` = "Table_S21_complete_P1_P8_feature_membership.csv",
  `Table S22` = "Table_S22_Stage3_performance.csv",
  `Table S23` = "Table_S23_final_integrated_predictors.csv",
  `Table S24` = "Table_S24_environmental_subdomain_and_omics_Wald.csv",
  `Table S25` = "Table_S25_environment_individual_omics_joint_profiles.csv",
  `Table S26` = "Table_S26_environment_combined_omics_profiles.csv"
)

iwalk(generated_supplementary_tables, function(tab, nm) {
  write_table(tab, generated_table_csv_names[[nm]])
})

# 本 workbook 只含 07 实际负责的 20 张表，按最终编号排列。
generated_workbook <- file.path(table_dir, "UKB_Supplementary_Tables_generated_by_07.xlsx")
openxlsx::write.xlsx(
  lapply(generated_supplementary_tables, as.data.frame),
  generated_workbook, overwrite = TRUE, asTable = FALSE
)

# 删除本脚本旧版输出中已取消或已改号的 CSV；仅匹配明确的旧文件名。
obsolete_table_csv_names <- c(
  "Table_S19_Stage2A_1SE_selection.csv",
  "Table_S20_Stage2A_selected_features.csv",
  "Table_S21_Stage2B_heldout_adjusted_associations.csv",
  "Table_S24_modality_score_associations.csv",
  "Table_S25_environmental_subdomain_Wald.csv",
  "Table_S26_environment_individual_omics_joint_high_risk.csv",
  "Table_S27_environment_combined_omics_profiles.csv",
  "Table_S28_environment_omics_interactions.csv",
  "Table_S29_complete_P1_P8_feature_membership.csv",
  "Table_S30_heldout_estimability_QA.csv",
  "Table_S31_5y_10y_calibration_source.csv",
  "Table_S32_integrated_risk_stratification.csv"
)
obsolete_paths <- file.path(table_dir, obsolete_table_csv_names)
existing_obsolete_paths <- obsolete_paths[file.exists(obsolete_paths)]
if (length(existing_obsolete_paths)) unlink(existing_obsolete_paths)
old_manifest <- file.path(intermediate_dir, "supplementary_table_manifest_S1_S32.csv")
if (file.exists(old_manifest)) unlink(old_manifest)

# -----------------------------------------------------------------------------
# 15.4 Final QA / checkpoint
# -----------------------------------------------------------------------------
expected_generated_numbers <- c(1:3, 10:26)
observed_generated_numbers <- as.integer(
  str_remove(names(generated_supplementary_tables), fixed("Table S"))
)
table_order_qa <- tibble(
  check = c(
    "generated tables are S1-S3 and S10-S26 only",
    "S4-S9 are absent from generated tables",
    "generated table order is ascending",
    "all 20 generated table CSV names defined",
    "generated CSV files exist",
    "obsolete generated CSVs removed",
    "S25 contains all four groups for all available outcomes and three biological domains"
  ),
  passed = c(
    identical(observed_generated_numbers, expected_generated_numbers),
    !any(observed_generated_numbers %in% 4:9),
    identical(observed_generated_numbers, sort(observed_generated_numbers)),
    length(generated_table_csv_names) == 20L &&
      identical(names(generated_table_csv_names), names(generated_supplementary_tables)),
    all(file.exists(file.path(table_dir, generated_table_csv_names))),
    !any(file.exists(obsolete_paths)),
    nrow(table_s25) == nrow(outcome_lookup) * 3L * 4L &&
      all(s25_group_qa$passed)
  )
)
write_log(table_order_qa, "supplementary_table_order_QA.csv")
if (!all(table_order_qa$passed)) {
  stop("Supplementary table order QA failed; inspect supplementary_table_order_QA.csv")
}
update_progress_checkpoint(
  "07 Section 0–15 completed; Table S1–S3/S10–S26 and Figure S1–S13 source ready",
  "运行 08_nature_figures_20261025_schemeB_EPMG.R，按正文 Figure 2–5 和补充 Figure S1–S13 顺序导出。"
)
cat(
  "\nSection 15 completed: Table S1–S3 and S10–S26 (20 tables).\n",
  "S25: all four joint groups; ", nrow(table_s25), " rows (",
  nrow(outcome_lookup), " outcomes × 3 domains × 4 groups).\n",
  "Generated workbook: ", generated_workbook, "\n", sep = ""
)

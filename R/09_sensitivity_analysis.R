#!/usr/bin/env Rscript
# =============================================================================
# 10. Sensitivity analyses / UKB incident multimorbidity / E-P-M-G (2026-10-29; AUC fix v2)
# =============================================================================
# Input:   Result/Sensitivity/<scenario>/For_R/*.csv ONLY.
# Output:  R_final_analysis/sensitivity/
#          tables/Table_Sensitivity_performance.csv (+ .xlsx if openxlsx exists)
#          figures/Figure_Sensitivity_performance.{pdf,png,tiff}
#          qa/*, Supplementary_Sensitivity_caption_and_notes.txt
# Scenarios: disease_free, domain_availability, landmark_2y, simple_imputation
# Outcomes: Overall, P1, ..., P8 (no sex-specific outcomes)
# Sections can run independently after Section 0; Section 2-4 reload saved RDS.
# The script does NOT refit models or read large server intermediates.
# No baseline estimates, confidence intervals, or P values are invented.
# =============================================================================

# =============================================================================
# Section 0. Settings and helpers / 配置与通用函数 ####
# =============================================================================
ROOT <- "/Users/yifan2/Desktop/日常工作/UKB_multimorbidity"
INPUT_ROOT <- file.path(ROOT, "Result", "Sensitivity")
# Primary Stage 3 final-model horizon data (do NOT substitute Stage 3B E/P/M/G
# combination AUC automatically, because it may represent a different model).
PRIMARY_RESULT_ROOTS <- c(file.path(ROOT, "Result"), file.path(ROOT, "Result_2"))
# Optional fallback if main Stage 3 fixed-horizon files are not available locally.
# The CSV MUST contain two columns: outcome,primary_auc10 (Overall/P1/.../P8).
PRIMARY_AUC10_TABLE <- file.path(INPUT_ROOT, "primary_10year_auc.csv")
OUTPUT_ROOT <- file.path(ROOT, "R_final_analysis", "sensitivity")

# Run all sections: c(1, 2, 3, 4). For a single section run e.g. c(3),
# after a successful Section 1 run. Section 0 is ALWAYS loaded first.
RUN_SECTIONS <- c(1, 2, 3, 4)

SCENARIOS <- c("disease_free", "domain_availability", "landmark_2y", "simple_imputation")
SCENARIO_LABEL <- c(
  disease_free = "Disease-free at baseline",
  domain_availability = "No entirely missing domain",
  landmark_2y = "Two-year landmark",
  simple_imputation = "Simple imputation"
)
OUTCOMES <- c("Overall", paste0("P", 1:8))
PLOT_METRIC <- "auc10"           # "auc10" (preferred) or "mean_auc" (ONLY after grid QA)
INCLUDE_MEAN_AUC_IN_TABLE <- FALSE  # set TRUE only after confirming common integration grid

# This is an analysis-summary/graphics script, not a model-training script.
for (pkg in c("data.table", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing R package: ", pkg, "; install.packages('", pkg, "')")
  }
}
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
options(stringsAsFactors = FALSE, scipen = 999)

TABLE_DIR <- file.path(OUTPUT_ROOT, "tables")
FIG_DIR <- file.path(OUTPUT_ROOT, "figures")
QA_DIR <- file.path(OUTPUT_ROOT, "qa")
for (d in c(TABLE_DIR, FIG_DIR, QA_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
CACHE <- file.path(QA_DIR, "sensitivity_normalized.rds")
section_on <- function(n) n %in% RUN_SECTIONS

# Standardize schema safely; the Python For_R schema may evolve.
std_name <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}
clean_scalar <- function(x) {
  y <- trimws(tolower(as.character(x)))
  y[is.na(y)] <- ""
  y
}
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))

# Choose a metric conservatively. Prefer explicit exact names, then search patterns.
# Exclude the primary/difference/development fields when selecting sensitivity metrics.
pick_col <- function(nms, exact = character(), patterns = character(),
                     omit = character()) {
  matches <- intersect(exact, nms)
  if (length(matches)) return(matches[[1]])
  candidates <- nms
  for (p in omit) candidates <- candidates[!grepl(p, candidates, perl = TRUE)]
  for (p in patterns) {
    hit <- candidates[grepl(p, candidates, perl = TRUE)]
    if (length(hit)) return(hit[[1]])
  }
  NA_character_
}

METRIC_SPECS <- list(
  auc10 = list(
    exact = c("auc_10y", "auc_10_year", "auc10", "auc_at_10y",
              "integrated_auc_10y", "heldout_auc_10y", "stage3_auc_10y",
              "integrated_10y_auc", "final_auc_10y", "stage3b_auc_10y"),
    patterns = c("auc.*(10y|10_year|10_years|year_10)", "(10y|10_year|year_10).*auc")
  ),
  mean_auc = list(
    exact = c("integrated_mean_time_dependent_auc", "integrated_mean_tdauc",
              "integrated_mean_auc", "mean_time_dependent_auc", "mean_tdauc",
              "mean_auc", "heldout_mean_auc", "stage3_mean_auc", "stage3b_mean_auc"),
    patterns = c("integrated.*mean.*auc", "mean.*(time_dependent|td).*auc", "mean.*auc")
  ),
  cindex = list(
    exact = c("integrated_c_index", "integrated_cindex", "c_index", "cindex",
              "heldout_c_index", "stage3_c_index", "stage3b_c_index"),
    patterns = c("integrated.*c_?index", "c_?index", "concordance")
  ),
  ibs = list(
    exact = c("integrated_ibs", "ibs", "integrated_brier_score"),
    patterns = c("(^|_)ibs($|_)", "integrated.*brier")
  )
)

# Distinguish absolute sensitivity estimates, primary values, and primary deltas.
# If primary/sensitivity are both explicitly present, no generic column is used.
metric_col <- function(nms, metric, role = c("sens", "primary", "delta")) {
  role <- match.arg(role)
  spec <- METRIC_SPECS[[metric]]
  if (role == "sens") {
    explicit <- paste0(c("sensitivity_", "sens_", "scenario_"), spec$exact[[1]])
    return(pick_col(
      nms,
      exact = c(explicit, spec$exact),
      patterns = spec$patterns,
      omit = c("(^|_)(primary|main|reference|ref|delta|diff|difference|change)(_|$)",
               "(^|_)(development|train|cv|se|sd|lower|upper|lcl|ucl)(_|$)")
    ))
  }
  tags <- if (role == "primary") c("primary", "main", "reference") else c("delta", "diff", "difference", "change")
  keys <- unique(unlist(lapply(tags, function(z) c(paste0(z, "_", spec$exact),
                                                   paste0(spec$exact, "_", z)))))
  patterns <- unlist(lapply(spec$patterns, function(p) {
    c(paste0("(^|_)(", paste(tags, collapse = "|"), ")_.*", p),
      paste0(p, ".*_(", paste(tags, collapse = "|"), ")($|_)") )
  }))
  pick_col(nms, exact = keys, patterns = patterns,
           omit = if (role == "primary") "(^|_)(delta|diff|difference|change)(_|$)" else character())
}

normalize_outcome <- function(raw_outcome, raw_type = "") {
  z <- clean_scalar(raw_outcome)
  k <- clean_scalar(raw_type)
  both <- paste(k, z, sep = "_")
  if (grepl("(^|_)(female|women|woman|male|men|man)(_|$)", both)) return(NA_character_)
  # Handle: P1, pattern_1, comorbidity_pattern_1_all, pattern1, etc.
  for (i in 1:8) {
    if (grepl(paste0("(^|[^0-9])(?:p|pattern|comorbidity_pattern)[_ -]*", i, "([^0-9]|$)"),
              both, perl = TRUE)) return(paste0("P", i))
  }
  if (z %in% c("all", "overall", "total", "general", "incident_multimorbidity",
               "comorbidity", "comorbidity_all", "overall_multimorbidity")) return("Overall")
  if (grepl("overall|comorbidity_all|incident_multimorbidity", both) &&
      !grepl("pattern", both)) return("Overall")
  NA_character_
}

read_scenario <- function(scenario) {
  csv <- file.path(INPUT_ROOT, scenario, "For_R", "sensitivity_full_summary_for_R.csv")
  if (!file.exists(csv)) return(NULL)
  raw <- fread(csv, showProgress = FALSE, encoding = "UTF-8")
  original_cols <- names(raw)
  setnames(raw, std_name(names(raw)))
  nms <- names(raw)
  if (!nrow(raw)) stop("Sensitivity summary has 0 rows: ", csv)
  outcome_col <- pick_col(nms,
    exact = c("outcome_label", "outcome", "outcome_name", "target", "endpoint", "disease_pattern"),
    patterns = c("^outcome", "^target$", "pattern"))
  type_col <- pick_col(nms, exact = c("c_type", "outcome_type", "cohort", "group", "analysis_group"))
  if (is.na(outcome_col) && is.na(type_col)) {
    stop("Cannot identify outcome column in: ", csv, "\nColumns: ", paste(nms, collapse = ", "))
  }
  oc <- if (is.na(outcome_col)) rep("", nrow(raw)) else as.character(raw[[outcome_col]])
  tc <- if (is.na(type_col)) rep("", nrow(raw)) else as.character(raw[[type_col]])
  outcome <- vapply(seq_len(nrow(raw)), function(i) normalize_outcome(oc[[i]], tc[[i]]), "")

  metric_names <- c("auc10", "mean_auc", "cindex", "ibs")
  selected <- rbindlist(lapply(metric_names, function(metric) {
    rbindlist(lapply(c("sens", "primary", "delta"), function(role) {
      data.table(scenario = scenario, metric = metric, role = role,
                 source_column = metric_col(nms, metric, role))
    }))
  }))
  sel_value <- function(metric, role) {
    m <- metric; r <- role
    col <- selected[selected$metric == m & selected$role == r, source_column][[1]]
    if (is.na(col)) return(rep(NA_real_, nrow(raw)))
    num(raw[[col]])
  }
  norm <- data.table(scenario = scenario, outcome = outcome,
     n = NA_real_, events = NA_real_,
     auc10 = sel_value("auc10", "sens"),
     mean_auc = sel_value("mean_auc", "sens"),
     cindex = sel_value("cindex", "sens"),
     ibs = sel_value("ibs", "sens"),
     primary_auc10 = sel_value("auc10", "primary"),
     primary_mean_auc = sel_value("mean_auc", "primary"),
     primary_cindex = sel_value("cindex", "primary"),
     delta_auc10 = sel_value("auc10", "delta"),
     delta_mean_auc = sel_value("mean_auc", "delta"),
     delta_cindex = sel_value("cindex", "delta")
  )
  n_col <- pick_col(nms,
    exact = c("n_heldout", "n_held_out", "heldout_n", "held_out_n", "n_test", "test_n",
              "n_participants", "n_samples", "n_total", "n"),
    patterns = c("^(n_held|held.*_n)", "^n_(participants|samples|total)$"),
    omit = c("event", "train", "dev"))
  e_col <- pick_col(nms,
    exact = c("n_heldout_events", "n_held_out_events", "heldout_events", "n_test_events",
              "test_events", "n_events", "event_count", "events"),
    patterns = c("held.*events", "^n_events$", "^events$"),
    omit = c("train", "dev"))
  if (!is.na(n_col)) norm[, n := num(raw[[n_col]])]
  if (!is.na(e_col)) norm[, events := num(raw[[e_col]])]

  # The pipeline's signed delta convention should be sensitivity minus primary.
  # Reconstruct only when the primary absolute value is explicitly supplied.
  for (metric in c("auc10", "mean_auc", "cindex")) {
    pcol <- paste0("primary_", metric)
    dcol <- paste0("delta_", metric)
    replacement <- norm[[metric]] - norm[[pcol]]
    idx <- which(is.na(norm[[dcol]]) & is.finite(replacement))
    if (length(idx)) set(norm, i = idx, j = dcol, value = replacement[idx])
    idx_check <- which(is.finite(norm[[dcol]]) & is.finite(replacement))
    if (length(idx_check) && any(abs(norm[[dcol]][idx_check] - replacement[idx_check]) > 0.0005)) {
      warning("Inconsistent precomputed delta vs sensitivity-primary for ", scenario,
              " metric ", metric, ". Inspect original columns; no subtraction was overwritten.")
    }
  }
  norm <- norm[outcome %in% OUTCOMES]
  if (!nrow(norm)) stop("No Overall or P1-P8 records found in ", csv,
                        ". Check outcome coding. Available raw outcomes: ",
                        paste(unique(oc), collapse = ", "))
  if (anyDuplicated(norm$outcome)) {
    stop("Multiple rows for the same outcome in ", csv,
         ". Inspect row-level subtype/model fields. This script will not pick an arbitrary model.")
  }
  if (!any(is.finite(norm$auc10) | is.finite(norm$mean_auc) | is.finite(norm$cindex))) {
    stop("No usable held-out AUC/C-index detected in ", csv,
         ". Inspect qa/input_column_inventory.csv and extend METRIC_SPECS.")
  }
  list(data = norm, columns = data.table(scenario = scenario, original = original_cols,
                                         standardized = nms), selected = selected,
       raw_n = nrow(raw), csv = csv)
}

# Read 10-year AUC from PRIMARY Stage 3 for the corresponding nine outcomes.
# Without this value a sensitivity-only auc_10y cannot yield delta_auc10.
read_primary_auc10 <- function() {
  out <- data.table(outcome = OUTCOMES, primary_auc10 = NA_real_, source_file = NA_character_)
  if (file.exists(PRIMARY_AUC10_TABLE)) {
    manual <- fread(PRIMARY_AUC10_TABLE, showProgress = FALSE)
    setnames(manual, std_name(names(manual)))
    if (!all(c("outcome", "primary_auc10") %in% names(manual))) {
      stop("Expected outcome,primary_auc10 in ", PRIMARY_AUC10_TABLE)
    }
    manual[, outcome := as.character(outcome)]
    if (anyDuplicated(manual$outcome)) stop("Duplicated outcomes in ", PRIMARY_AUC10_TABLE)
    for (oc in intersect(OUTCOMES, manual$outcome)) {
      v <- num(manual[outcome == oc, primary_auc10][[1]])
      if (is.finite(v)) out[outcome == oc, `:=`(primary_auc10 = v, source_file = PRIMARY_AUC10_TABLE)]
    }
  }
  for (oc in OUTCOMES) {
    if (is.finite(out[outcome == oc, primary_auc10][[1]])) next
    folder <- if (oc == "Overall") "comorbidity_all" else
      paste0("comorbidity_pattern_", substring(oc, 2), "_all")
    paths <- unlist(lapply(PRIMARY_RESULT_ROOTS, function(root) {
      file.path(root, c("Stage 3", "Stage_3", "Stage3"), folder, "fixed_horizon_performance.csv")
    }))
    hits <- paths[file.exists(paths)]
    if (!length(hits)) next
    values <- lapply(hits, function(path) {
      d <- fread(path, showProgress = FALSE)
      setnames(d, std_name(names(d)))
      if (!"time_dependent_auc" %in% names(d)) stop("Missing time_dependent_auc: ", path)
      if ("horizon_years" %in% names(d)) {
        yrs <- num(d$horizon_years)
      } else if ("horizon_days" %in% names(d)) {
        yrs <- num(d$horizon_days) / 365
      } else stop("Missing horizon_years/horizon_days: ", path)
      matched <- which(is.finite(yrs) & abs(yrs - 10) < 0.01)
      if (length(matched) != 1L) stop("Expected exactly one 10-year row in ", path)
      num(d$time_dependent_auc[matched])
    })
    v <- values[[1]]
    if (length(values) > 1L && any(vapply(values[-1], function(x) {
      is.finite(x) && is.finite(v) && abs(x - v) > 0.0005
    }, logical(1)))) {
      warning("Different primary 10-year AUC values found for ", oc,
              "; using first location: ", hits[[1]], ". Verify your data version.")
    }
    if (is.finite(v)) out[outcome == oc, `:=`(primary_auc10 = v, source_file = hits[[1]])]
  }
  out
}

format3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
format_delta <- function(x) ifelse(is.finite(x), sprintf("%+.3f", x), "NA")

load_normalized <- function() {
  if (!file.exists(CACHE)) stop("No Section 1 cache. Run Section 1 first.")
  readRDS(CACHE)
}

write_checkpoint <- function(completed, note) {
  writeLines(c(
    "# Section 10 sensitivity-analysis checkpoint",
    paste0("Completed: Section ", completed),
    paste0("At: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Note: ", note),
    "Next: run next Section, or set RUN_SECTIONS to a specific number."
  ), file.path(OUTPUT_ROOT, "checkpoint_10.md"), useBytes = TRUE)
}

# =============================================================================
# Section 1. Load For_R and QA / 读取结果和质量核查 ####
# =============================================================================
if (section_on(1)) {
  if (!dir.exists(INPUT_ROOT)) stop("Input directory missing: ", INPUT_ROOT)
  all_inputs <- list()
  inventory <- data.table(scenario = SCENARIOS,
    full_summary_exists = file.exists(file.path(INPUT_ROOT, SCENARIOS, "For_R", "sensitivity_full_summary_for_R.csv")),
    feature_stability_exists = file.exists(file.path(INPUT_ROOT, SCENARIOS, "For_R", "sensitivity_feature_stability_for_R.csv")),
    stage3b_combination_exists = file.exists(file.path(INPUT_ROOT, SCENARIOS, "For_R", "sensitivity_stage3b_combination_performance_for_R.csv")))
  fwrite(inventory, file.path(QA_DIR, "input_inventory.csv"))
  for (s in SCENARIOS) {
    if (!inventory[scenario == s, full_summary_exists]) {
      message("[NOT DOWNLOADED / SKIP] ", s, "/For_R/sensitivity_full_summary_for_R.csv")
      next
    }
    message("[READ] ", s)
    all_inputs[[s]] <- read_scenario(s)
  }
  if (!length(all_inputs)) {
    stop("No sensitivity_full_summary_for_R.csv found. Download the For_R directory for at least one scenario.")
  }
  combined <- rbindlist(lapply(all_inputs, function(x) x$data), fill = TRUE)
  combined[, scenario := factor(scenario, levels = SCENARIOS)]
  combined[, outcome := factor(outcome, levels = OUTCOMES)]
  setorder(combined, scenario, outcome)
  cols <- rbindlist(lapply(all_inputs, function(x) x$columns))
  chosen <- rbindlist(lapply(all_inputs, function(x) x$selected))
  fwrite(cols, file.path(QA_DIR, "input_column_inventory.csv"))
  fwrite(chosen, file.path(QA_DIR, "metric_source_columns.csv"))

  # Restore primary 10-year AUC from the actual primary Stage 3 (if present).
  # sensitivity_full_summary_for_R.csv exports sensitivity auc_10y but generally
  # does NOT export primary_auc_10y or delta_auc10.
  primary10 <- read_primary_auc10()
  fwrite(primary10, file.path(QA_DIR, "primary_10year_auc_sources.csv"))
  source_idx <- match(as.character(combined$outcome), primary10$outcome)
  restored <- primary10$primary_auc10[source_idx]
  discrepancy <- which(is.finite(combined$primary_auc10) & is.finite(restored) &
                       abs(combined$primary_auc10 - restored) > 0.0005)
  if (length(discrepancy)) {
    stop("Primary 10-year AUC differs between For_R and primary Stage 3 in rows ",
         paste(discrepancy, collapse = ", "), ". Please reconcile data versions.")
  }
  fill_idx <- which(!is.finite(combined$primary_auc10) & is.finite(restored))
  if (length(fill_idx)) combined[fill_idx, primary_auc10 := restored[fill_idx]]
  can_calc <- which(is.finite(combined$auc10) & is.finite(combined$primary_auc10))
  if (length(can_calc)) {
    actual_delta <- combined$auc10[can_calc] - combined$primary_auc10[can_calc]
    existing <- combined$delta_auc10[can_calc]
    bad <- which(is.finite(existing) & abs(existing - actual_delta) > 0.0005)
    if (length(bad)) stop("Conflicting delta_auc10 values in rows ",
                          paste(can_calc[bad], collapse = ", "), ". Check source files.")
    combined[can_calc, delta_auc10 := actual_delta]
  }
  if (!any(is.finite(combined$delta_auc10))) {
    warning("10-year AUC differences are unavailable. See qa/primary_10year_auc_sources.csv. ",
            "Download PRIMARY Stage 3/<outcome>/fixed_horizon_performance.csv, ",
            "or provide Result/Sensitivity/primary_10year_auc.csv ",
            "with outcome,primary_auc10. Do NOT substitute mean AUC.")
    fwrite(data.table(outcome = OUTCOMES, primary_auc10 = rep(NA_real_, length(OUTCOMES))),
           file.path(QA_DIR, "primary_10year_auc_TEMPLATE.csv"), na = "")
  }

  for (v in c("auc10", "mean_auc", "cindex", "ibs")) {
    idx <- which(is.finite(combined[[v]]) & (combined[[v]] < 0 | combined[[v]] > 1))
    if (length(idx)) warning(v, " has values outside [0,1] in rows: ", paste(idx, collapse = ", "))
  }
  bad_n <- combined[is.finite(n) & is.finite(events) & (events < 0 | n < 1 | events > n)]
  if (nrow(bad_n)) warning("Invalid n/events in ", nrow(bad_n), " rows; inspect QA.")
  coverage <- combined[, .(n_outcomes = .N, missing_auc10 = sum(!is.finite(auc10)),
       missing_mean_auc = sum(!is.finite(mean_auc)),
       missing_cindex = sum(!is.finite(cindex)),
       missing_delta_auc10 = sum(!is.finite(delta_auc10)),
       missing_delta_mean_auc = sum(!is.finite(delta_mean_auc)),
       missing_delta_cindex = sum(!is.finite(delta_cindex))), by = scenario]
  fwrite(coverage, file.path(QA_DIR, "coverage.csv"))
  if (any(is.finite(combined$delta_auc10) & !is.finite(combined$primary_auc10)) ||
      any(is.finite(combined$delta_cindex) & !is.finite(combined$primary_cindex))) {
    warning("Some reported difference columns lack an explicit primary value in For_R. ",
            "Verify that source deltas are sensitivity minus primary before interpreting signs.")
  }
  fwrite(combined, file.path(QA_DIR, "normalized_sensitivity_metrics.csv"))
  saveRDS(combined, CACHE)
  message("[QA] Available scenarios: ", paste(as.character(unique(combined$scenario)), collapse = ", "))
  message("[QA] Number of outcomes per scenario:")
  print(coverage)
  write_checkpoint(1, "Normalized source metrics and schema QA written; missing scenarios are skipped.")
}

# =============================================================================
# Section 2. One publication-ready table / 生成一张补充表 ####
# =============================================================================
if (section_on(2)) {
  z <- load_normalized()
  table <- copy(z)
  table[, `:=`(
    Outcome = as.character(outcome),
    `Sensitivity analysis` = unname(SCENARIO_LABEL[as.character(scenario)]),
    `Held-out N` = as.integer(round(n)),
    `Held-out events` = as.integer(round(events)),
    `10-year AUC` = format3(auc10),
    `Change in 10-year AUC` = format_delta(delta_auc10),
    `Mean time-dependent AUC` = format3(mean_auc),
    `Change in mean AUC` = format_delta(delta_mean_auc),
    `C-index` = format3(cindex),
    `Change in C-index` = format_delta(delta_cindex)
  )]
  keep <- c("Outcome", "Sensitivity analysis", "Held-out N", "Held-out events",
            "10-year AUC", "Change in 10-year AUC")
  if (isTRUE(INCLUDE_MEAN_AUC_IN_TABLE)) {
    keep <- c(keep, "Mean time-dependent AUC", "Change in mean AUC")
  }
  keep <- c(keep, "C-index", "Change in C-index")
  table <- table[, ..keep]
  fwrite(table, file.path(TABLE_DIR, "Table_Sensitivity_performance.csv"), na = "")
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sensitivity performance")
    openxlsx::writeData(wb, 1, table, withFilter = FALSE)
    openxlsx::setColWidths(wb, 1, cols = 1:ncol(table), widths = c(13, 30, 16, 18, rep(22, ncol(table) - 4)))
    openxlsx::freezePane(wb, 1, firstRow = TRUE)
    openxlsx::addStyle(wb, 1, openxlsx::createStyle(textDecoration = "bold",
      fgFill = "#DFE8EF", border = "Bottom"),
      rows = 1, cols = 1:ncol(table), gridExpand = TRUE)
    openxlsx::saveWorkbook(wb, file.path(TABLE_DIR, "Table_Sensitivity_performance.xlsx"), overwrite = TRUE)
  }
  write_checkpoint(2, paste("Table exported with", nrow(table), "outcome-scenario rows."))
}

# =============================================================================
# Section 3. One supplementary figure (2 panels) / 生成一张补充图 ####
# =============================================================================
if (section_on(3)) {
  z <- load_normalized()
  # Never silently switch from 10-year AUC to a different mean-AUC grid.
  if (!PLOT_METRIC %in% c("auc10", "mean_auc")) stop("Invalid PLOT_METRIC")
  use_ten <- identical(PLOT_METRIC, "auc10")
  auc_diff <- if (use_ten) z$delta_auc10 else z$delta_mean_auc
  auc_abs <- if (use_ten) z$auc10 else z$mean_auc
  auc_label <- if (use_ten) "10-year AUC" else "Mean time-dependent AUC"
  if (!any(is.finite(auc_abs))) {
    stop("Missing ", auc_label, " in sensitivity summary. Check qa/metric_source_columns.csv. ",
         "Change PLOT_METRIC only after confirming the same time grid in main and sensitivity analyses.")
  }
  if (use_ten && !any(is.finite(auc_diff))) {
    stop("10-year AUC is present, but the 10-year AUC CHANGE is missing for all scenarios. ",
         "No blank AUC panel will be generated. Place primary Stage 3 ",
         "fixed_horizon_performance.csv under Result/Stage 3/<outcome>/, ",
         "or supply Result/Sensitivity/primary_10year_auc.csv ",
         "with columns outcome,primary_auc10 (Overall,P1,...,P8). ",
         "Then rerun Section 1 and Section 3. ",
         "See qa/primary_10year_auc_sources.csv and qa/coverage.csv.")
  }
  metric_data <- rbindlist(list(
    data.table(scenario = z$scenario, outcome = z$outcome, metric = auc_label,
               delta = auc_diff, absolute = auc_abs),
    data.table(scenario = z$scenario, outcome = z$outcome, metric = "C-index",
               delta = z$delta_cindex, absolute = z$cindex)
  ))
  # If no primary deltas are available, show observed sensitivity performance,
  # not an invented change relative to the primary analysis.
  if (all(!is.finite(metric_data$delta))) {
    metric_data[, plot_value := absolute]
    display_kind <- "Observed performance in sensitivity analyses"
    axis_legend <- "Held-out discrimination"
    midpoint <- .75
  } else {
    metric_data[, plot_value := delta]
    display_kind <- "Change relative to the primary analysis"
    axis_legend <- "Sensitivity minus primary"
    midpoint <- 0
  }
  metric_data[, scenario_label := factor(unname(SCENARIO_LABEL[as.character(scenario)]),
                                  levels = unname(SCENARIO_LABEL[SCENARIOS]))]
  metric_data[, outcome_label := factor(as.character(outcome), levels = rev(OUTCOMES))]
  metric_data[, metric := factor(metric, levels = c(auc_label, "C-index"))]
  # Color limit shared across panels; neutral mid-grey avoids over-white legends.
  if (midpoint == 0) {
    max_abs <- max(abs(metric_data$plot_value[is.finite(metric_data$plot_value)]), 0.001)
    max_abs <- ceiling(max_abs * 100) / 100
    low <- -max_abs; high <- max_abs
  } else {
    low <- 0.50; high <- 1.00
  }
  fmt_tile <- function(v) {
    ifelse(is.finite(v), if (midpoint == 0) sprintf("%+.3f", v) else sprintf("%.3f", v), "")
  }
  metric_data[, tile_label := fmt_tile(plot_value)]

  # Publication layout: short multi-line x labels avoid left-edge clipping.
  # 展示标签换行、不旋转，既防止最左侧被截断，也避免长文字相互重叠。
  scenario_axis_labels <- c(
    "Disease-free at baseline" = "Disease-free\nat baseline",
    "No entirely missing domain" = "No entirely\nmissing\ndomain",
    "Two-year landmark" = "Two-year\nlandmark",
    "Simple imputation" = "Simple\nimputation"
  )

  # Set only three colorbar labels; remove all colorbar tick lines.
  # 色标刻度仅保留低值/零点/高值；guide_colorbar(ticks = FALSE) 删除白色竖线。
  color_breaks <- if (midpoint == 0) c(low, 0, high) else c(low, midpoint, high)
  color_labels <- if (midpoint == 0) {
    ifelse(abs(color_breaks) < 1e-10, "0.00", sprintf("%+.2f", color_breaks))
  } else {
    sprintf("%.2f", color_breaks)
  }

  p <- ggplot(metric_data, aes(x = scenario_label, y = outcome_label, fill = plot_value)) +
    geom_tile(color = "white", linewidth = .8, width = .98, height = .98, na.rm = FALSE) +
    geom_text(aes(label = tile_label), size = 2.7, fontface = "bold", na.rm = TRUE) +
    facet_grid(. ~ metric, scales = "free_x") +
    scale_fill_gradient2(
      low = "#477EA6", mid = "#D9DDE1", high = "#B75C56",
      midpoint = midpoint, limits = c(low, high),
      breaks = color_breaks, labels = color_labels,
      na.value = "#E9ECEF", name = axis_legend,
      oob = scales::squish
    ) +
    scale_x_discrete(
      drop = TRUE,
      labels = scenario_axis_labels,
      expand = expansion(add = c(0.35, 0.35))
    ) +
    guides(fill = guide_colorbar(
      title.position = "top", title.hjust = 0.5,
      barwidth = grid::unit(78, "mm"),
      barheight = grid::unit(4, "mm"),
      label.position = "bottom",
      ticks = FALSE, frame.colour = NA
    )) +
    labs(x = NULL, y = NULL, title = display_kind) +
    theme_minimal(base_family = "sans", base_size = 10) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.spacing.x = grid::unit(4, "mm"),
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "#E6E9ED", color = NA),
      # Two-line labels (rather than angled text) keep the leftmost label fully visible.
      axis.text.x = element_text(
        face = "bold", angle = 0, hjust = 0.5, vjust = 1,
        size = 7.7, lineheight = 0.96, margin = margin(t = 7)
      ),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 9, margin = margin(b = 3)),
      legend.text = element_text(face = "bold", size = 8.5),
      legend.margin = margin(t = 4),
      # Extra side margins protect axis labels and ensure nothing is cropped.
      plot.margin = margin(6, 14, 10, 18)
    )
  base <- file.path(FIG_DIR, "Figure_Sensitivity_performance")
  w <- 183 / 25.4; h <- 138 / 25.4
  pdf(paste0(base, ".pdf"), width = w, height = h, useDingbats = FALSE, family = "Helvetica")
  print(p); dev.off()
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(paste0(base, ".png"), width = 183, height = 138, units = "mm", res = 300, background = "white")
    print(p); dev.off()
    ragg::agg_tiff(paste0(base, ".tiff"), width = 183, height = 138, units = "mm", res = 600,
                   compression = "lzw", background = "white")
    print(p); dev.off()
  } else {
    ggsave(paste0(base, ".png"), plot = p, width = w, height = h, dpi = 300, bg = "white")
    ggsave(paste0(base, ".tiff"), plot = p, width = w, height = h, dpi = 600, bg = "white")
  }
  fwrite(metric_data, file.path(QA_DIR, "figure_source_data.csv"))
  writeLines(c(
    paste0("Figure metric: ", auc_label, " and C-index"),
    paste0("Figure data: ", display_kind),
    paste0("Scenarios present: ", paste(unique(as.character(z$scenario)), collapse = ", ")),
    "No uncertainty intervals or statistical significance are implied by colored cells."
  ), file.path(QA_DIR, "figure_metadata.txt"))
  write_checkpoint(3, "Figure exported in PDF, PNG, 600-dpi TIFF.")
}

# =============================================================================
# Section 4. Supplemental caption, feature stability QA, final checkpoint ####
# =============================================================================
if (section_on(4)) {
  z <- load_normalized()
  feature_sets <- list()
  for (s in SCENARIOS) {
    f <- file.path(INPUT_ROOT, s, "For_R", "sensitivity_feature_stability_for_R.csv")
    if (!file.exists(f)) next
    dt <- fread(f, showProgress = FALSE)
    dt[, sensitivity_analysis := s]
    feature_sets[[s]] <- dt
  }
  if (length(feature_sets)) {
    fwrite(rbindlist(feature_sets, fill = TRUE, use.names = TRUE),
           file.path(QA_DIR, "feature_stability_QA_only.csv"))
  }
  text <- c(
    "Suggested title: Robustness of multimorbidity prediction across sensitivity analyses",
    "",
    "Supplementary Table SXX. Predictive discrimination of the integrated model for overall and pattern-specific incident multimorbidity across sensitivity analyses.",
    "Footnotes: Results are based on the corresponding independent held-out evaluation set for each sensitivity analysis. Outcomes are ordered Overall, P1-P8. The four sensitivity analyses restrict the cohort to participants free of all 36 chronic diseases at baseline, exclude participants lacking data in any entire predictor domain, apply a two-year landmark, and replace the primary iterative imputation procedure with median imputation, respectively. AUC, area under the receiver operating characteristic curve; C-index, concordance index. Differences are sensitivity estimates minus the corresponding primary estimates where such values are provided in the For_R summaries. Because cohorts and risk-set definitions differ, comparisons are descriptive; no paired significance tests are implied.",
    "",
    "Supplementary Figure SXX. Changes in held-out predictive discrimination across sensitivity analyses.",
    "Legend: Cells show the difference in the indicated discrimination metric between each sensitivity analysis and the primary analysis, if primary-analysis differences are available. Positive differences indicate higher sensitivity-analysis discrimination. Grey cells indicate unavailable values. If deltas are unavailable, the figure instead displays absolute performance, as documented in qa/figure_metadata.txt. The two-year landmark uses time since the landmark rather than baseline recruitment; corresponding AUC horizons should be interpreted accordingly.",
    "",
    "Important verification: Check the metric_source_columns.csv to ensure the same model (integrated final combination) and the same AUC definition/time horizon are selected across scenarios. The original sensitivity pipeline may use a mean AUC integration grid different from later Stage 3B 3-10-year outputs. Do not combine those definitions in a figure. If the original For_R export lacks the desired metric, regenerate it on the server instead of fabricating it locally.",
    "",
    paste0("Scenarios currently detected: ", paste(unique(as.character(z$scenario)), collapse = ", ")),
    paste0("Outcomes currently detected: ", paste(unique(as.character(z$outcome)), collapse = ", "))
  )
  writeLines(text, file.path(OUTPUT_ROOT, "Supplementary_Sensitivity_caption_and_notes.txt"), useBytes = TRUE)
  write_checkpoint(4, "All requested table, figure, QA, notes generated from available For_R directories.")
  message("[DONE] Outputs in: ", OUTPUT_ROOT)
}

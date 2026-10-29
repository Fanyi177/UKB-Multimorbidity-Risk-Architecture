library(data.table)

# ============================================================
# 5. Definition of multimorbidity patterns and follow-up time
# Final K = 8 spectral-clustering solution
# ============================================================
#
# IMPORTANT DESIGN
# 1. Final disease-pattern solution: K = 8.
# 2. Pattern memberships follow the latest clustering result after restoring
#    the broader Hepatitis and Meningitis definitions:
#       disease_cluster_spe_corr_n_cluster=8_combination(3).csv
# 3. MAIN analysis files are stored directly under Source/.
# 4. SENSITIVITY analysis files are stored under Source/sensitivity_analysis/.
# 5. The overall multimorbidity variables created upstream are retained:
#       comorbidity
#       comorbidity_time
#       follow_time
# 6. A pattern event must occur:
#       disease date > recruitment_date
#       disease date <= censor_date
# 7. censor_date is inherited from the upstream disease-construction script:
#       min(administrative_end, death_date)
# 8. For pattern-specific incident multimorbidity:
#       baseline pattern disease count = 0:
#         second incident disease in the same pattern -> event
#       baseline pattern disease count = 1:
#         first additional incident disease in the same pattern -> event
# ============================================================


# ============================================================
# 0. Paths
# ============================================================

project_dir <- "/lustre/home/2411110208/UK_biobank"
cov_dir <- file.path(project_dir, "cov_data")

source_root <- file.path(
  project_dir,
  "code",
  "6.select_and_weight_the_env_vars_of_multimorbidity",
  "Source"
)

# MAIN analysis files are stored directly under Source/
source_dir <- source_root

# Sensitivity analysis has its own subfolder
sensitivity_source_dir <- file.path(
  source_root,
  "sensitivity_analysis"
)

dir.create(
  source_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  sensitivity_source_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

master_disease_file <- file.path(
  cov_dir,
  "UKB_diseases_data_all_strict_incident.rds"
)

if (!file.exists(master_disease_file)) {
  stop(
    paste0(
      "Cannot find master disease dataset:\n",
      master_disease_file
    )
  )
}


# ============================================================
# 1. Expected 36 diseases
# ============================================================

expected_36_diseases <- c(
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


# ============================================================
# 2. Final K = 8 pattern definition
# Latest clustering after restoring broader Hepatitis / Meningitis definitions
# disease_cluster_spe_corr_n_cluster=8_combination(3).csv
# ============================================================

pattern_diseases_K8 <- list(

  # P1: Neurodegenerative pattern
  P1 = c(
    "Dementia",
    "Parkinson_disease"
  ),

  # P2: Cardiac / cardiovascular pattern
  P2 = c(
    "Angina",
    "Atrial_fibrillation",
    "Heart_failure",
    "Myocardial_infarction"
  ),

  # P3: Mixed multisystem chronic-disease pattern
  P3 = c(
    "Chronic_kidney_disease",
    "Chronic_sinusitis",
    "Diabetes",
    "Eczema_or_dermatitis",
    "Glaucoma",
    "Hypertension",
    "Inflammatory_bowel_disease",
    "Irritable_bowel_syndrome",
    "Migraine",
    "Peripheral_vascular_disease",
    "Prostate_problem",
    "Tuberculosis",
    "Vestibular_disorder",
    "overall_cancer"
  ),

  # P4: Hepatic pattern
  P4 = c(
    "Cirrhosis",
    "Hepatitis"
  ),

  # P5: Systemic immune / musculoskeletal-endocrine pattern
  P5 = c(
    "Anaemia",
    "Multiple_sclerosis",
    "Osteoporosis",
    "Rheumatoid_arthritis",
    "Thyroid_problem"
  ),

  # P6: Psychiatric pattern
  P6 = c(
    "Anxiety_or_panic_attacks",
    "Depression",
    "Schizophrenia"
  ),

  # P7: Chronic respiratory pattern
  P7 = c(
    "Asthma",
    "Bronchiectasis",
    "Chronic_obstructive_pulmonary_disease"
  ),

  # P8: Neurological / cerebrovascular pattern
  P8 = c(
    "Epilepsy",
    "Meningitis",
    "Stroke"
  )
)


# ============================================================
# Clinical labels used only for descriptive output / QA
# They do NOT affect event construction.
# ============================================================

pattern_labels_K8 <- data.table(
  pattern_id = paste0("P", 1:8),
  pattern_label = c(
    "Neurodegenerative",
    "Cardiac/cardiovascular",
    "Mixed multisystem chronic disease",
    "Hepatic",
    "Systemic immune/musculoskeletal-endocrine",
    "Psychiatric",
    "Chronic respiratory",
    "Neurological/cerebrovascular"
  )
)

# ============================================================
# 3. Helper functions
# ============================================================

disease_to_time_col <- function(disease_names) {

  out <- paste0(
    disease_names,
    "_time"
  )

  out[
    disease_names == "overall_cancer"
  ] <- "cancer_date"

  out
}


validate_pattern_definition <- function(
    pattern_diseases,
    K,
    solution_label
) {

  all_pattern_diseases <- unlist(
    pattern_diseases,
    use.names = FALSE
  )

  if (length(pattern_diseases) != K) {
    stop(
      paste0(
        solution_label,
        ": expected ",
        K,
        " patterns, found ",
        length(pattern_diseases),
        "."
      )
    )
  }

  if (length(all_pattern_diseases) != 36L) {
    stop(
      paste0(
        solution_label,
        ": expected exactly 36 disease entries, found ",
        length(all_pattern_diseases),
        "."
      )
    )
  }

  if (anyDuplicated(all_pattern_diseases) > 0L) {

    duplicated_diseases <- unique(
      all_pattern_diseases[
        duplicated(all_pattern_diseases)
      ]
    )

    stop(
      paste0(
        solution_label,
        ": duplicated diseases across patterns: ",
        paste(
          duplicated_diseases,
          collapse = ", "
        )
      )
    )
  }

  missing_from_patterns <- setdiff(
    expected_36_diseases,
    all_pattern_diseases
  )

  extra_in_patterns <- setdiff(
    all_pattern_diseases,
    expected_36_diseases
  )

  if (
    length(missing_from_patterns) > 0L |
      length(extra_in_patterns) > 0L
  ) {

    stop(
      paste0(
        solution_label,
        ": disease coverage check failed.",
        "\nMissing: ",
        paste(
          missing_from_patterns,
          collapse = ", "
        ),
        "\nExtra: ",
        paste(
          extra_in_patterns,
          collapse = ", "
        )
      )
    )
  }

  definition_table <- rbindlist(
    lapply(
      seq_along(pattern_diseases),
      function(i) {

        data.table(
          solution = solution_label,
          pattern_id = paste0("P", i),
          pattern_number = i,
          disease = pattern_diseases[[i]]
        )
      }
    )
  )

  cat(
    "\n",
    solution_label,
    " disease coverage check passed.",
    "\nPatterns:",
    length(pattern_diseases),
    "\nTotal diseases:",
    nrow(definition_table),
    "\nOverall cancer included:",
    "overall_cancer" %in% definition_table$disease,
    "\n",
    sep = ""
  )

  definition_table
}


read_eids_from_source <- function(source_dir) {

  rds_file <- file.path(
    source_dir,
    "UKB_diseases_data.rds"
  )

  csv_file <- file.path(
    source_dir,
    "UKB_diseases_data.csv"
  )

  if (file.exists(rds_file)) {

    x <- readRDS(
      rds_file
    )

    setDT(
      x
    )

  } else if (file.exists(csv_file)) {

    x <- fread(
      csv_file
    )

  } else {

    stop(
      paste0(
        "Cannot find cohort disease file in:\n",
        source_dir
      )
    )
  }

  if (!"eid" %in% names(x)) {
    stop(
      paste0(
        "No eid column in source cohort: ",
        source_dir
      )
    )
  }

  if (anyDuplicated(x$eid) > 0L) {
    stop(
      paste0(
        "Duplicated eid in source cohort: ",
        source_dir
      )
    )
  }

  x[
    ,
    .(eid)
  ]
}


prepare_cohort_from_master <- function(
    master_data,
    cohort_eids,
    cohort_label
) {

  idx <- match(
    cohort_eids$eid,
    master_data$eid
  )

  if (anyNA(idx)) {
    stop(
      paste0(
        cohort_label,
        ": ",
        sum(is.na(idx)),
        " eid(s) are absent from master disease dataset."
      )
    )
  }

  out <- copy(
    master_data[
      idx
    ]
  )

  if (!identical(out$eid, cohort_eids$eid)) {
    stop(
      paste0(
        cohort_label,
        ": participant order mismatch after matching."
      )
    )
  }

  out
}


construct_one_pattern <- function(
    DT,
    diseases,
    pattern_number
) {

  time_cols <- disease_to_time_col(
    diseases
  )

  missing_time_cols <- setdiff(
    time_cols,
    names(DT)
  )

  if (length(missing_time_cols) > 0L) {
    stop(
      paste0(
        "Pattern ",
        pattern_number,
        " is missing disease-time columns: ",
        paste(
          missing_time_cols,
          collapse = ", "
        )
      )
    )
  }

  n <- nrow(
    DT
  )

  recruit_num <- as.numeric(
    DT$recruitment_date
  )

  censor_num <- as.numeric(
    DT$censor_date
  )

  baseline_n <- integer(
    n
  )

  incident_n <- integer(
    n
  )

  after_censor_n <- integer(
    n
  )

  first_incident_num <- rep(
    Inf,
    n
  )

  second_incident_num <- rep(
    Inf,
    n
  )

  for (time_col in time_cols) {

    x <- as.numeric(
      DT[[time_col]]
    )

    recorded <- !is.na(
      x
    )

    baseline_flag <-
      recorded &
      !is.na(recruit_num) &
      x <= recruit_num

    incident_flag <-
      recorded &
      !is.na(recruit_num) &
      !is.na(censor_num) &
      x > recruit_num &
      x <= censor_num

    after_censor_flag <-
      recorded &
      !is.na(censor_num) &
      x > censor_num

    baseline_n <-
      baseline_n +
      as.integer(
        baseline_flag
      )

    incident_n <-
      incident_n +
      as.integer(
        incident_flag
      )

    after_censor_n <-
      after_censor_n +
      as.integer(
        after_censor_flag
      )

    candidate <- x

    candidate[
      !incident_flag
    ] <- Inf

    old_first <- first_incident_num

    first_incident_num <- pmin(
      old_first,
      candidate
    )

    second_incident_num <- pmin(
      second_incident_num,
      pmax(
        old_first,
        candidate
      )
    )
  }

  first_incident_num[
    !is.finite(first_incident_num)
  ] <- NA_real_

  second_incident_num[
    !is.finite(second_incident_num)
  ] <- NA_real_

  # The strict-incident cohort already excludes overall baseline
  # multimorbidity >=2 across all 36 diseases.
  if (any(baseline_n >= 2L, na.rm = TRUE)) {
    stop(
      paste0(
        "Pattern ",
        pattern_number,
        ": found participant(s) with >=2 baseline diseases."
      )
    )
  }

  event <- as.integer(
    (
      baseline_n == 0L &
        incident_n >= 2L
    ) |
      (
        baseline_n == 1L &
          incident_n >= 1L
      )
  )

  event_date_num <- rep(
    NA_real_,
    n
  )

  idx_baseline0_event <- which(
    event == 1L &
      baseline_n == 0L
  )

  idx_baseline1_event <- which(
    event == 1L &
      baseline_n == 1L
  )

  event_date_num[
    idx_baseline0_event
  ] <- second_incident_num[
    idx_baseline0_event
  ]

  event_date_num[
    idx_baseline1_event
  ] <- first_incident_num[
    idx_baseline1_event
  ]

  event_date <- as.Date(
    event_date_num,
    origin = "1970-01-01"
  )

  followup_end_num <- ifelse(
    event == 1L,
    event_date_num,
    censor_num
  )

  followup_end_date <- as.Date(
    followup_end_num,
    origin = "1970-01-01"
  )

  follow_time <- as.numeric(
    followup_end_date -
      DT$recruitment_date
  )

  # If an event occurs exactly on death_date, event takes priority.
  death_censored <- as.integer(
    event == 0L &
      !is.na(DT$death_date) &
      !is.na(DT$censor_date) &
      DT$censor_date == DT$death_date
  )

  administrative_censored <- as.integer(
    event == 0L &
      death_censored == 0L
  )

  censor_reason <- fifelse(
    event == 1L,
    "Event",
    fifelse(
      death_censored == 1L,
      "Death",
      "Administrative end"
    )
  )

  prefix <- paste0(
    "comorbidity_pattern_",
    pattern_number
  )

  DT[
    ,
    (prefix) :=
      event
  ]

  DT[
    ,
    (paste0(prefix, "_event_date")) :=
      event_date
  ]

  # Backward-compatible:
  # *_time = event date for cases and censor_date for non-cases.
  DT[
    ,
    (paste0(prefix, "_time")) :=
      followup_end_date
  ]

  DT[
    ,
    (paste0(prefix, "_follow_time")) :=
      follow_time
  ]

  DT[
    ,
    (paste0(prefix, "_baseline_disease_count")) :=
      baseline_n
  ]

  DT[
    ,
    (paste0(prefix, "_incident_disease_count")) :=
      incident_n
  ]

  DT[
    ,
    (paste0(prefix, "_after_censor_disease_count")) :=
      after_censor_n
  ]

  DT[
    ,
    (paste0(prefix, "_death_censored")) :=
      death_censored
  ]

  DT[
    ,
    (paste0(prefix, "_censor_reason")) :=
      censor_reason
  ]

  # QA: event must be inside (recruitment_date, censor_date].
  if (
    any(
      event == 1L &
        (
          is.na(event_date) |
            event_date <= DT$recruitment_date |
            event_date > DT$censor_date
        ),
      na.rm = TRUE
    )
  ) {
    stop(
      paste0(
        "Pattern ",
        pattern_number,
        ": event date outside valid follow-up."
      )
    )
  }

  if (
    any(
      is.na(follow_time) |
        follow_time <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      paste0(
        "Pattern ",
        pattern_number,
        ": non-positive or missing follow-up detected."
      )
    )
  }

  summary_row <- data.table(
    pattern_id = paste0(
      "P",
      pattern_number
    ),
    pattern_number = pattern_number,
    n_diseases = length(
      diseases
    ),
    diseases = paste(
      diseases,
      collapse = " + "
    ),
    N = n,
    events = sum(
      event == 1L,
      na.rm = TRUE
    ),
    non_events = sum(
      event == 0L,
      na.rm = TRUE
    ),
    event_percent = 100 * sum(
      event == 1L,
      na.rm = TRUE
    ) / n,
    death_censored_non_events = sum(
      death_censored == 1L,
      na.rm = TRUE
    ),
    administrative_censored_non_events = sum(
      administrative_censored == 1L,
      na.rm = TRUE
    ),
    baseline_0_N = sum(
      baseline_n == 0L,
      na.rm = TRUE
    ),
    baseline_1_N = sum(
      baseline_n == 1L,
      na.rm = TRUE
    ),
    participants_with_post_censor_pattern_disease = sum(
      after_censor_n > 0L,
      na.rm = TRUE
    ),
    median_followup_years = median(
      follow_time / 365.25,
      na.rm = TRUE
    )
  )

  list(
    data = DT,
    summary = summary_row
  )
}


construct_all_patterns <- function(
    DT,
    pattern_diseases,
    cohort_label,
    solution_label
) {

  DT <- copy(
    DT
  )

  setDT(
    DT
  )

  required_base_cols <- c(
    "eid",
    "recruitment_date",
    "recruitment_centre",
    "administrative_end",
    "death_date",
    "censor_date",
    "valid_followup",
    "baseline_disease_count"
  )

  required_time_cols <- disease_to_time_col(
    expected_36_diseases
  )

  missing_cols <- setdiff(
    c(
      required_base_cols,
      expected_36_diseases,
      required_time_cols
    ),
    names(DT)
  )

  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        solution_label,
        " / ",
        cohort_label,
        ": required columns are missing:\n",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  if (
    any(
      !DT$valid_followup |
        is.na(DT$censor_date) |
        DT$censor_date <= DT$recruitment_date,
      na.rm = TRUE
    )
  ) {
    stop(
      paste0(
        solution_label,
        " / ",
        cohort_label,
        ": invalid follow-up found."
      )
    )
  }

  if (
    any(
      DT$baseline_disease_count >= 2L,
      na.rm = TRUE
    )
  ) {
    stop(
      paste0(
        solution_label,
        " / ",
        cohort_label,
        ": baseline multimorbidity >=2 still present."
      )
    )
  }

  pattern_summaries <- vector(
    "list",
    length(pattern_diseases)
  )

  for (i in seq_along(pattern_diseases)) {

    cat(
      "\n",
      solution_label,
      " / ",
      cohort_label,
      ": constructing P",
      i,
      " ...\n",
      sep = ""
    )

    tmp <- construct_one_pattern(
      DT = DT,
      diseases = pattern_diseases[[i]],
      pattern_number = i
    )

    DT <- tmp$data
    pattern_summaries[[i]] <- tmp$summary
  }

  pattern_summary <- rbindlist(
    pattern_summaries,
    use.names = TRUE,
    fill = TRUE
  )

  pattern_summary[
    ,
    solution :=
      solution_label
  ]

  setcolorder(
    pattern_summary,
    c(
      "solution",
      setdiff(
        names(pattern_summary),
        "solution"
      )
    )
  )

  list(
    data = DT,
    summary = pattern_summary
  )
}


save_pattern_outputs <- function(
    result,
    definition_table,
    source_dir,
    cohort_label,
    K
) {

  DT <- result$data
  pattern_summary <- result$summary

  pattern_count_table <- rbindlist(
    lapply(
      seq_len(K),
      function(i) {

        event_col <- paste0(
          "comorbidity_pattern_",
          i
        )

        data.table(
          solution = paste0(
            "K",
            K
          ),
          pattern_id = paste0(
            "P",
            i
          ),
          event = c(
            0L,
            1L
          ),
          N = c(
            sum(
              DT[[event_col]] == 0L,
              na.rm = TRUE
            ),
            sum(
              DT[[event_col]] == 1L,
              na.rm = TRUE
            )
          )
        )
      }
    )
  )

  fwrite(
    definition_table,
    file.path(
      source_dir,
      paste0(
        "comorbidity_pattern_definition_K",
        K,
        ".csv"
      )
    )
  )

  fwrite(
    pattern_summary,
    file.path(
      source_dir,
      paste0(
        "comorbidity_pattern_followup_summary_K",
        K,
        ".csv"
      )
    )
  )

  fwrite(
    pattern_count_table,
    file.path(
      source_dir,
      paste0(
        "combined_comorbidity_pattern_count_K",
        K,
        ".csv"
      )
    )
  )

  # IMPORTANT:
  # Separate K-specific disease files avoid overwriting one solution with another.
  fwrite(
    DT,
    file.path(
      source_dir,
      paste0(
        "UKB_diseases_data_with_patterns_K",
        K,
        ".csv"
      )
    )
  )

  saveRDS(
    DT,
    file.path(
      source_dir,
      paste0(
        "UKB_diseases_data_with_patterns_K",
        K,
        ".rds"
      )
    )
  )

  cat(
    "\n============================================================\n",
    cohort_label,
    " / K=",
    K,
    " PATTERN OUTCOME CONSTRUCTION COMPLETED\n",
    "N: ",
    nrow(DT),
    "\n",
    sep = ""
  )

  print(
    pattern_summary[
      ,
      .(
        pattern_id,
        n_diseases,
        events,
        event_percent,
        non_events,
        death_censored_non_events,
        administrative_censored_non_events,
        median_followup_years
      )
    ]
  )

  cat(
    "Outputs saved to:\n",
    source_dir,
    "\n============================================================\n"
  )

  invisible(
    list(
      data = DT,
      summary = pattern_summary,
      counts = pattern_count_table
    )
  )
}


cross_cohort_consistency_QA <- function(
    main_saved,
    sensitivity_saved,
    K,
    solution_label
) {

  main_pattern_data <- main_saved$data
  sensitivity_pattern_data <- sensitivity_saved$data

  idx_sensitivity_in_main <- match(
    sensitivity_pattern_data$eid,
    main_pattern_data$eid
  )

  if (anyNA(idx_sensitivity_in_main)) {
    stop(
      paste0(
        solution_label,
        ": sensitivity participants could not all be matched to main cohort."
      )
    )
  }

  for (i in seq_len(K)) {

    cols_to_check <- c(
      paste0(
        "comorbidity_pattern_",
        i
      ),
      paste0(
        "comorbidity_pattern_",
        i,
        "_event_date"
      ),
      paste0(
        "comorbidity_pattern_",
        i,
        "_time"
      ),
      paste0(
        "comorbidity_pattern_",
        i,
        "_follow_time"
      )
    )

    for (col in cols_to_check) {

      main_value <- main_pattern_data[
        idx_sensitivity_in_main,
        get(col)
      ]

      sensitivity_value <- sensitivity_pattern_data[
        ,
        get(col)
      ]

      if (
        !isTRUE(
          all.equal(
            main_value,
            sensitivity_value,
            check.attributes = FALSE
          )
        )
      ) {
        stop(
          paste0(
            solution_label,
            ": main/sensitivity consistency check failed for ",
            col,
            "."
          )
        )
      }
    }
  }

  cat(
    "\n",
    solution_label,
    ": MAIN/SENSITIVITY consistency QA passed.\n",
    sep = ""
  )
}


run_pattern_solution <- function(
    pattern_diseases,
    K,
    main_data,
    sensitivity_data,
    source_dir,
    sensitivity_source_dir
) {

  solution_label <- paste0(
    "K",
    K
  )

  definition_table <- validate_pattern_definition(
    pattern_diseases = pattern_diseases,
    K = K,
    solution_label = solution_label
  )

  main_result <- construct_all_patterns(
    DT = main_data,
    pattern_diseases = pattern_diseases,
    cohort_label = "MAIN",
    solution_label = solution_label
  )

  sensitivity_result <- construct_all_patterns(
    DT = sensitivity_data,
    pattern_diseases = pattern_diseases,
    cohort_label = "SENSITIVITY",
    solution_label = solution_label
  )

  main_saved <- save_pattern_outputs(
    result = main_result,
    definition_table = definition_table,
    source_dir = source_dir,
    cohort_label = "MAIN",
    K = K
  )

  sensitivity_saved <- save_pattern_outputs(
    result = sensitivity_result,
    definition_table = definition_table,
    source_dir = sensitivity_source_dir,
    cohort_label = "SENSITIVITY",
    K = K
  )

  cross_cohort_consistency_QA(
    main_saved = main_saved,
    sensitivity_saved = sensitivity_saved,
    K = K,
    solution_label = solution_label
  )

  list(
    definition = definition_table,
    main = main_saved,
    sensitivity = sensitivity_saved
  )
}


# ============================================================
# 4. Load master disease dataset
# ============================================================

cat(
  "\nLoading master disease dataset:\n",
  master_disease_file,
  "\n"
)

UKB_diseases_master <- readRDS(
  master_disease_file
)

setDT(
  UKB_diseases_master
)

if (anyDuplicated(UKB_diseases_master$eid) > 0L) {
  stop(
    "Master disease dataset contains duplicated eid."
  )
}

master_keep_cols <- c(
  "eid",
  "recruitment_date",
  "recruitment_centre",
  "administrative_end",
  "death_date",
  "censor_date",
  "valid_followup",
  "unknown_disease_timing_count",
  "unknown_disease_timing_any",
  "baseline_disease_count",
  "incident_disease_count",
  "ever_disease_count",
  "post_censor_disease_count",
  "eligible_incident_multimorbidity",
  "incident_multimorbidity",
  "incident_multimorbidity_event_date",
  "followup_end_date",
  "follow_time",
  "comorbidity",
  "comorbidity_time",
  expected_36_diseases,
  disease_to_time_col(
    expected_36_diseases
  )
)

master_keep_cols <- intersect(
  master_keep_cols,
  names(UKB_diseases_master)
)

UKB_diseases_master <- UKB_diseases_master[
  ,
  ..master_keep_cols
]


# ============================================================
# 5. Load MAIN and SENSITIVITY cohort IDs
# ============================================================

main_eids <- read_eids_from_source(
  source_dir
)

sensitivity_eids <- read_eids_from_source(
  sensitivity_source_dir
)

cat(
  "\nMain-analysis eid N:",
  nrow(main_eids),
  "\nSensitivity-analysis eid N:",
  nrow(sensitivity_eids),
  "\n"
)

if (
  length(
    setdiff(
      sensitivity_eids$eid,
      main_eids$eid
    )
  ) > 0L
) {
  stop(
    "Sensitivity cohort is not a subset of the main cohort."
  )
}


# ============================================================
# 6. Reconstruct cohort disease datasets from the full master table
# ============================================================

main_data <- prepare_cohort_from_master(
  master_data = UKB_diseases_master,
  cohort_eids = main_eids,
  cohort_label = "MAIN"
)

sensitivity_data <- prepare_cohort_from_master(
  master_data = UKB_diseases_master,
  cohort_eids = sensitivity_eids,
  cohort_label = "SENSITIVITY"
)



# ============================================================
# 7. FINAL SOLUTION: K = 8
# ============================================================
#
# K=8 pattern-specific output variables:
#   comorbidity_pattern_1 ... comorbidity_pattern_8
#
# For each pattern:
#   comorbidity_pattern_X
#   comorbidity_pattern_X_event_date
#   comorbidity_pattern_X_time
#   comorbidity_pattern_X_follow_time
#   comorbidity_pattern_X_baseline_disease_count
#   comorbidity_pattern_X_incident_disease_count
#   comorbidity_pattern_X_after_censor_disease_count
#   comorbidity_pattern_X_death_censored
#   comorbidity_pattern_X_censor_reason
#
# Overall multimorbidity remains:
#   comorbidity
#   comorbidity_time
#   follow_time
# ============================================================

K8_result <- run_pattern_solution(
  pattern_diseases = pattern_diseases_K8,
  K = 8L,
  main_data = main_data,
  sensitivity_data = sensitivity_data,
  source_dir = source_dir,
  sensitivity_source_dir = sensitivity_source_dir
)


# ============================================================
# 8. Add clinical pattern labels to definition and summary files
# ============================================================

K8_definition_labeled <- merge(
  K8_result$definition,
  pattern_labels_K8,
  by = "pattern_id",
  all.x = TRUE,
  sort = FALSE
)

setorder(
  K8_definition_labeled,
  pattern_number
)

K8_main_summary_labeled <- merge(
  K8_result$main$summary,
  pattern_labels_K8,
  by = "pattern_id",
  all.x = TRUE,
  sort = FALSE
)

setorder(
  K8_main_summary_labeled,
  pattern_number
)

K8_sensitivity_summary_labeled <- merge(
  K8_result$sensitivity$summary,
  pattern_labels_K8,
  by = "pattern_id",
  all.x = TRUE,
  sort = FALSE
)

setorder(
  K8_sensitivity_summary_labeled,
  pattern_number
)

fwrite(
  K8_definition_labeled,
  file.path(
    source_dir,
    "comorbidity_pattern_definition_K8_labeled.csv"
  )
)

fwrite(
  K8_main_summary_labeled,
  file.path(
    source_dir,
    "comorbidity_pattern_followup_summary_K8_labeled.csv"
  )
)

fwrite(
  K8_sensitivity_summary_labeled,
  file.path(
    sensitivity_source_dir,
    "comorbidity_pattern_followup_summary_K8_labeled.csv"
  )
)


# ============================================================
# 9. Main vs sensitivity event summary
# ============================================================

K8_event_summary_main <- copy(
  K8_main_summary_labeled
)

K8_event_summary_main[
  ,
  cohort := "MAIN"
]

K8_event_summary_sensitivity <- copy(
  K8_sensitivity_summary_labeled
)

K8_event_summary_sensitivity[
  ,
  cohort := "SENSITIVITY"
]

K8_event_summary_all <- rbindlist(
  list(
    K8_event_summary_main,
    K8_event_summary_sensitivity
  ),
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  K8_event_summary_all,
  file.path(
    source_root,
    "pattern_event_summary_K8_main_vs_sensitivity.csv"
  )
)


# ============================================================
# 10. Save canonical FINAL filenames for downstream Stage 1-3
# ============================================================
#
# Keep the K8-specific files generated above for reproducibility.
# Also write the canonical filenames below so downstream scripts do not need
# to carry "_K8" in every input path.
# ============================================================

fwrite(
  K8_result$main$data,
  file.path(
    source_dir,
    "UKB_diseases_data_with_patterns.csv"
  )
)

saveRDS(
  K8_result$main$data,
  file.path(
    source_dir,
    "UKB_diseases_data_with_patterns.rds"
  )
)

fwrite(
  K8_result$sensitivity$data,
  file.path(
    sensitivity_source_dir,
    "UKB_diseases_data_with_patterns.csv"
  )
)

saveRDS(
  K8_result$sensitivity$data,
  file.path(
    sensitivity_source_dir,
    "UKB_diseases_data_with_patterns.rds"
  )
)


# ============================================================
# 11. Hepatic-pattern overlap QA
# ============================================================
#
# IMPORTANT:
# In the current disease-definition script, Cirrhosis uses K74 and the broader
# Hepatitis phenotype also includes K74. Therefore the two phenotypes are not
# fully independent. This QA quantifies same-date overlap and should be retained
# for transparent interpretation of P4.
# ============================================================

hepatic_QA_main <- K8_result$main$data[
  ,
  .(
    cohort = "MAIN",
    N = .N,
    cirrhosis_recorded = sum(
      Cirrhosis == 1L,
      na.rm = TRUE
    ),
    hepatitis_recorded = sum(
      Hepatitis == 1L,
      na.rm = TRUE
    ),
    both_recorded = sum(
      Cirrhosis == 1L &
        Hepatitis == 1L,
      na.rm = TRUE
    ),
    same_onset_date = sum(
      !is.na(Cirrhosis_time) &
        !is.na(Hepatitis_time) &
        Cirrhosis_time == Hepatitis_time,
      na.rm = TRUE
    ),
    P4_events = sum(
      comorbidity_pattern_4 == 1L,
      na.rm = TRUE
    )
  )
]

hepatic_QA_sensitivity <- K8_result$sensitivity$data[
  ,
  .(
    cohort = "SENSITIVITY",
    N = .N,
    cirrhosis_recorded = sum(
      Cirrhosis == 1L,
      na.rm = TRUE
    ),
    hepatitis_recorded = sum(
      Hepatitis == 1L,
      na.rm = TRUE
    ),
    both_recorded = sum(
      Cirrhosis == 1L &
        Hepatitis == 1L,
      na.rm = TRUE
    ),
    same_onset_date = sum(
      !is.na(Cirrhosis_time) &
        !is.na(Hepatitis_time) &
        Cirrhosis_time == Hepatitis_time,
      na.rm = TRUE
    ),
    P4_events = sum(
      comorbidity_pattern_4 == 1L,
      na.rm = TRUE
    )
  )
]

hepatic_QA <- rbindlist(
  list(
    hepatic_QA_main,
    hepatic_QA_sensitivity
  ),
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  hepatic_QA,
  file.path(
    source_root,
    "hepatic_pattern_overlap_QA.csv"
  )
)

cat(
  "\nHepatic pattern overlap QA:\n"
)

print(
  hepatic_QA
)


# ============================================================
# 12. Final summary
# ============================================================

cat(
  "\n============================================================\n",
  "FINAL K=8 PATTERN OUTCOME CONSTRUCTION COMPLETED\n",
  "Both MAIN and SENSITIVITY cohorts passed consistency QA.\n",
  "All 36 diseases are represented exactly once across the 8 patterns.\n",
  "Pattern events are restricted to (recruitment_date, censor_date].\n",
  "Non-events are followed to the person-specific censor_date.\n",
  "Canonical downstream files:\n",
  "  MAIN: ",
  file.path(
    source_dir,
    "UKB_diseases_data_with_patterns.rds"
  ),
  "\n  SENSITIVITY: ",
  file.path(
    sensitivity_source_dir,
    "UKB_diseases_data_with_patterns.rds"
  ),
  "\n============================================================\n",
  sep = ""
)

cat(
  "\nFinal MAIN K=8 pattern summary:\n"
)

print(
  K8_main_summary_labeled[
    ,
    .(
      pattern_id,
      pattern_label,
      n_diseases,
      events,
      event_percent,
      median_followup_years
    )
  ]
)

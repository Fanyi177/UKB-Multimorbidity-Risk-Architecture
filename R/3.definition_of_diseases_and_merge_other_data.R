library(data.table)
library(dplyr)
# Definition of 36 diseases and multimodal data merging####
project_dir <- "/lustre/home/2411110208/UK_biobank"
disease_dir <- file.path(project_dir, "disease")

output_dir <- file.path(project_dir, "cov_data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
# 数据输入：直接读取 02 保存到 cov_data/ 的正式文件。
# 主要结果：沿用原有输出路径并覆盖同名文件。
# QA：统一存储于 cov_data/QA/，不混入正式结果。
recoded_input_dir <- output_dir
analysis_output_dir <- output_dir
qa_dir <- file.path(output_dir, "QA")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
cat("03 input:", recoded_input_dir, "\n")
cat("03 data output:", analysis_output_dir, "\n")
cat("03 QA output:", qa_dir, "\n")

# 先确认 02 的正式输出文件均可读取。
required_recoded_files <- file.path(recoded_input_dir, c(
  "recruitment_info.csv",
  "enviro_exposure_no_date.csv",
  "blood_urine_exposure.csv"
))
missing_recoded_files <- required_recoded_files[!file.exists(required_recoded_files)]
if (length(missing_recoded_files)) {
  stop("Required 02 input files are missing: ", paste(missing_recoded_files, collapse = " | "))
}

# ============================================================
# Helper functions
# ============================================================

parse_ukb_date <- function(
    x,
    col_name = NULL
) {
  
  if (inherits(x, "IDate")) {
    x <- as.Date(x)
  }
  
  if (inherits(x, "POSIXt")) {
    x <- as.Date(x)
  }
  
  if (inherits(x, "Date")) {
    
    # UKB Coding 819:
    # invalid/unknown timing should NOT be treated as real dates
    x[
      x %in% as.Date(
        c(
          "1900-01-01",
          "1909-09-09",
          "2037-07-07"
        )
      )
    ] <- as.Date(NA)
    
    # 1901 / 1902 / 1903 are retained because they indicate
    # occurrence before or around birth and therefore definitely
    # precede UKB recruitment.
    
    return(x)
  }
  
  
  # ----------------------------------------------------------
  # Character version
  # ----------------------------------------------------------
  
  x <- trimws(
    as.character(x)
  )
  
  x[
    x %in% c(
      "",
      "NA",
      "NaN",
      "NULL"
    )
  ] <- NA_character_
  
  
  # ----------------------------------------------------------
  # UKB Coding 819 labels
  # ----------------------------------------------------------
  
  # No usable event date
  x[
    x ==
      "Code has no event date"
  ] <- NA_character_
  
  
  # Clearly occurred before recruitment:
  # convert to UKB's sentinel dates
  x[
    x ==
      "Code has event date before participant's date of birth"
  ] <- "1901-01-01"
  
  x[
    x ==
      "Code has event date matching participant's date of birth"
  ] <- "1902-02-02"
  
  x[
    x ==
      paste0(
        "Code has event date after participant's date of birth ",
        "and falls in the same calendar year as date of birth"
      )
  ] <- "1903-03-03"
  
  
  # Future placeholder / system-default date:
  # do NOT use as a disease onset date
  x[
    grepl(
      "event date in the future.*place-holder|event date in the future.*system default",
      x,
      ignore.case = TRUE
    )
  ] <- NA_character_
  
  
  # ----------------------------------------------------------
  # Remove possible time component
  # ----------------------------------------------------------
  
  x <- sub(
    "T.*$",
    "",
    x
  )
  
  x <- sub(
    " .*?$",
    "",
    x
  )
  
  x <- gsub(
    "/",
    "-",
    x
  )
  
  
  out <- as.Date(
    rep(
      NA_character_,
      length(x)
    )
  )
  
  
  # YYYY-MM-DD
  idx_ymd <- !is.na(x) &
    grepl(
      "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$",
      x
    )
  
  if (any(idx_ymd)) {
    
    out[idx_ymd] <- as.Date(
      x[idx_ymd],
      format = "%Y-%m-%d"
    )
  }
  
  
  # DD-MM-YYYY
  idx_dmy <- !is.na(x) &
    is.na(out) &
    grepl(
      "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$",
      x
    )
  
  if (any(idx_dmy)) {
    
    out[idx_dmy] <- as.Date(
      x[idx_dmy],
      format = "%d-%m-%Y"
    )
  }
  
  
  # ----------------------------------------------------------
  # UKB invalid sentinel dates
  # ----------------------------------------------------------
  
  out[
    out %in% as.Date(
      c(
        "1900-01-01",
        "1909-09-09",
        "2037-07-07"
      )
    )
  ] <- as.Date(NA)
  
  
  # ----------------------------------------------------------
  # Remaining unexpected values
  # ----------------------------------------------------------
  
  bad <- !is.na(x) &
    is.na(out)
  
  if (any(bad)) {
    
    stop(
      paste0(
        "\nUnrecognised date value",
        if (!is.null(col_name)) {
          paste0(
            " in ",
            col_name
          )
        } else {
          ""
        },
        ".\nExamples:\n",
        paste(
          head(
            unique(
              x[bad]
            ),
            20
          ),
          collapse = "\n"
        )
      )
    )
  }
  
  out
}

row_min_date <- function(..., na.rm = TRUE) {
  
  z <- lapply(
    list(...),
    function(x) {
      as.numeric(
        parse_ukb_date(x)
      )
    }
  )
  
  out <- do.call(
    pmin,
    c(
      z,
      list(
        na.rm = na.rm
      )
    )
  )
  
  out[
    !is.finite(out)
  ] <- NA_real_
  
  as.Date(
    out,
    origin = "1970-01-01"
  )
}

normalize_icd <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("[^A-Z0-9]", "", x)
  x
}

is_malignant_icd10 <- function(x) {
  
  x <- normalize_icd(x)
  
  prefix_ok <- grepl("^C[0-9]{2}", x)
  
  code_num <- suppressWarnings(
    as.integer(substr(x, 2, 3))
  )
  
  prefix_ok &
    !is.na(code_num) &
    code_num >= 0 &
    code_num <= 97
}

is_malignant_icd9 <- function(x) {
  
  x <- gsub("[^0-9]", "", as.character(x))
  
  code_num <- suppressWarnings(
    as.integer(substr(x, 1, 3))
  )
  
  !is.na(code_num) &
    code_num >= 140 &
    code_num <= 208
}

find_one_csv <- function(path, patterns, label) {
  
  all_files <- list.files(
    path = path,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  hit <- rep(
    FALSE,
    length(all_files)
  )
  
  for (pat in patterns) {
    hit <- hit |
      grepl(
        pat,
        basename(all_files),
        ignore.case = TRUE
      )
  }
  
  files <- all_files[hit]
  
  if (length(files) == 0L) {
    stop(
      paste0(
        "Cannot find ",
        label,
        " CSV in: ",
        path,
        "\nExpected filename to match one of: ",
        paste(
          patterns,
          collapse = ", "
        )
      )
    )
  }
  
  if (length(files) > 1L) {
    stop(
      paste0(
        "More than one possible ",
        label,
        " CSV found:\n",
        paste(
          basename(files),
          collapse = "\n"
        )
      )
    )
  }
  
  files[[1]]
}

# ============================================================
# 1. Load ONLY first-occurrence disease chapter files
# ============================================================

disease_files <- list.files(
  path = disease_dir,
  pattern = "^D[0-9]+_.*\\.csv$",
  full.names = TRUE
)

print(basename(disease_files))

stopifnot(length(disease_files) == 16L)

data_list <- lapply(
  disease_files,
  function(file) {
    fread(file, header = TRUE)
  }
)

first_occurrence_data <- Reduce(
  function(x, y) {
    merge(x, y, by = "eid", all = TRUE)
  },
  data_list
)

setDT(first_occurrence_data)

# ============================================================
# 2. Load recruitment information and construct master disease dataset
# ============================================================
recruitment_data_file <-file.path(recoded_input_dir,"recruitment_info.csv")
recruitment_data<-fread(recruitment_data_file)
setDT(recruitment_data)

required_recruitment_cols <- c(
  "eid",
  "recruitment_date",
  "recruitment_centre"
)

missing_recruitment_cols <- setdiff(
  required_recruitment_cols,
  names(recruitment_data)
)

if (length(missing_recruitment_cols) > 0L) {
  stop(
    paste0(
      "Missing recruitment columns: ",
      paste(
        missing_recruitment_cols,
        collapse = ", "
      )
    )
  )
}

recruitment_data[
  ,
  recruitment_date :=
    parse_ukb_date(
      recruitment_date
    )
]

unexpected_centre <- setdiff(
  unique(
    na.omit(
      recruitment_data$recruitment_centre
    )
  ),
  c(
    "England",
    "Scotland",
    "Wales"
  )
)

if (length(unexpected_centre) > 0L) {
  stop(
    paste0(
      "recruitment_centre has not been harmonised to ",
      "England / Scotland / Wales. Unexpected values:\n",
      paste(
        unexpected_centre,
        collapse = ", "
      )
    )
  )
}

if (anyDuplicated(recruitment_data$eid) > 0L) {
  stop(
    "recruitment_data contains duplicated eid."
  )
}

if (anyDuplicated(first_occurrence_data$eid) > 0L) {
  stop(
    "first_occurrence_data contains duplicated eid."
  )
}

# Recruitment roster is the master cohort.
UKB_diseases_data <- merge(
  recruitment_data,
  first_occurrence_data,
  by = "eid",
  all.x = TRUE,
  sort = FALSE
)

setDT(UKB_diseases_data)

# ============================================================
# 3. Clean ONLY first-occurrence fields used in the 35 non-cancer diseases
# ============================================================
#
# Some p13* date fields are character columns and use "" for missing values.
# Because disease status below is defined using !is.na(), those blanks must
# first become true NA values.
#
# UKB Coding 819 text values such as "Code has no event date" are retained
# at this stage because they indicate that a disease record exists.
# row_min_date() parses the usable onset date. A present disease record with
# no usable date is flagged later as unknown disease timing and excluded from
# the primary strict-incident cohort.

disease_date_fields <- c(
  "p130020", "p130022", "p130024", "p130026", "p130028", "p130148", "p130194", "p130196",
  "p130198", "p130200", "p130202", "p130622", "p130624", "p130626", "p130628", "p130630",
  "p130632", "p130634", "p130636", "p130638", "p130640", "p130642", "p130644", "p130646",
  "p130648", "p130650", "p130652", "p130654", "p130656", "p130658", "p130692", "p130694",
  "p130696", "p130698", "p130700", "p130702", "p130704", "p130706", "p130708", "p130710",
  "p130712", "p130714", "p130744", "p130746", "p130748", "p130836", "p130838", "p130840",
  "p130842", "p130846", "p130874", "p130876", "p130878", "p130880", "p130882", "p130884",
  "p130886", "p130888", "p130894", "p130896", "p130904", "p130906", "p130992", "p130994",
  "p130996", "p130998", "p131022", "p131036", "p131038", "p131042", "p131048", "p131052",
  "p131186", "p131188", "p131252", "p131286", "p131290", "p131292", "p131294", "p131296",
  "p131298", "p131300", "p131302", "p131304", "p131306", "p131350", "p131354", "p131360",
  "p131362", "p131364", "p131366", "p131368", "p131380", "p131382", "p131384", "p131386",
  "p131388", "p131390", "p131392", "p131394", "p131396", "p131398", "p131400", "p131402",
  "p131404", "p131406", "p131408", "p131410", "p131468", "p131484", "p131486", "p131488",
  "p131490", "p131492", "p131494", "p131496", "p131498", "p131626", "p131628", "p131630",
  "p131638", "p131664", "p131666", "p131720", "p131722", "p131724", "p131726", "p131728",
  "p131730", "p131732", "p131734", "p131736", "p131738", "p131740", "p131848", "p131850",
  "p131962", "p131964", "p131966", "p131968", "p131970", "p131972", "p132004", "p132006",
  "p132008", "p132010", "p132014", "p132018", "p132020", "p132022", "p132024", "p132026",
  "p132028", "p132032", "p132034", "p132036", "p132038", "p132072", "p132074", "p132076",
  "p132202",
  # Hepatitis / broader liver disease
  "p131658", "p131660", "p131662", "p131668", "p131670", "p131672",
  # Meningitis / broader CNS infection-inflammatory disease
  "p130052", "p130066", "p131000", "p131002"
)

missing_disease_date_fields <- setdiff(
  disease_date_fields,
  names(UKB_diseases_data)
)

if (length(missing_disease_date_fields) > 0L) {
  stop(
    paste0(
      "Missing disease date fields: ",
      paste(
        missing_disease_date_fields,
        collapse = ", "
      )
    )
  )
}

first_occurrence_field_audit <- rbindlist(
  lapply(
    disease_date_fields,
    function(col) {
      
      x <- UKB_diseases_data[[col]]
      x_chr <- trimws(as.character(x))
      
      blank_like <- !is.na(x_chr) &
        x_chr %in% c(
          "",
          "NA",
          "NaN",
          "NULL"
        )
      
      special_code <- !is.na(x_chr) &
        grepl(
          "^Code has",
          x_chr
        )
      
      audit_row <- data.table(
        variable = col,
        class_before = paste(
          class(x),
          collapse = "/"
        ),
        n_blank_like = sum(
          blank_like,
          na.rm = TRUE
        ),
        n_special_code = sum(
          special_code,
          na.rm = TRUE
        )
      )
      
      if (is.character(x)) {
        x[blank_like] <- NA_character_
        
        set(
          UKB_diseases_data,
          j = col,
          value = x
        )
      }
      
      audit_row
    }
  ),
  use.names = TRUE,
  fill = TRUE
)

cat(
  "\nFirst-occurrence fields used in disease definitions:",
  length(disease_date_fields),
  "\nBlank-like values converted to NA:",
  sum(first_occurrence_field_audit$n_blank_like),
  "\nUKB special date codes retained:",
  sum(first_occurrence_field_audit$n_special_code),
  "\n"
)

###definition of Anaemia####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Anaemia = 
      if_else(
        !is.na(p130622) | !is.na(p130624) | !is.na(p130626)|!is.na(p130628)|
          !is.na(p130630) | !is.na(p130632) | !is.na(p130634)|!is.na(p130636)|
          !is.na(p130638) | !is.na(p130640) | !is.na(p130642)|!is.na(p130644)|
          !is.na(p130646) | !is.na(p130648) | !is.na(p130650)|!is.na(p130652)|
          !is.na(p130654) | !is.na(p130656) | !is.na(p130658), 
        1, 0
      ),
    Anaemia_time = 
      row_min_date(p130622, p130624, p130626,p130628,
                   p130630,p130632,p130634,p130636,
                   p130638,p130640,p130642,p130644,
                   p130646,p130648,p130650,p130652,
                   p130654,p130656,p130658,
                   na.rm = TRUE)
  )
###definition of Angina####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Angina = 
      if_else(
        !is.na(p131296),
        1, 0
      ),
    Angina_time = 
      row_min_date(p131296,
                   na.rm = TRUE)
  )
cat(
  "\nAngina source QA after blank normalization:\n",
  "p131296 missing:",
  sum(
    is.na(
      UKB_diseases_data$p131296
    )
  ),
  "\nAngina cases:",
  sum(
    UKB_diseases_data$Angina == 1L,
    na.rm = TRUE
  ),
  "\nAngina with missing usable onset date:",
  sum(
    UKB_diseases_data$Angina == 1L &
      is.na(
        UKB_diseases_data$Angina_time
      ),
    na.rm = TRUE
  ),
  "\n"
)

###definition of Anxiety or panic attacks####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Anxiety_or_panic_attacks = if_else(
      !is.na(p130904)|!is.na(p130906), 1, 0
    ),
    Anxiety_or_panic_attacks_time = row_min_date(p130904,p130906,na.rm = TRUE)
  )
###definition of Atrial fibrillation####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Atrial_fibrillation = if_else(
      !is.na(p131350), 1, 0
    ),
    Atrial_fibrillation_time = row_min_date(p131350,na.rm = TRUE)
  )
###definition of Asthma####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Asthma = if_else(
      !is.na(p131494)|!is.na(p131496), 1, 0
    ),
    Asthma_time = row_min_date(p131494,p131496,na.rm = TRUE)
  )
###definition of Bronchiectasis####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Bronchiectasis = if_else(
      !is.na(p131498), 1, 0
    ),
    Bronchiectasis_time = row_min_date(p131498,na.rm = TRUE)
  )
###definition of Chronic_kidney_disease####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Chronic_kidney_disease = 
      if_else(
        !is.na(p131290) | !is.na(p131292) | !is.na(p132004)| !is.na(p132006)|
          !is.na(p132008) | !is.na(p132010) | !is.na(p132014)| !is.na(p132018)|
          !is.na(p132020) | !is.na(p132022) | !is.na(p132024)| !is.na(p132026)|
          !is.na(p132028) | !is.na(p132032) | !is.na(p132034)| !is.na(p132036)|
          !is.na(p132038), 
        1, 0
      ),
    Chronic_kidney_disease_time = 
      row_min_date(p131290, p131292, p132004,p132006,
                   p132008,p132010,p132014,p132018,
                   p132020,p132022,p132024,p132026,
                   p132028,p132032,p132034,p132036,
                   p132038,
                   na.rm = TRUE)
  )
###definition of Chronic_obstructive_pulmonary disease####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Chronic_obstructive_pulmonary_disease = 
      if_else(
        !is.na(p131484) | !is.na(p131486) | !is.na(p131488)| !is.na(p131490)|
          !is.na(p131492),
        1, 0
      ),
    Chronic_obstructive_pulmonary_disease_time = 
      row_min_date(p131484,p131486,p131488,p131490,p131492,
                   na.rm = TRUE)
  )
###definition of Chronic_sinusitis####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Chronic_sinusitis = if_else(
      !is.na(p131468), 1, 0
    ),
    Chronic_sinusitis_time = row_min_date(p131468,na.rm = TRUE)
  )
### definition of Cirrhosis: K74 ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Cirrhosis = if_else(!is.na(p131666), 1L, 0L),
    Cirrhosis_time = if_else(
      Cirrhosis == 1L,
      parse_ukb_date(
        p131666,
        col_name = "p131666"
      ),
      as.Date(NA)
    )
  )
###definition of Dementia####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Dementia = if_else(
      !is.na(p130836)| !is.na(p130838) | !is.na(p130840) | !is.na(p130842)| !is.na(p130846)|
        !is.na(p131036) | !is.na(p131038), 1, 0
    ),
    Dementia_time = row_min_date(p130836,p130838,p130840,p130842,p130846,p131036,p131038,na.rm = TRUE)
  )
###definition of Depression####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Depression = if_else(
      !is.na(p130894) | !is.na(p130896), 1, 0
    ),
    Depression_time = row_min_date(p130894,p130896,na.rm = TRUE)
  )

###definition of Diabetes####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Diabetes = 
      if_else(
        !is.na(p130706) | !is.na(p130708) | !is.na(p130710)| !is.na(p130712)|
          !is.na(p130714) | !is.na(p132202), 
        1, 0
      ),
    Diabetes_time = 
      row_min_date(p130706, p130708, p130710,p130712,
                   p130714,p132202,
                   na.rm = TRUE)
  )
###definition of Eczema_or_dermatitis####
#use mutate to create the disease and time
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Eczema_or_dermatitis = 
      if_else(
        !is.na(p131720) | !is.na(p131722) | !is.na(p131724)| !is.na(p131726)|
          !is.na(p131728) | !is.na(p131730) | !is.na(p131732) | !is.na(p131734)| !is.na(p131736)|
          !is.na(p131738) | !is.na(p131740), 
        1, 0
      ),
    Eczema_or_dermatitis_time = 
      row_min_date(p131720, p131722, p131724,p131726,
                   p131728,p131730, p131732, p131734,p131736,
                   p131738,p131740,
                   na.rm = TRUE)
  )
###definition of Epilepsy####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Epilepsy =
      if_else(
        !is.na(p131048),
        1, 0
      ),
    Epilepsy_time = 
      row_min_date(p131048,
                   na.rm = TRUE)
  )

###definition of Glaucoma####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Glaucoma = if_else(
      !is.na(p131186)| !is.na(p131188), 1, 0
    ),
    Glaucoma_time = row_min_date(p131186,p131188,na.rm = TRUE)
  )
###definition of Heart_failure####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Heart_failure = if_else(
      !is.na(p131354), 1, 0
    ),
    Heart_failure_time = row_min_date(p131354,na.rm = TRUE)
  )
### definition of Hepatitis / broader liver disease ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Hepatitis = if_else(
      !is.na(p130194) |
        !is.na(p130196) |
        !is.na(p130198) |
        !is.na(p130200) |
        !is.na(p130202) |
        !is.na(p131658) |
        !is.na(p131660) |
        !is.na(p131662) |
        !is.na(p131664) |
        !is.na(p131666) |
        !is.na(p131668) |
        !is.na(p131670) |
        !is.na(p131672),
      1L,
      0L
    ),
    Hepatitis_time = if_else(
      Hepatitis == 1L,
      row_min_date(
        p130194,
        p130196,
        p130198,
        p130200,
        p130202,
        p131658,
        p131660,
        p131662,
        p131664,
        p131666,
        p131668,
        p131670,
        p131672,
        na.rm = TRUE
      ),
      as.Date(NA)
    )
  )
###definition of Hypertension####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Hypertension = if_else(
      !is.na(p131286)| !is.na(p131294), 1, 0
    ),
    Hypertension_time = row_min_date(p131286,p131294,
                                     na.rm = TRUE)
  )
###definition of Inflammatory_bowel_disease####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Inflammatory_bowel_disease = if_else(
      !is.na(p131626)| !is.na(p131628)| !is.na(p131630), 1, 0
    ),
    Inflammatory_bowel_disease_time = row_min_date(p131626,p131628,p131630,
                                                   na.rm = TRUE)
  )

###definition of Irritable_bowel_syndrome####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Irritable_bowel_syndrome = 
      if_else(
        !is.na(p131638),
        1, 0
      ),
    Irritable_bowel_syndrome_time =
      row_min_date(p131638,
                   na.rm = TRUE)
  )

### definition of Meningitis / broader CNS infection-inflammatory disease ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Meningitis = if_else(
      !is.na(p130024) |
        !is.na(p130052) |
        !is.na(p130066) |
        !is.na(p130148) |
        !is.na(p130992) |
        !is.na(p130994) |
        !is.na(p130996) |
        !is.na(p130998) |
        !is.na(p131000) |
        !is.na(p131002),
      1L,
      0L
    ),
    Meningitis_time = if_else(
      Meningitis == 1L,
      row_min_date(
        p130024,
        p130052,
        p130066,
        p130148,
        p130992,
        p130994,
        p130996,
        p130998,
        p131000,
        p131002,
        na.rm = TRUE
      ),
      as.Date(NA)
    )
  )
###definition of Migraine####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Migraine = 
      if_else(
        !is.na(p131052),
        1, 0
      ),
    Migraine_time = 
      row_min_date(p131052,
                   na.rm = TRUE)
  )
###definition of Multiple_sclerosis####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Multiple_sclerosis = 
      if_else(
        !is.na(p131042),
        1, 0
      ),
    Multiple_sclerosis_time =
      row_min_date(p131042,
                   na.rm = TRUE)
  )
###definition of Myocardial_infarction####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Myocardial_infarction = 
      if_else(
        !is.na(p131298) | !is.na(p131300) | !is.na(p131302)|!is.na(p131304)|
          !is.na(p131306), 
        1, 0
      ),
    Myocardial_infarction_time = 
      row_min_date(p131298, p131300, p131302,p131304,
                   p131306, 
                   na.rm = TRUE)
  )
###definition of Osteoporosis####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Osteoporosis = 
      if_else(
        !is.na(p131962) | !is.na(p131964) | !is.na(p131966)| !is.na(p131968)|
          !is.na(p131970) | !is.na(p131972) , 
        1, 0
      ),
    Osteoporosis_time = 
      row_min_date(p131962, p131964, p131966,p131968,
                   p131970,p131972,
                   na.rm = TRUE)
  )

###definition of Parkinson’s_disease####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Parkinson_disease = 
      if_else(
        !is.na(p131022),
        1, 0
      ),
    Parkinson_disease_time = 
      row_min_date(p131022,
                   na.rm = TRUE)
  )

###definition of Peripheral_vascular_disease####
#use mutate to create the disease and time
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Peripheral_vascular_disease = 
      if_else(
        !is.na(p131380) | !is.na(p131382) | !is.na(p131384)| !is.na(p131386)|
          !is.na(p131388) | !is.na(p131390) | !is.na(p131392)| !is.na(p131394)|
          !is.na(p131396) | !is.na(p131398) | !is.na(p131400)| !is.na(p131402)|
          !is.na(p131404) | !is.na(p131406) | !is.na(p131408)| !is.na(p131410), 
        1, 0
      ),
    Peripheral_vascular_disease_time = 
      row_min_date(p131380, p131382, p131384,p131386,
                   p131388,p131390,p131392,p131394,
                   p131396,p131398,p131400,p131402,
                   p131404,p131406,p131408,p131410, 
                   na.rm = TRUE)
  )
###definition of Prostate_problem####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Prostate_problem= 
      if_else(
        !is.na(p132072) | !is.na(p132074) | !is.na(p132076), 
        1, 0
      ),
    Prostate_problem_time = 
      row_min_date(p132072, p132074, p132076, 
                   na.rm = TRUE)
  )
###definition of Rheumatoid_arthritis  ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Rheumatoid_arthritis= 
      if_else(
        !is.na(p131848) | !is.na(p131850) , 
        1, 0
      ),
    Rheumatoid_arthritis_time = 
      row_min_date(p131848, p131850, 
                   na.rm = TRUE)
  )

###definition of  Schizophrenia  ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Schizophrenia= 
      if_else(
        !is.na(p130874) | !is.na(p130876) | !is.na(p130878) | !is.na(p130880) | !is.na(p130882) | !is.na(p130884) | !is.na(p130886) | !is.na(p130888) , #根据UKB36种疾病ICD-10编码汇总(1)替换成相应的变量名称
        1, 0
      ),
    Schizophrenia_time = 
      row_min_date(p130874, p130876, p130878,p130880,p130882,p130884,p130886,p130888,
                   na.rm = TRUE)
  )
###definition of Stroke ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Stroke= 
      if_else(
        !is.na(p131360) | !is.na(p131362) | !is.na(p131364) | !is.na(p131366) | !is.na(p131368) , 
        1, 0
      ),
    Stroke_time = 
      row_min_date(p131360,p131362, p131364, p131366,p131368,
                   na.rm = TRUE)
  )
###definition of Thyroid_problem  ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Thyroid_problem= 
      if_else(
        !is.na(p130692) |!is.na(p130694) | !is.na(p130696) | !is.na(p130698) | !is.na(p130700) | !is.na(p130702) | !is.na(p130704) | !is.na(p130744) | 
          !is.na(p130746) | !is.na(p130748), 
        1, 0
      ),
    Thyroid_problem_time = 
      row_min_date(p130692,p130694,p130696,p130698,p130700,p130702,p130704,p130744,p130746,p130748,
                   na.rm = TRUE)
  )
###definition of  Tuberculosis####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Tuberculosis= 
      if_else(
        !is.na(p130020) | !is.na(p130022) | !is.na(p130024) | !is.na(p130026) | !is.na(p130028), 
        1, 0
      ),
    Tuberculosis_time = 
      row_min_date(p130020,p130022,p130024,p130026,p130028,
                   na.rm = TRUE)
  )
###definition of Vestibular_disorder  ####
UKB_diseases_data <- UKB_diseases_data %>%
  mutate(
    Vestibular_disorder= 
      if_else(
        !is.na(p131252) , 
        1, 0
      ),
    Vestibular_disorder_time = 
      row_min_date(p131252,
                   na.rm = TRUE)
  )
# ============================================================
# QA for the restored broader Hepatitis and Meningitis definitions
# ============================================================
cat(
  "\nRestored disease-definition QA:\n",
  "Hepatitis cases:",
  sum(UKB_diseases_data$Hepatitis == 1L, na.rm = TRUE),
  "\nCirrhosis cases:",
  sum(UKB_diseases_data$Cirrhosis == 1L, na.rm = TRUE),
  "\nCirrhosis + Hepatitis overlap:",
  sum(
    UKB_diseases_data$Cirrhosis == 1L &
      UKB_diseases_data$Hepatitis == 1L,
    na.rm = TRUE
  ),
  "\nMeningitis cases:",
  sum(UKB_diseases_data$Meningitis == 1L, na.rm = TRUE),
  "\nTuberculosis + Meningitis overlap:",
  sum(
    UKB_diseases_data$Tuberculosis == 1L &
      UKB_diseases_data$Meningitis == 1L,
    na.rm = TRUE
  ),
  "\n"
)

###definition of Overall cancer (C00-C97; ICD9 140-208)####
# Overall cancer uses three sources:
#   1) Cancer registry: ICD10 C00-C97 and ICD9 140-208
#   2) Hospital inpatient Summary Diagnoses: ICD10 C00-C97
#      - p41270 = ICD10 diagnosis
#      - p41280 = date of first inpatient diagnosis for the corresponding ICD10 code
#   3) Death register: malignant cancer recorded in death causes
#
# The final cancer date is the earliest usable date across the three sources.
#
# IMPORTANT:
# The hospital source is added ONLY to overall_cancer here.
# The other 35 diseases continue to use UKB First Occurrence fields (p13*),
# because First Occurrence already incorporates hospital and other sources.

cancer_register_file <- find_one_csv(
  disease_dir,
  patterns = c(
    "^Cancer_register.*\\.csv$",
    "cancer.*register.*\\.csv$"
  ),
  label = "cancer register"
)

death_file <- find_one_csv(
  disease_dir,
  patterns = c(
    "^Death_date.*\\.csv$",
    "death.*date.*\\.csv$"
  ),
  label = "death"
)

summary_diagnoses_file <- file.path(
  disease_dir,
  "Summary_Diagnoses.csv"
)

if (!file.exists(summary_diagnoses_file)) {
  stop(
    paste0(
      "Cannot find hospital Summary Diagnoses file: ",
      summary_diagnoses_file
    )
  )
}

cancer_register <- fread(
  cancer_register_file,
  header = TRUE
)

death_data <- fread(
  death_file,
  header = TRUE
)

# 医院数据的 p41270 是一个单元格内使用 | 分隔的 ICD-10 列表。
# 先只读取诊断列表；对应的 p41280_a* 日期列在后续按需分批读取。
# 不再根据同一 eid 的 CSV 行序号推断诊断数组下标。
hospital_codes <- fread(
  summary_diagnoses_file,
  select = c("eid", "p41270")
)

setDT(cancer_register)
setDT(death_data)
setDT(hospital_codes)

# Cancer registry and death files are expected to have one row per participant.
for (obj_name in c("cancer_register", "death_data")) {
  obj <- get(obj_name)
  
  if (!"eid" %in% names(obj)) {
    stop(
      paste0(
        obj_name,
        " does not contain eid."
      )
    )
  }
  
  if (anyDuplicated(obj$eid) > 0L) {
    stop(
      paste0(
        obj_name,
        " contains duplicated eid."
      )
    )
  }
}

# 同一 eid 可有多行源数据，p41270 单元格还可能包含 | 分隔的多条诊断。
# 此处只统计源行结构，不把 eid 内部的行次序视作诊断数组下标。
if (!"eid" %in% names(hospital_codes)) {
  stop("hospital_codes does not contain eid.")
}
cat(
  "\nHospital Summary Diagnoses input structure:\n",
  "Rows:", nrow(hospital_codes),
  "\nUnique participants:", uniqueN(hospital_codes$eid),
  "\nExtra rows due to repeated eid:",
  nrow(hospital_codes) - uniqueN(hospital_codes$eid),
  "\nParticipants appearing on >1 row:",
  hospital_codes[, .N, by = eid][N > 1L, .N],
  "\n"
)


# ------------------------------------------------------------
# 1. Cancer registry:
#    earliest qualifying ICD10 C00-C97 / ICD9 140-208
# ------------------------------------------------------------

cancer_date_cols <- grep(
  "^p40005_i[0-9]+$",
  names(cancer_register),
  value = TRUE
)

for (col in cancer_date_cols) {
  set(
    cancer_register,
    j = col,
    value = parse_ukb_date(
      cancer_register[[col]],
      col_name = col
    )
  )
}

n_cancer <- nrow(cancer_register)

registry_best_date_num <- rep(
  Inf,
  n_cancer
)

registry_best_code <- rep(
  NA_character_,
  n_cancer
)

registry_icd10_flag <- rep(
  FALSE,
  n_cancer
)

registry_icd9_flag <- rep(
  FALSE,
  n_cancer
)

registry_indices <- sort(
  unique(
    suppressWarnings(
      as.integer(
        sub(
          "^.*_i",
          "",
          grep(
            "^p4000(5|6)_i[0-9]+$|^p40013_i[0-9]+$",
            names(cancer_register),
            value = TRUE
          )
        )
      )
    )
  )
)

registry_indices <- registry_indices[
  !is.na(registry_indices)
]

for (i in registry_indices) {
  
  date_col <- paste0(
    "p40005_i",
    i
  )
  
  icd10_col <- paste0(
    "p40006_i",
    i
  )
  
  icd9_col <- paste0(
    "p40013_i",
    i
  )
  
  if (!date_col %in% names(cancer_register)) {
    next
  }
  
  date_num <- as.numeric(
    cancer_register[[date_col]]
  )
  
  if (icd10_col %in% names(cancer_register)) {
    code10_raw <- cancer_register[[icd10_col]]
    is10 <- is_malignant_icd10(code10_raw)
  } else {
    code10_raw <- rep(NA_character_, n_cancer)
    is10 <- rep(FALSE, n_cancer)
  }
  
  if (icd9_col %in% names(cancer_register)) {
    code9_raw <- cancer_register[[icd9_col]]
    is9 <- is_malignant_icd9(code9_raw)
  } else {
    code9_raw <- rep(NA_character_, n_cancer)
    is9 <- rep(FALSE, n_cancer)
  }
  
  registry_icd10_flag <- registry_icd10_flag | is10
  registry_icd9_flag <- registry_icd9_flag | is9
  
  valid <- !is.na(date_num) &
    (
      is10 |
        is9
    )
  
  candidate_code <- fifelse(
    is10,
    normalize_icd(code10_raw),
    fifelse(
      is9,
      gsub(
        "[^0-9]",
        "",
        as.character(code9_raw)
      ),
      NA_character_
    )
  )
  
  replace_idx <- valid &
    date_num <
    registry_best_date_num
  
  registry_best_date_num[
    replace_idx
  ] <- date_num[
    replace_idx
  ]
  
  registry_best_code[
    replace_idx
  ] <- candidate_code[
    replace_idx
  ]
}

registry_best_date_num[
  !is.finite(registry_best_date_num)
] <- NA_real_

cancer_registry_summary <- data.table(
  eid = cancer_register$eid,
  
  ICD_10_overall_cancer = as.integer(
    registry_icd10_flag
  ),
  
  ICD_9_overall_cancer = as.integer(
    registry_icd9_flag
  ),
  
  registry_cancer_date = as.Date(
    registry_best_date_num,
    origin = "1970-01-01"
  ),
  
  registry_specific_cancer_code =
    registry_best_code
)


# ------------------------------------------------------------
# 2. Hospital inpatient Summary Diagnoses: repair ICD/date pairing
# ------------------------------------------------------------
# 已依据糖尿病项目证实的同型导出格式修正：
# p41270 在一个源行/单元格内保存多个用 | 分隔的 ICD-10 编码。
# 该单元格的第 k 个编码（k 从 1 计数）对应同一 CSV 源行的
# p41280_a(k-1)；同一 eid 的第几行不代表数组下标。
# 若实际导出格式不同则中止，不猜测下标。

if (!all(c("eid", "p41270") %in% names(hospital_codes))) {
  stop("Summary_Diagnoses.csv 缺少 eid/p41270")
}

hospital_codes[, source_row := .I]
hospital_codes[, has_icd_list :=
  !is.na(p41270) & nzchar(trimws(as.character(p41270)))]
nonempty_source_rows <- which(hospital_codes$has_icd_list)
if (!length(nonempty_source_rows)) {
  stop("Summary_Diagnoses.csv 中 p41270 没有非空的诊断列表")
}
# 预期导出结构含有 | 分隔的多诊断列表；若完全没有，需要确认是不是另一种导出。
if (!any(grepl("|", hospital_codes$p41270[nonempty_source_rows], fixed = TRUE))) {
  stop("Summary_Diagnoses.csv 的 p41270 完全没有 | 分隔符，",
       "与已核实的糖尿病项目导出结构不符；请先检查原始格式。")
}

# 新糖尿病项目的同型导出：每个 eid 至多一个非空诊断列表。
# 如果同一 eid 存在多条非空列表，先检查是否属于另一种结构。
nonempty_by_eid <- hospital_codes[has_icd_list == TRUE, .N, by = eid]
ambiguous_eid <- nonempty_by_eid[N > 1L]
if (nrow(ambiguous_eid)) {
  fwrite(ambiguous_eid, file.path(qa_dir, "QA_03_hospital_ambiguous_eid.csv"))
  stop("Summary_Diagnoses.csv: ", nrow(ambiguous_eid),
       " 个 eid 有多条非空 p41270 列表，不能假定与糖尿病项目相同。",
       "请核查 QA_03_hospital_ambiguous_eid.csv 后再处理。")
}

# 只展开 C00-C97 的癌症 ICD-10 编码；维持原始源行与列表内下标。
# 使用诊断编码开头的 Cxx 匹配，允许后面出现亚码或疾病名称。
hospital_code_chunks <- vector("list",
  ceiling(length(nonempty_source_rows) / 15000L))
hospital_chunk_size <- 15000L
for (chunk_id in seq_along(hospital_code_chunks)) {
  start_row <- (chunk_id - 1L) * hospital_chunk_size + 1L
  source_rows <- nonempty_source_rows[
    start_row:min(start_row + hospital_chunk_size - 1L,
                  length(nonempty_source_rows))
  ]
  lists <- strsplit(as.character(hospital_codes$p41270[source_rows]),
                    "|", fixed = TRUE)
  pieces <- lapply(seq_along(lists), function(j) {
    codes <- trimws(lists[[j]])
    valid <- which(is_malignant_icd10(codes))
    if (!length(valid)) return(NULL)
    # 原始列表位置 - 1，才是对应 p41280 日期的 array index。
    parsed_codes <- toupper(codes[valid])
    # 提取编码本体，不能把英文疾病描述拼进 ICD 字段。
    parsed_codes <- sub(
      "^([A-Z][0-9]{2}(?:\\.[0-9A-Z]+|[0-9A-Z]{1,4})?).*$",
      "\\1", parsed_codes, perl = TRUE
    )
    parsed_codes <- gsub("\\.", "", parsed_codes)
    data.table(
      eid = rep(hospital_codes$eid[source_rows[j]], length(valid)),
      source_row = rep(source_rows[j], length(valid)),
      diagnosis_array_index = valid - 1L,
      hospital_cancer_code = parsed_codes
    )
  })
  hospital_code_chunks[[chunk_id]] <-
    rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (chunk_id %% 10L == 0L) {
    cat("医院诊断列表拆分进度:", chunk_id, "/",
        length(hospital_code_chunks), "\n")
  }
}
hospital_cancer_long <- rbindlist(hospital_code_chunks,
                                  use.names = TRUE, fill = TRUE)
rm(hospital_code_chunks)
if (!nrow(hospital_cancer_long)) {
  stop("医院 p41270 的诊断列表中没有找到 C00-C97，请先检查导出结构")
}
stopifnot(!anyNA(hospital_cancer_long$diagnosis_array_index),
          all(hospital_cancer_long$diagnosis_array_index >= 0L))

# 按需读取日期数组，使用原 CSV 源行定位，而不是通过 eid 行序。
hospital_header <- names(fread(summary_diagnoses_file, nrows = 0L))
all_hospital_date_cols <- grep("^p41280_a[0-9]+$",
                               hospital_header, value = TRUE)
if (!length(all_hospital_date_cols)) {
  stop("Summary_Diagnoses.csv 不包含 p41280_a* 日期列")
}
needed_hospital_indices <- sort(unique(
  hospital_cancer_long$diagnosis_array_index
))
needed_hospital_cols <- paste0("p41280_a", needed_hospital_indices)
missing_hospital_cols <- setdiff(needed_hospital_cols, all_hospital_date_cols)
if (length(missing_hospital_cols)) {
  stop("p41270 癌症编码的数组索引没有对应 p41280 日期列: ",
       paste(head(missing_hospital_cols, 20L), collapse = ", "))
}

hospital_cancer_long[, hospital_paired_date := as.Date(NA)]
for (batch_start in seq.int(1L, length(needed_hospital_indices), by = 32L)) {
  batch_indices <- needed_hospital_indices[
    batch_start:min(batch_start + 31L, length(needed_hospital_indices))
  ]
  batch <- fread(summary_diagnoses_file,
                 select = paste0("p41280_a", batch_indices))
  if (nrow(batch) != nrow(hospital_codes)) {
    stop("医院 CSV 多次读取的源行数不一致，不能保证 ICD/date 一一配对")
  }
  for (k in batch_indices) {
    idx <- which(hospital_cancer_long$diagnosis_array_index == k)
    paired_date <- parse_ukb_date(
      batch[[paste0("p41280_a", k)]][
        hospital_cancer_long$source_row[idx]
      ], col_name = paste0("p41280_a", k)
    )
    set(hospital_cancer_long, i = idx,
        j = "hospital_paired_date", value = paired_date)
  }
  rm(batch)
  gc(verbose = FALSE)
}

n_hospital_cancer_codes <- nrow(hospital_cancer_long)
n_hospital_cancer_dated <- sum(!is.na(
  hospital_cancer_long$hospital_paired_date
))
if (n_hospital_cancer_dated == 0L) {
  stop("医院 C00-C97 有识别记录，但没有任何成功匹配的日期；请先检查源字段和配对方式")
}

hospital_pairing_QA <- data.table(
  metric = c("source_rows", "source_eids", "nonempty_p41270_rows",
             "malignant_C00_C97_codes", "malignant_codes_with_date",
             "malignant_codes_without_date", "malignant_code_date_pct",
             "max_malignant_array_index"),
  value = as.character(c(
    nrow(hospital_codes), uniqueN(hospital_codes$eid),
    length(nonempty_source_rows), n_hospital_cancer_codes,
    n_hospital_cancer_dated,
    n_hospital_cancer_codes - n_hospital_cancer_dated,
    round(100 * n_hospital_cancer_dated / n_hospital_cancer_codes, 3),
    max(hospital_cancer_long$diagnosis_array_index)
  ))
)
fwrite(hospital_pairing_QA,
       file.path(qa_dir, "QA_03_hospital_cancer_pairing.csv"))
fwrite(head(hospital_cancer_long, 30L),
       file.path(qa_dir, "QA_03_hospital_cancer_pair_examples.csv"))
cat("\n医院 C00-C97 诊断编码和 p41280 日期配对 QA:\n")
print(hospital_pairing_QA)

# 按 eid 合并住院癌症诊断；事件日期取该 eid 所有癌症诊断的最早日期。
hospital_cancer_summary <- hospital_cancer_long[
  , {
    dated_idx <- which(!is.na(hospital_paired_date))
    if (length(dated_idx)) {
      chosen <- dated_idx[which.min(hospital_paired_date[dated_idx])]
      best_date <- hospital_paired_date[chosen]
      best_code <- hospital_cancer_code[chosen]
    } else {
      best_date <- as.Date(NA)
      best_code <- hospital_cancer_code[1L]
    }
    list(
      hospital_cancer = 1L,
      hospital_cancer_date = best_date,
      hospital_specific_cancer_code = best_code,
      hospital_cancer_source_rows = .N
    )
  }, by = eid
]
# 与旧 03 输出字段保持兼容：源行数仍指原 CSV 里该 eid 的源行数。
hospital_source_counts <- hospital_codes[
  eid %in% hospital_cancer_summary$eid, .N, by = eid
]
setnames(hospital_source_counts, "N", "hospital_source_rows")
hospital_cancer_summary <- merge(
  hospital_cancer_summary, hospital_source_counts,
  by = "eid", all.x = TRUE, sort = FALSE
)
hospital_cancer_summary[, hospital_cancer_unknown_timing :=
  as.integer(is.na(hospital_cancer_date))]
if (anyDuplicated(hospital_cancer_summary$eid)) {
  stop("住院癌症诊断按 eid 汇总后出现重复 eid")
}
cat("医院诊断癌症人数:", nrow(hospital_cancer_summary),
    "；有有效最早日期:",
    sum(!is.na(hospital_cancer_summary$hospital_cancer_date)),
    "；日期未知:",
    sum(hospital_cancer_summary$hospital_cancer_unknown_timing), "\n")

# ------------------------------------------------------------
# 3. Death date and malignant cancer recorded in death causes
# ------------------------------------------------------------

death_date_cols <- grep(
  "^p40000_i[0-9]+$",
  names(death_data),
  value = TRUE
)

if (length(death_date_cols) == 0L) {
  stop(
    "No death date fields matching p40000_i* were found."
  )
}

for (col in death_date_cols) {
  set(
    death_data,
    j = col,
    value = parse_ukb_date(
      death_data[[col]],
      col_name = col
    )
  )
}

death_date_num <- rep(
  Inf,
  nrow(death_data)
)

for (col in death_date_cols) {
  
  x <- as.numeric(
    death_data[[col]]
  )
  
  x[
    is.na(x)
  ] <- Inf
  
  death_date_num <- pmin(
    death_date_num,
    x
  )
}

death_date_num[
  !is.finite(death_date_num)
] <- NA_real_

death_date <- as.Date(
  death_date_num,
  origin = "1970-01-01"
)

death_cause_cols <- grep(
  "^p4000(1|2)(_|$)",
  names(death_data),
  value = TRUE
)

death_cancer_flag <- rep(
  FALSE,
  nrow(death_data)
)

death_cancer_code <- rep(
  NA_character_,
  nrow(death_data)
)

if (length(death_cause_cols) > 0L) {
  
  for (col in death_cause_cols) {
    
    x <- death_data[[col]]
    
    is_cancer <- is_malignant_icd10(
      x
    )
    
    use_code <- is_cancer &
      is.na(death_cancer_code)
    
    death_cancer_code[
      use_code
    ] <- normalize_icd(
      x[
        use_code
      ]
    )
    
    death_cancer_flag <-
      death_cancer_flag |
      is_cancer
  }
  
} else {
  
  warning(
    paste0(
      "No death-cause fields p40001/p40002 were found. ",
      "Death-register cancer cannot be added, ",
      "but death dates will still be used for censoring."
    )
  )
}

death_cancer_date <- as.Date(
  rep(
    NA_real_,
    nrow(death_data)
  ),
  origin = "1970-01-01"
)

idx_death_cancer <- which(
  death_cancer_flag &
    !is.na(death_date)
)

death_cancer_date[
  idx_death_cancer
] <- death_date[
  idx_death_cancer
]

death_summary <- data.table(
  eid = death_data$eid,
  
  death_date =
    death_date,
  
  death_cancer =
    as.integer(
      death_cancer_flag
    ),
  
  death_cancer_date =
    death_cancer_date,
  
  death_specific_cancer_code =
    death_cancer_code
)


# ------------------------------------------------------------
# 4. Combine cancer registry + hospital inpatient + death register
# ------------------------------------------------------------

cancer_data <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = "eid",
      all = TRUE,
      sort = FALSE
    )
  },
  list(
    cancer_registry_summary,
    hospital_cancer_summary,
    death_summary
  )
)

# Participants absent from a source have no record from that source.
for (col in c(
  "ICD_10_overall_cancer",
  "ICD_9_overall_cancer",
  "hospital_cancer",
  "death_cancer"
)) {
  cancer_data[
    is.na(get(col)),
    (col) := 0L
  ]
}


# ------------------------------------------------------------
# Previous definition:
# cancer registry + death register
# ------------------------------------------------------------

cancer_data[
  ,
  previous_overall_cancer :=
    as.integer(
      ICD_10_overall_cancer == 1L |
        ICD_9_overall_cancer == 1L |
        death_cancer == 1L
    )
]

cancer_data[
  ,
  previous_cancer_date :=
    row_min_date(
      registry_cancer_date,
      death_cancer_date
    )
]


# ------------------------------------------------------------
# New definition:
# cancer registry + hospital inpatient + death register
# ------------------------------------------------------------

cancer_data[
  ,
  cancer_date :=
    row_min_date(
      registry_cancer_date,
      hospital_cancer_date,
      death_cancer_date
    )
]

cancer_data[
  ,
  overall_cancer :=
    as.integer(
      previous_overall_cancer == 1L |
        hospital_cancer == 1L
    )
]

cancer_data[
  ,
  cancer_unknown_timing :=
    as.integer(
      overall_cancer == 1L &
        is.na(cancer_date)
    )
]


# Earliest source-specific code for QA / description.
cancer_data[
  ,
  specific_cancer_code :=
    fcase(
      
      overall_cancer == 0L,
      NA_character_,
      
      !is.na(registry_cancer_date) &
        registry_cancer_date == cancer_date,
      registry_specific_cancer_code,
      
      !is.na(hospital_cancer_date) &
        hospital_cancer_date == cancer_date,
      hospital_specific_cancer_code,
      
      !is.na(death_cancer_date) &
        death_cancer_date == cancer_date,
      death_specific_cancer_code,
      
      default = NA_character_
    )
]


# ------------------------------------------------------------
# QA: quantify how much the hospital source changes cancer
# ------------------------------------------------------------

cancer_data[
  ,
  hospital_new_cancer :=
    as.integer(
      hospital_cancer == 1L &
        previous_overall_cancer == 0L
    )
]

cancer_data[
  ,
  hospital_earlier_date_existing_cancer :=
    as.integer(
      previous_overall_cancer == 1L &
        !is.na(hospital_cancer_date) &
        (
          is.na(previous_cancer_date) |
            hospital_cancer_date <
            previous_cancer_date
        )
    )
]

cancer_data[
  ,
  hospital_new_or_earlier_date :=
    as.integer(
      !is.na(hospital_cancer_date) &
        (
          is.na(previous_cancer_date) |
            hospital_cancer_date <
            previous_cancer_date
        )
    )
]

previous_cancer_N <- sum(
  cancer_data$previous_overall_cancer == 1L,
  na.rm = TRUE
)

new_cancer_N <- sum(
  cancer_data$overall_cancer == 1L,
  na.rm = TRUE
)

new_hospital_only_N <- sum(
  cancer_data$hospital_new_cancer == 1L,
  na.rm = TRUE
)

earlier_date_existing_N <- sum(
  cancer_data$hospital_earlier_date_existing_cancer == 1L,
  na.rm = TRUE
)

hospital_new_or_earlier_N <- sum(
  cancer_data$hospital_new_or_earlier_date == 1L,
  na.rm = TRUE
)

new_unknown_timing_N <- sum(
  cancer_data$overall_cancer == 1L &
    is.na(cancer_data$cancer_date),
  na.rm = TRUE
)

cat(
  "\n============================================================\n",
  "OVERALL CANCER DEFINITION COMPARISON\n",
  "Previous definition = cancer registry + death register\n",
  "New definition      = cancer registry + hospital inpatient + death register\n",
  "------------------------------------------------------------\n",
  "Registry ICD10 C00-C97:",
  sum(
    cancer_data$ICD_10_overall_cancer == 1L,
    na.rm = TRUE
  ),
  "\nRegistry ICD9 140-208:",
  sum(
    cancer_data$ICD_9_overall_cancer == 1L,
    na.rm = TRUE
  ),
  "\nHospital inpatient ICD10 C00-C97:",
  sum(
    cancer_data$hospital_cancer == 1L,
    na.rm = TRUE
  ),
  "\nDeath-register ICD10 C00-C97:",
  sum(
    cancer_data$death_cancer == 1L,
    na.rm = TRUE
  ),
  "\n------------------------------------------------------------\n",
  "Previous overall cancer N:",
  previous_cancer_N,
  "\nNew overall cancer N:",
  new_cancer_N,
  "\nNet change in overall cancer N:",
  new_cancer_N - previous_cancer_N,
  "\nNew cancer participants identified ONLY after adding hospital:",
  new_hospital_only_N,
  "\nExisting cancer participants whose earliest date becomes earlier after hospital:",
  earlier_date_existing_N,
  "\nParticipants for whom hospital supplies a new or earlier usable cancer date:",
  hospital_new_or_earlier_N,
  "\nNew definition: cancer records with unknown onset timing:",
  new_unknown_timing_N,
  "\n============================================================\n"
)

if (new_cancer_N < previous_cancer_N) {
  stop(
    "QA failed: adding hospital cancer unexpectedly reduced overall cancer count."
  )
}

if (
  new_cancer_N - previous_cancer_N !=
  new_hospital_only_N
) {
  stop(
    paste0(
      "QA failed: net increase in overall cancer does not equal ",
      "hospital-only newly identified cancer participants."
    )
  )
}


# 癌症三来源汇总暂存于内存，待 MAIN 队列人数核实后再写入正式目录。

# Merge cancer + death information into the 35 non-cancer diseases.
UKB_diseases_data <- merge(
  UKB_diseases_data,
  cancer_data[
    ,
    .(
      eid,
      overall_cancer,
      cancer_date,
      specific_cancer_code,
      death_date
    )
  ],
  by = "eid",
  all.x = TRUE,
  sort = FALSE
)

setDT(
  UKB_diseases_data
)


# ============================================================
# Explicit 36-disease variable list
# ============================================================

disease_cols <- c(
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

time_cols <- c(
  "Anaemia_time",
  "Angina_time",
  "Anxiety_or_panic_attacks_time",
  "Atrial_fibrillation_time",
  "Asthma_time",
  "Bronchiectasis_time",
  "Chronic_kidney_disease_time",
  "Chronic_obstructive_pulmonary_disease_time",
  "Chronic_sinusitis_time",
  "Cirrhosis_time",
  "Dementia_time",
  "Depression_time",
  "Diabetes_time",
  "Eczema_or_dermatitis_time",
  "Epilepsy_time",
  "Glaucoma_time",
  "Heart_failure_time",
  "Hepatitis_time",
  "Hypertension_time",
  "Inflammatory_bowel_disease_time",
  "Irritable_bowel_syndrome_time",
  "Meningitis_time",
  "Migraine_time",
  "Multiple_sclerosis_time",
  "Myocardial_infarction_time",
  "Osteoporosis_time",
  "Parkinson_disease_time",
  "Peripheral_vascular_disease_time",
  "Prostate_problem_time",
  "Rheumatoid_arthritis_time",
  "Schizophrenia_time",
  "Stroke_time",
  "Thyroid_problem_time",
  "Tuberculosis_time",
  "Vestibular_disorder_time",
  "cancer_date"
)

if (length(disease_cols) != 36L) {
  stop(
    paste0(
      "Expected 36 diseases, found ",
      length(disease_cols),
      "."
    )
  )
}

missing_disease_cols <- setdiff(
  disease_cols,
  names(
    UKB_diseases_data
  )
)

missing_time_cols <- setdiff(
  time_cols,
  names(
    UKB_diseases_data
  )
)

if (
  length(
    missing_disease_cols
  ) > 0L |
  length(
    missing_time_cols
  ) > 0L
) {
  
  stop(
    paste0(
      "Missing disease columns: ",
      paste(
        missing_disease_cols,
        collapse = ", "
      ),
      "\nMissing disease-time columns: ",
      paste(
        missing_time_cols,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# Disease records with unknown onset timing
# ============================================================
#
# If disease status is present but onset time is unusable, we cannot know
# whether the disease was prevalent at recruitment or incident afterwards.
# These participants are therefore flagged and excluded from the PRIMARY
# strict incident multimorbidity cohort.

unknown_disease_timing_count <- integer(
  nrow(UKB_diseases_data)
)

unknown_timing_by_disease <- vector(
  "list",
  length(disease_cols)
)

for (j in seq_along(disease_cols)) {
  
  disease_col <- disease_cols[[j]]
  time_col <- time_cols[[j]]
  
  unknown_flag <-
    UKB_diseases_data[[disease_col]] == 1L &
    is.na(
      UKB_diseases_data[[time_col]]
    )
  
  unknown_flag[
    is.na(unknown_flag)
  ] <- FALSE
  
  unknown_disease_timing_count <-
    unknown_disease_timing_count +
    as.integer(
      unknown_flag
    )
  
  unknown_timing_by_disease[[j]] <- data.table(
    disease = disease_col,
    n_unknown_timing = sum(
      unknown_flag,
      na.rm = TRUE
    )
  )
}

UKB_diseases_data[
  ,
  unknown_disease_timing_count :=
    unknown_disease_timing_count
]

UKB_diseases_data[
  ,
  unknown_disease_timing_any :=
    unknown_disease_timing_count > 0L
]

unknown_timing_by_disease <- rbindlist(
  unknown_timing_by_disease,
  use.names = TRUE,
  fill = TRUE
)

cat(
  "\nParticipants with >=1 disease record but unknown onset timing:",
  sum(
    UKB_diseases_data$unknown_disease_timing_any,
    na.rm = TRUE
  ),
  "\n"
)

# ============================================================
# Strict incident multimorbidity
# ============================================================
#
# Primary estimand:
# first transition from <2 chronic conditions at recruitment
# to >=2 chronic conditions during valid follow-up.
#
# baseline disease count = 0:
#   second distinct incident disease -> event.
#
# baseline disease count = 1:
#   first additional distinct incident disease -> event.
#
# baseline disease count >= 2:
#   excluded at baseline, regardless of later disease.
#
# No 999 outcome category is used.


# ------------------------------------------------------------
# Administrative censor dates
# ------------------------------------------------------------
#
# These dates are carried forward from the old workflow.
# IMPORTANT: before the final analysis is frozen, verify that these are
# valid coverage endpoints for the FULL first-occurrence source used for
# all 36 diseases, rather than hospital inpatient data alone.

ADMIN_CENSOR_DATES_VERIFIED <- FALSE

if (!ADMIN_CENSOR_DATES_VERIFIED) {
  warning(
    paste0(
      "Administrative censor dates are carried forward from ",
      "the old workflow and still require source-level verification."
    )
  )
}

UKB_diseases_data[
  ,
  administrative_end :=
    fcase(
      
      recruitment_centre ==
        "Wales",
      as.Date(
        "2022-05-31"
      ),
      
      recruitment_centre ==
        "Scotland",
      as.Date(
        "2022-08-31"
      ),
      
      recruitment_centre ==
        "England",
      as.Date(
        "2023-03-31"
      ),
      
      default =
        as.Date(NA)
    )
]


# Individual censoring = min(administrative end, death date)
UKB_diseases_data[
  ,
  censor_date :=
    row_min_date(
      administrative_end,
      death_date
    )
]


# ------------------------------------------------------------
# Explicit follow-up validity
# ------------------------------------------------------------

UKB_diseases_data[
  ,
  valid_followup :=
    !is.na(
      recruitment_date
    ) &
    !is.na(
      censor_date
    ) &
    censor_date >
    recruitment_date
]


# ------------------------------------------------------------
# Count distinct diseases at baseline and within valid follow-up
# ------------------------------------------------------------

n_person <- nrow(
  UKB_diseases_data
)

recruit_num <- as.numeric(
  UKB_diseases_data$recruitment_date
)

censor_num <- as.numeric(
  UKB_diseases_data$censor_date
)

baseline_disease_count <- integer(
  n_person
)

incident_disease_count <- integer(
  n_person
)

ever_disease_count <- integer(
  n_person
)

post_censor_disease_count <- integer(
  n_person
)

first_incident_num <- rep(
  Inf,
  n_person
)

second_incident_num <- rep(
  Inf,
  n_person
)

disease_audit <- vector(
  "list",
  length(
    time_cols
  )
)

for (j in seq_along(time_cols)) {
  
  col <- time_cols[[j]]
  
  x <- as.numeric(
    UKB_diseases_data[[col]]
  )
  
  recorded <- !is.na(
    x
  )
  
  baseline_flag <-
    recorded &
    !is.na(
      recruit_num
    ) &
    x <= recruit_num
  
  incident_flag <-
    recorded &
    UKB_diseases_data$valid_followup &
    x > recruit_num &
    x <= censor_num
  
  post_censor_flag <-
    recorded &
    UKB_diseases_data$valid_followup &
    x > censor_num
  
  ever_disease_count <-
    ever_disease_count +
    as.integer(
      recorded
    )
  
  baseline_disease_count <-
    baseline_disease_count +
    as.integer(
      baseline_flag
    )
  
  incident_disease_count <-
    incident_disease_count +
    as.integer(
      incident_flag
    )
  
  post_censor_disease_count <-
    post_censor_disease_count +
    as.integer(
      post_censor_flag
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
  
  disease_audit[[j]] <- data.table(
    disease =
      disease_cols[[j]],
    
    time_variable =
      col,
    
    n_recorded =
      sum(
        recorded,
        na.rm = TRUE
      ),
    
    n_baseline =
      sum(
        baseline_flag,
        na.rm = TRUE
      ),
    
    n_incident_within_censor =
      sum(
        incident_flag,
        na.rm = TRUE
      ),
    
    n_after_censor =
      sum(
        post_censor_flag,
        na.rm = TRUE
      ),
    
    n_unknown_timing =
      sum(
        UKB_diseases_data[[disease_cols[[j]]]] == 1L &
          is.na(
            UKB_diseases_data[[time_cols[[j]]]]
          ),
        na.rm = TRUE
      )
  )
}

first_incident_num[
  !is.finite(
    first_incident_num
  )
] <- NA_real_

second_incident_num[
  !is.finite(
    second_incident_num
  )
] <- NA_real_


UKB_diseases_data[
  ,
  baseline_disease_count :=
    baseline_disease_count
]

UKB_diseases_data[
  ,
  incident_disease_count :=
    incident_disease_count
]

UKB_diseases_data[
  ,
  ever_disease_count :=
    ever_disease_count
]

UKB_diseases_data[
  ,
  post_censor_disease_count :=
    post_censor_disease_count
]


# ------------------------------------------------------------
# Primary baseline eligibility
# ------------------------------------------------------------

UKB_diseases_data[
  ,
  eligible_incident_multimorbidity :=
    valid_followup &
    !unknown_disease_timing_any &
    baseline_disease_count < 2L
]


# ------------------------------------------------------------
# Strict incident multimorbidity event
# ------------------------------------------------------------

UKB_diseases_data[
  ,
  incident_multimorbidity :=
    as.integer(
      eligible_incident_multimorbidity &
        (
          (
            baseline_disease_count ==
              0L &
              incident_disease_count >=
              2L
          ) |
            (
              baseline_disease_count ==
                1L &
                incident_disease_count >=
                1L
            )
        )
    )
]

event_date_num <- rep(
  NA_real_,
  n_person
)

idx_baseline0_event <- which(
  UKB_diseases_data$incident_multimorbidity ==
    1L &
    UKB_diseases_data$baseline_disease_count ==
    0L
)

idx_baseline1_event <- which(
  UKB_diseases_data$incident_multimorbidity ==
    1L &
    UKB_diseases_data$baseline_disease_count ==
    1L
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

UKB_diseases_data[
  ,
  incident_multimorbidity_event_date :=
    as.Date(
      event_date_num,
      origin = "1970-01-01"
    )
]


# Event cases end follow-up at event; non-events end at censor.
UKB_diseases_data[
  ,
  followup_end_date :=
    fifelse(
      incident_multimorbidity ==
        1L,
      incident_multimorbidity_event_date,
      censor_date
    )
]

UKB_diseases_data[
  ,
  followup_end_date :=
    as.Date(
      followup_end_date,
      origin = "1970-01-01"
    )
]

UKB_diseases_data[
  ,
  follow_time :=
    as.numeric(
      followup_end_date -
        recruitment_date
    )
]


# ------------------------------------------------------------
# Backward-compatible names
# ------------------------------------------------------------
#
# "comorbidity" now means strict incident multimorbidity (0/1 only).
# "comorbidity_time" is the event/censor follow-up-end date.
# There is intentionally NO 999 category.

UKB_diseases_data[
  ,
  comorbidity :=
    incident_multimorbidity
]

UKB_diseases_data[
  ,
  comorbidity_time :=
    followup_end_date
]


# ------------------------------------------------------------
# Explicit exclusion reason
# ------------------------------------------------------------

UKB_diseases_data[
  ,
  primary_exclusion_reason :=
    fcase(
      
      is.na(
        recruitment_date
      ),
      "Missing recruitment date",
      
      is.na(
        recruitment_centre
      ),
      "Missing recruitment country",
      
      is.na(
        censor_date
      ),
      "Missing censor date",
      
      !is.na(
        censor_date
      ) &
        !is.na(
          recruitment_date
        ) &
        censor_date <=
        recruitment_date,
      "Non-positive valid follow-up",
      
      unknown_disease_timing_any,
      "Disease record with unknown onset timing",
      
      baseline_disease_count >=
        2L,
      "Baseline multimorbidity (>=2 diseases)",
      
      default =
        "Included in primary incident cohort"
    )
]


# ============================================================
# QA: event/censor consistency
# ============================================================

if (
  any(
    UKB_diseases_data$incident_multimorbidity ==
    1L &
    (
      is.na(
        UKB_diseases_data$
        incident_multimorbidity_event_date
      ) |
      UKB_diseases_data$
      incident_multimorbidity_event_date <=
      UKB_diseases_data$
      recruitment_date |
      UKB_diseases_data$
      incident_multimorbidity_event_date >
      UKB_diseases_data$
      censor_date
    ),
    na.rm = TRUE
  )
) {
  stop(
    "QA failed: at least one event date is outside valid follow-up."
  )
}

if (
  any(
    UKB_diseases_data$
    eligible_incident_multimorbidity &
    (
      is.na(
        UKB_diseases_data$
        follow_time
      ) |
      UKB_diseases_data$
      follow_time <=
      0
    ),
    na.rm = TRUE
  )
) {
  stop(
    "QA failed: eligible primary cohort contains non-positive follow-up."
  )
}


# ============================================================
# Cohort flow and disease audit
# ============================================================

cohort_flow <- data.table(
  step = c(
    "Recruitment roster",
    "Invalid/missing follow-up",
    "Disease record with unknown onset timing",
    "Baseline multimorbidity (>=2 diseases)",
    "Primary incident multimorbidity cohort",
    "Primary cohort: baseline disease count = 0",
    "Primary cohort: baseline disease count = 1",
    "Primary incident multimorbidity events",
    "Primary non-events/censored"
  ),
  
  N = c(
    nrow(
      UKB_diseases_data
    ),
    
    sum(
      !UKB_diseases_data$
        valid_followup,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        valid_followup &
        UKB_diseases_data$
        unknown_disease_timing_any,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        valid_followup &
        !UKB_diseases_data$
        unknown_disease_timing_any &
        UKB_diseases_data$
        baseline_disease_count >=
        2L,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        eligible_incident_multimorbidity,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        eligible_incident_multimorbidity &
        UKB_diseases_data$
        baseline_disease_count ==
        0L,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        eligible_incident_multimorbidity &
        UKB_diseases_data$
        baseline_disease_count ==
        1L,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        eligible_incident_multimorbidity &
        UKB_diseases_data$
        incident_multimorbidity ==
        1L,
      na.rm = TRUE
    ),
    
    sum(
      UKB_diseases_data$
        eligible_incident_multimorbidity &
        UKB_diseases_data$
        incident_multimorbidity ==
        0L,
      na.rm = TRUE
    )
  )
)

print(
  cohort_flow
)

cat(
  "\nStrict incident multimorbidity status in eligible cohort:\n"
)

print(
  table(
    UKB_diseases_data[
      eligible_incident_multimorbidity ==
        TRUE,
      incident_multimorbidity
    ],
    useNA = "ifany"
  )
)

cat(
  "\nBaseline disease count in eligible cohort:\n"
)

print(
  table(
    UKB_diseases_data[
      eligible_incident_multimorbidity ==
        TRUE,
      baseline_disease_count
    ],
    useNA = "ifany"
  )
)

cat(
  "\nParticipants with >=1 disease onset after censor date:",
  sum(
    UKB_diseases_data$
      post_censor_disease_count >
      0L,
    na.rm = TRUE
  ),
  "\n"
)

disease_definition_audit <- rbindlist(
  disease_audit,
  use.names = TRUE,
  fill = TRUE
)


# ============================================================
# Final primary cohort
# ============================================================

UKB_diseases_data_all <- copy(
  UKB_diseases_data
)

UKB_diseases_data <- UKB_diseases_data_all[
  eligible_incident_multimorbidity ==
    TRUE
]

# 修复住院 ICD-10 / 诊断日期匹配后，MAIN 人数应以实际合格人数为准。
# 不强制等于历史人数：疾病发生日期修正可能改变基线疾病数量和纳入资格。
# 仍保留空队列、重复 eid 等实质性错误检查，并记录新旧人数供论文更新。
if (nrow(UKB_diseases_data) == 0L) {
  stop("MAIN cohort is empty; please inspect disease onset and eligibility logic.")
}
if (anyDuplicated(UKB_diseases_data$eid) > 0L) {
  stop("MAIN cohort contains duplicated eid.")
}

fwrite(
  cohort_flow,
  file.path(qa_dir, "QA_03_cohort_flow_after_hospital_fix.csv")
)

historical_main_n <- 309378L  # 仅作旧版人数对照，不作为纳入或停止条件
current_main_n <- nrow(UKB_diseases_data)
main_cohort_n_comparison <- data.table(
  metric = c("Historical_MAIN_N", "Current_MAIN_N", "Difference_current_minus_historical"),
  N = c(historical_main_n, current_main_n, current_main_n - historical_main_n)
)
fwrite(
  main_cohort_n_comparison,
  file.path(qa_dir, "QA_03_main_cohort_n_comparison.csv")
)
cat(
  "\nMAIN cohort after hospital diagnosis/date correction:", current_main_n,
  "\nHistorical MAIN cohort:", historical_main_n,
  "\nDifference:", current_main_n - historical_main_n,
  "\nActual cohort size is accepted; check the QA files for the reason.\n"
)



# Save three-source cancer ascertainment and comparison variables.
fwrite(
  cancer_data,
  file.path(
    analysis_output_dir,
    "cancer_data.csv"
  )
)

saveRDS(
  cancer_data,
  file.path(
    analysis_output_dir,
    "cancer_data.rds"
  )
)



# ============================================================
# Count distributions
# ============================================================

baseline_disease_count_table <-
  UKB_diseases_data_all[
    ,
    .N,
    by =
      baseline_disease_count
  ][
    order(
      baseline_disease_count
    )
  ]

setnames(
  baseline_disease_count_table,
  "N",
  "frequency"
)

ever_disease_count_table <-
  UKB_diseases_data_all[
    ,
    .N,
    by =
      ever_disease_count
  ][
    order(
      ever_disease_count
    )
  ]

setnames(
  ever_disease_count_table,
  "N",
  "frequency"
)


# ============================================================
# Export
# ============================================================
fwrite(
  UKB_diseases_data_all,
  file.path(
    analysis_output_dir,
    "UKB_diseases_data_all_strict_incident.csv"
  )
)

saveRDS(
  UKB_diseases_data_all,
  file.path(
    analysis_output_dir,
    "UKB_diseases_data_all_strict_incident.rds"
  )
)


# Familiar filename = PRIMARY eligible strict-incident cohort
# ============================================================
# Final compact disease dataset
# eid + 36 disease indicators + 36 disease onset dates
# ============================================================

disease_keep_cols <- c(
  "eid",
  disease_cols,
  time_cols
)

UKB_diseases_data_compact <- copy(
  UKB_diseases_data[
    ,
    ..disease_keep_cols
  ]
)

cat(
  "\nCompact disease dataset:\n",
  "N participants:",
  nrow(UKB_diseases_data_compact),
  "\nN variables:",
  ncol(UKB_diseases_data_compact),
  "\n"
)

stopifnot(
  ncol(UKB_diseases_data_compact) == 73L
)

print(
  names(UKB_diseases_data_compact)
)
fwrite(
  UKB_diseases_data_compact,
  file.path(
    analysis_output_dir,
    "UKB_diseases_data.csv"
  )
)

saveRDS(
  UKB_diseases_data,
  file.path(
    analysis_output_dir,
    "UKB_diseases_data.rds"
  )
)

fwrite(
  cohort_flow,
  file.path(
    analysis_output_dir,
    "cohort_flow_strict_incident_multimorbidity.csv"
  )
)

fwrite(
  disease_definition_audit,
  file.path(
    qa_dir,
    "disease_definition_audit.csv"
  )
)


fwrite(
  baseline_disease_count_table,
  file.path(
    analysis_output_dir,
    "baseline_disease_count.csv"
  )
)


cat(
  "\nCompleted strict incident multimorbidity construction.\n",
  "Final primary cohort N:",
  nrow(
    UKB_diseases_data
  ),
  "\nEvents:",
  sum(
    UKB_diseases_data$
      incident_multimorbidity ==
      1L,
    na.rm = TRUE
  ),
  "\nNon-events:",
  sum(
    UKB_diseases_data$
      incident_multimorbidity ==
      0L,
    na.rm = TRUE
  ),
  "\n"
)
#merge other data####
# ============================================================
# Main analysis and sensitivity-analysis multimodal datasets
# ============================================================
#
# COHORT DEFINITIONS
# ------------------
# Main analysis cohort:
#   1) valid follow-up;
#   2) exclude participants with >=1 disease record whose onset timing is unknown;
#   3) exclude participants with baseline multimorbidity (>=2 diseases at recruitment).
#
# Sensitivity-analysis cohort:
#   starts from the main analysis cohort and additionally excludes participants
#   who are completely missing ANY of the four views:
#   Environment, Biomarkers, Metabolomics, or Genetics.
#
# Partial missingness within an available view is retained in BOTH analyses.
# No imputation is performed here.

# ============================================================
# 1. Main-analysis disease/outcome base
# ============================================================

analysis_base_cols <- c(
  "eid",
  disease_cols,
  time_cols,
  "baseline_disease_count",
  "incident_disease_count",
  "ever_disease_count",
  "post_censor_disease_count",
  "incident_multimorbidity",
  "incident_multimorbidity_event_date",
  "censor_date",
  "followup_end_date",
  "follow_time"
)

analysis_base_cols <- intersect(
  analysis_base_cols,
  names(UKB_diseases_data)
)

# UKB_diseases_data has already been restricted above to
# eligible_incident_multimorbidity == TRUE, i.e. the MAIN cohort.
analysis_base_main <- copy(
  UKB_diseases_data[
    ,
    ..analysis_base_cols
  ]
)

if (anyDuplicated(analysis_base_main$eid) > 0L) {
  stop("analysis_base_main contains duplicated eid.")
}
# 检查结局数据与上一步实际 MAIN cohort 完全一致；不强制历史样本量。
if (nrow(analysis_base_main) != nrow(UKB_diseases_data) ||
    !identical(analysis_base_main$eid, UKB_diseases_data$eid)) {
  stop("MAIN outcome base does not match the newly defined eligible cohort.")
}

cat(
  "\n============================================================\n",
  "MAIN ANALYSIS COHORT\n",
  "N:", nrow(analysis_base_main), "\n",
  "Events:", sum(analysis_base_main$incident_multimorbidity == 1L, na.rm = TRUE), "\n",
  "Non-events:", sum(analysis_base_main$incident_multimorbidity == 0L, na.rm = TRUE), "\n",
  "============================================================\n"
)

# ============================================================
# 2. Main-analysis exclusion flow
# ============================================================
# Sequential exclusions are calculated from the original recruitment roster.

main_flow <- data.table(
  step = c(
    "Recruitment roster",
    "Valid follow-up",
    "Exclude disease record with unknown onset timing",
    "Exclude baseline multimorbidity (>=2 diseases)",
    "Main analysis cohort"
  ),
  N = c(
    nrow(UKB_diseases_data_all),
    sum(UKB_diseases_data_all$valid_followup, na.rm = TRUE),
    sum(
      UKB_diseases_data_all$valid_followup &
        !UKB_diseases_data_all$unknown_disease_timing_any,
      na.rm = TRUE
    ),
    sum(
      UKB_diseases_data_all$valid_followup &
        !UKB_diseases_data_all$unknown_disease_timing_any &
        UKB_diseases_data_all$baseline_disease_count < 2L,
      na.rm = TRUE
    ),
    nrow(analysis_base_main)
  )
)

main_flow[
  ,
  excluded_from_previous := c(
    NA_integer_,
    head(N, -1L) - tail(N, -1L)
  )
]

if (main_flow$N[4L] != nrow(analysis_base_main) ||
    main_flow$N[5L] != nrow(analysis_base_main)) {
  stop("Main cohort flow does not match analysis_base_main.")
}

print(main_flow)

# ============================================================
# 3. Helper functions for view availability
# ============================================================

count_observed <- function(DT, cols) {
  n_obs <- integer(nrow(DT))
  
  for (col in cols) {
    x <- DT[[col]]
    
    if (is.character(x)) {
      invalid_text <- c("","NA","NaN","NULL","Not applicable",
                        "Do not know","Prefer not to answer")
      observed <- !is.na(x) & !(trimws(x) %in% invalid_text)
    } else {
      observed <- !is.na(x) & is.finite(as.numeric(x))
    }
    
    n_obs <- n_obs + as.integer(observed)
  }
  
  n_obs
}

add_modality_flag <- function(DT, feature_cols, prefix) {
  feature_cols <- intersect(feature_cols, names(DT))
  
  if (length(feature_cols) == 0L) {
    stop(paste0("No feature columns found for ", prefix, "."))
  }
  
  n_obs <- count_observed(DT, feature_cols)
  
  DT[, (paste0(prefix, "_n_observed")) := n_obs]
  DT[, (paste0(prefix, "_missing_prop")) := 1 - n_obs / length(feature_cols)]
  DT[, (paste0(prefix, "_all_missing")) := n_obs == 0L]
}

# ============================================================
# 4. Environment / phenotypic view
# ============================================================

enviro_file <- file.path(recoded_input_dir,"enviro_exposure_no_date.csv")
enviro_data <- fread(enviro_file, header = TRUE)
setDT(enviro_data)

if (!"eid" %in% names(enviro_data)) {
  stop("enviro_data does not contain eid.")
}

if (anyDuplicated(enviro_data$eid) > 0L) {
  stop("enviro_data has duplicated eid.")
}

core_covariates_env <- c(
  "recruitment_age",
  "sex",
  "ethnicity_group",
  "recruitment_centre",
  "IMD_total",
  "BMI",
  "smoking_status"
)

# Core covariates do not determine whether the Environment view itself exists.
env_feature_cols <- setdiff(
  names(enviro_data),
  c("eid", core_covariates_env)
)

add_modality_flag(enviro_data, env_feature_cols, "env")
# 只报告、绝不统一删除问卷负值：需按每个 UKB Field ID 核对编码
negative_special <- c(-1,-2,-3,-7,-10)
env_negative_QA <- rbindlist(lapply(env_feature_cols,function(nm) {
  z <- enviro_data[[nm]]
  if (!is.numeric(z)) return(NULL)
  zz <- intersect(unique(z),negative_special)
  if (!length(zz)) return(NULL)
  data.table(variable=nm,n_special=sum(z %in% negative_special,na.rm=TRUE),
    observed_codes=paste(zz,collapse=";"))
}),use.names=TRUE,fill=TRUE)
if (!nrow(env_negative_QA)) env_negative_QA <- data.table(variable=character(),n_special=integer(),observed_codes=character())
fwrite(env_negative_QA,file.path(qa_dir,"QA_03_environment_negative_codes.csv"))

# ============================================================
# 5. NMR metabolomics view
# ============================================================

NMR_metabolites_2_file <- file.path(output_dir, "NMR_metabolites_2.csv")
NMR_metabolites_file <- file.path(output_dir, "blood_NMR_metabolomics.csv")

NMR_metabolites_2 <- fread(NMR_metabolites_2_file, header = TRUE)
NMR_metabolites <- fread(NMR_metabolites_file, header = TRUE)
setDT(NMR_metabolites_2)
setDT(NMR_metabolites)

if (anyDuplicated(NMR_metabolites_2$eid) > 0L) {
  stop("NMR_metabolites_2 has duplicated eid.")
}

if (anyDuplicated(NMR_metabolites$eid) > 0L) {
  stop("blood_NMR_metabolomics has duplicated eid.")
}

NMR_metabolites <- merge(
  NMR_metabolites,
  NMR_metabolites_2,
  by = "eid",
  all = TRUE,
  sort = FALSE,
  suffixes = c(".x", ".y")
)

nmr_merge_QA <- data.table(variable="23477-0.0",both_n=0L,discordant_n=0L,
                            max_absolute_difference=NA_real_)
if (all(c("23477-0.0.x", "23477-0.0.y") %in% names(NMR_metabolites))) {
  xx <- suppressWarnings(as.numeric(NMR_metabolites[["23477-0.0.x"]]))
  yy <- suppressWarnings(as.numeric(NMR_metabolites[["23477-0.0.y"]]))
  both <- is.finite(xx) & is.finite(yy)
  diffs <- abs(xx[both]-yy[both])
  nmr_merge_QA <- data.table(variable="23477-0.0",both_n=sum(both),
    discordant_n=sum(diffs>1e-8),
    max_absolute_difference=if (length(diffs)) max(diffs) else NA_real_)
  if (nmr_merge_QA$discordant_n>0L)
    warning("NMR duplicate 23477-0.0 contains conflicting values; inspect QA_03_NMR_merge.csv")
  NMR_metabolites[, `23477-0.0` := fcoalesce(`23477-0.0.x`, `23477-0.0.y`)]
  NMR_metabolites[, c("23477-0.0.x", "23477-0.0.y") := NULL]
}
fwrite(nmr_merge_QA,file.path(qa_dir,"QA_03_NMR_merge.csv"))

nmr_remove <- intersect(
  c(
    "23758-0.0",
    "23753-0.0",
    "23769-0.0",
    "23789-0.0",
    "23860-0.0",
    "23889-0.0",
    "23782-1.0",
    "23614-1.0",
    "23477-1.0",
    "23782-0.0"
  ),
  names(NMR_metabolites)
)

if (length(nmr_remove) > 0L) {
  NMR_metabolites[, (nmr_remove) := NULL]
}

if (anyDuplicated(NMR_metabolites$eid) > 0L) {
  stop("NMR_metabolites has duplicated eid after merging.")
}

nmr_feature_cols <- setdiff(names(NMR_metabolites), "eid")
add_modality_flag(NMR_metabolites, nmr_feature_cols, "metab")

# ============================================================
# 6. Genetics / PRS view
# ============================================================
# Residualise each PRS for genotype batch + recruitment centre + PC1-PC20.
# Sex is not included in residualisation.
# PRSs are residualised independently; complete data across all PRSs are not required.
# No z-standardisation is performed here.

PRS_file <- file.path(output_dir, "PRS.csv")
PRS_batch_file <- file.path(output_dir, "PRS_batch.csv")

PRS <- fread(PRS_file, header = TRUE)
PRS_batch <- fread(PRS_batch_file, header = TRUE)
setDT(PRS)
setDT(PRS_batch)

if (anyDuplicated(PRS$eid) > 0L) {
  stop("PRS has duplicated eid.")
}

if (anyDuplicated(PRS_batch$eid) > 0L) {
  stop("PRS_batch has duplicated eid.")
}

prs_remove <- intersect(c("p26238", "p26250"), names(PRS))
if (length(prs_remove) > 0L) {
  PRS[, (prs_remove) := NULL]
}

required_prs_batch_cols <- c(
  "eid",
  "p54_i0",
  "p22000",
  paste0("p22009_a", 1:20)
)

missing_prs_batch_cols <- setdiff(required_prs_batch_cols, names(PRS_batch))
if (length(missing_prs_batch_cols) > 0L) {
  stop(
    paste0(
      "Missing PRS adjustment variables: ",
      paste(missing_prs_batch_cols, collapse = ", ")
    )
  )
}

PRS_batch <- PRS_batch[, ..required_prs_batch_cols]
PRS <- merge(PRS, PRS_batch, by = "eid", all.x = TRUE, sort = FALSE)
DT <- as.data.table(PRS)

get_field_num <- function(nm) {
  suppressWarnings(
    as.integer(
      sub("^p", "", sub("_.*$", "", nm))
    )
  )
}

field_num <- get_field_num(names(DT))
prs_cols <- names(DT)[
  !is.na(field_num) &
    field_num >= 26204 &
    field_num <= 26289
]

batch_col <- grep("^p22000", names(DT), value = TRUE)[1]
centre_col <- grep("^p54", names(DT), value = TRUE)[1]
pc_cols <- grep("^p22009_a(1[0-9]|20|[1-9])$", names(DT), value = TRUE)

stopifnot(
  length(prs_cols) > 0L,
  !is.na(batch_col),
  !is.na(centre_col),
  length(pc_cols) == 20L
)

# UKB p22000 negative batch codes are legitimate; PRS/PC signed values legitimate.
prs_batch_raw <- as.character(DT[[batch_col]])
prs_batch_QA <- data.table(batch=prs_batch_raw)[,.N,by=batch][order(batch)]
fwrite(prs_batch_QA,file.path(qa_dir,"QA_03_PRS_batch.csv"))
DT[, (batch_col) := as.factor(get(batch_col))]
DT[, (centre_col) := as.factor(get(centre_col))]
prs_numeric_QA <- rbindlist(lapply(c(pc_cols,prs_cols),function(cc) {
  original <- DT[[cc]]
  original_chr <- as.character(original)
  y <- suppressWarnings(as.numeric(original_chr))
  observed <- !is.na(original_chr) & nzchar(trimws(original_chr))
  failed <- observed & !is.finite(y)
  qa <- data.table(variable=cc,n_original_nonmissing=sum(observed),
    n_negative=sum(is.finite(y) & y<0),n_new_invalid=sum(failed),
    examples=paste(head(unique(original_chr[failed]),8L),collapse=" | "))
  y[!is.finite(y)] <- NA_real_
  set(DT,j=cc,value=y)
  qa
}),use.names=TRUE,fill=TRUE)
fwrite(prs_numeric_QA,file.path(qa_dir,"QA_03_PRS_PC_numeric.csv"))
if (any(prs_numeric_QA$n_new_invalid>0L))
  stop("PRS or PC numeric conversion lost values; inspect QA_03_PRS_PC_numeric.csv")

cov_complete <- complete.cases(
  DT[, c(batch_col, centre_col, pc_cols), with = FALSE]
)

sub_cov <- copy(DT[cov_complete])
sub_cov[, (batch_col) := droplevels(as.factor(get(batch_col)))]
sub_cov[, (centre_col) := droplevels(as.factor(get(centre_col)))]

form <- as.formula(
  paste0(
    "~ ",
    batch_col,
    " + ",
    centre_col,
    " + ",
    paste(pc_cols, collapse = " + ")
  )
)

X_all <- model.matrix(form, data = sub_cov)
if (anyNA(X_all)) {
  stop("PRS residualisation design matrix contains NA.")
}
prs_design_QA <- data.table(n_cov_complete=nrow(X_all),n_parameters=ncol(X_all),
  rank=qr(X_all)$rank,n_incomplete_cov=nrow(DT)-sum(cov_complete),
  n_valid_negative_batches=sum(prs_batch_raw %in% as.character(-11:-1),na.rm=TRUE))
fwrite(prs_design_QA,file.path(qa_dir,"QA_03_PRS_design.csv"))
if (prs_design_QA$rank<ncol(X_all))
  warning("PRS adjustment matrix rank-deficient; review QA_03_PRS_design.csv")

prs_resid_cols <- paste0(prs_cols, "_resid")
for (new_col in prs_resid_cols) {
  DT[, (new_col) := NA_real_]
}

original_idx <- which(cov_complete)
PRS_residualisation_audit <- vector("list", length(prs_cols))

for (j in seq_along(prs_cols)) {
  prs_col <- prs_cols[[j]]
  new_col <- prs_resid_cols[[j]]
  y <- sub_cov[[prs_col]]
  available <- !is.na(y)
  
  if (sum(available) <= ncol(X_all)) {
    PRS_residualisation_audit[[j]] <- data.table(
      PRS = prs_col,
      N = sum(available),
      status = "insufficient observations"
    )
    next
  }
  
  X <- X_all[available, , drop = FALSE]
  y2 <- y[available]
  resid <- qr.resid(qr(X), y2)
  target_idx <- original_idx[available]
  
  set(
    DT,
    i = target_idx,
    j = new_col,
    value = resid
  )
  
  PRS_residualisation_audit[[j]] <- data.table(
    PRS = prs_col,
    N = sum(available),
    status = "OK"
  )
}

PRS_residualisation_audit <- rbindlist(
  PRS_residualisation_audit,
  use.names = TRUE,
  fill = TRUE
)

# PRS 残差负数正常；只做 QA，不删除任何负值
prs_residual_QA <- rbindlist(lapply(prs_resid_cols,function(nm) {
  z <- DT[[nm]]
  data.table(variable=nm,n_observed=sum(is.finite(z)),
    n_negative=sum(is.finite(z) & z<0),n_missing=sum(is.na(z)),
    mean=if (any(is.finite(z))) mean(z,na.rm=TRUE) else NA_real_)
}))
fwrite(PRS_residualisation_audit,file.path(qa_dir,"QA_03_PRS_residualisation.csv"))
fwrite(prs_residual_QA,file.path(qa_dir,"QA_03_PRS_residuals.csv"))
genetics_data <- DT[, c("eid", prs_resid_cols), with = FALSE]
add_modality_flag(genetics_data, prs_resid_cols, "gene")

# ============================================================
# 7. Blood and urine biomarker view
# ============================================================

biomarker_file <- file.path(recoded_input_dir,"blood_urine_exposure.csv")
biomarker_data <- fread(biomarker_file, header = TRUE)
setDT(biomarker_data)

if (!"eid" %in% names(biomarker_data)) {
  stop("biomarker_data does not contain eid.")
}

if (anyDuplicated(biomarker_data$eid) > 0L) {
  stop("biomarker_data has duplicated eid.")
}

biomarker_feature_cols <- setdiff(names(biomarker_data), "eid")
add_modality_flag(biomarker_data, biomarker_feature_cols, "biomarker")

# ============================================================
# 8. Build a MAIN-cohort availability table
# ============================================================
# Each view is left-joined to the MAIN eid roster.
# Therefore participants are NOT excluded merely because an entire view is missing.

main_eid <- analysis_base_main[, .(eid)]

get_view_availability <- function(main_eid, view_data, flag_col) {
  tmp <- merge(
    main_eid,
    view_data[, c("eid", flag_col), with = FALSE],
    by = "eid",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Participants absent from a source are completely missing that view.
  tmp[is.na(get(flag_col)), (flag_col) := TRUE]
  tmp
}

env_avail <- get_view_availability(main_eid, enviro_data, "env_all_missing")
biomarker_avail <- get_view_availability(main_eid, biomarker_data, "biomarker_all_missing")
metab_avail <- get_view_availability(main_eid, NMR_metabolites, "metab_all_missing")
gene_avail <- get_view_availability(main_eid, genetics_data, "gene_all_missing")

availability_main <- Reduce(
  function(x, y) merge(x, y, by = "eid", all.x = TRUE, sort = FALSE),
  list(env_avail, biomarker_avail, metab_avail, gene_avail)
)

setDT(availability_main)

if (anyDuplicated(availability_main$eid) > 0L) {
  stop("availability_main contains duplicated eid.")
}

if (!identical(availability_main$eid, main_eid$eid)) {
  availability_main <- availability_main[match(main_eid$eid, eid)]
}

stopifnot(identical(availability_main$eid, main_eid$eid))

availability_main[
  ,
  availability_pattern := paste0(
    "E", as.integer(!env_all_missing),
    "_B", as.integer(!biomarker_all_missing),
    "_M", as.integer(!metab_all_missing),
    "_G", as.integer(!gene_all_missing)
  )
]

modality_availability_summary <- data.table(
  modality = c(
    "Environment",
    "Blood and urine biomarkers",
    "Metabolomics",
    "Genetics"
  ),
  total_N = nrow(availability_main),
  available_N = c(
    sum(!availability_main$env_all_missing),
    sum(!availability_main$biomarker_all_missing),
    sum(!availability_main$metab_all_missing),
    sum(!availability_main$gene_all_missing)
  ),
  all_missing_N = c(
    sum(availability_main$env_all_missing),
    sum(availability_main$biomarker_all_missing),
    sum(availability_main$metab_all_missing),
    sum(availability_main$gene_all_missing)
  )
)

modality_availability_summary[
  ,
  available_percent := available_N / total_N * 100
]

availability_pattern_table <- availability_main[
  ,
  .N,
  by = availability_pattern
][order(-N)]

print(modality_availability_summary)
print(availability_pattern_table)
fwrite(modality_availability_summary,
       file.path(qa_dir,"QA_03_domain_availability.csv"))
fwrite(availability_pattern_table,
       file.path(qa_dir,"QA_03_domain_patterns.csv"))

# ============================================================
# 9. Sensitivity-analysis cohort
# ============================================================
# Exclude participants who are completely missing >=1 entire view.

sensitivity_eid <- availability_main[
  env_all_missing == FALSE &
    biomarker_all_missing == FALSE &
    metab_all_missing == FALSE &
    gene_all_missing == FALSE,
  .(eid)
]

if (anyDuplicated(sensitivity_eid$eid) > 0L) {
  stop("sensitivity_eid contains duplicated eid.")
}

cat(
  "\n============================================================\n",
  "SENSITIVITY ANALYSIS COHORT\n",
  "Main analysis N:", nrow(main_eid), "\n",
  "Sensitivity common-multimodal N:", nrow(sensitivity_eid), "\n",
  "Excluded for >=1 completely missing view:",
  nrow(main_eid) - nrow(sensitivity_eid), "\n",
  "============================================================\n"
)

# ============================================================
# 10. Sensitivity-analysis exclusion flow
# ============================================================
# Sequentially require each whole view to be present.

sensitivity_flow <- data.table(
  step = c(
    "Main analysis cohort",
    "+ Environment available",
    "+ Biomarkers available",
    "+ Metabolomics available",
    "+ Genetics available = sensitivity cohort"
  ),
  N = c(
    nrow(availability_main),
    availability_main[env_all_missing == FALSE, .N],
    availability_main[
      env_all_missing == FALSE &
        biomarker_all_missing == FALSE,
      .N
    ],
    availability_main[
      env_all_missing == FALSE &
        biomarker_all_missing == FALSE &
        metab_all_missing == FALSE,
      .N
    ],
    nrow(sensitivity_eid)
  )
)

sensitivity_flow[
  ,
  excluded_from_previous := c(
    NA_integer_,
    head(N, -1L) - tail(N, -1L)
  )
]

print(sensitivity_flow)

# ============================================================
# 11. Helper: create aligned view datasets
# ============================================================
# For MAIN analysis, all main eid are retained. If a participant is absent from
# a source, the participant remains in the corresponding view with NA features.
#
# For SENSITIVITY analysis, all four views are guaranteed to be present.

availability_suffixes <- c(
  "_n_observed",
  "_missing_prop",
  "_all_missing"
)

remove_availability_cols <- function(DT) {
  drop_cols <- names(DT)[
    grepl(
      "(_n_observed|_missing_prop|_all_missing)$",
      names(DT)
    )
  ]
  
  if (length(drop_cols) > 0L) {
    DT[, (drop_cols) := NULL]
  }
  
  DT
}

align_view_to_eid <- function(eid_dt, view_dt, view_name) {
  out <- merge(
    eid_dt,
    view_dt,
    by = "eid",
    all.x = TRUE,
    sort = FALSE
  )
  
  out <- out[match(eid_dt$eid, eid)]
  
  if (anyDuplicated(out$eid) > 0L) {
    stop(paste0(view_name, " output contains duplicated eid."))
  }
  
  if (!identical(out$eid, eid_dt$eid)) {
    stop(paste0(view_name, " output eid order does not match target eid."))
  }
  
  remove_availability_cols(out)
}

align_outcome_to_eid <- function(eid_dt, outcome_dt, dataset_name) {
  idx <- match(eid_dt$eid, outcome_dt$eid)
  
  if (anyNA(idx)) {
    stop(
      paste0(
        dataset_name,
        " outcome data are missing ",
        sum(is.na(idx)),
        " target participants."
      )
    )
  }
  
  out <- copy(outcome_dt[idx])
  
  if (!identical(out$eid, eid_dt$eid)) {
    stop(paste0(dataset_name, " outcome eid order mismatch."))
  }
  
  out
}

# ============================================================
# 12. Build MAIN-analysis datasets
# ============================================================

# Independent MAIN-cohort missingness per variable.
# Note: not the 60% development missingness used by Stage 1 S4 filter.
view_QA <- rbindlist(lapply(names(list(
  Environment=enviro_data,Biomarkers=biomarker_data,
  Metabolomics=NMR_metabolites,Genetics=genetics_data)),function(domain) {
  D <- switch(domain,Environment=enviro_data,Biomarkers=biomarker_data,
              Metabolomics=NMR_metabolites,Genetics=genetics_data)
  cols <- setdiff(names(D),c("eid",grep("(_n_observed|_missing_prop|_all_missing)$",names(D),value=TRUE)))
  idx <- match(analysis_base_main$eid,D$eid)
  rbindlist(lapply(cols,function(nm) {
    z <- D[[nm]][idx]
    missing <- if (is.character(z)) is.na(z) | trimws(z)=="" else
      is.na(z) | !is.finite(as.numeric(z))
    data.table(variable=nm,missing_pct=100*mean(missing),
      n_missing=sum(missing),n_observed=sum(!missing))
  }))[,domain:=domain]
}),use.names=TRUE,fill=TRUE)
fwrite(view_QA,file.path(qa_dir,"QA_03_main_missingness.csv"))

main_environment <- align_view_to_eid(
  main_eid,
  enviro_data,
  "Main environment"
)

main_biomarkers <- align_view_to_eid(
  main_eid,
  biomarker_data,
  "Main biomarkers"
)

main_metabolomics <- align_view_to_eid(
  main_eid,
  NMR_metabolites,
  "Main metabolomics"
)

main_genetics <- align_view_to_eid(
  main_eid,
  genetics_data,
  "Main genetics"
)

main_disease <- align_outcome_to_eid(
  main_eid,
  analysis_base_main,
  "Main"
)

# Convenience file for sex-stratified analyses.
if (!"sex" %in% names(main_environment)) {
  stop("sex is not found in the main Environment dataset.")
}

main_eid_sex <- main_environment[, .(eid, sex)]

# ============================================================
# 13. Build SENSITIVITY-analysis datasets
# ============================================================

sensitivity_environment <- align_view_to_eid(
  sensitivity_eid,
  enviro_data,
  "Sensitivity environment"
)

sensitivity_biomarkers <- align_view_to_eid(
  sensitivity_eid,
  biomarker_data,
  "Sensitivity biomarkers"
)

sensitivity_metabolomics <- align_view_to_eid(
  sensitivity_eid,
  NMR_metabolites,
  "Sensitivity metabolomics"
)

sensitivity_genetics <- align_view_to_eid(
  sensitivity_eid,
  genetics_data,
  "Sensitivity genetics"
)

sensitivity_disease <- align_outcome_to_eid(
  sensitivity_eid,
  analysis_base_main,
  "Sensitivity"
)

if (!"sex" %in% names(sensitivity_environment)) {
  stop("sex is not found in the sensitivity Environment dataset.")
}

sensitivity_eid_sex <- sensitivity_environment[, .(eid, sex)]

# ============================================================
# 14. Cross-dataset QA
# ============================================================

stopifnot(
  nrow(main_environment) == nrow(main_eid),
  nrow(main_biomarkers) == nrow(main_eid),
  nrow(main_metabolomics) == nrow(main_eid),
  nrow(main_genetics) == nrow(main_eid),
  nrow(main_disease) == nrow(main_eid),
  nrow(main_eid_sex) == nrow(main_eid)
)

stopifnot(
  identical(main_environment$eid, main_eid$eid),
  identical(main_biomarkers$eid, main_eid$eid),
  identical(main_metabolomics$eid, main_eid$eid),
  identical(main_genetics$eid, main_eid$eid),
  identical(main_disease$eid, main_eid$eid),
  identical(main_eid_sex$eid, main_eid$eid)
)

stopifnot(
  nrow(sensitivity_environment) == nrow(sensitivity_eid),
  nrow(sensitivity_biomarkers) == nrow(sensitivity_eid),
  nrow(sensitivity_metabolomics) == nrow(sensitivity_eid),
  nrow(sensitivity_genetics) == nrow(sensitivity_eid),
  nrow(sensitivity_disease) == nrow(sensitivity_eid),
  nrow(sensitivity_eid_sex) == nrow(sensitivity_eid)
)

stopifnot(
  identical(sensitivity_environment$eid, sensitivity_eid$eid),
  identical(sensitivity_biomarkers$eid, sensitivity_eid$eid),
  identical(sensitivity_metabolomics$eid, sensitivity_eid$eid),
  identical(sensitivity_genetics$eid, sensitivity_eid$eid),
  identical(sensitivity_disease$eid, sensitivity_eid$eid),
  identical(sensitivity_eid_sex$eid, sensitivity_eid$eid)
)

# Sensitivity participants must truly have >=1 observed feature in every view.
sensitivity_check <- availability_main[match(sensitivity_eid$eid, eid)]
stopifnot(
  all(sensitivity_check$env_all_missing == FALSE),
  all(sensitivity_check$biomarker_all_missing == FALSE),
  all(sensitivity_check$metab_all_missing == FALSE),
  all(sensitivity_check$gene_all_missing == FALSE)
)

cat(
  "\n============================================================\n",
  "Participant alignment checks passed.\n",
  "Main analysis N:", nrow(main_eid), "\n",
  "Sensitivity analysis N:", nrow(sensitivity_eid), "\n",
  "============================================================\n"
)

cat("\nSex distribution in MAIN cohort:\n")
print(table(main_eid_sex$sex, useNA = "ifany"))

cat("\nSex distribution in SENSITIVITY cohort:\n")
print(table(sensitivity_eid_sex$sex, useNA = "ifany"))

# ============================================================
# 15. Export directories
# ============================================================

source_root <- "/lustre/home/2411110208/UK_biobank/code/6.select_and_weight_the_env_vars_of_multimorbidity/Source"
# 主分析直接覆盖 Source 原有同名文件；敏感性直接覆盖 sensitivity_analysis/ 同名文件。
dir.create(source_root, recursive = TRUE, showWarnings = FALSE)
sensitivity_source_dir <- file.path(source_root,"sensitivity_analysis")

for (dd in c(sensitivity_source_dir)) {
  dir.create(dd, showWarnings = FALSE, recursive = TRUE)
}

# ============================================================
# 16. Save MAIN-analysis datasets
# ============================================================

fwrite(main_environment, file.path(source_root, "final_environment_2.csv"))
saveRDS(main_environment, file.path(source_root, "final_environment_2.rds"))

fwrite(main_biomarkers, file.path(source_root, "final_biomarkers.csv"))
saveRDS(main_biomarkers, file.path(source_root, "final_biomarkers.rds"))

fwrite(main_metabolomics, file.path(source_root, "final_metabolomics.csv"))
saveRDS(main_metabolomics, file.path(source_root, "final_metabolomics.rds"))

fwrite(main_genetics, file.path(source_root, "final_genetics.csv"))
saveRDS(main_genetics, file.path(source_root, "final_genetics.rds"))

fwrite(main_disease, file.path(source_root, "UKB_diseases_data.csv"))
saveRDS(main_disease, file.path(source_root, "UKB_diseases_data.rds"))

fwrite(main_eid_sex, file.path(source_root, "final_eid_sex.csv"))
saveRDS(main_eid_sex, file.path(source_root, "final_eid_sex.rds"))

fwrite(main_flow, file.path(source_root, "cohort_flow_main_analysis.csv"))

# ============================================================
# 17. Save SENSITIVITY-analysis datasets
# ============================================================

fwrite(sensitivity_environment, file.path(sensitivity_source_dir, "final_environment_2.csv"))
saveRDS(sensitivity_environment, file.path(sensitivity_source_dir, "final_environment_2.rds"))

fwrite(sensitivity_biomarkers, file.path(sensitivity_source_dir, "final_biomarkers.csv"))
saveRDS(sensitivity_biomarkers, file.path(sensitivity_source_dir, "final_biomarkers.rds"))

fwrite(sensitivity_metabolomics, file.path(sensitivity_source_dir, "final_metabolomics.csv"))
saveRDS(sensitivity_metabolomics, file.path(sensitivity_source_dir, "final_metabolomics.rds"))

fwrite(sensitivity_genetics, file.path(sensitivity_source_dir, "final_genetics.csv"))
saveRDS(sensitivity_genetics, file.path(sensitivity_source_dir, "final_genetics.rds"))

fwrite(sensitivity_disease, file.path(sensitivity_source_dir, "UKB_diseases_data.csv"))
saveRDS(sensitivity_disease, file.path(sensitivity_source_dir, "UKB_diseases_data.rds"))

fwrite(sensitivity_eid_sex, file.path(sensitivity_source_dir, "final_eid_sex.csv"))
saveRDS(sensitivity_eid_sex, file.path(sensitivity_source_dir, "final_eid_sex.rds"))

fwrite(sensitivity_flow, file.path(sensitivity_source_dir, "cohort_flow_sensitivity_analysis.csv"))

# ============================================================
# 19. Final summary
# ============================================================

cat(
  "\nCompleted multimodal cohort construction.\n",
  "\nMAIN ANALYSIS:\n",
  "  Definition: valid follow-up + no unknown disease onset timing + baseline disease count <2.\n",
  "  N:", nrow(main_eid), "\n",
  "  Events:", sum(main_disease$incident_multimorbidity == 1L, na.rm = TRUE), "\n",
  "  Output directory:", source_root, "\n",
  "\nSENSITIVITY ANALYSIS:\n",
  "  Definition: MAIN cohort + all four views not completely missing.\n",
  "  N:", nrow(sensitivity_eid), "\n",
  "  Events:", sum(sensitivity_disease$incident_multimorbidity == 1L, na.rm = TRUE), "\n",
  "  Output directory:", sensitivity_source_dir, "\n"
)

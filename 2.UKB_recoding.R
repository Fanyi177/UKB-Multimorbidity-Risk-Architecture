library(dplyr)
library(stringr)
library(data.table)
library(readxl)
# load uncoded dataset with all exposome variables
load("/lustre/home/2411110208/UK_biobank/cov_data/ukb_all_exposures.RData")

# Keep the full preprocessing workflow as data.table
setDT(dat)

# turn all "prefer not to answer", "do not know", "not applicable" to NA
# exclude recruitment date b/c it returns error
missing_values <- c(
  "Prefer not to answer",
  "Do not know",
  "Not applicable",
  "Prefer not to answer (group 1)",
  "Prefer not to answer (group 2)",
  "Do not know (group 1)",
  "Do not know (group 2)",
  ""
)

char_cols <- names(dat)[
  vapply(dat, is.character, logical(1))
]

char_cols <- setdiff(
  char_cols,
  "recruitment_date"
)

for (col in char_cols) {
  idx <- which(dat[[col]] %chin% missing_values)
  if (length(idx) > 0L) {
    set(dat, i = idx, j = col, value = NA_character_)
  }
}


# #export#

# save(dat,
#      column_key, 
#      file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_all_exposures.RData")
# write.csv(column_key,"/lustre/home/2311110213/UKB_YiFan/column_key.csv")

# ============================================================
# General helper functions
# ============================================================

# 根据起止变量名取得一段列名
cols_between <- function(DT, first_col, last_col) {
  
  i1 <- match(first_col, names(DT))
  i2 <- match(last_col, names(DT))
  
  if (is.na(i1) || is.na(i2)) {
    stop(
      paste0(
        "Cannot find column range: ",
        first_col,
        " -> ",
        last_col
      )
    )
  }
  
  if (i1 > i2) {
    stop(
      paste0(
        "Column order incorrect: ",
        first_col,
        " occurs after ",
        last_col
      )
    )
  }
  
  names(DT)[i1:i2]
}


# 快速按名称删除变量
drop_cols <- function(DT, cols) {
  
  cols <- intersect(
    cols,
    names(DT)
  )
  
  if (length(cols) > 0) {
    DT[, (cols) := NULL]
  }
  
  invisible(DT)
}


# 两个变量求均值，全缺失时返回 NA 而不是 NaN
row_mean2 <- function(x, y) {
  
  z <- rowMeans(
    cbind(x, y),
    na.rm = TRUE
  )
  
  z[is.nan(z)] <- NA_real_
  
  z
}


# multi-select character -> binary
make_dummy <- function(x, pattern) {
  
  fifelse(
    is.na(x),
    NA_integer_,
    as.integer(
      grepl(
        pattern,
        x,
        fixed = TRUE
      )
    )
  )
}

### Early life factors####
unique(dat$country_birth)
unique(dat$breastfed)
unique(dat$body_size_10yrs_old)
unique(dat$height_10yrs_old)
unique(dat$handedness)
unique(dat$adopted)
unique(dat$part_multiple_birth)
unique(dat$maternal_smoking)
unique(dat$birth_weight)
summary(dat$birth_weight)
str(dat$birth_weight)
unique(dat$birth_weight_known)
unique(dat$country_birth_non_UK)
unique(dat$childhood_love)
unique(dat$childhood_physical_abuse)
unique(dat$childhood_hate)
unique(dat$childhood_sex_abuse)
unique(dat$childhood_doctor)

# convert binary  to numeric
# convert ordinary  to numeric
dat$breastfed<-ifelse(dat$breastfed=="No",0,1)
dat$body_size_10yrs_old<-ifelse(dat$body_size_10yrs_old=="Thinner",0,
                                ifelse(dat$body_size_10yrs_old=="About average",1,2))
dat$height_10yrs_old<-ifelse(dat$height_10yrs_old=="Shorter",0,
                             ifelse(dat$height_10yrs_old=="About average",1,2))
dat$adopted<-ifelse(dat$adopted=="No",0,1)
dat$part_multiple_birth<-ifelse(dat$part_multiple_birth=="No",0,1)
dat$maternal_smoking<-ifelse(dat$maternal_smoking=="No",0,1)
#
dat$country_birth_non_UK<-ifelse(dat$country_birth_non_UK=="China"|dat$country_birth_non_UK=="Hongkong"|
                                   dat$country_birth_non_UK=="Singapore"|dat$country_birth_non_UK=="Malaysia"|
                                   dat$country_birth_non_UK=="Japan"|dat$country_birth_non_UK=="Thailand"|
                                   dat$country_birth_non_UK=="South Korea"| dat$country_birth_non_UK=="Tibet",0,1)

dat$childhood_love<-ifelse(dat$childhood_love=="Never true",0,ifelse(dat$childhood_love=="Rarely true",1,
                                                                     ifelse(dat$childhood_love=="Sometimes true",2,
                                                                            ifelse(dat$childhood_love=="Often",3,4))))


dat$childhood_physical_abuse<-ifelse(dat$childhood_physical_abuse=="Never true",0,ifelse(dat$childhood_physical_abuse=="Rarely true",1,
                                                                                         ifelse(dat$childhood_physical_abuse=="Sometimes true",2,
                                                                                                ifelse(dat$childhood_physical_abuse=="Often",3,4))))

dat$childhood_hate<-ifelse(dat$childhood_hate=="Never true",0,ifelse(dat$childhood_hate=="Rarely true",1,
                                                                     ifelse(dat$childhood_hate=="Sometimes true",2,
                                                                            ifelse(dat$childhood_hate=="Often",3,4))))

dat$childhood_sex_abuse<-ifelse(dat$childhood_sex_abuse=="Never true",0,ifelse(dat$childhood_sex_abuse=="Rarely true",1,
                                                                               ifelse(dat$childhood_sex_abuse=="Sometimes true",2,
                                                                                      ifelse(dat$childhood_sex_abuse=="Often",3,4))))

dat$childhood_doctor<-ifelse(dat$childhood_doctor=="Never true",0,ifelse(dat$childhood_doctor=="Rarely true",1,
                                                                         ifelse(dat$childhood_doctor=="Sometimes true",2,
                                                                                ifelse(dat$childhood_doctor=="Often",3,4))))

## Stressful life events####
unique(dat$sle)
table(is.na(dat$sle))
#transfort to creat 6 depent binary variable 
#self illness/injury/assault
dat$self_illness_injury_assault <- ifelse(  
  is.na(dat$sle),  
  NA,  
  ifelse(grepl("Serious illness, injury or assault to yourself", dat$sle), 1, 0)  
) 
table(dat$self_illness_injury_assault)
#self illness/injury/assault of close relative
dat$relative_illness_injury_assault <- ifelse(  
  is.na(dat$sle),  
  NA,  
  ifelse(grepl("Serious illness, injury or assault of a close relative", dat$sle), 1, 0)  
) 
table(dat$relative_illness_injury_assault)
#death of a close relative
dat$death_relative <- ifelse(  
  is.na(dat$sle),  
  NA,  
  ifelse(grepl("Death of a close relative", dat$sle), 1, 0)  
) 
table(dat$death_relative)

#death of a partner
dat$death_partner <- ifelse(  
  is.na(dat$sle),  
  NA,  
  ifelse(grepl("Death of a spouse or partner", dat$sle), 1, 0)  
) 
table(dat$death_partner)
#Marital separation/divorce
dat$divorce <- ifelse(  
  is.na(dat$sle),  
  NA,  
  ifelse(grepl("Marital separation/divorce", dat$sle), 1, 0)  
) 
table(dat$divorce)
#Financial difficulties
dat$financial_difficulty <- ifelse(  
  is.na(dat$sle),  
  NA,  
  ifelse(grepl("Financial difficulties", dat$sle), 1, 0)  
) 
table(dat$financial_difficulty)
#subset df to only include new dummy variables
drop_cols(dat, "sle")

### Social factors####
unique(dat$family_visit_freq)
unique(dat$confide_freq)
unique(dat$loneliness)

# convert ordinary  to numeric
# set reference category
dat$family_visit_freq<-ifelse(dat$family_visit_freq=="No friends/family outside household",0,
                              ifelse(dat$family_visit_freq=="Never or almost never",1,
                                     ifelse(dat$family_visit_freq=="Once every few months",2,
                                            ifelse(dat$family_visit_freq=="About once a month",3,
                                                   ifelse(dat$family_visit_freq=="About once a week",4,
                                                          ifelse(dat$family_visit_freq=="2-4 times a week",5,6))))))
dat$confide_freq<-ifelse(dat$confide_freq=="Never or almost never",0,
                         ifelse(dat$confide_freq=="Once every few months",1,
                                ifelse(dat$confide_freq=="About once a month",2,
                                       ifelse(dat$confide_freq=="About once a week",3,
                                              ifelse(dat$confide_freq=="2-4 times a week",4,5)))))
dat$loneliness<-ifelse(dat$loneliness=="No",0,1)

##transfort social_activity to creat 5 depent binary variable 
unique(dat$social_activity)
dat$gym <- ifelse(  
  is.na(dat$social_activity),  
  NA,  
  ifelse(grepl("Sports club or gym", dat$social_activity), 1, 0)  
) 
table(dat$gym)

# pub or social group
dat$pub <- ifelse(  
  is.na(dat$social_activity),  
  NA,  
  ifelse(grepl("Pub or social club", dat$social_activity), 1, 0)  
) 
table(dat$pub)

# religious group
dat$religious_group <- ifelse(  
  is.na(dat$social_activity),  
  NA,  
  ifelse(grepl("Religious group", dat$social_activity), 1, 0)  
) 
table(dat$religious_group)
#adult education class

dat$adult_education <- ifelse(  
  is.na(dat$social_activity),  
  NA,  
  ifelse(grepl("Adult education class", dat$social_activity), 1, 0)  
) 
table(dat$adult_education)

#Other group activity
dat$other_group_activity <- ifelse(  
  is.na(dat$social_activity),  
  NA,  
  ifelse(grepl("Other group activity", dat$social_activity), 1, 0)  
) 
table(dat$other_group_activity)
#remove columns used to make dummy vars
drop_cols(dat, "social_activity")

### Mental health####
unique(dat$bipolar_MDD)
sort(unique(dat$neuroticism))
unique(dat$mood_swings)
unique(dat$miserableness)
unique(dat$irritability)
unique(dat$sensitivity)
unique(dat$fed_up_feelings)
unique(dat$nervous)
unique(dat$worrier)
unique(dat$tense)
unique(dat$worry_embarassment)
unique(dat$nerves)
unique(dat$guilty_feelings)
unique(dat$risk_taking)
unique(dat$depressed_mood_freq)
unique(dat$unenthusiasm_freq)
unique(dat$tenseness_freq)
unique(dat$tiredness_freq)
unique(dat$doctor_visit_mental_health)
unique(dat$psychiatrist_visit_mental_health)
unique(dat$depressed_whole_week)
sort(unique(dat$depression_duration))
sort(unique(dat$depression_episodes))
unique(dat$unenthusiastic_whole_week)
sort(unique(dat$unenthusiastic_duration))
sort(unique(dat$unenthusiastic_episodes))
unique(dat$manic)
unique(dat$irritable)
unique(dat$manic_duration)
unique(dat$manic_episodes)
sort(unique(dat$happiness))
sort(unique(dat$job_satisfaction))
sort(unique(dat$health_satisfaction))
sort(unique(dat$family_satisfaction))
sort(unique(dat$friends_satisfaction))
sort(unique(dat$financial_satisfaction))

# convert binary  to numeric
# convert ordinary  to numeric
dat$neuroticism <- as.numeric(dat$neuroticism)
dat$mood_swings <- ifelse(dat$mood_swings=="No",0,1)
#  
start_col <- which(names(dat) == "miserableness")  
end_col <- which(names(dat) == "risk_taking")  
for (col in start_col:end_col) {  
  dat[[col]] <- ifelse(dat[[col]] == "No", 0, 1)  
}  
#
start_col <- which(names(dat) == "depressed_mood_freq")  
end_col <- which(names(dat) == "tiredness_freq")  
for (col in start_col:end_col) {  
  dat[[col]] <- ifelse(dat[[col]] == "Not at all", 0, ifelse(dat[[col]] =="Several days",1,
                                                             ifelse(dat[[col]] =="More than half the days",2,3)))  
}
#  
start_col <- which(names(dat) == "doctor_visit_mental_health")  
end_col <- which(names(dat) == "depressed_whole_week")  
for (col in start_col:end_col) {  
  dat[[col]] <- ifelse(dat[[col]] == "No", 0, 1)  
} 
#
dat$depression_duration <- as.numeric(dat$depression_duration)
dat$depression_episodes[which(dat$depression_episodes == -3)] <- NA         
dat$depression_duration[which(dat$depression_duration == -1)] <- NA 

dat$depression_episodes <- as.numeric(dat$depression_episodes)
dat$depression_episodes[which(dat$depression_episodes == -3)] <- NA         
dat$depression_episodes[which(dat$depression_episodes == -1)] <- NA 
#
dat$unenthusiastic_whole_week <- ifelse(dat$unenthusiastic_whole_week=="No",0,1)
#
dat$unenthusiastic_duration <- as.numeric(dat$unenthusiastic_duration)
dat$unenthusiastic_duration[which(dat$unenthusiastic_duration == -3)] <- NA         
dat$unenthusiastic_duration[which(dat$unenthusiastic_duration == -1)] <- NA 

dat$unenthusiastic_episodes <- as.numeric(dat$unenthusiastic_episodes)
dat$unenthusiastic_episodes[which(dat$unenthusiastic_episodes == -3)] <- NA         
dat$unenthusiastic_episodes[which(dat$unenthusiastic_episodes == -1)] <- NA 
#  
start_col <- which(names(dat) == "manic")  
end_col <- which(names(dat) == "irritable")  
for (col in start_col:end_col) {  
  dat[[col]] <- ifelse(dat[[col]] == "No", 0, 1)  
}
#
dat$manic_duration <-ifelse(dat$manic_duration=="Less than a week",0,
                            ifelse(dat$manic_duration=="At least two days, but less than a week",1,2))

dat$manic_episodes <-ifelse(dat$manic_episodes=="No problems",0,1)

#
start_col <- which(names(dat) == "happiness")  
end_col <- which(names(dat) == "financial_satisfaction")  
for (col in start_col:end_col) {  
  dat[[col]] <- ifelse(dat[[col]] == "Extremely unhappy", 0,
                       ifelse(dat[[col]] == "Very unhappy", 1,
                              ifelse(dat[[col]] == "Moderately unhappy", 2,
                                     ifelse(dat[[col]] == "Moderately happy", 3,
                                            ifelse(dat[[col]] == "Very happy", 4,5)))))  
} 

### transfort manic_symptoms into 5 depent variable
unique(dat$manic_symptoms)
#more creative
dat$manic_symptoms_creative <- ifelse(  
  is.na(dat$manic_symptoms),  
  NA,  
  ifelse(grepl("I was more creative or had more ideas than usual", dat$manic_symptoms), 1, 0)  
) 
table(dat$manic_symptoms_creative)
# more active
dat$manic_symptoms_active <- ifelse(  
  is.na(dat$manic_symptoms),  
  NA,  
  ifelse(grepl("I was more active than usual", dat$manic_symptoms), 1, 0)  
) 
table(dat$manic_symptoms_active)
# I needed less sleep than usual
dat$manic_symptoms_less_sleep <- ifelse(  
  is.na(dat$manic_symptoms),  
  NA,  
  ifelse(grepl("I needed less sleep than usual", dat$manic_symptoms), 1, 0)  
) 
table(dat$manic_symptoms_less_sleep)
# I was more talkative than usual
dat$manic_symptoms_talkative <- ifelse(  
  is.na(dat$manic_symptoms),  
  NA,  
  ifelse(grepl("I was more talkative than usual", dat$manic_symptoms), 1, 0)  
) 
table(dat$manic_symptoms_talkative)
# All of the above
dat$manic_symptoms_all <- ifelse(  
  is.na(dat$manic_symptoms),  
  NA,  
  ifelse(grepl("All of the above", dat$manic_symptoms), 1, 0)  
) 
table(dat$manic_symptoms_all)

#remove excluded columns and those used to make dummy vars
drop_cols(
  dat,
  c(
    "bipolar",
    "MDD_moderate",
    "MDD_severe",
    "MDD_single_ep",
    "manic_symptoms"
  )
)
### Sociodemographics####
unique(dat$accommodation_type)
unique(dat$own_or_rent)
unique(dat$gas_fuel_heat.0.0)
unique(dat$years_at_address)
unique(dat$hshld_number)
unique(dat$hshld_relations.0.0)
unique(dat$hshld_vehicles)
unique(dat$hshld_income)
unique(dat$education_years.0.0)
unique(dat$employment_corrected)
unique(dat$employment_distance)
unique(dat$employment.0.0)
unique(dat$employment_years)
unique(dat$employment_hours)
unique(dat$employment_travel_freq)
unique(dat$commute_transport.0.0)
unique(dat$employment_standing)
unique(dat$employment_heavy_manual)
unique(dat$shift_work)
unique(dat$night_shift_work)
unique(dat$noisy_workplace)
unique(dat$immigration_year)
unique(dat$ethnicity)
unique(dat$public_assistance.0.0)
unique(dat$private_healthcare)
unique(dat$recruitment_age)
unique(dat$birth_year)
unique(dat$sex)
unique(dat$recruitment_centre)
str(dat$Cascot_confidence_score)
# convert binary  to numeric
# convert ordinary  to numeric
dat$years_at_address <- as.numeric(dat$years_at_address)
dat$years_at_address[which(dat$years_at_address == -10)] <- 0.5
dat$years_at_address[which(dat$years_at_address == -3)] <- NA
dat$years_at_address[which(dat$years_at_address == -1)] <- NA

dat$hshld_number <- as.numeric(dat$hshld_number)
dat$hshld_number[which(dat$hshld_number == -3)] <- NA
dat$hshld_number[which(dat$hshld_number == -1)] <- NA
#
dat$hshld_vehicles <-ifelse(dat$hshld_vehicles=="None",0,ifelse(dat$hshld_vehicles=="One",1,
                                                                ifelse(dat$hshld_vehicles=="Two",2,
                                                                       ifelse(dat$hshld_vehicles=="Three",3,4))))

dat$hshld_income <-ifelse(dat$hshld_income=="Less than 18,000",0,ifelse(dat$hshld_income=="18,000 to 30,999",1,
                                                                        ifelse(dat$hshld_income=="31,000 to 51,999",2,
                                                                               ifelse(dat$hshld_income=="52,000 to 100,000",3,4))))

#
dat$education_age_complete <- as.numeric(dat$education_age_complete)
dat$education_age_complete[which(dat$education_age_complete == -3)] <- NA
dat$education_age_complete[which(dat$education_age_complete == -2)] <- NA
dat$education_age_complete[which(dat$education_age_complete == -1)] <- NA

# table(dat$employment_corrected) # check to see most frequent level
dat$employment_distance <- as.numeric(dat$employment_distance)
dat$employment_distance[which(dat$employment_distance == -10)] <- 0.5
dat$employment_distance[which(dat$employment_distance == -3)] <- NA
dat$employment_distance[which(dat$employment_distance == -1)] <- NA

dat$employment_years <- as.numeric(dat$employment_years)
dat$employment_years[which(dat$employment_years == -10)] <- 0.5
dat$employment_years[which(dat$employment_years == -3)] <- NA
dat$employment_years[which(dat$employment_years == -1)] <- NA

dat$employment_hours <- as.numeric(dat$employment_hours)
dat$employment_hours[which(dat$employment_hours == -3)] <- NA
dat$employment_hours[which(dat$employment_hours == -1)] <- NA

dat$employment_travel_freq <- as.numeric(dat$employment_travel_freq)
dat$employment_travel_freq[which(dat$employment_travel_freq == -10)] <- 0.5
dat$employment_travel_freq[which(dat$employment_travel_freq == -3)] <- NA
dat$employment_travel_freq[which(dat$employment_travel_freq == -1)] <- NA
#
dat$employment_standing <-ifelse(dat$employment_standing=="Never/rarely",0,ifelse(dat$employment_standing=="Sometimes",1,
                                                                                  ifelse(dat$employment_standing=="Usually",2,3)))

dat$employment_heavy_manual <-ifelse(dat$employment_heavy_manual=="Never/rarely",0,ifelse(dat$employment_heavy_manual=="Sometimes",1,
                                                                                          ifelse(dat$employment_heavy_manual=="Usually",2,3)))
dat$shift_work <-ifelse(dat$shift_work=="Never/rarely",0,ifelse(dat$shift_work=="Sometimes",1,
                                                                ifelse(dat$shift_work=="Usually",2,3)))
dat$night_shift_work <-ifelse(dat$night_shift_work=="Never/rarely",0,ifelse(dat$night_shift_work=="Sometimes",1,
                                                                            ifelse(dat$night_shift_work=="Usually",2,3)))
dat$noisy_workplace <-ifelse(dat$noisy_workplace=="No",0,ifelse(dat$noisy_workplace=="Yes, for less than a year",1,
                                                                ifelse(dat$noisy_workplace=="Yes, for around 1-5 years",2,3)))
#
dat$immigration_year <- as.numeric(dat$immigration_year)
dat$immigration_year[which(dat$immigration_year == -3)] <- NA
dat$immigration_year[which(dat$immigration_year == -1)] <- NA

table(dat$ethnicity) # check to see most frequent level
#
dat$private_healthcare <-ifelse(dat$private_healthcare=="No, never",0,ifelse(dat$private_healthcare=="Yes, sometimes",1,
                                                                             ifelse(dat$private_healthcare=="Yes, most of the time",2,3)))

dat$recruitment_age <- as.numeric(dat$recruitment_age)

dat$birth_year <- as.numeric(dat$birth_year)

table(dat$sex) # check to see most frequent level
dat$sex <-ifelse(dat$sex=="Female",0,1)
#
table(dat$recruitment_centre)
### dummy variables

## gas fuel heat
unique(dat$gas_fuel_heat.0.0)
# create dummy var for A gas hob or gas cooker
dat$gas_hob_heat <- ifelse(  
  is.na(dat$gas_fuel_heat.0.0),  
  NA,  
  ifelse(grepl("A gas hob or gas cooker", dat$gas_fuel_heat.0.0), 1, 0)  
) 
table(dat$gas_hob_heat)
# create dummy var for A gas fire that you use regularly in winter time
dat$gas_fire_heat <- ifelse(  
  is.na(dat$gas_fuel_heat.0.0),  
  NA,  
  ifelse(grepl("A gas fire that you use regularly in winter time", dat$gas_fuel_heat.0.0), 1, 0)  
) 
table(dat$gas_fire_heat)

# create dummy var for An open solid fuel fire that you use regularly in winter time
dat$open_fire_heat <- ifelse(  
  is.na(dat$gas_fuel_heat.0.0),  
  NA,  
  ifelse(grepl("An open solid fuel fire that you use regularly in winter time", dat$gas_fuel_heat.0.0), 1, 0)  
) 
table(dat$open_fire_heat)
## heating type
unique(dat$heating_type.0.0)
# create dummy var for Gas central heating

dat$gas_central_heat <- ifelse(  
  is.na(dat$heating_type.0.0),  
  NA,  
  ifelse(grepl("Gas central heating", dat$heating_type.0.0), 1, 0)  
) 
table(dat$gas_central_heat)
# create dummy var for Electric storage heaters

dat$electric_storage_heat <- ifelse(  
  is.na(dat$heating_type.0.0),  
  NA,  
  ifelse(grepl("Electric storage heaters", dat$heating_type.0.0), 1, 0)  
) 
table(dat$electric_storage_heat)

# create dummy var for Oil (kerosene) central heating

dat$oil_central_heat <- ifelse(  
  is.na(dat$heating_type.0.0),  
  NA,  
  ifelse(grepl("Oil", dat$heating_type.0.0), 1, 0)  
) 
table(dat$oil_central_heat)

# create dummy var for Portable gas or paraffin heaters

dat$portable_gas_heat <- ifelse(  
  is.na(dat$heating_type.0.0),  
  NA,  
  ifelse(grepl("Portable gas or paraffin heaters", dat$heating_type.0.0), 1, 0)  
) 
table(dat$portable_gas_heat)

# create dummy var for Solid fuel central heating

dat$solid_fuel_heat <- ifelse(  
  is.na(dat$heating_type.0.0),  
  NA,  
  ifelse(grepl("Solid fuel central heating", dat$heating_type.0.0), 1, 0)  
) 
table(dat$solid_fuel_heat)

# create dummy var for Open fire without central heating

dat$open_fire_no_central_heat <- ifelse(  
  is.na(dat$heating_type.0.0),  
  NA,  
  ifelse(grepl("Open fire without central heating", dat$heating_type.0.0), 1, 0)  
) 
table(dat$open_fire_no_central_heat)

## household relations

unique(dat$hshld_relations.0.0)
# create dummy var for Husband, wife or partner
dat$hshld_partner <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Husband, wife or partner", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_partner)
# create dummy var for Son and/or daughter (include step-children)

dat$hshld_children <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Son and/or daughter", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_children)

# create dummy var for Brother and/or sister

dat$hshld_siblings <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Brother and/or sister", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_siblings)

# create dummy var for Mother and/or father

dat$hshld_parents <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Mother and/or father", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_parents)

# create dummy var for Grandparent

dat$hshld_grandparent <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Grandparent", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_grandparent)

# create dummy var for Grandchild

dat$hshld_grandchild <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Grandchild", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_grandchild)

# create dummy var for Other related

dat$hshld_other_related <- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Other related", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_other_related)

# create dummy var for Other unrelated

dat$hshld_other_unrelated<- ifelse(  
  is.na(dat$hshld_relations.0.0),  
  NA,  
  ifelse(grepl("Other unrelated", dat$hshld_relations.0.0), 1, 0)  
) 
table(dat$hshld_other_unrelated)
## education qualifications

unique(dat$education_years.0.0)

# create dummy var for College or University degree

dat$education_college<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("College or University degree", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_college)

# create dummy var for A levels/AS levels or equivalent

dat$education_A_levels<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("A levels/AS levels or equivalent", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_A_levels)

# create dummy var for O levels/GCSEs or equivalent

dat$education_O_levels<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("O levels/GCSEs or equivalent", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_O_levels)

# create dummy var for CSEs or equivalent
dat$education_CSEs<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("CSEs or equivalent", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_CSEs)

# create dummy var for NVQ or HND or HNC or equivalent

dat$education_NVQ<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("NVQ or HND or HNC or equivalent", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_NVQ)

# create dummy var for Other professional qualifications eg: nursing, teaching

dat$education_other_professional<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("Other professional qualifications eg: nursing, teaching", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_other_professional)

# create dummy var for None of the above
dat$education_none<- ifelse(  
  is.na(dat$education_years.0.0),  
  NA,  
  ifelse(grepl("None of the above", dat$education_years.0.0), 1, 0)  
) 
table(dat$education_none)

## employment

unique(dat$employment.0.0)
# create dummy var for In paid employment or self-employed

dat$employed<- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("In paid employment or self-employed", dat$employment.0.0), 1, 0)  
) 
table(dat$employed)

# create dummy var for Retired

dat$retired <- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("Retired", dat$employment.0.0), 1, 0)  
) 
table(dat$retired )

# create dummy var for Looking after home and/or family
dat$home_maker <- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("Looking after home and/or family", dat$employment.0.0), 1, 0)  
) 
table(dat$home_maker )
# create dummy var for Unable to work because of sickness or disability

dat$unemployed_disability <- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("Unable to work because of sickness or disability", dat$employment.0.0), 1, 0)  
) 
table(dat$unemployed_disability )


# create dummy var for Unemployed
dat$unemployed <- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("Unemployed", dat$employment.0.0), 1, 0)  
) 
table(dat$unemployed)



# create dummy var for Doing unpaid or voluntary work
dat$volunteer <- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("Doing unpaid or voluntary work", dat$employment.0.0), 1, 0)  
) 
table(dat$volunteer)


# create dummy var for Full or part-time student

dat$student <- ifelse(  
  is.na(dat$employment.0.0),  
  NA,  
  ifelse(grepl("Full or part-time student", dat$employment.0.0), 1, 0)  
) 
table(dat$student)
## transport type

unique(dat$commute_transport.0.0)

# create dummy var for Car/motor vehicle
dat$car_commute <- ifelse(  
  is.na(dat$commute_transport.0.0),  
  NA,  
  ifelse(grepl("Car/motor vehicle", dat$commute_transport.0.0), 1, 0)  
) 
table(dat$car_commute)
# create dummy var for Walk
dat$walk_commute <- ifelse(  
  is.na(dat$commute_transport.0.0),  
  NA,  
  ifelse(grepl("Walk", dat$commute_transport.0.0), 1, 0)  
) 
table(dat$walk_commute)

# create dummy var for Public transport

dat$public_transport_commute <- ifelse(  
  is.na(dat$commute_transport.0.0),  
  NA,  
  ifelse(grepl("Public transport", dat$commute_transport.0.0), 1, 0)  
) 
table(dat$public_transport_commute)
# create dummy var for Cycle

dat$cycle_commute <- ifelse(  
  is.na(dat$commute_transport.0.0),  
  NA,  
  ifelse(grepl("Cycle", dat$commute_transport.0.0), 1, 0)  
) 
table(dat$cycle_commute)

## public assistance

unique(dat$public_assistance.0.0)

# create dummy var for Attendance allowance
dat$attendance_allowance <- ifelse(  
  is.na(dat$public_assistance.0.0),  
  NA,  
  ifelse(grepl("Attendance allowance", dat$public_assistance.0.0), 1, 0)  
) 
table(dat$attendance_allowance)

# create dummy var for Disability living allowance

dat$disability_allowance <- ifelse(  
  is.na(dat$public_assistance.0.0),  
  NA,  
  ifelse(grepl("Disability living allowance", dat$public_assistance.0.0), 1, 0)  
) 
table(dat$disability_allowance)

# create dummy var for Blue badge
dat$blue_badge <- ifelse(  
  is.na(dat$public_assistance.0.0),  
  NA,  
  ifelse(grepl("Blue badge", dat$public_assistance.0.0), 1, 0)  
) 
table(dat$blue_badge)

dat$Cascot_confidence_score<-as.numeric(dat$Cascot_confidence_score)

#remove columns used to make dummy vars
drop_cols(
  dat,
  c(
    "gas_fuel_heat.0.0",
    "heating_type.0.0",
    "hshld_relations.0.0",
    "education_years.0.0",
    "employment.0.0",
    "commute_transport.0.0",
    "public_assistance.0.0"
  )
)
### Family health history####
unique(dat$death_close_family)
unique(dat$father_alive)
unique(dat$adopted_father_alive)
unique(dat$father_age)
unique(dat$father_age_death)
unique(dat$mother_alive)
unique(dat$adopted_mother_alive)
unique(dat$mother_age)
unique(dat$mother_age_death)
unique(dat$number_brothers)
unique(dat$number_adopted_brothers)
unique(dat$number_sisters)
unique(dat$number_adopted_sisters)
unique(dat$number_older_siblings)
#
family_illness_vars <- c(
  "adopted_father_illness",
  "adopted_mother_illness.0.0",
  "adopted_sibling_illness.0.0",
  "father_illness.0.0",
  "mother_illness.0.0",
  "sibling_illness.0.0"
)

missing_family_vars <- setdiff(
  family_illness_vars,
  names(dat)
)

if (length(missing_family_vars) > 0) {
  print(
    missing_family_vars
  )
}
#
# convert binary  to numeric
# convert ordinary  to numeric
binary_vars <- c(
  "death_close_family",
  "father_alive",
  "adopted_father_alive",
  "mother_alive",
  "adopted_mother_alive"
)

for (col in binary_vars) {
  x <- dat[[col]]
  set(
    dat,
    j = col,
    value = fcase(
      is.na(x), NA_real_,
      x == "No", 0,
      x == "Yes", 1,
      default = NA_real_
    )
  )
}

continous_vars <- c(
  "father_age",
  "father_age_death",
  "mother_age",
  "mother_age_death",
  "number_brothers",
  "number_adopted_brothers",
  "number_adopted_sisters",
  "number_older_siblings",
  "number_sisters"
)

dat[
  ,
  (continous_vars) := lapply(
    .SD,
    function(x) suppressWarnings(as.numeric(x))
  ),
  .SDcols = continous_vars
]
### dummy variables


unique(dat$adopted_father_illness)

## mother illness history
# dummy var for alzheimer's history

dat$mother_alzheimers <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Alzheimer's disease/dementia", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_alzheimers)


# dummy var for severe depression history
dat$mother_severe_depression <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Severe depression", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_severe_depression)

# dummy var for parkinsons history
dat$mother_parkinsons <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Parkinson's disease", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_parkinsons)

# dummy var for diabetes history

dat$mother_diabetes <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Diabetes", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_diabetes)


# dummy var for high bloood pressure history

dat$mother_high_blood_pressure <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("High blood pressure", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_high_blood_pressure)

# dummy var for chronic bronchitis/emphysema history
dat$mother_bronchitis_emphysema <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Chronic bronchitis/emphysema", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_bronchitis_emphysema)

# dummy var for breast cancer history
dat$mother_breast_cancer <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Breast cancer", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_breast_cancer)


# dummy var for bowelcancer history

dat$mother_bowel_cancer <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Bowel cancer", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_bowel_cancer)


# dummy var for lung cancer history
dat$mother_lung_cancer <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Lung cancer", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_lung_cancer)

# dummy var for stroke history

dat$mother_stroke <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Stroke", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_stroke)


# dummy var for heart disease history

dat$mother_heart_disease <- ifelse(  
  is.na(dat$mother_illness.0.0),  
  NA,  
  ifelse(grepl("Heart disease", dat$mother_illness.0.0), 1, 0)  
) 
table(dat$mother_heart_disease)


## adopted mother illness history
dat$adopted_mother_alzheimers <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Alzheimer's disease/dementia", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_alzheimers)


# dummy var for severe depression history
dat$adopted_mother_severe_depression <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Severe depression", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_severe_depression)

# dummy var for parkinsons history
dat$adopted_mother_parkinsons <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Parkinson's disease", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_parkinsons)

# dummy var for diabetes history

dat$adopted_mother_diabetes <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Diabetes", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_diabetes)


# dummy var for high bloood pressure history

dat$adopted_mother_high_blood_pressure <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("High blood pressure", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_high_blood_pressure)

# dummy var for chronic bronchitis/emphysema history
dat$adopted_mother_bronchitis_emphysema <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Chronic bronchitis/emphysema", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_bronchitis_emphysema)

# dummy var for breast cancer history
dat$adopted_mother_breast_cancer <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Breast cancer", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_breast_cancer)


# dummy var for bowelcancer history

dat$adopted_mother_bowel_cancer <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Bowel cancer", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_bowel_cancer)


# dummy var for lung cancer history
dat$adopted_mother_lung_cancer <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Lung cancer", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_lung_cancer)

# dummy var for stroke history

dat$adopted_mother_stroke <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Stroke", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_stroke)


# dummy var for heart disease history

dat$adopted_mother_heart_disease <- ifelse(  
  is.na(dat$adopted_mother_illness.0.0),  
  NA,  
  ifelse(grepl("Heart disease", dat$adopted_mother_illness.0.0), 1, 0)  
) 
table(dat$adopted_mother_heart_disease)


## father illness history
dat$father_alzheimers <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Alzheimer's disease/dementia", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_alzheimers)


# dummy var for severe depression history
dat$father_severe_depression <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Severe depression", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_severe_depression)

# dummy var for parkinsons history
dat$father_parkinsons <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Parkinson's disease", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_parkinsons)

# dummy var for diabetes history

dat$father_diabetes <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Diabetes", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_diabetes)


# dummy var for high bloood pressure history

dat$father_high_blood_pressure <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("High blood pressure", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_high_blood_pressure)

# dummy var for chronic bronchitis/emphysema history
dat$father_bronchitis_emphysema <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Chronic bronchitis/emphysema", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_bronchitis_emphysema)

# dummy var for prostate_cancer history
dat$father_prostate_cancer <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Prostate cancer", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_prostate_cancer)


# dummy var for bowelcancer history

dat$father_bowel_cancer <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Bowel cancer", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_bowel_cancer)


# dummy var for lung cancer history
dat$father_lung_cancer <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Lung cancer", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_lung_cancer)

# dummy var for stroke history

dat$father_stroke <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Stroke", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_stroke)


# dummy var for heart disease history

dat$father_heart_disease <- ifelse(  
  is.na(dat$father_illness.0.0),  
  NA,  
  ifelse(grepl("Heart disease", dat$father_illness.0.0), 1, 0)  
) 
table(dat$father_heart_disease)


## adopted father illness history

dat$adopted_father_alzheimers <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Alzheimer's disease/dementia", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_alzheimers)


# dummy var for severe depression history
dat$adopted_father_severe_depression <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Severe depression", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_severe_depression)

# dummy var for parkinsons history
dat$adopted_father_parkinsons <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Parkinson's disease", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_parkinsons)

# dummy var for diabetes history

dat$adopted_father_diabetes <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Diabetes", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_diabetes)


# dummy var for high bloood pressure history

dat$adopted_father_high_blood_pressure <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("High blood pressure", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_high_blood_pressure)

# dummy var for chronic bronchitis/emphysema history
dat$adopted_father_bronchitis_emphysema <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Chronic bronchitis/emphysema", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_bronchitis_emphysema)

# dummy var for prostate_cancer history
dat$adopted_father_prostate_cancer <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Prostate cancer", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_prostate_cancer)


# dummy var for bowelcancer history

dat$adopted_father_bowel_cancer <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Bowel cancer", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_bowel_cancer)


# dummy var for lung cancer history
dat$adopted_father_lung_cancer <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Lung cancer", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_lung_cancer)

# dummy var for stroke history

dat$adopted_father_stroke <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Stroke", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_stroke)


# dummy var for heart disease history

dat$adopted_father_heart_disease <- ifelse(  
  is.na(dat$adopted_father_illness),  
  NA,  
  ifelse(grepl("Heart disease", dat$adopted_father_illness), 1, 0)  
) 
table(dat$adopted_father_heart_disease)

## sibling illness history
dat$sibling_alzheimers <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Alzheimer's disease/dementia", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_alzheimers)


# dummy var for severe depression history
dat$sibling_severe_depression <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Severe depression", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_severe_depression)

# dummy var for parkinsons history
dat$sibling_parkinsons <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Parkinson's disease", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_parkinsons)

# dummy var for diabetes history

dat$sibling_diabetes <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Diabetes", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_diabetes)


# dummy var for high bloood pressure history

dat$sibling_high_blood_pressure <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("High blood pressure", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_high_blood_pressure)

# dummy var for chronic bronchitis/emphysema history
dat$sibling_bronchitis_emphysema <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Chronic bronchitis/emphysema", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_bronchitis_emphysema)

# dummy var for Breast cancer history
dat$sibling_breast_cancer <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Breast cancer", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_breast_cancer)

# dummy var for prostate_cancer history
dat$sibling_prostate_cancer <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Prostate cancer", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_prostate_cancer)


# dummy var for bowelcancer history

dat$sibling_bowel_cancer <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Bowel cancer", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_bowel_cancer)


# dummy var for lung cancer history
dat$sibling_lung_cancer <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Lung cancer", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_lung_cancer)

# dummy var for stroke history

dat$sibling_stroke <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Stroke", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_stroke)


# dummy var for heart disease history

dat$sibling_heart_disease <- ifelse(  
  is.na(dat$sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Heart disease", dat$sibling_illness.0.0), 1, 0)  
) 
table(dat$sibling_heart_disease)


## adopted sibling illness history

# dummy var for Breast cancer history
dat$adopted_sibling_breast_cancer <- ifelse(  
  is.na(dat$adopted_sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Breast cancer", dat$adopted_sibling_illness.0.0), 1, 0)  
) 
table(dat$adopted_sibling_breast_cancer)

# dummy var for prostate_cancer history
dat$adopted_sibling_prostate_cancer <- ifelse(  
  is.na(dat$adopted_sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Prostate cancer", dat$adopted_sibling_illness.0.0), 1, 0)  
) 
table(dat$adopted_sibling_prostate_cancer)


# dummy var for bowelcancer history

dat$adopted_sibling_bowel_cancer <- ifelse(  
  is.na(dat$adopted_sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Bowel cancer", dat$adopted_sibling_illness.0.0), 1, 0)  
) 
table(dat$adopted_sibling_bowel_cancer)


# dummy var for lung cancer history
dat$adopted_sibling_lung_cancer <- ifelse(  
  is.na(dat$adopted_sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Lung cancer", dat$adopted_sibling_illness.0.0), 1, 0)  
) 
table(dat$adopted_sibling_lung_cancer)

# dummy var for stroke history

dat$adopted_sibling_stroke <- ifelse(  
  is.na(dat$adopted_sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Stroke", dat$adopted_sibling_illness.0.0), 1, 0)  
) 
table(dat$adopted_sibling_stroke)


# dummy var for heart disease history

dat$adopted_sibling_heart_disease <- ifelse(  
  is.na(dat$adopted_sibling_illness.0.0),  
  NA,  
  ifelse(grepl("Heart disease", dat$adopted_sibling_illness.0.0), 1, 0)  
) 
table(dat$adopted_sibling_heart_disease)

#remove columns used to make dummy vars
drop_cols(dat, family_illness_vars)
### Medications####
unique(dat$other_meds)
sort(unique(dat$number_medications))
sort(unique(dat$diabetes_insulin_1yr))

# convert binary  to numeric
# convert ordinary  to numeric
dat$other_meds<-ifelse(dat$other_meds=="No",0,1)
dat$number_medications <- as.numeric(dat$number_medications)
dat$diabetes_insulin_1yr<-ifelse(dat$diabetes_insulin_1yr=="No",0,1)
### dummy variables 

## meds

unique(dat$meds_0_0)
unique(dat$meds_hormones)

# The same medication constructs are collected in sex-specific UKB source fields.
# Create each from the appropriate source, then harmonise to one final variable.

dat[, cholesterol_meds_1 := make_dummy(meds_0_0, "Cholesterol lowering medication")]
dat[, blood_pressure_meds_1 := make_dummy(meds_0_0, "Blood pressure medication")]
dat[, insulin_1 := make_dummy(meds_0_0, "Insulin")]

dat[, cholesterol_meds_2 := make_dummy(meds_hormones, "Cholesterol lowering medication")]
dat[, blood_pressure_meds_2 := make_dummy(meds_hormones, "Blood pressure medication")]
dat[, insulin_2 := make_dummy(meds_hormones, "Insulin")]

dat[, cholesterol_meds := pmax(cholesterol_meds_1, cholesterol_meds_2, na.rm = TRUE)]
dat[, blood_pressure_meds := pmax(blood_pressure_meds_1, blood_pressure_meds_2, na.rm = TRUE)]
dat[, insulin := pmax(insulin_1, insulin_2, na.rm = TRUE)]

dat[
  is.na(cholesterol_meds_1) & is.na(cholesterol_meds_2),
  cholesterol_meds := NA_integer_
]
dat[
  is.na(blood_pressure_meds_1) & is.na(blood_pressure_meds_2),
  blood_pressure_meds := NA_integer_
]
dat[
  is.na(insulin_1) & is.na(insulin_2),
  insulin := NA_integer_
]

drop_cols(
  dat,
  c(
    "cholesterol_meds_1",
    "cholesterol_meds_2",
    "blood_pressure_meds_1",
    "blood_pressure_meds_2",
    "insulin_1",
    "insulin_2"
  )
)

table(dat$cholesterol_meds, useNA = "ifany")
table(dat$blood_pressure_meds, useNA = "ifany")
table(dat$insulin, useNA = "ifany")

# pain medications

unique(dat$pain_meds)

# dummy var for Aspirin

dat$aspirin <- ifelse(  
  is.na(dat$pain_meds),  
  NA,  
  ifelse(grepl("Aspirin", dat$pain_meds), 1, 0)  
) 
table(dat$aspirin)


# dummy var for Paracetamol

dat$paracetamol <- ifelse(  
  is.na(dat$pain_meds),  
  NA,  
  ifelse(grepl("Paracetamol", dat$pain_meds), 1, 0)  
) 
table(dat$paracetamol)


# dummy var for Ibuprofen (e.g. Nurofen)
dat$ibuprofen <- ifelse(  
  is.na(dat$pain_meds),  
  NA,  
  ifelse(grepl("Ibuprofen", dat$pain_meds), 1, 0)  
) 
table(dat$ibuprofen)

# dummy var for Omeprazole (e.g. Zanprol)

dat$omeprazole<- ifelse(  
  is.na(dat$pain_meds),  
  NA,  
  ifelse(grepl("Omeprazole", dat$pain_meds), 1, 0)  
) 
table(dat$omeprazole)


# dummy var for Ranitidine (e.g. Zantac)
dat$ranitidine<- ifelse(  
  is.na(dat$pain_meds),  
  NA,  
  ifelse(grepl("Ranitidine", dat$pain_meds), 1, 0)  
) 
table(dat$ranitidine)


# dummy var for Laxatives (e.g. Dulcolax, Senokot)
dat$laxatives<- ifelse(  
  is.na(dat$pain_meds),  
  NA,  
  ifelse(grepl("Laxatives", dat$pain_meds), 1, 0)  
) 
table(dat$laxatives)


## vitamins

unique(dat$vitamins)


# dummy var for Multivitamins +/- minerals

dat$multivitamin_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Multivitamins", dat$vitamins), 1, 0)  
) 
table(dat$multivitamin_supplements)

# dummy var for Vitamin A
dat$vitamin_A_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Vitamin A", dat$vitamins), 1, 0)  
) 
table(dat$vitamin_A_supplements)

# dummy var for Vitamin B

dat$vitamin_B_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Vitamin B", dat$vitamins), 1, 0)  
) 
table(dat$vitamin_B_supplements)
# dummy var for Vitamin C
dat$vitamin_C_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Vitamin C", dat$vitamins), 1, 0)  
) 
table(dat$vitamin_C_supplements)

# dummy var for Vitamin D
dat$vitamin_D_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Vitamin D", dat$vitamins), 1, 0)  
) 
table(dat$vitamin_D_supplements)
# dummy var for Vitamin E

dat$vitamin_E_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Vitamin E", dat$vitamins), 1, 0)  
) 
table(dat$vitamin_E_supplements)

# dummy var for Folic acid or Folate (Vit B9)
dat$folate_supplements<- ifelse(  
  is.na(dat$vitamins),  
  NA,  
  ifelse(grepl("Folic acid or Folate",dat$vitamins), 1, 0)  
) 
table(dat$folate_supplements)


## minerals

unique(dat$minerals)

# dummy var for Fish oil (including cod liver oil)
dat$fish_oil_supplements<- ifelse(  
  is.na(dat$minerals),  
  NA,  
  ifelse(grepl("Fish oil",dat$minerals), 1, 0)  
) 
table(dat$fish_oil_supplements)


# dummy var for Calcium
dat$calcium_supplements<- ifelse(  
  is.na(dat$minerals),  
  NA,  
  ifelse(grepl("Calcium",dat$minerals), 1, 0)  
) 
table(dat$calcium_supplements)


# dummy var for Selenium
dat$selenium_supplements<- ifelse(  
  is.na(dat$minerals),  
  NA,  
  ifelse(grepl("Selenium",dat$minerals), 1, 0)  
) 
table(dat$selenium_supplements)


# dummy var for Glucosamine
dat$glucosamine_supplements<- ifelse(  
  is.na(dat$minerals),  
  NA,  
  ifelse(grepl("Glucosamine",dat$minerals), 1, 0)  
) 
table(dat$glucosamine_supplements)

# dummy var for Iron
dat$iron_supplements<- ifelse(  
  is.na(dat$minerals),  
  NA,  
  ifelse(grepl("Iron",dat$minerals), 1, 0)  
) 
table(dat$iron_supplements)


# dummy var for Zinc
dat$zinc_supplements<- ifelse(  
  is.na(dat$minerals),  
  NA,  
  ifelse(grepl("Zinc",dat$minerals), 1, 0)  
) 
table(dat$zinc_supplements)

#remove columns used to make dummy vars
drop_cols(
  dat,
  c(
    "meds_0_0",
    "meds_hormones",
    "pain_meds",
    "vitamins",
    "minerals"
  )
)
### Medical conditions#####
unique(dat$fracture_last_5yrs)
unique(dat$fracture_fall)
unique(dat$glasses)
sort(unique(dat$glasses_age))
unique(dat$myopia_eye)
unique(dat$hypertropia_eye)
unique(dat$presbyopia_eye)
unique(dat$astigmatism_eye)
unique(dat$strabismus_eye)
unique(dat$amblyopia_eye)
unique(dat$other_eye)
unique(dat$other_serious_eye)
unique(dat$other_eye_problems)
unique(dat$diabetes_eye)
unique(dat$glaucoma_eye)
unique(dat$injury_eye)
unique(dat$cataract_eye)
unique(dat$macular_degeneration_eye)
sort(unique(dat$diabetes_eye_age))
sort(unique(dat$glaucoma_age))
sort(unique(dat$injury_eye_age))
sort(unique(dat$cataract_age))
sort(unique(dat$macular_degeneration_age))
sort(unique(dat$other_serious_eye_age))
unique(dat$overall_health)
unique(dat$chronic_illness)
unique(dat$falls_last_1yr)
unique(dat$weight_change_last_1yr)
unique(dat$chest_weeze_last_1yr)
unique(dat$shortness_breath)
unique(dat$leg_pain_walking)
unique(dat$leg_pain_standing)
unique(dat$leg_pain_calves)
unique(dat$leg_pain_walking_uphill)
unique(dat$leg_pain_walking_normal)
unique(dat$leg_pain_walking_disappears)
unique(dat$leg_pain_walking_action)
unique(dat$leg_pain_walking_standing)
unique(dat$leg_artery_surgery)
unique(dat$leg_surgery)
unique(dat$headaches_3months)
unique(dat$facial_pain_3months)
unique(dat$neck_pain_3months)
unique(dat$stomach_pain_3months)
unique(dat$back_pain_3months)
unique(dat$hip_pain_3months)
unique(dat$knee_pain_3months)
unique(dat$general_pain_3months)
unique(dat$chest_pain)
unique(dat$chest_pain_walking_normal)
unique(dat$chest_pain_ceases_still)
unique(dat$chest_pain_walking_uphill)
unique(dat$major_operations)
unique(dat$other_operations)
sort(unique(dat$number_operations))
unique(dat$pace_maker)
unique(dat$hearing_difficulty)
unique(dat$hearing_difficulty_background)
unique(dat$hearing_aid)
unique(dat$cochlear_implant)
unique(dat$tinnitus)
unique(dat$tinnitus_severity)
unique(dat$loud_music_exposure)
unique(dat$bowel_cancer_screening)
sort(unique(dat$bowel_cancer_screening_recent))
unique(dat$PSA_test)
sort(unique(dat$PSA_test_time_since))

# binary vars

vars <- c("fracture_last_5yrs","fracture_fall","glasses","other_eye_problems","chronic_illness",
          "chest_weeze_last_1yr","shortness_breath","leg_pain_walking","leg_pain_standing",
          "leg_pain_calves","leg_pain_walking_uphill","leg_pain_walking_normal","leg_pain_walking_disappears",
          "leg_artery_surgery","facial_pain_3months","neck_pain_3months","stomach_pain_3months",
          "back_pain_3months","hip_pain_3months","knee_pain_3months","general_pain_3months",
          "chest_pain","chest_pain_ceases_still","pace_maker","hearing_difficulty_background",
          "hearing_aid","cochlear_implant","bowel_cancer_screening","PSA_test","major_operations",
          "other_operations","headaches_3months")

for (col in vars){
  dat[[col]] <- ifelse(dat[[col]]=="No",0,1)
}
# numeric 
vars <- c("glasses_age","diabetes_eye_age","glaucoma_age","injury_eye_age","cataract_age",
          "macular_degeneration_age","other_serious_eye_age","number_operations","bowel_cancer_screening_recent",
          "PSA_test_time_since")

for (col in vars){
  dat[[col]][which(dat[[col]]=="Less than 1 year ago")]=0.5
  dat[[col]][which(dat[[col]]=="Less than a year ago")]=0.5
  dat[[col]] <- as.numeric(dat[[col]])
}
# catagery vars
vars <- c("myopia_eye","hypertropia_eye","presbyopia_eye","astigmatism_eye","strabismus_eye",
          "amblyopia_eye","other_eye","other_serious_eye","diabetes_eye","glaucoma_eye",
          "injury_eye","cataract_eye","macular_degeneration_eye")
#
dat$overall_health <- ifelse(dat$overall_health=="Poor",0,
                             ifelse(dat$overall_health=="Fair",1,
                                    ifelse(dat$overall_health=="Good",2,3)))  
#
dat$falls_last_1yr <- ifelse(dat$falls_last_1yr=="No falls",0,
                             ifelse(dat$falls_last_1yr=="Only one fall",1,2)) 

dat$weight_change_last_1yr <- ifelse(dat$weight_change_last_1yr=="Yes - lost weight",0,
                                     ifelse(dat$weight_change_last_1yr=="No - weigh about the same",1,2)) 

dat$leg_pain_walking_action<- ifelse(dat$leg_pain_walking_action=="Stop",0,
                                     ifelse(dat$leg_pain_walking_action=="Slow down",1,2)) 
#
dat$leg_pain_walking_standing<- ifelse(dat$leg_pain_walking_standing=="Pain usually disappears in less than 10 minutes",0,1)
#
dat$leg_surgery<- ifelse(dat$leg_surgery=="No",0,
                         ifelse(dat$leg_surgery=="Yes, leg below the knee",1,
                                ifelse(dat$leg_surgery=="Yes, toes",2,3)))
#
dat$hearing_difficulty <- ifelse(dat$hearing_difficulty=="No",0,
                                 ifelse(dat$hearing_difficulty=="Yes",1,2)) 
#
dat$tinnitus<- ifelse(dat$tinnitus=="No, never",0,
                      ifelse(dat$tinnitus=="Yes, but not now, but have in the past",1,
                             ifelse(dat$tinnitus=="Yes, now some of the time",2,
                                    ifelse(dat$tinnitus=="Yes, now a lot of the time",3,4))))

dat$tinnitus_severity<- ifelse(dat$tinnitus_severity=="Not at all",0,
                               ifelse(dat$tinnitus_severity=="Slightly",1,
                                      ifelse(dat$tinnitus_severity=="Moderately",2,3)))

#
dat$loud_music_exposure <- ifelse(dat$loud_music_exposure =="No",0,
                                  ifelse(dat$loud_music_exposure =="Yes, for less than a year",1,
                                         ifelse(dat$loud_music_exposure =="Yes, for around 1-5 years",2,3)))
#

dat$chest_pain_walking_normal<- ifelse(dat$chest_pain_walking_normal=="Unable to walk on the level",0,
                                       ifelse(dat$chest_pain_walking_normal=="No",1,2))

dat$chest_pain_walking_uphill<- ifelse(dat$chest_pain_walking_uphill=="Unable to walk up hills or to hurry",0,
                                       ifelse(dat$chest_pain_walking_uphill=="No",1,2))
### dummy vars
## fracture site

unique(dat$fracture_site)

# dummy var for Spine
dat$spine_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Spine", dat$fracture_site), 1, 0)  
) 
table(dat$spine_fracture)

# dummy var for Ankle
dat$ankle_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Ankle", dat$fracture_site), 1, 0)  
) 
table(dat$ankle_fracture)

# dummy var for Wrist
dat$wrist_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Wrist", dat$fracture_site), 1, 0)  
) 
table(dat$wrist_fracture)

# dummy var for Arm
dat$arm_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Arm", dat$fracture_site), 1, 0)  
) 
table(dat$arm_fracture)


# dummy var for Hip 

dat$hip_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Hip", dat$fracture_site), 1, 0)  
) 
table(dat$hip_fracture)


# dummy var for Leg 
dat$leg_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Leg", dat$fracture_site), 1, 0)  
) 
table(dat$leg_fracture)

# dummy var for Other bones 

dat$other_fracture <- ifelse(  
  is.na(dat$fracture_site),  
  NA,  
  ifelse(grepl("Other bones", dat$fracture_site), 1, 0)  
) 
table(dat$other_fracture)


## glasses reason

unique(dat$glasses_reason_0_0)

# dummy var for short sighted

dat$glasses_myopia <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("For short-sightedness", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_myopia)

# dummy var for short sighted

dat$glasses_presbyopia <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("For just reading/near work as you are getting older", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_presbyopia)

# dummy var for long sighted
dat$glasses_hypermetropia <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("For long-sightedness", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_hypermetropia)

# dummy var for lazy eye

dat$glasses_amblyopia <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("For a 'lazy' eye or an eye with poor vision since childhood", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_amblyopia)

# dummy var for astigmatism
dat$glasses_astigmatism <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("For 'astigmatism'", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_astigmatism)


# dummy var for strabismus

dat$glasses_strabismus  <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("For a 'squint' or 'turn' in an eye since childhood", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_strabismus )


# dummy var for other
dat$glasses_other <- ifelse(  
  is.na(dat$glasses_reason_0_0),  
  NA,  
  ifelse(grepl("Other eye condition", dat$glasses_reason_0_0), 1, 0)  
) 
table(dat$glasses_other )

## eye disorders

unique(dat$eye_disorders)

# dummy var for Glaucoma 

dat$eye_glaucoma<- ifelse(  
  is.na(dat$eye_disorders),  
  NA,  
  ifelse(grepl("Glaucoma", dat$eye_disorders), 1, 0)  
) 
table(dat$eye_glaucoma)


# dummy var for Glaucoma 
dat$eye_macular_degeneration<- ifelse(  
  is.na(dat$eye_disorders),  
  NA,  
  ifelse(grepl("Macular degeneration", dat$eye_disorders), 1, 0)  
) 
table(dat$eye_macular_degeneration)



# dummy var for Diabetes related eye disease
dat$eye_diabetes_eye<- ifelse(  
  is.na(dat$eye_disorders),  
  NA,  
  ifelse(grepl("Diabetes related eye disease", dat$eye_disorders), 1, 0)  
) 
table(dat$eye_diabetes_eye)

# dummy var for Cataract 

dat$eye_cataract<- ifelse(  
  is.na(dat$eye_disorders),  
  NA,  
  ifelse(grepl("Cataract", dat$eye_disorders), 1, 0)  
) 
table(dat$eye_cataract)


# dummy var for Other serious eye condition 
dat$eye_other_serious_eye_condition<- ifelse(  
  is.na(dat$eye_disorders),  
  NA,  
  ifelse(grepl("Other serious eye condition", dat$eye_disorders), 1, 0)  
) 
table(dat$eye_other_serious_eye_condition)


# dummy var for Injury or trauma resulting in loss of vision 

dat$eye_trauma_vision_loss<- ifelse(  
  is.na(dat$eye_disorders),  
  NA,  
  ifelse(grepl("Injury or trauma resulting in loss of vision", dat$eye_disorders), 1, 0)  
) 
table(dat$eye_trauma_vision_loss)


## mouth problems
unique(dat$mouth_problems)


# dummy var for Bleeding gums
dat$bleeding_gums<- ifelse(  
  is.na(dat$mouth_problems),  
  NA,  
  ifelse(grepl("Bleeding gums", dat$mouth_problems), 1, 0)  
) 
table(dat$bleeding_gums)


# dummy var for Dentures
dat$dentures<- ifelse(  
  is.na(dat$mouth_problems),  
  NA,  
  ifelse(grepl("Dentures", dat$mouth_problems), 1, 0)  
) 
table(dat$dentures)


# dummy var for Toothache
dat$toothache <- ifelse(  
  is.na(dat$mouth_problems),  
  NA,  
  ifelse(grepl("Toothache", dat$mouth_problems), 1, 0)  
) 
table(dat$toothache)


# dummy var for Mouth ulcers
dat$mouth_ulcers <- ifelse(  
  is.na(dat$mouth_problems),  
  NA,  
  ifelse(grepl("Mouth ulcers", dat$mouth_problems), 1, 0)  
) 
table(dat$mouth_ulcers)


# dummy var for Painful gums
dat$painful_gums <- ifelse(  
  is.na(dat$mouth_problems),  
  NA,  
  ifelse(grepl("Painful gums", dat$mouth_problems), 1, 0)  
) 
table(dat$painful_gums)

# dummy var for Loose teeth
dat$loose_teeth <- ifelse(  
  is.na(dat$mouth_problems),  
  NA,  
  ifelse(grepl("Loose teeth", dat$mouth_problems), 1, 0)  
) 
table(dat$loose_teeth)

## pain last month
unique(dat$pain_last_1month)

# dummy var for Neck or shoulder pain
dat$neck_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Neck or shoulder pain", dat$pain_last_1month), 1, 0)  
) 
table(dat$neck_pain_1month)



# dummy var for Pain all over the body
dat$whole_body_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Pain all over the body", dat$pain_last_1month), 1, 0)  
) 
table(dat$whole_body_pain_1month)


# dummy var for Headache
dat$headaches_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Headache", dat$pain_last_1month), 1, 0)  
) 
table(dat$headaches_1month)


# dummy var for Knee pain 
dat$knee_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Knee pain", dat$pain_last_1month), 1, 0)  
) 
table(dat$knee_pain_1month)



# dummy var for Stomach or abdominal pain 

dat$stomach_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Stomach or abdominal pain", dat$pain_last_1month), 1, 0)  
) 
table(dat$stomach_pain_1month)


# dummy var for Facial pain
dat$facial_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Facial pain", dat$pain_last_1month), 1, 0)  
) 
table(dat$facial_pain_1month)

# dummy var for Back pain
dat$back_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Back pain", dat$pain_last_1month), 1, 0)  
) 
table(dat$back_pain_1month)


# dummy var for Hip pain
dat$hip_pain_1month <- ifelse(  
  is.na(dat$pain_last_1month),  
  NA,  
  ifelse(grepl("Hip pain", dat$pain_last_1month), 1, 0)  
) 
table(dat$hip_pain_1month)


#remove columns used to make dummy vars
drop_cols(
  dat,
  c(
    "fracture_site",
    "glasses_reason_0_0",
    "eye_disorders",
    "mouth_problems",
    "pain_last_1month"
  )
)

### Male reproduction####

unique(dat$age_facial_hair)
unique(dat$age_voice)
unique(dat$balding)
sort(unique(dat$number_children))

# convert binary  to numeric
# convert ordinary  to numeric
ordinary_vars<-c("age_facial_hair","age_voice")

for (col in ordinary_vars){
  dat[[col]]<-ifelse(dat[[col]]=="Younger than average",0,ifelse(dat[[col]]=="About average age",1,2))
}

dat$number_children <- as.numeric(dat$number_children)
### Female reproduction####
unique(dat$mammogram)
sort(unique(dat$mammogram_years_since))
unique(dat$cervical_smear)
sort(unique(dat$cervical_smear_years_since))
sort(unique(dat$menarche_age))
unique(dat$menopause)
sort(unique(dat$period_time_since))
sort(unique(dat$menstrual_cycle_length))
unique(dat$menstruating_today)
sort(unique(dat$number_births))
sort(unique(dat$first_child_birth_weight))
sort(unique(dat$birth_age))
sort(unique(dat$first_birth_age))
sort(unique(dat$last_birth_age))
unique(dat$stillbirth)
sort(unique(dat$number_stillbirths))
sort(unique(dat$number_miscarriages))
sort(unique(dat$number_pregnancy_terminations))
unique(dat$oral_contraceptive)
sort(unique(dat$oral_contraceptive_age_first))
sort(unique(dat$oral_contraceptive_age_last))
unique(dat$HRT)
sort(unique(dat$HRT_age_first))
sort(unique(dat$HRT_age_last))
unique(dat$hysterectomy)
sort(unique(dat$hysterectomy_age))
unique(dat$bilateral_oophorectomy)
sort(unique(dat$bilateral_oophorectomy_age))
unique(dat$pregnant)

# convert binary  to numeric
# convert ordinary  to numeric

dat$menopause[which(dat$menopause == "Not sure - had a hysterectomy")] <- NA         
dat$menopause[which(dat$menopause == "Not sure - other reason")] <- NA  
dat$hysterectomy[which(dat$hysterectomy == "Not sure")] <- NA  
dat$bilateral_oophorectomy[which(dat$bilateral_oophorectomy == "Not sure")] <- NA 
dat$pregnant[which(dat$pregnant == "Unsure")] <- NA  
binary_vars<-c("mammogram","cervical_smear","menstruating_today","stillbirth",
               "oral_contraceptive","HRT","menopause","hysterectomy",
               "bilateral_oophorectomy","pregnant")
for (col in binary_vars){
  dat[[col]]<-ifelse(dat[[col]]=="No",0,1)
}

dat$mammogram_years_since <- as.numeric(dat$mammogram_years_since)
#
dat$cervical_smear_years_since[which(dat$cervical_smear_years_since == "Less than a year ago")] <- 0.5   
dat$cervical_smear_years_since <- as.numeric(dat$cervical_smear_years_since)

dat$menarche_age <- as.numeric(dat$menarche_age)

dat$period_time_since <- as.numeric(dat$period_time_since)
#
dat$menstrual_cycle_length[which(dat$menstrual_cycle_length == "Irregular cycle")] <- NA
dat$menstrual_cycle_length <- as.numeric(dat$menstrual_cycle_length)


dat$number_births <- as.numeric(dat$number_births)

dat$first_child_birth_weight <- as.numeric(dat$first_child_birth_weight)

dat$birth_age <- as.numeric(dat$birth_age)

dat$first_birth_age <- as.numeric(dat$first_birth_age)
dat$last_birth_age <- as.numeric(dat$last_birth_age)
#
dat$number_stillbirths <- as.numeric(dat$number_stillbirths)
dat$number_miscarriages <- as.numeric(dat$number_miscarriages)
dat$number_pregnancy_terminations <- as.numeric(dat$number_pregnancy_terminations)
#
dat$oral_contraceptive_age_first <- as.numeric(dat$oral_contraceptive_age_first)
dat$HRT_age_last <- ifelse(dat$HRT_age_last == "Still taking HRT",   
                           dat$recruitment_age,   
                           dat$HRT_age_last)  
dat$oral_contraceptive_age_last <- as.numeric(dat$oral_contraceptive_age_last)
#
dat$HRT_age_first <- as.numeric(dat$HRT_age_first)
dat$HRT_age_last <- as.numeric(dat$HRT_age_last)
dat$hysterectomy_age <- as.numeric(dat$hysterectomy_age)
dat$bilateral_oophorectomy_age <- as.numeric(dat$bilateral_oophorectomy_age)
dat$menopause_age <- as.numeric(dat$menopause_age)
### Electronics use####
unique(dat$mobile_phone_duration)
unique(dat$mobile_phone_weekly_usage)
unique(dat$speakerphone)
unique(dat$mobile_phone_2yrs)
unique(dat$head_side_phone)
unique(dat$computer_games)

# convert binary  to numeric
# convert ordinary  to numeric
dat$mobile_phone_duration<-ifelse(dat$mobile_phone_duration=="Never used mobile phone at least once per week",0,
                                  ifelse(dat$mobile_phone_duration=="One year or less",1,
                                         ifelse(dat$mobile_phone_duration=="Two to four years",2,
                                                ifelse(dat$mobile_phone_duration=="Five to eight years",3,4))
                                         
                                  ))
dat$mobile_phone_weekly_usage<-ifelse(dat$mobile_phone_weekly_usage=="Less than 5mins",0,
                                      ifelse(dat$mobile_phone_weekly_usage=="5-29 mins",1,
                                             ifelse(dat$mobile_phone_weekly_usage=="30-59 mins",2,
                                                    ifelse(dat$mobile_phone_weekly_usage=="1-3 hours",3,4))
                                             
                                      ))

dat$speakerphone<-ifelse(dat$speakerphone=="Never or almost never",0,
                         ifelse(dat$speakerphone=="Less than half the time",1,
                                ifelse(dat$speakerphone=="About half the time",2,
                                       ifelse(dat$speakerphone=="More than half the time",3,4))
                                
                         ))
dat[
  mobile_phone_2yrs == "I didn't use a mobile phone two years ago",
  mobile_phone_2yrs := "No"
]

dat[
  ,
  mobile_phone_2yrs := fcase(
    is.na(mobile_phone_2yrs), NA_real_,
    mobile_phone_2yrs == "No", 0,
    mobile_phone_2yrs == "Yes, use is now less frequent", 1,
    mobile_phone_2yrs == "Yes, use is now more frequent", 2,
    default = NA_real_
  )
]
#
table(dat$head_side_phone)
#
dat$computer_games<-ifelse(dat$computer_games=="Never/rarely",0,
                           ifelse(dat$computer_games=="Sometimes",1,2
                           ))

### Sleep####
sort(unique(dat$sleep_hours))
unique(dat$easy_wake)
unique(dat$chronotype)
unique(dat$nap)
unique(dat$sleep_difficulty)
unique(dat$snoring)
unique(dat$narcolepsy)
# convert binary  to numeric
# convert ordinary  to numeric

dat$sleep_hours <- as.numeric(dat$sleep_hours)
dat$easy_wake <- ifelse(dat$easy_wake=="Not at all easy",0,
                        ifelse(dat$easy_wake=="Not very easy",1,
                               ifelse(dat$easy_wake=="Fairly easy",2,3)))
#
table(dat$chronotype)

#
dat$nap <- ifelse(dat$nap=="Never/rarely",0,
                  ifelse(dat$nap=="Sometimes",1,2))
dat$sleep_difficulty <- ifelse(dat$sleep_difficulty=="Never/rarely",0,
                               ifelse(dat$sleep_difficulty=="Sometimes",1,2))
#
dat$snoring <- ifelse(dat$snoring=="No",0,1)
#
dat$narcolepsy <- ifelse(dat$narcolepsy=="Never/rarely",0,
                         ifelse(dat$narcolepsy=="Sometimes",1,
                                ifelse(dat$narcolepsy=="Often",2,3)))
### Smoking####
unique(dat$ever_smoked)
table(dat$ever_smoked)
sort(unique(dat$pack_years_prop))
sum(is.na(dat$pack_years))
sort(unique(dat$pack_years))
sum(is.na(dat$pack_years_prop))
unique(dat$smoking_status)
table(dat$smoking_status)
unique(dat$tobacco)
unique(dat$past_tobacco)
table(dat$past_tobacco)
unique(dat$light_smoking)
sort(unique(dat$smoking_start_age_current))
unique(dat$tobacco_type)
unique(dat$previous_smoker)
sort(unique(dat$number_cigarettes))
sort(unique(dat$cigarettes_stop_age))
sort(unique(dat$previous_number_cigarettes))
unique(dat$waking_smoke_time)
unique(dat$smoking_10yrs)
sort(unique(dat$smoking_start_age_former))
unique(dat$tobacco_type_previous)
sort(unique(dat$number_cigarettes_previous))
sort(unique(dat$smoking_stop_age))
unique(dat$ever_stop_smoking_6months)
sort(unique(dat$number_quit_attempts))
unique(dat$hshld_smokers)
sort(unique(dat$tobacco_exposure_home))
sort(unique(dat$tobacco_exposure_outside))
sort(unique(dat$resume_smoking_likelihood))
sort(unique(dat$difficulty_not_smoking_1day))
sort(unique(dat$stop_smoking_attempt))
sort(unique(dat$stop_smoking_desire))
# convert binary  to numeric
# convert ordinary  to numeric
binary_vars<-c("ever_smoked","light_smoking","ever_stop_smoking_6months","previous_smoker")
for (col in binary_vars){
  dat[[col]]<-ifelse(dat[[col]]=="No",0,1)
}
#
dat$pack_years_prop <- as.numeric(dat$pack_years_prop)
dat$pack_years <- as.numeric(dat$pack_years)
#
dat$smoking_status <- ifelse(dat$smoking_status=="Never",0,
                             ifelse(dat$smoking_status=="Previous",1,2))

#
dat$tobacco <- ifelse(dat$tobacco=="No",0,
                      ifelse(dat$tobacco=="Only occasionally",1,2))
dat$past_tobacco <- ifelse(dat$past_tobacco=="I have never smoked",0,
                           ifelse(dat$past_tobacco=="Just tried once or twice",1,
                                  ifelse(dat$past_tobacco=="Smoked occasionally",2,3)))
#
dat$smoking_start_age_current <- as.numeric(dat$smoking_start_age_current)
#
table(dat$tobacco_type)
#
dat$number_cigarettes[which(dat$number_cigarettes == "Less than one a day")] <- 0.5  
dat$number_cigarettes <- as.numeric(dat$number_cigarettes)
#
dat$cigarettes_stop_age <- as.numeric(dat$cigarettes_stop_age)
#
dat[
  previous_number_cigarettes == "Less than one a day",
  previous_number_cigarettes := "0.5"
]
dat[, previous_number_cigarettes := suppressWarnings(as.numeric(previous_number_cigarettes))]
#
dat$waking_smoke_time <- ifelse(dat$waking_smoke_time=="Less than 5 minutes",0,
                                ifelse(dat$waking_smoke_time=="Between 5-15 minutes",1,
                                       ifelse(dat$waking_smoke_time=="Between 30 minutes - 1 hour",2,
                                              ifelse(dat$waking_smoke_time=="Between 1 and 2 hours",3,4))))
#
table(dat$smoking_10yrs)
dat$smoking_10yrs <- ifelse(dat$smoking_10yrs=="Less nowadays?",0,
                            ifelse(dat$smoking_10yrs=="About the same?",1,2))
#
dat$smoking_start_age_former <- as.numeric(dat$smoking_start_age_former)
#
table(dat$tobacco_type_previous)
#
dat$number_cigarettes_previous[which(dat$number_cigarettes_previous == "Less than one a day")] <- 0.5  
dat$number_cigarettes_previous <- as.numeric(dat$number_cigarettes_previous)
#
dat$smoking_stop_age <- as.numeric(dat$smoking_stop_age)

#
dat$number_quit_attempts <- as.numeric(dat$number_quit_attempts)
dat$hshld_smokers <- ifelse(dat$hshld_smokers=="No",0,
                            ifelse(dat$hshld_smokers=="Yes, one household member smokes",1,2))

table(dat$hshld_smokers)
#
dat$tobacco_exposure_home <- as.numeric(dat$tobacco_exposure_home)
#
dat$tobacco_exposure_outside <- as.numeric(dat$tobacco_exposure_outside)
#
dat$resume_smoking_likelihood <- ifelse(dat$resume_smoking_likelihood=="No, definitely not",0,
                                        ifelse(dat$resume_smoking_likelihood=="No, probably not",1,
                                               ifelse(dat$resume_smoking_likelihood=="Yes, probably",2,3)))
#
dat$difficulty_not_smoking_1day <- ifelse(dat$difficulty_not_smoking_1day=="Very easy",0,
                                          ifelse(dat$difficulty_not_smoking_1day=="Fairly easy",1,
                                                 ifelse(dat$difficulty_not_smoking_1day=="Fairly difficult",2,3)))
#
dat$stop_smoking_attempt <- ifelse(dat$stop_smoking_attempt=="No",0,
                                   ifelse(dat$stop_smoking_attempt=="Yes, tried but was not able to stop or stopped for less than 6 months",1,2))
#
dat$stop_smoking_desire <- ifelse(dat$stop_smoking_desire=="No, definitely not",0,
                                  ifelse(dat$stop_smoking_desire=="No, probably not",1,
                                         ifelse(dat$stop_smoking_desire=="Yes, probably",2,3)))
### dummy variables

## why reduced smoking

unique(dat$why_reduced_smoking_0)

# dummy var for health
dat$reduced_smoking_health <- ifelse(  
  is.na(dat$why_reduced_smoking_0),  
  NA,  
  ifelse(grepl("Health precaution", dat$why_reduced_smoking_0), 1, 0)  
) 
table(dat$reduced_smoking_health)

# dummy var for Illness or ill health
dat$reduced_smoking_illness <- ifelse(  
  is.na(dat$why_reduced_smoking_0),  
  NA,  
  ifelse(grepl("Illness or ill health", dat$why_reduced_smoking_0), 1, 0)  
) 
table(dat$reduced_smoking_illness)

# dummy var for Illness or Financial reasons
dat$reduced_smoking_financial <- ifelse(  
  is.na(dat$why_reduced_smoking_0),  
  NA,  
  ifelse(grepl("Financial reasons", dat$why_reduced_smoking_0), 1, 0)  
) 
table(dat$reduced_smoking_financial)

# dummy var for Illness or Doctor's advice
dat$reduced_smoking_doctor <- ifelse(  
  is.na(dat$why_reduced_smoking_0),  
  NA,  
  ifelse(grepl("Doctor's advice", dat$why_reduced_smoking_0), 1, 0)  
) 
table(dat$reduced_smoking_doctor)

## why stopped smoking

unique(dat$why_stopped_smoking)

# dummy var for health
dat$stopped_smoking_health <- ifelse(  
  is.na(dat$why_stopped_smoking),  
  NA,  
  ifelse(grepl("Health precaution", dat$why_stopped_smoking), 1, 0)  
) 
table(dat$stopped_smoking_health)

# dummy var for Illness or ill health
dat$stopped_smoking_illness <- ifelse(  
  is.na(dat$why_stopped_smoking),  
  NA,  
  ifelse(grepl("Illness or ill health", dat$why_stopped_smoking), 1, 0)  
) 
table(dat$stopped_smoking_illness)

# dummy var for Illness or Financial reasons

dat$stopped_smoking_financial <- ifelse(  
  is.na(dat$why_stopped_smoking),  
  NA,  
  ifelse(grepl("Financial reasons", dat$why_stopped_smoking), 1, 0)  
) 
table(dat$stopped_smoking_financial)

# dummy var for Illness or Doctor's advice

dat$stopped_smoking_doctor <- ifelse(  
  is.na(dat$why_stopped_smoking),  
  NA,  
  ifelse(grepl("Doctor's advice", dat$why_stopped_smoking), 1, 0)  
) 
table(dat$stopped_smoking_doctor)
#remove columns used to make dummy vars
drop_cols(
  dat,
  c(
    "why_reduced_smoking_0",
    "why_stopped_smoking"
  )
)
### Alcohol####

unique(dat$alcohol_status)
unique(dat$alcohol_freq)
unique(dat$alcohol_former)
sort(unique(dat$red_wine_monthly))
sort(unique(dat$white_wine_monthly))
sort(unique(dat$beer_monthly))
sort(unique(dat$spirits_monthly))
sort(unique(dat$fortified_wine_monthly))
sort(unique(dat$other_alcohol_monthly))
sort(unique(dat$red_wine_weekly))
sort(unique(dat$white_wine_weekly))
sort(unique(dat$beer_weekly))
sort(unique(dat$spirits_weekly))
sort(unique(dat$fortified_wine_weekly))
sort(unique(dat$other_alcohol_weekly))
unique(dat$alcohol_with_meals)
unique(dat$alcohol_10yrs)
unique(dat$reason_reducing_alcohol)
unique(dat$reason_stopped_alcohol)
#
sum(is.na(dat$alcohol_status))
dat$alcohol_status <- ifelse(dat$alcohol_status=="Never",0,
                             ifelse(dat$alcohol_status=="Previous",1,2))
dat$alcohol_freq <- ifelse(dat$alcohol_freq=="Never",0,
                           ifelse(dat$alcohol_freq=="Special occasions only",1,
                                  ifelse(dat$alcohol_freq=="One to three times a month",2,
                                         ifelse(dat$alcohol_freq=="Once or twice a week",3,
                                                ifelse(dat$alcohol_freq=="Three or four times a week",4,5)))))
#
sum(is.na(dat$alcohol_former))
dat$alcohol_former <- ifelse(dat$alcohol_former=="No",0,1)
#
dat$red_wine_monthly <- as.numeric(dat$red_wine_monthly)
dat$white_wine_monthly <- as.numeric(dat$white_wine_monthly)
dat$beer_monthly <- as.numeric(dat$beer_monthly)
dat$spirits_monthly <- as.numeric(dat$spirits_monthly)
dat$fortified_wine_monthly <- as.numeric(dat$fortified_wine_monthly)
dat$other_alcohol_monthly <- as.numeric(dat$other_alcohol_monthly)
dat$red_wine_weekly <- as.numeric(dat$red_wine_weekly)
dat$white_wine_weekly <- as.numeric(dat$white_wine_weekly)
dat$beer_weekly <- as.numeric(dat$beer_weekly)
dat$spirits_weekly <- as.numeric(dat$spirits_weekly)
dat$fortified_wine_weekly <- as.numeric(dat$fortified_wine_weekly)
dat$other_alcohol_weekly <- as.numeric(dat$other_alcohol_weekly)
#
table(dat$alcohol_with_meals)
#
dat$alcohol_10yrs <- ifelse(dat$alcohol_10yrs=="Less nowadays",0,
                            ifelse(dat$alcohol_10yrs=="About the same",1,2))
table(dat$reason_reducing_alcohol)
table(dat$reason_stopped_alcohol)
### Diet#####

sort(unique(dat$cooked_veg))
sort(unique(dat$salad_or_raw_veg))
sort(unique(dat$fresh_fruit))
sort(unique(dat$dried_fruit))
sort(unique(dat$oily_fish))
sort(unique(dat$non_oily_fish))
sort(unique(dat$processed_meat))
sort(unique(dat$poultry))
sort(unique(dat$beef))
sort(unique(dat$lamb))
sort(unique(dat$pork))
sort(unique(dat$meat_age_last))
sort(unique(dat$cheese))
sort(unique(dat$milk_type))
sort(unique(dat$spread_type))
sort(unique(dat$spread_type_details))
sort(unique(dat$bread))
sort(unique(dat$bread_type))
sort(unique(dat$cereal))
sort(unique(dat$cereal_type))
sort(unique(dat$salt))
sort(unique(dat$tea))
sort(unique(dat$coffee_type))
sort(unique(dat$hot_drink_temp))
sort(unique(dat$water))
sort(unique(dat$diet_change_5yrs))
sort(unique(dat$diet_variation))
sort(unique(dat$coffee))
# transform into numeric
cols<- c("cooked_veg","salad_or_raw_veg","fresh_fruit","dried_fruit","meat_age_last",
         "bread","cereal","tea","water","coffee")
for (col in cols) {
  dat[[col]][which(dat[[col]] == "Less than one")] <-0.5
  dat[[col]] <- as.numeric(dat[[col]])
}
# transform ordinary vars into continous vars
cols<- c("oily_fish","non_oily_fish","processed_meat","poultry","beef",
         "lamb","pork","cheese")
for (col in cols) {
  dat[[col]]<-ifelse(dat[[col]]=="Never",0,
                     ifelse(dat[[col]]=="Less than once a week",1,
                            ifelse(dat[[col]]=="Once a week",2,
                                   ifelse(dat[[col]]=="2-4 times a week",3,
                                          ifelse(dat[[col]]=="5-6 times a week",4,5)))))
}
#
table(dat$milk_type)
#
table(dat$spread_type)
#
table(dat$spread_type_details)
table(dat$bread_type)
#
dat$cereal <- as.numeric(dat$cereal)
table(dat$cereal_type)

dat$salt <- factor(dat$salt, ordered = FALSE)
dat$salt <- relevel(dat$salt, ref = "Never/rarely")
#
dat$salt<-ifelse(dat$salt=="Never/rarely",0,
                 ifelse(dat$salt=="Sometimes",1,
                        ifelse(dat$salt=="Usually",2,3)))
#
table(dat$coffee_type)
#
dat$hot_drink_temp<-ifelse(dat$hot_drink_temp=="Do not drink hot drinks",0,
                           ifelse(dat$hot_drink_temp=="Warm",1,
                                  ifelse(dat$hot_drink_temp=="Hot",2,3)))
#
table(dat$diet_change_5yrs)
#
dat$diet_variation<-ifelse(dat$diet_variation=="Never/rarely",0,
                           ifelse(dat$diet_variation=="Sometimes",1,2))

### dummy variables

## never eat

unique(dat$never_eat)
# dummy var for Sugar or foods/drinks containing sugar

dat$never_eat_sugar <- ifelse(  
  is.na(dat$never_eat),  
  NA,  
  ifelse(grepl("Sugar or foods/drinks containing sugar", dat$never_eat), 1, 0)  
) 
table(dat$never_eat_sugar)

# dummy var for Eggs or foods containing eggs
dat$never_eat_eggs <- ifelse(  
  is.na(dat$never_eat),  
  NA,  
  ifelse(grepl("Eggs or foods containing eggs", dat$never_eat), 1, 0)  
) 
table(dat$never_eat_eggs)

# dummy var for Wheat products
dat$never_eat_wheat <- ifelse(  
  is.na(dat$never_eat),  
  NA,  
  ifelse(grepl("Wheat products", dat$never_eat), 1, 0)  
) 
table(dat$never_eat_wheat)

# dummy var for Dairy products 

dat$never_eat_dairy <- ifelse(  
  is.na(dat$never_eat),  
  NA,  
  ifelse(grepl("Dairy products", dat$never_eat), 1, 0)  
) 
table(dat$never_eat_dairy)

#remove columns used to make dummy vars
drop_cols(dat, "never_eat")

### Sun exposure#####

sort(unique(dat$summer_outdoors_time))
sort(unique(dat$winter_outdoors_time))
sort(unique(dat$skin_color))
sort(unique(dat$skin_tan_ease))
sort(unique(dat$childhood_sunburn_number))
sort(unique(dat$hair_color))
sort(unique(dat$facial_aging))
sort(unique(dat$sun_protection_use))
sort(unique(dat$solarium_freq))
#
cols<- c("summer_outdoors_time","winter_outdoors_time","childhood_sunburn_number","solarium_freq")
for (col in cols) {
  dat[[col]][which(dat[[col]] == "Less than once a year")] <-0.5
  dat[[col]] <- as.numeric(dat[[col]])
}
#
table(dat$skin_color)
#
dat$skin_tan_ease<-ifelse(dat$skin_tan_ease=="Never tan, only burn",0,
                          ifelse(dat$skin_tan_ease=="Get mildly or occasionally tanned",1,
                                 ifelse(dat$skin_tan_ease=="Get moderately tanned",2,3)))
#
table(dat$hair_color)
#
dat$facial_aging<-ifelse(dat$facial_aging=="Younger than you are",0,
                         ifelse(dat$facial_aging=="About your age",1,2))
#
dat$sun_protection_use<-ifelse(dat$sun_protection_use=="Do not go out in sunshine",0,
                               ifelse(dat$sun_protection_use=="Never/rarely",1,
                                      ifelse(dat$sun_protection_use=="Sometimes",2,
                                             ifelse(dat$sun_protection_use=="Most of the time",3,4))))

### Sex history####
sort(unique(dat$sex_age_first))
sort(unique(dat$number_sex_partners))
sort(unique(dat$same_sex_intercourse))
sort(unique(dat$number_same_sex_partners))
#
dat[
  ,
  never_had_sex := fcase(
    is.na(sex_age_first), NA_integer_,
    sex_age_first == "Never had sex", 1L,
    default = 0L
  )
]

dat[
  sex_age_first == "Never had sex",
  sex_age_first := NA_character_
]

cols <- c("sex_age_first", "number_sex_partners", "number_same_sex_partners")
dat[
  ,
  (cols) := lapply(
    .SD,
    function(x) suppressWarnings(as.numeric(x))
  ),
  .SDcols = cols
]
#

dat$same_sex_intercourse <- ifelse(dat$same_sex_intercourse=="No",0,1)

# remove excluded variables

drop_cols(dat, "sex_questions_answered")

### Physical activity####

sort(unique(dat$above_activity_recommendation))
sort(unique(dat$above_activity_recommendation_incl_walk))
sort(unique(dat$IPAQ_activity_group))
sort(unique(dat$MET_mins_moderate_activity))
sort(unique(dat$MET_mins_vigorous_activity))
sort(unique(dat$MET_mins_walking))
sort(unique(dat$days_activity_sum))
sort(unique(dat$mins_activity_sum))
sort(unique(dat$fast_driving))
sort(unique(dat$heavy_DIY_duration))
sort(unique(dat$light_DIY_duration))
sort(unique(dat$moderate_activity_duration))
sort(unique(dat$other_exercise_duration))
sort(unique(dat$strenuous_sports_duration))#
sort(unique(dat$vigorous_activity_duration))
sort(unique(dat$walks_duration))
sort(unique(dat$pleasure_walks_duration))
sort(unique(dat$heavy_DIY_freq_4wks))
sort(unique(dat$light_DIY_freq_4wks))
sort(unique(dat$other_exercise_freq_4wks))
sort(unique(dat$stair_climbing_freq_4wks))
sort(unique(dat$strenuous_sports_freq_4wks))
sort(unique(dat$pleasure_walks_freq_4wks))
sort(unique(dat$moderate_activity_over_10mins_days))
sort(unique(dat$vigorous_activity_over_10mins_days))
sort(unique(dat$walked_over_10mins_days))
sort(unique(dat$driving_time))
sort(unique(dat$computer_time))
sort(unique(dat$TV_time))
sort(unique(dat$usual_walking_pace))
sort(unique(dat$basal_metabolic_rate))
sort(unique(dat$MET_mins_sum))
# continous vars
cols<-c("MET_mins_moderate_activity","MET_mins_vigorous_activity","MET_mins_walking",
        "days_activity_sum","mins_activity_sum","moderate_activity_duration",
        "vigorous_activity_duration","walks_duration","moderate_activity_over_10mins_days",
        "vigorous_activity_over_10mins_days","driving_time","computer_time","TV_time",
        "MET_mins_sum","basal_metabolic_rate")

for (col in cols){
  dat[[col]][which(dat[[col]] == "Less than an hour a day")] <-0.5
  dat[[col]][which(dat[[col]] == "Unable to walk")] <-0
  dat[[col]] <- as.numeric(dat[[col]])
}
#binary
cols<-c("above_activity_recommendation","above_activity_recommendation_incl_walk")

for (col in cols){
  dat[[col]] <- ifelse(dat[[col]]=="No",0,1)
}
#
dat$IPAQ_activity_group <- ifelse(dat$IPAQ_activity_group =="low",0,
                                  ifelse(dat$IPAQ_activity_group =="moderate",1,2)
                                  
)
#
dat$fast_driving <- ifelse(dat$fast_driving =="Do not drive on the motorway",0,
                           ifelse(dat$fast_driving =="Never/rarely",1,
                                  ifelse(dat$fast_driving =="Sometimes",2,
                                         ifelse(dat$fast_driving =="Often",3,4))))
#
cols<-c("heavy_DIY_duration","light_DIY_duration","other_exercise_duration",
        "strenuous_sports_duration","pleasure_walks_duration")

for (col in cols){
  dat[[col]] <- ifelse(dat[[col]]=="Less than 15 minutes",0,
                       ifelse(dat[[col]]=="Between 15 and 30 minutes",1,
                              ifelse(dat[[col]]=="Between 30 minutes and 1 hour",2, 
                                     ifelse(dat[[col]]=="Between 1 and 1.5 hours",3, 
                                            ifelse(dat[[col]]=="Between 1.5 and 2 hours",4,
                                                   ifelse(dat[[col]]=="Between 2 and 3 hours",5,6))))))
}
#
cols<-c("heavy_DIY_freq_4wks","light_DIY_freq_4wks","other_exercise_freq_4wks",
        "strenuous_sports_freq_4wks","pleasure_walks_freq_4wks")

for (col in cols){
  dat[[col]] <- ifelse(dat[[col]]=="Once in the last 4 weeks",0,
                       ifelse(dat[[col]]=="2-3 times in the last 4 weeks",1,
                              ifelse(dat[[col]]=="Once a week",2, 
                                     ifelse(dat[[col]]=="2-3 times a week",3, 
                                            ifelse(dat[[col]]=="4-5 times a week",4,5)))))
}


#
dat$stair_climbing_freq_4wks <- ifelse(dat$stair_climbing_freq_4wks=="None",0,
                                       ifelse(dat$stair_climbing_freq_4wks=="1-5 times a day",1, 
                                              ifelse(dat$stair_climbing_freq_4wks=="6-10 times a day",2,
                                                     ifelse(dat$stair_climbing_freq_4wks=="11-15 times a day",3,
                                                            ifelse(dat$stair_climbing_freq_4wks=="16-20 times a day",4,5)))))
#
dat$usual_walking_pace[which(dat$usual_walking_pace == "None of the above")] <-NA
dat$usual_walking_pace <- ifelse(dat$usual_walking_pace=="Slow pace",0,
                                 ifelse(dat$usual_walking_pace=="Steady average pace",1, 2))

dat$walked_over_10mins_days[which(dat$walked_over_10mins_days=="Unable to walk")]<-0
dat$walked_over_10mins_days<-as.numeric(dat$walked_over_10mins_days)
### dummy variables

## physical activity type

unique(dat$physical_activity_type)
# dummy var for Light DIY (eg: pruning, watering the lawn)

dat$light_DIY_4wks <- ifelse(  
  is.na(dat$physical_activity_type),  
  NA,  
  ifelse(grepl("Light DIY", dat$physical_activity_type), 1, 0)  
) 
table(dat$light_DIY_4wks)
# dummy var for Walking for pleasure (not as a means of transport) 
dat$pleasure_walks_4wks <- ifelse(  
  is.na(dat$physical_activity_type),  
  NA,  
  ifelse(grepl("Walking for pleasure", dat$physical_activity_type), 1, 0)  
) 
table(dat$pleasure_walks_4wks)


# dummy var for Other exercises (eg: swimming, cycling, keep fit, bowling)
dat$other_exercise_4wks <- ifelse(  
  is.na(dat$physical_activity_type),  
  NA,  
  ifelse(grepl("Other exercises", dat$physical_activity_type), 1, 0)  
) 
table(dat$other_exercise_4wks)

# dummy var for Heavy DIY (eg: weeding, lawn mowing, carpentry, digging)
dat$heavy_DIY_4wks <- ifelse(  
  is.na(dat$physical_activity_type),  
  NA,  
  ifelse(grepl("Heavy DIY", dat$physical_activity_type), 1, 0)  
) 
table(dat$heavy_DIY_4wks)


# dummy var for Strenuous sports 
dat$strenuous_sports_4wks <- ifelse(  
  is.na(dat$physical_activity_type),  
  NA,  
  ifelse(grepl("Strenuous sports", dat$physical_activity_type), 1, 0)  
) 
table(dat$strenuous_sports_4wks)

## transport type

unique(dat$transport_type.0.0)

# dummy var for Car/motor vehicle

dat$car_transport_4wks <- ifelse(  
  is.na(dat$transport_type.0.0),  
  NA,  
  ifelse(grepl("Car/motor vehicle", dat$transport_type.0.0), 1, 0)  
) 
table(dat$car_transport_4wks)

# dummy var for Walk
dat$walk_transport_4wks <- ifelse(  
  is.na(dat$transport_type.0.0),  
  NA,  
  ifelse(grepl("Walk", dat$transport_type.0.0), 1, 0)  
) 
table(dat$walk_transport_4wks)


# dummy var for Public transport
dat$public_transport_4wks <- ifelse(  
  is.na(dat$transport_type.0.0),  
  NA,  
  ifelse(grepl("Public transport", dat$transport_type.0.0), 1, 0)  
) 
table(dat$public_transport_4wks)


# dummy var for Cycle
dat$cycle_transport_4wks <- ifelse(  
  is.na(dat$transport_type.0.0),  
  NA,  
  ifelse(grepl("Cycle", dat$transport_type.0.0), 1, 0)  
) 
table(dat$cycle_transport_4wks)

# remove columns used to make dummy vars
drop_cols(
  dat,
  c(
    "physical_activity_type",
    "transport_type.0.0"
  )
)
### Vision####
sort(unique(dat$visual_acuity_left))
sort(unique(dat$visual_acuity_right))

dat$visual_acuity_left <- as.numeric(dat$visual_acuity_left)
dat$visual_acuity_right <- as.numeric(dat$visual_acuity_right)


### Hearing#####
sort(unique(dat$speech_reception_threshold_left))
sort(unique(dat$speech_reception_threshold_right))

dat$speech_reception_threshold_left <- as.numeric(dat$speech_reception_threshold_left)
dat$speech_reception_threshold_right <- as.numeric(dat$speech_reception_threshold_right)

### Blood pressure####

sort(unique(dat$diastolic_bp1))
sort(unique(dat$systolic_bp1))
sort(unique(dat$pulse_rate1))

dat$diastolic_bp1 <- as.numeric(dat$diastolic_bp1)
dat$systolic_bp1 <- as.numeric(dat$systolic_bp1)
dat$pulse_rate1 <- as.numeric(dat$pulse_rate1)


# remove excluded variables
drop_cols(
  dat,
  c(
    "diastolic_bp_manual",
    "systolic_bp_manual",
    "pulse_rate_manual"
  )
)


### Arterial stiffness####
sort(unique(dat$pulse_wave_notch_position))
sort(unique(dat$pulse_wave_peak_position))
sort(unique(dat$pulse_wave_shoulder_position))
sort(unique(dat$pulse_wave_arterial_stiffness_index))
sort(unique(dat$pulse_wave_peak_to_peak_time))
sort(unique(dat$pulse_wave_reflection_index))

dat$pulse_wave_notch_position <- as.numeric(dat$pulse_wave_notch_position)
dat$pulse_wave_peak_position <- as.numeric(dat$pulse_wave_peak_position)
dat$pulse_wave_shoulder_position <- as.numeric(dat$pulse_wave_shoulder_position)
dat$pulse_wave_arterial_stiffness_index <- as.numeric(dat$pulse_wave_arterial_stiffness_index)
dat$pulse_wave_peak_to_peak_time <- as.numeric(dat$pulse_wave_peak_to_peak_time)
dat$pulse_wave_reflection_index <- as.numeric(dat$pulse_wave_reflection_index)

# remove excluded variables
drop_cols(dat, "pulse_rate_a_stiff")

### Anthropometrics####

sort(unique(dat$waist_circumference))
sort(unique(dat$weight))
sort(unique(dat$BMI))
sort(unique(dat$hip_circumference))
sort(unique(dat$standing_height))
sort(unique(dat$seated_height))
sort(unique(dat$sitting_height))
sort(unique(dat$seating_box_height))#
sort(unique(dat$hand_grip_strength_left))
sort(unique(dat$hand_grip_strength_right))
sort(unique(dat$leg_fat_free_mass_right))
sort(unique(dat$leg_predicted_mass_left))
sort(unique(dat$leg_predicted_mass_right))
sort(unique(dat$arm_fat_percentage_left))
sort(unique(dat$arm_fat_percentage_right))
sort(unique(dat$arm_fat_mass_left))
sort(unique(dat$arm_fat_mass_right))
sort(unique(dat$arm_fat_free_mass_right))
sort(unique(dat$arm_fat_free_mass_left))
sort(unique(dat$arm_predicted_mass_left))
sort(unique(dat$arm_predicted_mass_right))
sort(unique(dat$trunk_fat_percentage))
sort(unique(dat$trunk_fat_mass))
sort(unique(dat$trunk_fat_free_mass))
sort(unique(dat$trunk_predicted_mass))
sort(unique(dat$body_fat_percent))
sort(unique(dat$whole_body_fat_mass))
sort(unique(dat$whole_body_fat_free_mass))
sort(unique(dat$whole_body_water_mass))
sort(unique(dat$leg_fat_percentage_left))
sort(unique(dat$leg_fat_percentage_right))
sort(unique(dat$leg_fat_mass_left))
sort(unique(dat$leg_fat_mass_right))
sort(unique(dat$leg_fat_free_mass_left))
sort(unique(dat$whole_body_impedance))
sort(unique(dat$arm_left_impedance))
sort(unique(dat$arm_right_impedance))
sort(unique(dat$leg_left_impedance))
sort(unique(dat$leg_right_impedance))
sort(unique(dat$basal_metabolic_rate))
sort(unique(dat$MET_mins_sum))

#gsub the cm from seating_box_height
dat$seating_box_height <- gsub("cm", "", dat$seating_box_height)
# get column numbers of IMD vars
names(dat)
anthro_cols <- cols_between(
  dat,
  "hand_grip_strength_left",
  "MET_mins_sum"
)
str(
  dat[
    ,
    ..anthro_cols
  ]
)

# convert all to numeric
dat[
  ,
  (anthro_cols) :=
    lapply(
      .SD,
      function(x) {
        suppressWarnings(
          as.numeric(x)
        )
      }
    ),
  .SDcols = anthro_cols
]
str(dat[, ..anthro_cols])

# remove excluded variables
drop_cols(
  dat,
  c(
    "weight_manual",
    "weight_impedance",
    "BMI_impedance",
    "seating_box_height",
    "whole_body_impedance_manual",
    "arm_left_impedance_manual",
    "arm_right_impedance_manual",
    "leg_left_impedance_manual",
    "leg_right_impedance_manual"
  )
)

### Lung function ####
sort(unique(dat$FVC_best))
sort(unique(dat$FVC_z_score))
sort(unique(dat$FEV1_best))
sort(unique(dat$FEV1_predicted))
sort(unique(dat$FEV1_predicted_percentage))
sort(unique(dat$FEV1_z_score))
sort(unique(dat$FEV1_FVC_ratio_z_score))


dat$FVC_best <- as.numeric(dat$FVC_best)
dat$FVC_z_score <- as.numeric(dat$FVC_z_score)
dat$FEV1_best <- as.numeric(dat$FEV1_best)
dat$FEV1_predicted <- as.numeric(dat$FEV1_predicted)
dat$FEV1_predicted_percentage <- as.numeric(dat$FEV1_predicted_percentage)
dat$FEV1_z_score <- as.numeric(dat$FEV1_z_score)
dat$FEV1_FVC_ratio_z_score <- as.numeric(dat$FEV1_FVC_ratio_z_score)

# remove excluded variables
drop_cols(
  dat,
  c(
    "blow_acceptability_text_0_0",
    "blow_acceptability_text_0_1",
    "blow_acceptability_text_0_2",
    "blow_acceptability_0_0",
    "blow_acceptability_0_1",
    "blow_acceptability_0_2",
    "number_spirometry_measurements",
    "spirometry_QC",
    "blow_ordering_0_0",
    "blow_ordering_0_1",
    "blow_ordering_0_2",
    "blow_result_ranking_0_0",
    "blow_result_ranking_0_1",
    "blow_result_ranking_0_2",
    "blow_data_points_0_0",
    "blow_data_points_0_1",
    "blow_data_points_0_2",
    "FVC_0_0",
    "FVC_0_1",
    "FVC_0_2",
    "FEV1_0_0",
    "FEV1_0_1",
    "FEV1_0_2",
    "peak_expiratory_flow_0_0",
    "peak_expiratory_flow_0_1",
    "peak_expiratory_flow_0_2"
  )
)

### Bone mineral density####

sort(unique(dat$fractured_heel))#
sort(unique(dat$bone_density_measure_foot))#
sort(unique(dat$heel_sound_speed))
sort(unique(dat$ankle_spacing_width))
sort(unique(dat$heel_broadband_attenuation))
sort(unique(dat$heel_QUI))
sort(unique(dat$heel_bone_mineral_density))
sort(unique(dat$heel_bone_mineral_density_t_score))
sort(unique(dat$fractured_heel_left))#
sort(unique(dat$fractured_heel_right))#
sort(unique(dat$ankle_spacing_width_left))
sort(unique(dat$ankle_spacing_width_right))
sort(unique(dat$heel_sound_speed_left))
sort(unique(dat$heel_sound_speed_right))
sort(unique(dat$heel_broadband_attenuation_left))
sort(unique(dat$heel_broadband_attenuation_right))
sort(unique(dat$heel_QUI_left))
sort(unique(dat$heel_QUI_right))
sort(unique(dat$heel_bone_mineral_density_left))
sort(unique(dat$heel_bone_mineral_density_right))
sort(unique(dat$heel_bone_mineral_density_t_score_left))
sort(unique(dat$heel_bone_mineral_density_t_score_right))

# binary vars
vars<- c("fractured_heel")

for (col in vars){
  dat[[col]][which(dat[[col]] == "Unsure")] <- NA  
  dat[[col]]<- ifelse(dat[[col]]=="No",0,1)
}
dat$bone_density_measure_foot<- ifelse(dat$bone_density_measure_foot=="left",0,1)

vars<- c("fractured_heel_left","fractured_heel_right")

for (col in vars){
  dat[[col]][which(dat[[col]] == "unsure")] <- NA  
  dat[[col]]<- ifelse(dat[[col]]=="no",0,1)
}
# continous vars
dat$heel_sound_speed <- as.numeric(dat$heel_sound_speed)
dat$ankle_spacing_width <- as.numeric(dat$ankle_spacing_width)
dat$heel_broadband_attenuation <- as.numeric(dat$heel_broadband_attenuation)
dat$heel_QUI <- as.numeric(dat$heel_QUI)
dat$heel_bone_mineral_density <- as.numeric(dat$heel_bone_mineral_density)
dat$heel_bone_mineral_density_t_score <- as.numeric(dat$heel_bone_mineral_density_t_score)
dat$ankle_spacing_width_left <- as.numeric(dat$ankle_spacing_width_left)
dat$ankle_spacing_width_right <- as.numeric(dat$ankle_spacing_width_right)
dat$heel_sound_speed_left <- as.numeric(dat$heel_sound_speed_left)
dat$heel_sound_speed_right <- as.numeric(dat$heel_sound_speed_right)
dat$heel_broadband_attenuation_left <- as.numeric(dat$heel_broadband_attenuation_left)
dat$heel_broadband_attenuation_right <- as.numeric(dat$heel_broadband_attenuation_right)
dat$heel_QUI_left <- as.numeric(dat$heel_QUI_left)
dat$heel_QUI_right <- as.numeric(dat$heel_QUI_right)
dat$heel_bone_mineral_density_left <- as.numeric(dat$heel_bone_mineral_density_left)
dat$heel_bone_mineral_density_right <- as.numeric(dat$heel_bone_mineral_density_right)
dat$heel_bone_mineral_density_t_score_left <- as.numeric(dat$heel_bone_mineral_density_t_score_left)
dat$heel_bone_mineral_density_t_score_right <- as.numeric(dat$heel_bone_mineral_density_t_score_right)


# remove excluded variables
drop_cols(
  dat,
  c(
    "heel_sound_speed_manual",
    "heel_broadband_attenuation_manual",
    "heel_QUI_manual",
    "heel_bone_mineral_density_manual",
    "heel_bone_ultrasound_t_score_manual",
    "heel_sound_speed_manual_left",
    "heel_sound_speed_manual_right",
    "heel_broadband_attenuation_manual_left",
    "heel_broadband_attenuation_manual_right",
    "heel_QUI_manual_left",
    "heel_QUI_manual_right",
    "heel_bone_mineral_density_manual_left",
    "heel_bone_mineral_density_manual_right",
    "heel_bone_mineral_density_t_score_manual_left",
    "heel_bone_mineral_density_t_score_manual_right"
  )
)

### Material deprivation ####
imd_country_cols <- grep(
  "^IMD_.*_(England|Scotland|Wales)$",
  names(dat),
  value = TRUE
)

# 转为 numeric
dat[
  ,
  (imd_country_cols) :=
    lapply(
      .SD,
      function(x) {
        suppressWarnings(
          as.numeric(x)
        )
      }
    ),
  .SDcols = imd_country_cols
]


# ------------------------------------------------------------
# Combine country-specific variables
# Each participant should normally have data from only one
# country-specific source.
# ------------------------------------------------------------

dat[
  ,
  IMD_services :=
    fcoalesce(
      IMD_services_Scotland,
      IMD_services_Wales
    )
]

dat[
  ,
  IMD_crime :=
    fcoalesce(
      IMD_crime_England,
      IMD_crime_Scotland
    )
]

dat[
  ,
  IMD_education :=
    fcoalesce(
      IMD_education_England,
      IMD_education_Scotland,
      IMD_education_Wales
    )
]

dat[
  ,
  IMD_employment :=
    fcoalesce(
      IMD_employment_England,
      IMD_employment_Scotland,
      IMD_employment_Wales
    )
]

dat[
  ,
  IMD_health :=
    fcoalesce(
      IMD_health_England,
      IMD_health_Scotland,
      IMD_health_Wales
    )
]

dat[
  ,
  IMD_housing :=
    fcoalesce(
      IMD_housing_England,
      IMD_housing_Scotland,
      IMD_housing_Wales
    )
]

dat[
  ,
  IMD_income :=
    fcoalesce(
      IMD_income_England,
      IMD_income_Scotland,
      IMD_income_Wales
    )
]

dat[
  ,
  IMD_total :=
    fcoalesce(
      IMD_total_England,
      IMD_total_Scotland,
      IMD_total_Wales
    )
]
cat(
  "\nParticipants with >1 non-missing IMD_total country value:",
  dat[
    ,
    sum(
      rowSums(
        !is.na(
          cbind(
            IMD_total_England,
            IMD_total_Scotland,
            IMD_total_Wales
          )
        )
      ) > 1
    )
  ],
  "\n"
)

# Remove original country-specific fields after harmonisation.
# The three single-country-only subindices below are also removed because
# their missingness is structural by country and they cannot form a common UK-wide feature.
imd_old_cols <- c(
  "IMD_services_Scotland",
  "IMD_services_Wales",
  "IMD_crime_England",
  "IMD_crime_Scotland",
  "IMD_education_England",
  "IMD_education_Scotland",
  "IMD_education_Wales",
  "IMD_employment_England",
  "IMD_employment_Scotland",
  "IMD_employment_Wales",
  "IMD_health_England",
  "IMD_health_Scotland",
  "IMD_health_Wales",
  "IMD_housing_England",
  "IMD_housing_Scotland",
  "IMD_housing_Wales",
  "IMD_income_England",
  "IMD_income_Scotland",
  "IMD_income_Wales",
  "IMD_total_England",
  "IMD_total_Scotland",
  "IMD_total_Wales",
  "IMD_community_safety_Wales",
  "IMD_living_environ_England",
  "IMD_physical_environ_Wales"
)

drop_cols(dat, imd_old_cols)

### physical environment ####
dat[
  ,
  close_to_major_road := fcase(
    is.na(close_to_major_road), NA_real_,
    close_to_major_road == "No", 0,
    close_to_major_road == "Yes", 1,
    default = NA_real_
  )
]

# Keep the original preprocessing rule: pollution variables are stored at x10
# in this source workflow, so convert them back by dividing by 10.
pol_cols <- cols_between(
  dat,
  "NO2_2005",
  "PM2.5_to_10_2010"
)

dat[
  ,
  (pol_cols) := lapply(
    .SD,
    function(x) suppressWarnings(as.numeric(x)) / 10
  ),
  .SDcols = pol_cols
]

# merge repeated annual measures into unique variables
dat[
  ,
  NO2 := rowMeans(
    .SD,
    na.rm = TRUE
  ),
  .SDcols = c(
    "NO2_2005",
    "NO2_2006",
    "NO2_2007",
    "NO2_2010"
  )
]
dat[is.nan(NO2), NO2 := NA_real_]

dat[
  ,
  PM10 := rowMeans(
    .SD,
    na.rm = TRUE
  ),
  .SDcols = c(
    "PM10_2007",
    "PM10_2010"
  )
]
dat[is.nan(PM10), PM10 := NA_real_]

dat[
  ,
  `:=`(
    NO = NO_2010,
    PM2.5_absorbance = PM2.5_absorbance_2010,
    PM2.5 = PM2.5_2010,
    PM2.5_to_10 = PM2.5_to_10_2010
  )
]

drop_cols(
  dat,
  c(
    "NO2_2005",
    "NO2_2006",
    "NO2_2007",
    "NO2_2010",
    "NO_2010",
    "PM10_2007",
    "PM10_2010",
    "PM2.5_absorbance_2010",
    "PM2.5_2010",
    "PM2.5_to_10_2010"
  )
)

#check if numeric
enviro_numeric_cols <- cols_between(
  dat,
  "inverse_distance_nearest_major_road",
  "distance_to_coast"
)

str(dat[, ..enviro_numeric_cols])

dat[
  ,
  (enviro_numeric_cols) := lapply(
    .SD,
    function(x) suppressWarnings(as.numeric(x))
  ),
  .SDcols = enviro_numeric_cols
]

str(dat[, ..enviro_numeric_cols])
table(dat$close_to_major_road)

# recode factor / numeric
table(dat$home_population_density)
sort(unique(dat$home_population_density))
dat[
  home_population_density == "Postcode not linkable",
  home_population_density := NA_character_
]
dat[
  ,
  home_population_density := suppressWarnings(as.numeric(home_population_density))
]


# ### Cognition function####
# sort(unique(dat$identify_matches_mean_time))
# sort(unique(dat$maximum_digits_remembered))
# sort(unique(dat$fluid_intelligence))
# sort(unique(dat$prospective_memory))
# sort(unique(dat$incorrect_matches_round1))
# sort(unique(dat$incorrect_matches_round2))
# sort(unique(dat$incorrect_matches_round3))
# 
# dat$identify_matches_mean_time <- as.numeric(dat$identify_matches_mean_time)
# 
# dat$maximum_digits_remembered <- as.numeric(dat$maximum_digits_remembered)
# dat$maximum_digits_remembered[which(dat$maximum_digits_remembered == "-1")] <- NA 
# 
# dat$fluid_intelligence <- as.numeric(dat$fluid_intelligence)
# 
# dat$prospective_memory <- factor(dat$prospective_memory, 
#                                  ordered = FALSE)
# 
# dat$prospective_memory <- relevel(dat$prospective_memory, 
#                                   ref = "Instruction not recalled, either skipped or incorrect")
# 
# dat$incorrect_matches_round1 <- as.numeric(dat$incorrect_matches_round1)
# dat$incorrect_matches_round2 <- as.numeric(dat$incorrect_matches_round2)
# dat$incorrect_matches_round3 <- as.numeric(dat$incorrect_matches_round3)
# dat <- dat %>%  
#   mutate(incorrect_matches_round = rowMeans(select(., incorrect_matches_round1, incorrect_matches_round2, incorrect_matches_round3), na.rm = TRUE)) %>%  
#   select(-incorrect_matches_round1, -incorrect_matches_round2, -incorrect_matches_round3)  
# 



### Blood biomarkers ####
blood_cols <- cols_between(
  dat,
  "alanine_aminotransferase",
  "vitamin_D"
)

#check if numeric
str(dat[, ..blood_cols])


### Blood cell count####
blood_count_cols <- cols_between(
  dat,
  "basophill_count",
  "leukocyte_count"
)

#check if numeric
str(dat[, ..blood_count_cols])


### Urine biomarkers #####
urine_cols <- cols_between(
  dat,
  "creatinine_urine",
  "sodium_urine"
)

#check if numeric
str(dat[, ..urine_cols])

# spilt into 2 documents####
names(dat)

env_cols <- unique(
  c(
    cols_between(dat, "eid", "townsend_deprivation_index"),
    cols_between(dat, "IMD_services", "IMD_total"),
    cols_between(dat, "close_to_major_road", "home_population_density"),
    cols_between(dat, "NO2", "PM2.5_to_10"),
    cols_between(dat, "self_illness_injury_assault", "cycle_transport_4wks")
  )
)

enviro_exposure <- copy(dat[, ..env_cols])

blood_urine_cols <- unique(
  c(
    "eid",
    cols_between(dat, "alanine_aminotransferase", "sodium_urine")
  )
)
blood_urine_exposure <- copy(dat[, ..blood_urine_cols])

## recoding the recruitment_centre####
enviro_exposure[
  ,
  recruitment_centre := fcase(
    is.na(recruitment_centre), NA_character_,
    recruitment_centre %chin% c("Cardiff", "Wrexham", "Swansea"), "Wales",
    recruitment_centre %chin% c("Edinburgh", "Glasgow"), "Scotland",
    default = "England"
  )
]

# Keep recruitment information separately for disease cohort construction
recruitment_info <- copy(
  enviro_exposure[
    ,
    .(
      eid,
      recruitment_date,
      recruitment_centre
    )
  ]
)

enviro_exposure_no_date <- copy(enviro_exposure)
enviro_exposure_no_date[, recruitment_date := NULL]

# ============================================================
# Final harmonisation of high-cardinality / redundant variables
# ============================================================

# ------------------------------------------------------------
# Ethnicity
# ------------------------------------------------------------
enviro_exposure_no_date[
  ,
  ethnicity_group := fcase(
    is.na(ethnicity), NA_character_,
    ethnicity %chin% c(
      "British",
      "Irish",
      "Any other white background"
    ), "White",
    ethnicity %chin% c(
      "Indian",
      "Pakistani",
      "Bangladeshi"
    ), "South_Asian",
    ethnicity %chin% c(
      "African",
      "Caribbean",
      "Black or Black British",
      "Any other Black background"
    ), "Black",
    ethnicity == "Chinese", "Chinese",
    grepl("mixed", ethnicity, ignore.case = TRUE), "Mixed",
    default = "Other"
  )
]

# ------------------------------------------------------------
# Accommodation
# ------------------------------------------------------------
enviro_exposure_no_date[
  ,
  accommodation_type := fcase(
    is.na(accommodation_type), NA_character_,
    accommodation_type == "A house or bungalow", "House",
    default = "Non_house"
  )
]

# ------------------------------------------------------------
# Housing tenure
# ------------------------------------------------------------
enviro_exposure_no_date[
  ,
  own_or_rent := fcase(
    is.na(own_or_rent), NA_character_,
    own_or_rent %chin% c(
      "Own outright",
      "Own outright (by you or someone in your household)"
    ), "Own_outright",
    own_or_rent %chin% c(
      "Own with mortgage",
      "Own with a mortgage"
    ), "Mortgage",
    own_or_rent %chin% c(
      "Rent from local authority",
      "Rent privately",
      "Rent - from local authority, local council, housing association",
      "Rent - from private landlord or letting agency"
    ), "Rent",
    default = "Other"
  )
]

# ------------------------------------------------------------
# Country of birth
# Republic of Ireland is Non_UK when the category is explicitly named "UK"
# ------------------------------------------------------------
enviro_exposure_no_date[
  ,
  country_birth := fcase(
    is.na(country_birth), NA_character_,
    country_birth %chin% c(
      "England",
      "Scotland",
      "Wales",
      "Northern Ireland"
    ), "UK",
    default = "Non_UK"
  )
]


# ============================================================
# Bilateral summary variables
# ============================================================

enviro_exposure_no_date[
  ,
  hand_grip_strength_mean := row_mean2(
    hand_grip_strength_left,
    hand_grip_strength_right
  )
]

enviro_exposure_no_date[
  ,
  visual_acuity_mean := row_mean2(
    visual_acuity_left,
    visual_acuity_right
  )
]

enviro_exposure_no_date[
  ,
  hearing_threshold_mean := row_mean2(
    speech_reception_threshold_left,
    speech_reception_threshold_right
  )
]


# ============================================================
# Remove original / redundant variables
# ============================================================

vars_to_remove_final <- c(
  # replaced by ethnicity_group
  "ethnicity",
  
  # redundant / misleading country-of-birth derivative
  "country_birth_non_UK",
  
  # redundant with actual birth_weight
  "birth_weight_known",
  
  # redundant with recruitment_age
  "birth_year",
  
  # coding-quality variable rather than substantive predictor
  "Cascot_confidence_score",
  
  # replaced by bilateral means
  "hand_grip_strength_left",
  "hand_grip_strength_right",
  "visual_acuity_left",
  "visual_acuity_right",
  "speech_reception_threshold_left",
  "speech_reception_threshold_right"
)

drop_cols(
  enviro_exposure_no_date,
  vars_to_remove_final
)


# ============================================================
# Final quick check
# ============================================================

cat(
  "\nFinal environmental/phenotypic variables:",
  ncol(enviro_exposure_no_date) - 1,
  "\n"
)

cat("\nEthnicity:\n")
print(
  table(
    enviro_exposure_no_date$ethnicity_group,
    useNA = "ifany"
  )
)

cat("\nAccommodation:\n")
print(
  table(
    enviro_exposure_no_date$accommodation_type,
    useNA = "ifany"
  )
)

cat("\nHousing tenure:\n")
print(
  table(
    enviro_exposure_no_date$own_or_rent,
    useNA = "ifany"
  )
)

cat("\nCountry of birth:\n")
print(
  table(
    enviro_exposure_no_date$country_birth,
    useNA = "ifany"
  )
)

cat(
  "\nObjects prepared:",
  "\n  enviro_exposure:",
  nrow(enviro_exposure), "rows x", ncol(enviro_exposure), "cols",
  "\n  enviro_exposure_no_date:",
  nrow(enviro_exposure_no_date), "rows x", ncol(enviro_exposure_no_date), "cols",
  "\n  blood_urine_exposure:",
  nrow(blood_urine_exposure), "rows x", ncol(blood_urine_exposure), "cols",
  "\n  recruitment_info:",
  nrow(recruitment_info), "rows x", ncol(recruitment_info), "cols",
  "\n"
)

names(enviro_exposure_no_date)
## ============================================================
## Final exclusion of variables with high risk of outcome leakage####
## 原则：
## 仅排除接近疾病诊断、疾病特异治疗或疾病筛查问卷的变量
## 一般健康状态、普通症状、功能状态和医疗利用不再排除
## ============================================================

vars_to_remove_environment <- unique(
  c(
    
    # ========================================================
    # 1. Disease-specific treatment / procedures
    # ========================================================
    "diabetes_insulin_1yr",
    "blood_pressure_meds",
    "insulin",
    
    "leg_artery_surgery",
    "leg_surgery",
    "pace_maker",
    
    
    # ========================================================
    # 2. Explicit eye-disease indicators
    # ========================================================
    "diabetes_eye",
    "glaucoma_eye",
    "cataract_eye",
    "macular_degeneration_eye",
    
    "diabetes_eye_age",
    "glaucoma_age",
    "cataract_age",
    "macular_degeneration_age",
    
    "eye_glaucoma",
    "eye_macular_degeneration",
    "eye_diabetes_eye",
    "eye_cataract",
    
    
    # ========================================================
    # 3. Angina-like questionnaire
    # Keep general chest_pain
    # ========================================================
    "chest_pain_walking_normal",
    "chest_pain_ceases_still",
    "chest_pain_walking_uphill",
    
    
    # ========================================================
    # 4. Claudication / PAD-like questionnaire
    # ========================================================
    "leg_pain_walking",
    "leg_pain_standing",
    "leg_pain_calves",
    "leg_pain_walking_uphill",
    "leg_pain_walking_normal",
    "leg_pain_walking_disappears",
    "leg_pain_walking_action",
    "leg_pain_walking_standing",
    
    
    # ========================================================
    # 5. Mental-disease / episode proxies
    # Keep general psychological traits
    # ========================================================
    "bipolar_MDD",
    
    "doctor_visit_mental_health",
    "psychiatrist_visit_mental_health",
    
    "depressed_whole_week",
    "depression_duration",
    "depression_episodes",
    
    "unenthusiastic_whole_week",
    "unenthusiastic_duration",
    "unenthusiastic_episodes",
    
    "manic",
    "irritable",
    "manic_duration",
    "manic_episodes",
    
    "manic_symptoms_creative",
    "manic_symptoms_active",
    "manic_symptoms_less_sleep",
    "manic_symptoms_talkative",
    "manic_symptoms_all"
  )
)

drop_cols(
  enviro_exposure_no_date,
  vars_to_remove_environment
)
names(enviro_exposure_no_date)

##export####
fwrite(enviro_exposure_no_date,'/lustre/home/2411110208/UK_biobank/cov_data/enviro_exposure_no_date.csv',row.names = F)
saveRDS(enviro_exposure_no_date,'/lustre/home/2411110208/UK_biobank/cov_data/enviro_exposure_no_date.rds')
fwrite(blood_urine_exposure,'/lustre/home/2411110208/UK_biobank/cov_data/blood_urine_exposure.csv',row.names = F)
saveRDS(blood_urine_exposure,'/lustre/home/2411110208/UK_biobank/cov_data/blood_urine_exposure.rds')
fwrite(recruitment_info,'/lustre/home/2411110208/UK_biobank/cov_data/recruitment_info.csv',row.names = F)
saveRDS(recruitment_info,'/lustre/home/2411110208/UK_biobank/cov_data/recruitment_info.rds')
#查看各维度变量数####
coding_file <- "/lustre/home/2411110208/UK_biobank/UKB变量coding.xlsx"

ukb_coding <- read_excel(
  coding_file,
  sheet = "Python_env_manifest"
)

names(ukb_coding)


# ============================================================
# 2. 当前 recoding 后实际剩余的环境/表型变量
# ============================================================

current_env_vars <- data.frame(
  Variable = names(enviro_exposure_no_date)
)


# ============================================================
# 3. left_join 到变量字典
# ============================================================

env_var_check <- current_env_vars %>%
  left_join(
    ukb_coding,
    by = "Variable"
  )

dim(env_var_check)

dimension_detail <- env_var_check %>%
  filter(
    `Stage1 candidate` == "Yes"
  ) %>%
  count(
    `Analysis big dimension`,
    `Small dimension`,
    name = "N_variables"
  ) %>%
  arrange(
    `Analysis big dimension`,
    desc(N_variables)
  )



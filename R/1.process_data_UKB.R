library(dplyr)
library(data.table)
#loading and merge data
folder_path <- "/lustre/home/2311110213/UKB_YiFan/cov"  
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)  
# 
data_list <- lapply(csv_files, function(file) {  
  fread(file, header = T)  
})  

UKB_data <- Reduce(function(x, y) {  
  merge(x, y, by = "eid", all = TRUE)  
}, data_list)  

#range the variable
all_cols <- colnames(UKB_data)
write.csv(data.frame(variable = colnames(UKB_data)), "/lustre/home/2311110213/UKB_YiFan/UKB_variable_names.csv", row.names = FALSE)
# 
other_cols <- sort(all_cols[all_cols != "eid"])
# 
UKB_data <- UKB_data[, c("eid", other_cols), with = FALSE]
###Early life factors####
elf<-subset(UKB_data,select = c("eid","p1647_i0","p1677_i0","p1687_i0","p1697_i0","p1707_i0",
                                "p1767_i0","p1777_i0","p1787_i0","p20022_i0","p120_i0",
                                "p20115_i0","p20489","p20488","p20487",
                                "p20490","p20491"))
colnames(elf)
old_cols <- colnames(elf)
elf <- 
  rename(elf,
         eid = eid,    
         country_birth = p1647_i0,
         breastfed = p1677_i0,
         body_size_10yrs_old = p1687_i0,
         height_10yrs_old = p1697_i0,
         handedness = p1707_i0,
         adopted = p1767_i0,
         part_multiple_birth = p1777_i0,
         maternal_smoking = p1787_i0,
         birth_weight = p20022_i0,
         birth_weight_known = p120_i0,
         country_birth_non_UK = p20115_i0,
         childhood_love =p20489,
         childhood_physical_abuse = p20488,
         childhood_hate = p20487,
         childhood_sex_abuse = p20490,
         childhood_doctor = p20491
  )

new_cols <- colnames(elf)
column_key_elf <- as.data.frame(cbind(old_cols, new_cols))

save(elf, 
     column_key_elf,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_elf.RData")
### mental_health_Adult stressful life events (SLE)####
mental_health_sle<-subset(UKB_data,select = c("eid","p6145_i0"))

old_cols <- colnames(mental_health_sle)

mental_health_sle <- 
  rename(mental_health_sle,
         eid = eid,    
         sle = p6145_i0
  )

new_cols <- colnames(mental_health_sle)
column_key_sle <- as.data.frame(cbind(old_cols, new_cols))

save(mental_health_sle,
     column_key_sle,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_sle.RData")

### mental_health_Social support####
mental_health_soc <-subset(UKB_data,select = c("eid","p1031_i0","p6160_i0","p2110_i0",
                                               "p2020_i0"))
old_cols <- colnames(mental_health_soc)

mental_health_soc <- 
  rename(mental_health_soc,
         eid = eid,    
         family_visit_freq = p1031_i0,
         social_activity = p6160_i0,
         confide_freq = p2110_i0,
         loneliness = p2020_i0
  )

new_cols <- colnames(mental_health_soc)
column_key_soc <- as.data.frame(cbind(old_cols, new_cols))

save(mental_health_soc, 
     column_key_soc,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_social.RData")

### mental_health####
mental_health<-subset(UKB_data,select = c("eid","p20126_i0","p20122_i0","p20124_i0",
                                          "p20125_i0","p20123_i0","p20127_i0",
                                          "p1920_i0","p1930_i0","p1940_i0","p1950_i0",
                                          "p1960_i0","p1970_i0","p1980_i0","p1990_i0",
                                          "p2000_i0","p2010_i0","p2030_i0","p2040_i0",
                                          "p2050_i0","p2060_i0","p2070_i0","p2080_i0",
                                          "p2090_i0","p2100_i0","p4598_i0","p4609_i0",
                                          "p4620_i0","p4631_i0","p5375_i0","p5386_i0",
                                          "p4642_i0","p4653_i0","p6156_i0.x","p5663_i0",
                                          "p5674_i0","p4526_i0","p4537_i0","p4548_i0",
                                          "p4559_i0","p4570_i0","p4581_i0"))


old_cols <- colnames(mental_health)

mental_health <-
  rename(mental_health,
         eid = eid,
         bipolar_MDD = p20126_i0,
         bipolar = p20122_i0,
         MDD_moderate = p20124_i0,
         MDD_severe = p20125_i0,
         MDD_single_ep = p20123_i0,
         neuroticism = p20127_i0,
         mood_swings = p1920_i0,
         miserableness = p1930_i0,
         irritability = p1940_i0,
         sensitivity = p1950_i0,
         fed_up_feelings = p1960_i0,
         nervous = p1970_i0,
         worrier = p1980_i0,
         tense = p1990_i0,
         worry_embarassment = p2000_i0,
         nerves = p2010_i0,
         guilty_feelings = p2030_i0,
         risk_taking = p2040_i0,
         depressed_mood_freq = p2050_i0,
         unenthusiasm_freq = p2060_i0,
         tenseness_freq = p2070_i0,
         tiredness_freq = p2080_i0,
         doctor_visit_mental_health = p2090_i0,
         psychiatrist_visit_mental_health = p2100_i0,
         depressed_whole_week = p4598_i0,
         depression_duration = p4609_i0,
         depression_episodes = p4620_i0,
         unenthusiastic_whole_week = p4631_i0,
         unenthusiastic_duration = p5375_i0,
         unenthusiastic_episodes = p5386_i0,
         manic = p4642_i0,
         irritable = p4653_i0,
         manic_symptoms = p6156_i0.x,
         manic_duration = p5663_i0,
         manic_episodes = p5674_i0,
         happiness = p4526_i0,
         job_satisfaction = p4537_i0,
         health_satisfaction = p4548_i0,
         family_satisfaction = p4559_i0,
         friends_satisfaction = p4570_i0,
         financial_satisfaction = p4581_i0,
  )

new_cols <- colnames(mental_health)
column_key_mental_health <- as.data.frame(cbind(old_cols, new_cols))

save(mental_health,
     column_key_mental_health,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_mental_health.RData") 
### Sociodemographics####
socio_dem<-subset(UKB_data,select = c("eid","p670_i0","p680_i0","p6139_i0","p6140_i0",
                                      "p699_i0","p709_i0","p6141_i0","p728_i0","p738_i0",
                                      "p6138_i0","p845_i0","p20119_i0","p796_i0","p6142_i0",
                                      "p757_i0","p767_i0","p777_i0","p6143_i0","p806_i0",
                                      "p816_i0","p826_i0","p3426_i0","p4825_i0","p3659_i0",
                                      "p21000_i0","p6146_i0","p4674_i0","p21022","p34",
                                      "p31","p54_i0","p53_i0.x","p20121_i0"))
old_cols <- colnames(socio_dem)

socio_dem <- 
  rename(socio_dem,
         eid = eid,    
         accommodation_type = p670_i0,
         own_or_rent = p680_i0,
         gas_fuel_heat.0.0 = p6139_i0,
         heating_type.0.0 = p6140_i0,
         years_at_address = p699_i0,
         hshld_number = p709_i0,
         hshld_relations.0.0 = p6141_i0,
         hshld_vehicles = p728_i0,
         hshld_income = p738_i0,
         education_years.0.0 = p6138_i0,
         education_age_complete = p845_i0,
         employment_corrected = p20119_i0,
         employment_distance = p796_i0,
         employment.0.0 = p6142_i0,
         employment_years = p757_i0,
         employment_hours = p767_i0,
         employment_travel_freq = p777_i0,
         commute_transport.0.0 = p6143_i0,
         employment_standing = p806_i0, 
         employment_heavy_manual = p816_i0,
         shift_work = p826_i0,
         night_shift_work = p3426_i0,
         noisy_workplace = p4825_i0,
         immigration_year = p3659_i0,
         ethnicity = p21000_i0,
         public_assistance.0.0 = p6146_i0,
         private_healthcare = p4674_i0,
         recruitment_age = p21022,
         birth_year = p34,
         sex = p31,
         recruitment_centre = p54_i0,
         recruitment_date = p53_i0.x,
         Cascot_confidence_score = p20121_i0
  )
new_cols <- colnames(socio_dem)
column_key_socio_dem <- as.data.frame(cbind(old_cols, new_cols))

save(socio_dem, 
     column_key_socio_dem,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_sociodemographics.RData")  
### family health history####
fam_health <- subset(UKB_data,select = c("eid","p20112_i0","p20113_i0","p20114_i0","p20107_i0",
                                         "p20110_i0","p20111_i0","p4501_i0","p1797_i0","p3912_i0",
                                         "p2946_i0","p1807_i0","p1835_i0","p3942_i0","p1845_i0",
                                         "p3526_i0","p1873_i0","p3972_i0","p1883_i0","p3982_i0",
                                         "p5057_i0"))
old_cols <- colnames(fam_health)

fam_health <- 
  rename(fam_health,
         eid = eid,    
         adopted_father_illness = p20112_i0,
         
         adopted_mother_illness.0.0 = p20113_i0,
         
         adopted_sibling_illness.0.0 = p20114_i0,
         
         father_illness.0.0 = p20107_i0,
         
         mother_illness.0.0 = p20110_i0,
         
         sibling_illness.0.0 = p20111_i0,
         
         death_close_family = p4501_i0,
         father_alive = p1797_i0,
         adopted_father_alive = p3912_i0,
         father_age = p2946_i0,
         father_age_death = p1807_i0,
         mother_alive = p1835_i0,
         adopted_mother_alive = p3942_i0,
         mother_age = p1845_i0,
         mother_age_death = p3526_i0,
         number_brothers = p1873_i0,
         number_adopted_brothers = p3972_i0,
         number_sisters = p1883_i0,
         number_adopted_sisters = p3982_i0,
         number_older_siblings = p5057_i0
  )

new_cols <- colnames(fam_health)
column_key_fam_health <- as.data.frame(cbind(old_cols, new_cols))

save(fam_health,
     column_key_fam_health,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_family_health_history.RData")

### Sex-related characteristics-male reproduction ####
Sex_related_male_repro <- subset(UKB_data,select = c("eid","p2375_i0","p2385_i0","p2395_i0",
                                                     "p2405_i0"))
old_cols <- colnames(Sex_related_male_repro)

Sex_related_male_repro <-
  rename(Sex_related_male_repro,
         eid = eid,
         age_facial_hair = p2375_i0,
         age_voice = p2385_i0,
         balding = p2395_i0,
         number_children = p2405_i0
  )

new_cols <- colnames(Sex_related_male_repro)
column_key_male_repro <- as.data.frame(cbind(old_cols, new_cols))

save(Sex_related_male_repro, 
     column_key_male_repro,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_male_reproduction.RData")   

### Sex-related characteristics-female reproduction####
Sex_related_female_repro <- subset(UKB_data,select = c("eid","p2674_i0","p2684_i0","p2694_i0","p2704_i0",
                                                       "p2714_i0","p2724_i0","p3700_i0","p3710_i0","p3720_i0",
                                                       "p2734_i0","p2744_i0","p3872_i0","p2754_i0","p2764_i0",
                                                       "p2774_i0","p3829_i0","p3839_i0","p3849_i0","p2784_i0",
                                                       "p2794_i0","p2804_i0","p2814_i0","p3536_i0","p3546_i0",
                                                       "p3591_i0","p2824_i0","p2834_i0","p3882_i0","p3140_i0",
                                                       "p3581_i0"))

old_cols <- colnames(Sex_related_female_repro)

Sex_related_female_repro <-   
  rename(Sex_related_female_repro,  
         eid = eid,  # 保持不变  
         mammogram = p2674_i0,  
         mammogram_years_since = p2684_i0,  
         cervical_smear = p2694_i0,  
         cervical_smear_years_since = p2704_i0,  
         menarche_age = p2714_i0,  
         menopause = p2724_i0,  
         period_time_since = p3700_i0,  
         menstrual_cycle_length = p3710_i0,  
         menstruating_today = p3720_i0,  
         number_births = p2734_i0,  
         first_child_birth_weight = p2744_i0,  
         birth_age = p3872_i0,  
         first_birth_age = p2754_i0,  
         last_birth_age = p2764_i0,  
         stillbirth = p2774_i0,  
         number_stillbirths = p3829_i0,  
         number_miscarriages = p3839_i0,  
         number_pregnancy_terminations = p3849_i0,  
         oral_contraceptive = p2784_i0,  
         oral_contraceptive_age_first = p2794_i0,  
         oral_contraceptive_age_last = p2804_i0,  
         HRT = p2814_i0,  
         HRT_age_first = p3536_i0,  
         HRT_age_last = p3546_i0,  
         hysterectomy = p3591_i0,  
         hysterectomy_age = p2824_i0,  
         bilateral_oophorectomy = p2834_i0,  
         bilateral_oophorectomy_age = p3882_i0,  
         pregnant = p3140_i0,
         menopause_age = p3581_i0,
  )  

new_cols <- colnames(Sex_related_female_repro)
column_key_female_repro <- as.data.frame(cbind(old_cols, new_cols))

save(Sex_related_female_repro, 
     column_key_female_repro,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_female_reproduction.RData")  

###Lifestyle and environmental exp_physical activity####
Lifestyle_and_environmental_exp_phys_act <- subset(UKB_data,select = c("eid","p22035_i0",
                                                                       "p22036_i0","p22032_i0","p22038_i0",
                                                                       "p22039_i0","p22037_i0","p22033_i0","p22034_i0","p1100_i0",
                                                                       "p2634_i0","p1001_i0","p914_i0","p874_i0","p981_i0",
                                                                       "p2624_i0","p1011_i0","p3637_i0","p943_i0","p991_i0",
                                                                       "p971_i0","p884_i0","p864_i0","p1090_i0","p1080_i0",
                                                                       "p1070_i0","p6164_i0","p6162_i0","p924_i0","p894_i0",
                                                                       "p904_i0","p1021_i0.y","p3647_i0"))

old_cols <- colnames(Lifestyle_and_environmental_exp_phys_act)

Lifestyle_and_environmental_exp_phys_act <-   
  rename(Lifestyle_and_environmental_exp_phys_act,  
         eid = eid,  # 保持不变  
         above_activity_recommendation = p22035_i0,  
         above_activity_recommendation_incl_walk = p22036_i0,  
         IPAQ_activity_group = p22032_i0,  
         MET_mins_moderate_activity = p22038_i0,  
         MET_mins_vigorous_activity = p22039_i0,  
         MET_mins_walking = p22037_i0,  
         days_activity_sum = p22033_i0,  
         mins_activity_sum = p22034_i0,  
         fast_driving = p1100_i0,  
         heavy_DIY_duration = p2634_i0,  
         light_DIY_duration = p1021_i0.y,  # 注意保留.y  
         moderate_activity_duration = p894_i0,  
         other_exercise_duration = p3647_i0,  
         strenuous_sports_duration = p1001_i0,  
         vigorous_activity_duration = p914_i0,  
         walks_duration = p874_i0,  
         pleasure_walks_duration = p981_i0,  
         heavy_DIY_freq_4wks = p2624_i0,  
         light_DIY_freq_4wks = p1011_i0,  
         other_exercise_freq_4wks = p3637_i0,  
         stair_climbing_freq_4wks = p943_i0,  
         strenuous_sports_freq_4wks = p991_i0,  
         pleasure_walks_freq_4wks = p971_i0,  
         moderate_activity_over_10mins_days = p884_i0,  
         vigorous_activity_over_10mins_days = p904_i0,  
         walked_over_10mins_days = p864_i0,  
         driving_time = p1090_i0,  
         computer_time = p1080_i0,  
         TV_time = p1070_i0,  
         physical_activity_type = p6164_i0,  
         transport_type.0.0 = p6162_i0,
         usual_walking_pace = p924_i0  
  )  
new_cols <- colnames(Lifestyle_and_environmental_exp_phys_act)
column_key_phys_act <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_phys_act, 
     column_key_phys_act,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_physical_activity.RData")
###Lifestyle and environmental exp_electronics use####
Lifestyle_and_environmental_exp_electronics <- subset(UKB_data,select = c("eid","p1110_i0",
                                                                          "p1120_i0","p1130_i0","p1140_i0",
                                                                          "p1150_i0","p2237_i0"))
old_cols <- colnames(Lifestyle_and_environmental_exp_electronics)

Lifestyle_and_environmental_exp_electronics <- 
  rename(Lifestyle_and_environmental_exp_electronics,
         eid = eid,
         mobile_phone_duration = p1110_i0,
         mobile_phone_weekly_usage = p1120_i0,
         speakerphone = p1130_i0,
         mobile_phone_2yrs = p1140_i0,
         head_side_phone = p1150_i0,
         computer_games = p2237_i0
  )

new_cols <- colnames(Lifestyle_and_environmental_exp_electronics)
column_key_electronics <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_electronics, 
     column_key_electronics,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_electronics.RData")
###Lifestyle and environmental exp_sleep####
Lifestyle_and_environmental_exp_sleep <- subset(UKB_data,select = c("eid","p1160_i0",
                                                                    "p1170_i0","p1180_i0","p1190_i0",
                                                                    "p1200_i0","p1210_i0","p1220_i0"))
old_cols <- colnames(Lifestyle_and_environmental_exp_sleep)

Lifestyle_and_environmental_exp_sleep <- 
  rename(Lifestyle_and_environmental_exp_sleep,
         eid = eid,
         sleep_hours = p1160_i0,
         easy_wake = p1170_i0,
         chronotype = p1180_i0,
         nap = p1190_i0,
         sleep_difficulty = p1200_i0,
         snoring = p1210_i0,
         narcolepsy = p1220_i0
  )

new_cols <- colnames(Lifestyle_and_environmental_exp_sleep)
column_key_sleep <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_sleep, 
     column_key_sleep,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_sleep.RData") 


###Lifestyle and environmental exp_smoking####
Lifestyle_and_environmental_exp_smok <- subset(UKB_data,select = c("eid","p20160_i0","p20162_i0","p20161_i0",
                                                                   "p20116_i0","p1239_i0","p1249_i0","p2644_i0",
                                                                   "p3436_i0","p3446_i0","p5959_i0","p3456_i0",
                                                                   "p6194_i0","p6183_i0","p3466_i0","p3506_i0",
                                                                   "p6158_i0","p2867_i0","p2877_i0","p2887_i0",
                                                                   "p2897_i0","p2907_i0","p6157_i0","p2926_i0",
                                                                   "p1259_i0","p1269_i0","p1279_i0","p3476_i0",
                                                                   "p3486_i0","p3496_i0","p2936_i0"))
old_cols <- colnames(Lifestyle_and_environmental_exp_smok)

Lifestyle_and_environmental_exp_smok <-
  rename(Lifestyle_and_environmental_exp_smok,
         eid = eid,
         ever_smoked = p20160_i0,
         pack_years_prop = p20162_i0,
         pack_years = p20161_i0,
         smoking_status = p20116_i0,
         tobacco = p1239_i0,
         past_tobacco = p1249_i0,
         light_smoking = p2644_i0,
         smoking_start_age_current = p3436_i0,
         tobacco_type = p3446_i0,
         previous_smoker = p5959_i0,
         number_cigarettes = p3456_i0,
         cigarettes_stop_age = p6194_i0,
         previous_number_cigarettes = p6183_i0,
         waking_smoke_time = p3466_i0,
         smoking_10yrs = p3506_i0,
         why_reduced_smoking_0 = p6158_i0,
         smoking_start_age_former = p2867_i0,
         tobacco_type_previous = p2877_i0,
         number_cigarettes_previous = p2887_i0,
         smoking_stop_age = p2897_i0,
         ever_stop_smoking_6months = p2907_i0,
         why_stopped_smoking = p6157_i0,
         number_quit_attempts = p2926_i0,
         hshld_smokers = p1259_i0,
         tobacco_exposure_home = p1269_i0,
         tobacco_exposure_outside = p1279_i0,
         resume_smoking_likelihood = p2936_i0,
         difficulty_not_smoking_1day = p3476_i0,
         stop_smoking_attempt = p3486_i0,
         stop_smoking_desire = p3496_i0
  )

new_cols <- colnames(Lifestyle_and_environmental_exp_smok)
column_key_smoking <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_smok, 
     column_key_smoking,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_smoking.RData") 

###Lifestyle and environmental exp_alcohol####
Lifestyle_and_environmental_exp_alcohol  <- subset(UKB_data,select = c("eid","p20117_i0","p1558_i0","p3731_i0",
                                                                       "p4407_i0","p4418_i0","p4429_i0","p4440_i0",
                                                                       "p4451_i0","p4462_i0","p1568_i0","p1578_i0",
                                                                       "p1588_i0","p1598_i0","p1608_i0","p5364_i0",
                                                                       "p1618_i0","p1628_i0","p2664_i0","p3859_i0"))
old_cols <- colnames(Lifestyle_and_environmental_exp_alcohol)

Lifestyle_and_environmental_exp_alcohol <-
  rename(Lifestyle_and_environmental_exp_alcohol,
         eid = eid,
         alcohol_status = p20117_i0,
         alcohol_freq = p1558_i0,
         alcohol_former = p3731_i0,
         red_wine_monthly = p4407_i0,
         white_wine_monthly = p4418_i0,
         beer_monthly = p4429_i0,
         spirits_monthly = p4440_i0,
         fortified_wine_monthly = p4451_i0,
         other_alcohol_monthly = p4462_i0,
         red_wine_weekly = p1568_i0,
         white_wine_weekly = p1578_i0,
         beer_weekly = p1588_i0,
         spirits_weekly = p1598_i0,
         fortified_wine_weekly = p1608_i0,
         other_alcohol_weekly = p5364_i0,
         alcohol_with_meals = p1618_i0,
         alcohol_10yrs = p1628_i0,
         reason_reducing_alcohol = p2664_i0,
         reason_stopped_alcohol = p3859_i0
  )


new_cols <- colnames(Lifestyle_and_environmental_exp_alcohol)
column_key_alcohol <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_alcohol, 
     column_key_alcohol,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_alcohol.RData") 
###Lifestyle and environmental exp_diet#####
Lifestyle_and_environmental_exp_diet  <- subset(UKB_data,select = c("eid","p1289_i0","p1299_i0","p1309_i0",
                                                                    "p1319_i0","p1329_i0","p1339_i0","p1349_i0",
                                                                    "p1359_i0","p1369_i0","p1379_i0","p1389_i0",
                                                                    "p1448_i0","p1458_i0","p1468_i0","p1478_i0",
                                                                    "p1488_i0","p1498_i0","p1508_i0","p1518_i0",
                                                                    "p1528_i0","p1538_i0","p1548_i0","p2654_i0",
                                                                    "p6144_i0","p3680_i0","p1408_i0","p1418_i0",
                                                                    "p1428_i0","p1438_i0"))
old_cols <- colnames(Lifestyle_and_environmental_exp_diet)

Lifestyle_and_environmental_exp_diet <-   
  rename(Lifestyle_and_environmental_exp_diet,  
         eid = eid,                       
         cooked_veg = p1289_i0,            
         salad_or_raw_veg = p1299_i0,      
         fresh_fruit = p1309_i0,           
         dried_fruit = p1319_i0,          
         oily_fish = p1329_i0,            
         non_oily_fish = p1339_i0,       
         processed_meat = p1349_i0,       
         poultry = p1359_i0,               
         beef = p1369_i0,                
         lamb = p1379_i0,                  
         pork = p1389_i0,                   
         meat_age_last = p3680_i0,         
         never_eat = p6144_i0,          
         cheese = p1408_i0,               
         milk_type = p1418_i0,            
         spread_type = p1428_i0,           
         spread_type_details = p2654_i0,  
         bread = p1438_i0,                  
         bread_type = p1448_i0,            
         cereal = p1458_i0,                
         cereal_type = p1468_i0,          
         salt = p1478_i0,                  
         tea = p1488_i0,                   
         coffee_type = p1508_i0,          
         hot_drink_temp = p1518_i0,     
         water = p1528_i0,                 
         diet_change_5yrs = p1538_i0,     
         diet_variation = p1548_i0 ,
         coffee = p1498_i0
  )  
new_cols <- colnames(Lifestyle_and_environmental_exp_diet)
column_key_diet <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_diet, 
     column_key_diet,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_diet.RData")
###Lifestyle and environmental exp_sun#####
Lifestyle_and_environmental_exp_sun  <- subset(UKB_data,select = c("eid","p1717_i0","p1727_i0","p1737_i0",
                                                                   "p1747_i0","p1757_i0","p2277_i0","p1050_i0",
                                                                   "p1060_i0","p2267_i0"))
old_cols <- colnames(Lifestyle_and_environmental_exp_sun)

Lifestyle_and_environmental_exp_sun <-  
  rename(Lifestyle_and_environmental_exp_sun,  
         eid = eid,  # If you wish to keep "eid" unchanged  
         summer_outdoors_time = p1050_i0,  
         winter_outdoors_time = p1060_i0,  
         skin_color = p1717_i0,  
         skin_tan_ease = p1727_i0,  
         childhood_sunburn_number = p1737_i0,  
         hair_color = p1747_i0,  
         facial_aging = p1757_i0,  
         sun_protection_use = p2267_i0,  
         solarium_freq = p2277_i0  
  ) 

new_cols <- colnames(Lifestyle_and_environmental_exp_sun)
column_key_sun <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_sun,
     column_key_sun,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_sun.RData")

###Lifestyle_and_environmental_exp_sex history####
Lifestyle_and_environmental_exp_sex_hist  <- subset(UKB_data,select = c("eid","p2129_i0","p2139_i0","p2149_i0",
                                                                        "p2159_i0","p3669_i0"))

old_cols <- colnames(Lifestyle_and_environmental_exp_sex_hist)

Lifestyle_and_environmental_exp_sex_hist <-
  rename(Lifestyle_and_environmental_exp_sex_hist,
         eid = eid,
         sex_questions_answered = p2129_i0,
         sex_age_first = p2139_i0,
         number_sex_partners = p2149_i0,
         same_sex_intercourse = p2159_i0,
         number_same_sex_partners = p3669_i0
  )

new_cols <- colnames(Lifestyle_and_environmental_exp_sex_hist)
column_key_sex_hist <- as.data.frame(cbind(old_cols, new_cols))

save(Lifestyle_and_environmental_exp_sex_hist,
     column_key_sex_hist,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_sex_history.RData")

###Physical_examination_vision####
Physical_examination_vision <- subset(UKB_data,select = c("eid","p5208_i0","p5201_i0"))
old_cols <- colnames(Physical_examination_vision)

Physical_examination_vision<-
  rename(Physical_examination_vision,
         eid = eid,
         visual_acuity_left = p5208_i0,
         visual_acuity_right = p5201_i0
  )

new_cols <- colnames(Physical_examination_vision)
column_key_vision <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_vision, 
     column_key_vision,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_vision.RData")

###Physical_examination_hearing####
Physical_examination_hearing <- subset(UKB_data,select = c("eid","p20019_i0","p20021_i0"))
old_cols <- colnames(Physical_examination_hearing)

Physical_examination_hearing <-
  rename(Physical_examination_hearing,
         eid =eid,
         speech_reception_threshold_left = p20019_i0,
         speech_reception_threshold_right = p20021_i0
  )

new_cols <- colnames(Physical_examination_hearing)
column_key_hearing <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_hearing, 
     column_key_hearing,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_hearing.RData")

###Physical_examination_blood pressure######
Physical_examination_bp <- subset(UKB_data,select = c("eid","p4079_i0_a1","p4080_i0_a1","p94_i0_a1",
                                                      "p93_i0_a1","p95_i0_a1","p102_i0_a1"))

old_cols <- colnames(Physical_examination_bp)

Physical_examination_bp <- 
  rename(Physical_examination_bp,
         eid =eid,
         diastolic_bp1 = p4079_i0_a1,
         diastolic_bp_manual = p94_i0_a1,
         systolic_bp1 = p4080_i0_a1,
         systolic_bp_manual = p93_i0_a1,
         pulse_rate1 = p102_i0_a1,
         pulse_rate_manual = p95_i0_a1
  )

new_cols <- colnames(Physical_examination_bp)
column_key_bp <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_bp, 
     column_key_bp,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_blood_pressure.RData")
###Physical_examination_arterial stiffness#####
Physical_examination_a_stiff <- subset(UKB_data,select = c("eid","p4194_i0","p4195_i0","p4196_i0",
                                                           "p4198_i0","p4199_i0","p4200_i0","p21021_i0"))
old_cols <- colnames(Physical_examination_a_stiff)

Physical_examination_a_stiff <-
  rename(Physical_examination_a_stiff,
         eid = eid,
         pulse_wave_notch_position = p4199_i0,
         pulse_wave_peak_position = p4198_i0,
         pulse_wave_shoulder_position = p4200_i0,
         pulse_rate_a_stiff = p4194_i0,
         pulse_wave_arterial_stiffness_index = p21021_i0,
         pulse_wave_peak_to_peak_time = p4196_i0,
         pulse_wave_reflection_index = p4195_i0
  )

new_cols <- colnames(Physical_examination_a_stiff)
column_key_a_stiff <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_a_stiff,
     column_key_a_stiff,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_arterial_stiffness.RData")

###Physical_examination_anthropometrics####
Physical_examination_anthro <- subset(UKB_data,select = c("eid","p46_i0","p47_i0","p48_i0",
                                                          "p49_i0","p50_i0","p51_i0","p21002_i0",
                                                          "p21001_i0","p20015_i0","p3077_i0","p3160_i0",
                                                          "p23098_i0","p23104_i0","p23113_i0","p23118_i0",
                                                          "p23114_i0","p23123_i0","p23119_i0","p23124_i0",
                                                          "p23120_i0","p23121_i0","p23125_i0","p23126_i0",
                                                          "p23122_i0","p23127_i0","p23128_i0","p23129_i0",
                                                          "p23130_i0","p23099_i0","p23100_i0","p23101_i0",
                                                          "p23102_i0","p23115_i0","p23111_i0","p23116_i0",
                                                          "p23112_i0","p23117_i0","p23106_i0","p23110_i0",
                                                          "p23109_i0","p23108_i0","p23107_i0","p6218_i0",
                                                          "p6222_i0","p6221_i0","p6220_i0","p6219_i0","p23105_i0",
                                                          "p22040_i0"))

old_cols <- colnames(Physical_examination_anthro)

Physical_examination_anthro <-  
  rename(Physical_examination_anthro,  
         eid = eid,    
         waist_circumference = p48_i0,  
         weight = p21002_i0,  
         BMI = p21001_i0,  
         hip_circumference = p49_i0,  
         standing_height = p50_i0,  
         seated_height = p51_i0,  
         sitting_height = p20015_i0,  
         seating_box_height = p3077_i0,  
         weight_manual = p3160_i0,  
         hand_grip_strength_left = p46_i0,  
         hand_grip_strength_right = p47_i0,  
         weight_impedance = p23098_i0,  
         BMI_impedance = p23104_i0,  
         leg_fat_free_mass_right = p23113_i0,  
         leg_predicted_mass_left = p23118_i0,  
         leg_predicted_mass_right = p23114_i0,  
         arm_fat_percentage_left = p23123_i0,  
         arm_fat_percentage_right = p23119_i0,  
         arm_fat_mass_left = p23124_i0,  
         arm_fat_mass_right = p23120_i0,  
         arm_fat_free_mass_right = p23121_i0,  
         arm_fat_free_mass_left = p23125_i0,  
         arm_predicted_mass_left = p23126_i0,  
         arm_predicted_mass_right = p23122_i0,  
         trunk_fat_percentage = p23127_i0,  
         trunk_fat_mass = p23128_i0,  
         trunk_fat_free_mass = p23129_i0,  
         trunk_predicted_mass = p23130_i0,  
         body_fat_percent = p23099_i0,  
         whole_body_fat_mass = p23100_i0,  
         whole_body_fat_free_mass = p23101_i0,  
         whole_body_water_mass = p23102_i0,  
         leg_fat_percentage_left = p23115_i0,  
         leg_fat_percentage_right = p23111_i0,  
         leg_fat_mass_left = p23116_i0,  
         leg_fat_mass_right = p23112_i0,  
         leg_fat_free_mass_left = p23117_i0,  
         whole_body_impedance = p23106_i0,  
         arm_left_impedance = p23110_i0,  
         arm_right_impedance = p23109_i0,  
         leg_left_impedance = p23108_i0,  
         leg_right_impedance = p23107_i0,  
         whole_body_impedance_manual = p6218_i0,  
         arm_left_impedance_manual = p6222_i0,  
         arm_right_impedance_manual = p6221_i0,  
         leg_left_impedance_manual = p6220_i0,  
         leg_right_impedance_manual = p6219_i0,
         basal_metabolic_rate = p23105_i0,
         MET_mins_sum = p22040_i0
  ) 

new_cols <- colnames(Physical_examination_anthro)
column_key_anthro <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_anthro, 
     column_key_anthro,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_anthropometry.RData")
###Physical_examination_spirometry (lung function)####
Physical_examination_spiro <- subset(UKB_data,select = c("eid","p20031_i0_a0","p20031_i0_a1","p20031_i0_a2",
                                                         "p3061_i0_a0","p3061_i0_a1","p3061_i0_a2","p3137_i0",
                                                         "p20255","p3065_i0_a0","p3065_i0_a1","p3065_i0_a2",
                                                         "p3059_i0_a0","p3059_i0_a1","p3059_i0_a2",
                                                         "p3066_i0_a0","p3066_i0_a1","p3066_i0_a2",
                                                         "p3062_i0_a0","p3062_i0_a1","p3062_i0_a2",
                                                         "p20151_i0","p20257","p3063_i0_a0","p3063_i0_a1","p3063_i0_a2",
                                                         "p20150_i0","p20153_i0","p20154_i0","p20256",
                                                         "p20258","p3064_i0_a0","p3064_i0_a1","p3064_i0_a2"))
old_cols <- colnames(Physical_examination_spiro)

Physical_examination_spiro <-  
  rename(Physical_examination_spiro,  
         eid = eid,   
         blow_acceptability_text_0_0 = p20031_i0_a0,  
         blow_acceptability_text_0_1 = p20031_i0_a1,  
         blow_acceptability_text_0_2 = p20031_i0_a2,  
         blow_acceptability_0_0 = p3061_i0_a0,  
         blow_acceptability_0_1 = p3061_i0_a1,  
         blow_acceptability_0_2 = p3061_i0_a2,  
         number_spirometry_measurements = p3137_i0,  
         spirometry_QC = p20255,  
         blow_ordering_0_0 = p3065_i0_a0,  
         blow_ordering_0_1 = p3065_i0_a1,  
         blow_ordering_0_2 = p3065_i0_a2,  
         blow_result_ranking_0_0 = p3059_i0_a0,  
         blow_result_ranking_0_1 = p3059_i0_a1,  
         blow_result_ranking_0_2 = p3059_i0_a2,  
         blow_data_points_0_0 = p3066_i0_a0,  
         blow_data_points_0_1 = p3066_i0_a1,  
         blow_data_points_0_2 = p3066_i0_a2,  
         FVC_0_0 = p3062_i0_a0,  
         FVC_0_1 = p3062_i0_a1,  
         FVC_0_2 = p3062_i0_a2,  
         FVC_best = p20151_i0,  
         FVC_z_score = p20257,  
         FEV1_0_0 = p3063_i0_a0,  
         FEV1_0_1 = p3063_i0_a1,  
         FEV1_0_2 = p3063_i0_a2,  
         FEV1_best = p20150_i0,  
         FEV1_predicted = p20153_i0,  
         FEV1_predicted_percentage = p20154_i0,  
         FEV1_z_score = p20256,  
         FEV1_FVC_ratio_z_score = p20258,  
         peak_expiratory_flow_0_0 = p3064_i0_a0,  
         peak_expiratory_flow_0_1 = p3064_i0_a1,  
         peak_expiratory_flow_0_2 = p3064_i0_a2  
  )  

new_cols <- colnames(Physical_examination_spiro)
column_key_spiro <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_spiro,
     column_key_spiro,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_spirometry.RData") 

###Physical_examination_heel bone mineral density####
Physical_examination_bmd <- subset(UKB_data,select = c("eid","p3082_i0","p3081_i0","p3146_i0",
                                                       "p3143_i0","p3144_i0","p3147_i0","p3148_i0",
                                                       "p78_i0","p3086_i0","p3085_i0","p3083_i0",
                                                       
                                                       "p3084_i0.x","p77_i0","p4093_i0","p4096_i0",
                                                       "p4100_i0","p4119_i0","p4103_i0","p4122_i0",
                                                       "p4101_i0","p4120_i0","p4104_i0.y","p4123_i0",
                                                       "p4105_i0","p4124_i0","p4106_i0","p4125_i0",
                                                       "p4142_i0","p4147_i0","p4141_i0","p4146_i0",
                                                       "p4139_i0","p4144_i0","p4140_i0","p4145_i0",
                                                       "p4138_i0","p4143_i0"))
old_cols <- colnames(Physical_examination_bmd )

Physical_examination_bmd <-   
  rename(Physical_examination_bmd,  
         eid = eid,   
         fractured_heel = p3082_i0,  
         bone_density_measure_foot = p3081_i0,  
         heel_sound_speed = p3146_i0,  
         ankle_spacing_width = p3143_i0,  
         heel_broadband_attenuation = p3144_i0,  
         heel_QUI = p3147_i0,  
         heel_bone_mineral_density = p3148_i0,  
         heel_bone_mineral_density_t_score = p78_i0,  
         heel_sound_speed_manual = p3086_i0,  
         heel_broadband_attenuation_manual = p3085_i0,  
         heel_QUI_manual = p3083_i0,  
         heel_bone_mineral_density_manual = p3084_i0.x,    
         heel_bone_ultrasound_t_score_manual = p77_i0,  
         fractured_heel_left = p4093_i0,  
         fractured_heel_right = p4096_i0,  
         ankle_spacing_width_left = p4100_i0,  
         ankle_spacing_width_right = p4119_i0,  
         heel_sound_speed_left = p4103_i0,  
         heel_sound_speed_right = p4122_i0,  
         heel_broadband_attenuation_left = p4101_i0,  
         heel_broadband_attenuation_right = p4120_i0,  
         heel_QUI_left = p4104_i0.y,    
         heel_QUI_right = p4123_i0,  
         heel_bone_mineral_density_left = p4105_i0,  
         heel_bone_mineral_density_right = p4124_i0,  
         heel_bone_mineral_density_t_score_left = p4106_i0,  
         heel_bone_mineral_density_t_score_right = p4125_i0,  
         heel_sound_speed_manual_left = p4142_i0,  
         heel_sound_speed_manual_right = p4147_i0,  
         heel_broadband_attenuation_manual_left = p4141_i0,  
         heel_broadband_attenuation_manual_right = p4146_i0,  
         heel_QUI_manual_left = p4139_i0,  
         heel_QUI_manual_right = p4144_i0,  
         heel_bone_mineral_density_manual_left = p4140_i0,  
         heel_bone_mineral_density_manual_right = p4145_i0,  
         heel_bone_mineral_density_t_score_manual_left = p4138_i0,  
         heel_bone_mineral_density_t_score_manual_right = p4143_i0  
  )  
new_cols <- colnames(Physical_examination_bmd)
column_key_bmd <- as.data.frame(cbind(old_cols, new_cols))

save(Physical_examination_bmd,
     column_key_bmd,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_bmd.RData") 
###Sociodemographics_indices of material deprivation + townsend deprivation index####
Sociodemographics_imd <- subset(UKB_data,select = c("eid","p26433","p26422","p26425",
                                                    "p26416","p26434","p26414","p26431",
                                                    "p26421","p26412","p26419","p26413",
                                                    "p26430","p26420","p26415","p26432",
                                                    "p26423","p26411","p26428","p26418",
                                                    "p26410","p26427","p26426","p26417",
                                                    "p26424","p22189","p26429"))
old_cols <- colnames(Sociodemographics_imd)

Sociodemographics_imd <-   
  rename(Sociodemographics_imd,  
         eid = eid,  # Keeping "eid" unchanged  
         IMD_services_Scotland = p26433,  
         IMD_services_Wales = p26422,  
         IMD_community_safety_Wales = p26425,  
         IMD_crime_England = p26416,  
         IMD_crime_Scotland = p26434,  
         IMD_education_England = p26414,  
         IMD_education_Scotland = p26431,  
         IMD_education_Wales = p26421,  
         IMD_employment_England = p26412,  
         IMD_employment_Wales = p26419, 
         IMD_employment_Scotland = p26429, 
         IMD_health_England = p26413,  
         IMD_health_Scotland = p26430,  
         IMD_health_Wales = p26420,  
         IMD_housing_England = p26415,  
         IMD_housing_Scotland = p26432,  
         IMD_housing_Wales = p26423,  
         IMD_income_England = p26411,  
         IMD_income_Scotland = p26428,  
         IMD_income_Wales = p26418,  
         IMD_total_England = p26410,  
         IMD_total_Scotland = p26427,  
         IMD_total_Wales = p26426,  
         IMD_living_environ_England = p26417,  
         IMD_physical_environ_Wales = p26424,    
         townsend_deprivation_index = p22189
  )  

new_cols <- colnames(Sociodemographics_imd)
column_key_imd <- as.data.frame(cbind(old_cols, new_cols))

save(Sociodemographics_imd,
     column_key_imd,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_imd.RData")

###Environmental location and atmo_physical environment####
enviro <- subset(UKB_data,select = c("eid","p24014","p24012","p24010",
                                     "p24016","p24017","p24018","p24003",
                                     "p24004","p24019","p24005","p24007",
                                     "p24006","p24008","p24015","p24013",
                                     "p24011","p24009","p24020","p24021",
                                     "p24022","p24023","p24024","p24506_i0",
                                     "p24507_i0","p24500_i0","p24503_i0",
                                     "p24501_i0","p24504_i0","p24502_i0",
                                     "p24505_i0","p24508_i0","p20118_i0"))

old_cols <- colnames(enviro)

enviro <-   
  rename(enviro,  
         eid = eid,  # Keeping "eid" unchanged  
         close_to_major_road = p24014,  
         inverse_distance_nearest_major_road = p24012,  
         inverse_distance_nearest_road = p24010,  
         NO2_2005 = p24016,  
         NO2_2006 = p24017,  
         NO2_2007 = p24018,  
         NO2_2010 = p24003,  
         NO_2010 = p24004,  
         PM10_2007 = p24019,  
         PM10_2010 = p24005,  
         PM2.5_absorbance_2010 = p24007,  
         PM2.5_2010 = p24006,  
         PM2.5_to_10_2010 = p24008,  
         road_length_sum_100m = p24015,  
         traffic_load_major_roads = p24013,  
         traffic_intensity_nearest_major_road = p24011,  
         traffic_intensity_nearest_road = p24009,  
         daytime_sound_average = p24020,  
         evening_sound_average = p24021,  
         night_sound_average = p24022,  
         natural_environment_buffer_1000m = p24506_i0,  
         natural_environment_buffer_300m = p24507_i0,  
         greenspace_buffer_1000m = p24500_i0,  
         greenspace_buffer_300m = p24503_i0,  
         domestic_garden_buffer_1000m = p24501_i0,  
         domestic_garden_buffer_300m = p24504_i0,  
         water_buffer_1000m = p24502_i0,  
         water_buffer_300m = p24505_i0,  
         distance_to_coast = p24508_i0,  
         home_population_density = p20118_i0,
         sound_average_16hr = p24023,
         sound_average_24hr = p24024,
  )  

new_cols <- colnames(enviro)
column_key_enviro <- as.data.frame(cbind(old_cols, new_cols))

save(enviro, 
     column_key_enviro,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_environment.RData")

###Health and medical history_medications####
Health_and_medical_history_meds <- subset(UKB_data,select = c(
  "eid","p6177_i0","p6153_i0","p2492_i0",
  "p6154_i0","p6155_i0","p6179_i0","p137_i0",
  "p2986_i0"
))
old_cols <- colnames(Health_and_medical_history_meds)

Health_and_medical_history_meds <-   
  rename(Health_and_medical_history_meds,  
         eid = eid,  
         meds_0_0 = p6177_i0,          
         
         meds_hormones = p6153_i0,  
         
         other_meds = p2492_i0,        
         pain_meds = p6154_i0,     
         
         vitamins = p6155_i0,      
         
         minerals = p6179_i0,        
         
         number_medications = p137_i0 ,
         diabetes_insulin_1yr = p2986_i0,
  )  

new_cols <- colnames(Health_and_medical_history_meds)
column_key_meds <- as.data.frame(cbind(old_cols, new_cols))

save(Health_and_medical_history_meds,
     column_key_meds,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_medications.RData")   

###Health and medical history_medical conditions #####
Health_and_medical_history_med_cond <- subset(UKB_data,select = c(
  "eid","p2415_i0","p2844_i0","p2178_i0",
  "p2188_i0","p2296_i0","p2306_i0","p2316_i0",
  "p2335_i0","p4717_i0","p4728_i0","p5452_i0",
  "p5463_i0","p5474_i0","p5485_i0","p5496_i0",
  "p5507_i0","p5518_i0","p5529_i0","p5540_i0",
  "p6159_i0","p3799_i0","p4067_i0","p3404_i0",
  "p3571_i0","p3741_i0","p3414_i0","p3773_i0",
  "p2956_i0","p3606_i0","p3616_i0",
  "p3751_i0","p136_i0",
  "p3079_i0","p2247_i0","p2257_i0","p3393_i0",
  "p4792_i0","p4803_i0","p4814_i0","p4836_i0",
  "p2345_i0","p2355_i0","p2365_i0","p3809_i0",
  
  "p2463_i0","p6151_i0","p3005_i0",
  "p2207_i0","p2217_i0",
  "p6147_i0","p5843_i0","p5832_i0","p5610_i0",
  "p5855_i0","p6205_i0","p5408_i0","p5877_i0",
  "p5934_i0","p2227_i0","p6148_i0","p5890_i0",
  "p6119_i0","p5419_i0","p5441_i0","p5912_i0",
  "p5901_i0","p4689_i0","p5430_i0","p4700_i0",
  "p5923_i0","p5945_i0","p6149_i0"))

old_cols <- colnames(Health_and_medical_history_med_cond)

Health_and_medical_history_med_cond <-   
  rename(Health_and_medical_history_med_cond,  
         eid = eid,  
         fracture_last_5yrs = p2463_i0,
         fracture_site = p6151_i0,
         fracture_fall = p3005_i0,
         
         glasses = p2207_i0,                     
         glasses_age = p2217_i0,                   
         glasses_reason_0_0 = p6147_i0, 
         myopia_eye = p5843_i0,  
         hypertropia_eye = p5832_i0,  
         presbyopia_eye = p5610_i0,  
         astigmatism_eye = p5855_i0,  
         strabismus_eye = p6205_i0,  
         amblyopia_eye = p5408_i0,  
         other_eye = p5877_i0,  
         other_serious_eye = p5934_i0,  
         other_eye_problems = p2227_i0,  
         eye_disorders = p6148_i0,
         diabetes_eye = p5890_i0,  
         glaucoma_eye = p6119_i0,  
         injury_eye = p5419_i0,  
         cataract_eye = p5441_i0,  
         macular_degeneration_eye = p5912_i0,  
         diabetes_eye_age = p5901_i0,  
         glaucoma_age = p4689_i0,  
         injury_eye_age = p5430_i0,  
         cataract_age = p4700_i0,  
         macular_degeneration_age = p5923_i0,  
         other_serious_eye_age = p5945_i0,  
         mouth_problems = p6149_i0,
         
         overall_health = p2178_i0,                     
         chronic_illness = p2188_i0,                    
         falls_last_1yr = p2296_i0,                   
         weight_change_last_1yr = p2306_i0,             
         chest_weeze_last_1yr = p2316_i0,               
         shortness_breath = p4717_i0,                   
         leg_pain_walking = p4728_i0,                   
         leg_pain_standing = p5452_i0,                 
         leg_pain_calves = p5463_i0,                   
         leg_pain_walking_uphill = p5474_i0,          
         leg_pain_walking_normal = p5485_i0,          
         leg_pain_walking_disappears = p5496_i0,       
         leg_pain_walking_action = p5507_i0,         
         leg_pain_walking_standing = p5518_i0,         
         leg_artery_surgery = p5529_i0,                
         leg_surgery = p5540_i0,                        
         pain_last_1month = p6159_i0,            
         
         headaches_3months = p3799_i0,                 
         facial_pain_3months = p4067_i0,               
         neck_pain_3months = p3404_i0,                  
         back_pain_3months = p3571_i0,                
         stomach_pain_3months = p3741_i0,              
         hip_pain_3months = p3414_i0,                
         knee_pain_3months = p3773_i0,              
         general_pain_3months = p2956_i0,             
         chest_pain = p2335_i0,                       
         chest_pain_walking_normal = p3606_i0,          
         chest_pain_ceases_still = p3616_i0,        
         chest_pain_walking_uphill = p3751_i0,      
         major_operations = p2415_i0,                  
         other_operations = p2844_i0,                
         number_operations = p136_i0,                 
         pace_maker = p3079_i0,                       
         hearing_difficulty = p2247_i0,              
         hearing_difficulty_background = p2257_i0,    
         hearing_aid = p3393_i0,                   
         cochlear_implant = p4792_i0,                  
         tinnitus = p4803_i0,                        
         tinnitus_severity = p4814_i0,               
         loud_music_exposure = p4836_i0,               
         bowel_cancer_screening = p2345_i0,              
         bowel_cancer_screening_recent = p2355_i0,     
         PSA_test = p2365_i0,                           
         PSA_test_time_since = p3809_i0               
  )  

new_cols <- colnames(Health_and_medical_history_med_cond)
column_key_med_cond <- as.data.frame(cbind(old_cols, new_cols))

save(Health_and_medical_history_med_cond, 
     column_key_med_cond,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_medical_conditions.RData") 

# ### Cognition function####
# Cognition_function<-subset(UKB_data,select = c(
#   "eid","p20016_i0","p399_i0_a1","p399_i0_a2",
#   "p399_i0_a3","p20018_i0","p20023_i0","p4282_i0"))
# 
# old_cols <- colnames(Cognition_function)
# 
# Cognition_function <-   
#   rename(Cognition_function,  
#          eid = eid,                            
#          identify_matches_mean_time = p20023_i0,
#          maximum_digits_remembered = f.4282.0.0,
#          fluid_intelligence = f.20016.0.0,
#          prospective_memory = f.20018.0.0,
#          incorrect_matches_round1 = p399_i0_a1,
#          incorrect_matches_round2 = p399_i0_a2,
#          incorrect_matches_round3 = p399_i0_a3) 
# Cognition_function <- colnames(Cognition_function)
# column_key_Cognition_function <- as.data.frame(cbind(old_cols, new_cols))
# 
# save(Cognition_function, 
#      column_key_Cognition_function,
#      file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_Cognition_function.RData") 
### blood biomarkers####
blood <- subset(UKB_data,select = c(
  "eid","p30620_i0","p30600_i0","p30610_i0",
  "p30630_i0","p30640_i0","p30650_i0","p30710_i0",
  "p30680_i0","p30690_i0","p30700_i0","p30720_i0",
  "p30660_i0","p30730_i0","p30740_i0","p30750_i0",
  "p30760_i0","p30770_i0","p30780_i0","p30790_i0",
  "p30800_i0","p30810_i0","p30820_i0","p30830_i0",
  "p30850_i0","p30840_i0","p30860_i0","p30870_i0",
  "p30880_i0","p30670_i0","p30890_i0"))
old_cols <- colnames(blood)

blood <-   
  rename(blood,  
         eid = eid,                           # Keeping "eid" unchanged  
         alanine_aminotransferase = p30620_i0, # Updated variable name  
         albumin = p30600_i0,                  # Updated variable name  
         alkaline_phosphatase = p30610_i0,     # Updated variable name  
         apolipoprotein_A = p30630_i0,        # Updated variable name  
         apolipoprotein_B = p30640_i0,        # Updated variable name  
         aspartate_aminotransferase = p30650_i0, # Updated variable name  
         C_reactive_protein = p30710_i0,      # Updated variable name  
         calcium = p30680_i0,                  # Updated variable name  
         cholesterol = p30690_i0,              # Updated variable name  
         creatinine = p30700_i0,               # Updated variable name  
         cystatin_C = p30720_i0,               # Updated variable name  
         direct_bilirubin = p30660_i0,         # Updated variable name  
         gamma_glutamyltransferase = p30730_i0,# Updated variable name  
         glucose = p30740_i0,                  # Updated variable name  
         hbA1c = p30750_i0,                     # Updated variable name  
         HDL_cholesterol = p30760_i0,          # Updated variable name  
         IGF1 = p30770_i0,                     # Updated variable name  
         LDL_direct = p30780_i0,               # Updated variable name  
         lipoprotein_A = p30790_i0,           # Updated variable name  
         oestradiol = p30800_i0,               # Updated variable name  
         phosphate = p30810_i0,                # Updated variable name  
         rheumatoid_factor = p30820_i0,        # Updated variable name  
         SHBG = p30830_i0,                     # Updated variable name  
         testosterone = p30850_i0,             # Updated variable name  
         total_bilirubin = p30840_i0,          # Updated variable name  
         total_protein = p30860_i0,            # Updated variable name  
         triglycerides = p30870_i0,            # Updated variable name  
         urate = p30880_i0,                    # Updated variable name  
         urea = p30670_i0,                     # Updated variable name  
         vitamin_D = p30890_i0                  # Updated variable name  
  )  

new_cols <- colnames(blood)
column_key_blood <- as.data.frame(cbind(old_cols, new_cols))

save(blood, 
     column_key_blood,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_blood_biomarkers.RData") 

### blood counts####

blood_count <- subset(UKB_data,select = c(
  "eid","p30160_i0","p30220_i0","p30150_i0",
  "p30210_i0","p30030_i0","p30020_i0","p30300_i0",
  "p30290_i0","p30280_i0","p30120_i0","p30180_i0",
  "p30050_i0","p30060_i0","p30040_i0","p30100_i0",
  "p30260_i0","p30270_i0","p30130_i0","p30190_i0",
  "p30140_i0","p30200_i0","p30170_i0","p30230_i0",
  "p30080_i0","p30090_i0","p30110_i0","p30010_i0",
  "p30070_i0","p30250_i0","p30240_i0","p30000_i0"))

old_cols <- colnames(blood_count)

blood_count <-   
  rename(blood_count,  
         eid = eid,                            # Keeping "eid" unchanged  
         basophill_count = p30160_i0,         # Updated variable name  
         basophill_percentage = p30220_i0,    # Updated variable name  
         eosinophill_count = p30150_i0,       # Updated variable name  
         eosinophill_percentage = p30210_i0,  # Updated variable name  
         haematocrit_percentage = p30030_i0,  # Updated variable name  
         haemoglobin_concentration = p30020_i0,# Updated variable name  
         high_light_scatter_reticulocyte_count = p30300_i0,  # Updated variable name  
         high_light_scatter_reticulocyte_percentage = p30290_i0, # Updated variable name  
         immature_reticulocyte_fraction = p30280_i0,          # Updated variable name  
         lymphocyte_count = p30120_i0,          # Updated variable name  
         lymphocyte_percentage = p30180_i0,     # Updated variable name  
         mean_corpuscular_haemoglobin = p30050_i0,          # Updated variable name  
         mean_corpuscular_haemoglobin_concentration = p30060_i0, # Updated variable name  
         mean_corpuscular_volume = p30040_i0,   # Updated variable name  
         mean_platelet_volume = p30100_i0,      # Updated variable name  
         mean_reticulocyte_volume = p30260_i0,  # Updated variable name  
         mean_sphered_cell_volume = p30270_i0,  # Updated variable name  
         monocyte_count = p30130_i0,            # Updated variable name  
         monocyte_percentage = p30190_i0,       # Updated variable name  
         neutrophill_count = p30140_i0,         # Updated variable name  
         neutrophill_percentage = p30200_i0,    # Updated variable name  
         nucleated_red_blood_cell_count = p30170_i0, # Updated variable name  
         nucleated_red_blood_cell_percentage = p30230_i0, # Updated variable name  
         platelet_count = p30080_i0,            # Updated variable name  
         platelet_crit = p30090_i0,             # Updated variable name  
         platelet_distribution_width = p30110_i0,# Updated variable name  
         erythrocyte_count = p30010_i0,        # Updated variable name  
         erythrocyte_distribution_width = p30070_i0, # Updated variable name  
         reticulocyte_count = p30250_i0,        # Updated variable name  
         reticulocyte_percentage = p30240_i0,   # Updated variable name  
         leukocyte_count = p30000_i0            # Updated variable name  
  )  
new_cols <- colnames(blood_count)
column_key_blood_count <- as.data.frame(cbind(old_cols, new_cols))

save(blood_count,
     column_key_blood_count,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_blood_count.RData")

### urine biomarkers

urine <- subset(UKB_data,select = c(
  "eid","p30510_i0","p30500_i0","p30520_i0",
  "p30530_i0"))


old_cols <- colnames(urine)

urine <-  
  rename(urine,  
         eid = eid,                           # Keeping "eid" unchanged  
         creatinine_urine = p30510_i0,       # Updated variable name from f.30510.0.0  
         microalbumin_urine = p30500_i0,      # Updated variable name from f.30500.0.0  
         potassium_urine = p30520_i0,         # Updated variable name from f.30520.0.0  
         sodium_urine = p30530_i0              # Updated variable name from f.30530.0.0  
  )  
new_cols <- colnames(urine)
column_key_urine <- as.data.frame(cbind(old_cols, new_cols))

save(urine,
     column_key_urine,
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_urine.RData")


#merge all exposure datasets into single, raw dataset####
exposures <- 
  list(
    elf,
    mental_health_sle,
    mental_health_soc,
    socio_dem,
    fam_health,
    Health_and_medical_history_med_cond,
    Health_and_medical_history_meds,
    Sex_related_male_repro,
    Sex_related_female_repro,
    mental_health,
    Lifestyle_and_environmental_exp_electronics,
    Lifestyle_and_environmental_exp_sleep,
    Lifestyle_and_environmental_exp_smok,
    Lifestyle_and_environmental_exp_alcohol,
    Lifestyle_and_environmental_exp_diet,
    Lifestyle_and_environmental_exp_sun,
    Lifestyle_and_environmental_exp_sex_hist,
    Lifestyle_and_environmental_exp_phys_act,
    Physical_examination_vision,
    Physical_examination_hearing,
    Physical_examination_bp,
    Physical_examination_a_stiff,
    Physical_examination_anthro,
    Physical_examination_spiro,
    Physical_examination_bmd,
    Sociodemographics_imd ,
    blood,
    blood_count,
    urine,
    enviro
  )

# merge all individuals datasets into single dataframe
dat <- Reduce(function(x, y) 
  merge(x = x, 
        y = y, 
        by = "eid",  
        all = TRUE), 
  exposures)
# combine all column keys
column_key <- 
  rbind(
    column_key_elf,
    column_key_sle,
    column_key_soc,
    column_key_socio_dem,
    column_key_fam_health,
    column_key_med_cond,
    column_key_meds,
    column_key_male_repro,
    column_key_female_repro,
    column_key_mental_health,
    column_key_electronics,
    column_key_sleep,
    column_key_smoking,
    column_key_alcohol,
    column_key_diet,
    column_key_sun,
    column_key_sex_hist,
    column_key_phys_act,
    column_key_vision,
    column_key_hearing,
    column_key_bp,
    column_key_a_stiff,
    column_key_anthro,
    column_key_spiro,
    column_key_bmd,
    column_key_imd,
    column_key_blood,
    column_key_blood_count,
    column_key_urine,
    column_key_enviro
  )

# remove duplicated eid rows
column_key <- column_key[!duplicated(column_key), ]

# check column key has all variables
nrow(column_key) == ncol(dat)

save(dat,
     column_key, 
     file = "/lustre/home/2311110213/UKB_YiFan/cov_data/ukb_all_exposures.RData")

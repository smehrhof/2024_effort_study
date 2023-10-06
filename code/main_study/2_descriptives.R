####################################################################################
###############--------------- DESCRIPTIVE STATISTICS ---------------###############
####################################################################################

### In this script: 
# (1) Demographics
# (2) Psychiatric comorbidities
# (3) Task metrics
# (4) Questionnaire measures

# Set working directory
here::i_am("github/effort-study/code/main_study/data_processing.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")

# source datasets
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")
main_data_excl <- readRDS("data/processed_data/main_study/online_data_excl.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate)

### (1) Demographics -----------------------------------------------

# Included sample

main_data$demographics %>% 
  summarise(N=n(), N_perc = n()/994, 
            mean_age = mean(age), sd_age = sd(age), 
            median_ses = median(ses), iqr_upper_ses = quantile(ses, 0.25), iqr_lower_ses = quantile(ses, 0.75))

main_data$demographics %>% 
  tabyl(gender)

main_data$demographics %>% 
  tabyl(ethnicity)

# Excluded sample

main_data_excl$demographics %>% 
  summarise(N=n(), N_perc = n()/994, 
            mean_age = mean(age), sd_age = sd(age), 
            median_ses = median(ses), iqr_upper_ses = quantile(ses, 0.25), iqr_lower_ses = quantile(ses, 0.75))

main_data_excl$demographics %>% 
  tabyl(gender)

main_data_excl$demographics %>% 
  tabyl(ethnicity)

### (2) Psychiatric comorbidities -----------------------------------------------

main_data$demographics %>% 
  summarise(N_psych_neurdev = sum(psych_neurdev), 
            N_psych_neurdev_prec = sum(psych_neurdev)/n())

# Depression
main_data$demographics$psych_neurdev_condition %>% 
  str_subset("Major depressive disorder|depression|Depression|MDD|mdd") %>% 
  length()

# Anxiety
main_data$demographics$psych_neurdev_condition %>% 
  str_subset("Social anxiety disorder|Generalised anxiety disorder") %>% 
  length()

# AHDH
main_data$demographics$psych_neurdev_condition %>% 
  str_subset("Attention deficit/hyperactivity disorder|ADHD|adhd") %>% 
  length()

# Anti-depressant use
main_data$demographics %>% 
  summarise(N_antid = sum(antidepressant, na.rm = TRUE), 
            N_antid_prec = sum(antidepressant, na.rm = TRUE)/n())

### (3) Task metrics -----------------------------------------------

# Start time
main_data$game_meta %>%
  # temporarily change all days to the same, to get mean time
  mutate(start_time = update(start_time, year=2000, month=1, mday=1)) %>% 
  summarise(mean_time = mean(start_time), sd_time_min = sd(start_time)/60)

# Duration
main_data$game_meta %>%
  mutate(time_diff = abs(difftime(start_time, end_time))) %>%
  summarise(mean_time_diff = mean(time_diff), sd_time_diff = sd(time_diff), 
            min_time_diff = min(time_diff), max_time_diff = max(time_diff))
  
# Mean clicking calibration
main_data$game_meta %>%
  summarise(mean_cali = mean(clicking_calibration), sd_cali = sd(clicking_calibration), 
            min_cali = min(clicking_calibration), max_cali = max(clicking_calibration))

### (4) Questionnaire measures -----------------------------------------------

# Psychiatric
main_data$questionnaire %>% 
            # SHAPS
  summarise(mean_shaps = mean(shaps_sumScore), sd_shaps = sd(shaps_sumScore), 
            min_shaps = min(shaps_sumScore), max_shaps = max(shaps_sumScore),
            # DARS
            mean_dars = mean(dars_sumScore), sd_dars = sd(dars_sumScore), 
            min_dars = min(dars_sumScore), max_dars = max(dars_sumScore),
            # AES
            mean_aes = mean(aes_sumScore), sd_aes = sd(aes_sumScore), 
            min_aes = min(aes_sumScore), max_aes = max(aes_sumScore)) %>% 
  print(width = Inf)

# Circadian 
main_data$questionnaire %>% 
            # MEQ
  summarise(mean_meq = mean(meq_sumScore), sd_meq = sd(meq_sumScore), 
            min_meq = min(meq_sumScore), max_meq = max(meq_sumScore),
            # MCTQ
            mean_mctq = seconds_to_period(mean(period_to_seconds(hm(mctq_MSF_SC)), na.rm = TRUE)), 
            sd_mctq = seconds_to_period(sd(period_to_seconds(hm(mctq_MSF_SC)), na.rm = TRUE)), 
            min_mctq = seconds_to_period(min(period_to_seconds(hm(mctq_MSF_SC)), na.rm = TRUE)), 
            max_mctq = seconds_to_period(max(period_to_seconds(hm(mctq_MSF_SC)), na.rm = TRUE))) %>% 
  print(width = Inf)

# Metabolic
main_data$questionnaire %>% 
            # BMI
  summarise(mean_bmi = mean(bmi_result, na.rm = TRUE), sd_bmi = sd(bmi_result, na.rm = TRUE), 
            min_bmi = min(bmi_result, na.rm = TRUE), max_bmi = max(bmi_result, na.rm = TRUE),
            # FINDRISC
            mean_findrisc = mean(findrisc_sumScore), sd_findrisc = sd(findrisc_sumScore), 
            min_findrisc = min(findrisc_sumScore), max_findrisc = max(findrisc_sumScore)) %>% 
  print(width = Inf)








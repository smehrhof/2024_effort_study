####################################################################################
###############--------------- DESCRIPTIVE STATISTICS ---------------###############
####################################################################################

### In this script: 
# (1) Demographics
# (2) Psychiatric questionnaires

# Set working directory
here::i_am("github/effort-study/code/test_retest/1_descriptives.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")

# source datasets
retest_data <- readRDS("data/processed_data/test_retest/retest_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate, purrr, magrittr)

### (1) Demographics -----------------------------------------------

retest_data$demographics %>% 
  summarise(N=n(), N_perc = (n()/30)*100, 
            mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age),
            median_ses = median(ses), iqr_upper_ses = quantile(ses, 0.25), iqr_lower_ses = quantile(ses, 0.75))

retest_data$demographics %>% 
  tabyl(gender)

retest_data$demographics %>% 
  tabyl(ethnicity)


### (2) Psychiatric comorbidities -----------------------------------------------

# Included sample

retest_data$demographics %>% 
  summarise(N_psych_neurdev = sum(psych_neurdev), 
            N_psych_neurdev_prec = sum(psych_neurdev)/n())

retest_data$demographics %>% 
  filter(psych_neurdev == 1) %>% 
  select(psych_neurdev_condition)





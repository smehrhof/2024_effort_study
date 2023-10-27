################################################################################
###############--------------- DATA PREPROCESSING ---------------###############
################################################################################

### In this script: 
##> Main data
# (1) Read-in and combine data
# (2) Parse data
# (3) Make modification according to recruitment notes
# (4) Questionnaire data processing 
# (5) Pre-registered data exclusion
# (6) Data monitoring
# (7) Game based exclusion
# (8) Save cleaned data
##> Circadian follow-up data
# (9) Read-in and parse data
# (11) Questionnaire data processing 
# (12) Pre-registered data exclusion
# (13)
# (14) Save cleaned data

# Set working directory
here::i_am("github/effort-study/code/analyses/1_data_processing.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/parsing_fun.R")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, purrr, magrittr)

##> Main data -----------------------------------------------
### (1) Read-in and combine data -----------------------------------------------

# JATOS data

# List data file names
data_files <- list.files(path = here::here("data/raw_data/main_study"), 
                         recursive = TRUE, pattern = ".txt", full.names = TRUE)

all_jatos_dat <- list ()

for(i in seq_along(data_files)){
  jatos_txt_file <- data_files[i]
  
  
  if(grepl("/female_noMH_18-24_s1_extra2.txt", jatos_txt_file)){
    data_5f54d772f5bba984fdf8676e <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(jatos_txt_file, encoding="UTF-8", warn=F), collapse = ',')))
    
    data_5f54d772f5bba984fdf8676e[[2]]$prolific_id[!is.na(data_5f54d772f5bba984fdf8676e[[2]]$prolific_id)] <- "5f54d772f5bba984fdf8676e"
    data_5f54d772f5bba984fdf8676e[[2]]$study_id[!is.na(data_5f54d772f5bba984fdf8676e[[2]]$study_id)] <- "6311248c26a9fa23037c9f0b"
    data_5f54d772f5bba984fdf8676e[[2]]$session_id[!is.na(data_5f54d772f5bba984fdf8676e[[2]]$session_id)] <- "63125d42a097a3fdc02f2624"
    
    all_jatos_dat <- append(all_jatos_dat, data_5f54d772f5bba984fdf8676e)
  } else if(grepl("/female_withMH_55_s6_extra.txt", jatos_txt_file)){
    data_63012780a9852e8d7977a5fd <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(jatos_txt_file, encoding="UTF-8", warn=F), collapse = ',')))
    
    data_63012780a9852e8d7977a5fd[[1]] <- tibble::add_column(data_63012780a9852e8d7977a5fd[[1]], 
                                                             "prolific_id" = rep("63012780a9852e8d7977a5fd", nrow(data_63012780a9852e8d7977a5fd[[1]])), .after = 7)
    data_63012780a9852e8d7977a5fd[[1]] <- tibble::add_column(data_63012780a9852e8d7977a5fd[[1]], 
                                                             "study_id" = rep("63012780a9852e8d7977a5fd", nrow(data_63012780a9852e8d7977a5fd[[1]])), .after = 8)
    data_63012780a9852e8d7977a5fd[[1]] <- tibble::add_column(data_63012780a9852e8d7977a5fd[[1]], 
                                                             "session_id" = rep("63012780a9852e8d7977a5fd", nrow(data_63012780a9852e8d7977a5fd[[1]])), .after = 9)
    
    data_63012780a9852e8d7977a5fd[[3]] <- tibble::add_column(data_63012780a9852e8d7977a5fd[[3]], 
                                                             "prolific_id" = rep("63012780a9852e8d7977a5fd", nrow(data_63012780a9852e8d7977a5fd[[3]])), .after = 7)
    data_63012780a9852e8d7977a5fd[[3]] <- tibble::add_column(data_63012780a9852e8d7977a5fd[[3]], 
                                                             "study_id" = rep("63012780a9852e8d7977a5fd", nrow(data_63012780a9852e8d7977a5fd[[3]])), .after = 8)
    data_63012780a9852e8d7977a5fd[[3]] <- tibble::add_column(data_63012780a9852e8d7977a5fd[[3]], 
                                                             "session_id" = rep("63012780a9852e8d7977a5fd", nrow(data_63012780a9852e8d7977a5fd[[3]])), .after = 9)
    
    all_jatos_dat <- append(all_jatos_dat, data_63012780a9852e8d7977a5fd)
  } else if(grepl("/male_noMH_55_s10.txt", jatos_txt_file)){
    group_data <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(jatos_txt_file, encoding="UTF-8", warn=F), collapse = ',')))
    group_data <- group_data[-4]
    
    all_jatos_dat <- append(all_jatos_dat, group_data)   
  } else if(grepl("/male_noMH_55_s13_extra3.txt", jatos_txt_file)){
    group_data <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(jatos_txt_file, encoding="UTF-8", warn=F), collapse = ',')))
    group_data <- group_data[1]
    
    all_jatos_dat <- append(all_jatos_dat, group_data)   
  } else if(grepl("/female_noMH_35-44_s12.txt", jatos_txt_file)){
    group_data <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(jatos_txt_file, encoding="UTF-8", warn=F), collapse = ',')))
    group_data <- group_data[-4]
    
    all_jatos_dat <- append(all_jatos_dat, group_data)   
  } else {
    group_data <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(jatos_txt_file, encoding="UTF-8", warn=F), collapse = ',')))
    
    all_jatos_dat <- append(all_jatos_dat, group_data)   
  }
  
}

# Prolific data

# List data file names
meta_data_files <- list.files(path = here::here("data/raw_data/main_study"), 
                              recursive = TRUE, pattern = ".csv", full.names = TRUE)
meta_data <- lapply(meta_data_files, read.csv)
meta_data <- data.table::rbindlist(meta_data) %>% as_tibble()

meta_data %<>% 
  rowwise() %>% 
  mutate("Participant.id" = id_shuffle(Participant.id)) 

### (2) Parse data -------------------------------------------------------------

online_data <- parsing_fun(file = all_jatos_dat, 
                           study_part = "online",
                           demographic_dat = TRUE,
                           task_dat = TRUE,
                           modelling_dat = TRUE,
                           questionnaire_dat = TRUE,
                           raw_data = TRUE, 
                           exclude_subjs = c("5a47c383e074d600013912cc|369450"))


### (3) Make modification according to recruitment notes -----------------------

### For the first three subjects that completed the study with an extra link, the findrisc response 1
# (age) is not recorded because data wasn't transferred from the demographic data

findrisc_na_id <- online_data$questionnaire[is.na(online_data$questionnaire$findrisc_response_1),]$subj_id

for(i in seq_along(findrisc_na_id)){
  subj_age <- online_data$demographics[online_data$demographics$subj_id == findrisc_na_id[i],]$age
  online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],]$findrisc_response_raw_1 <- subj_age
  
  if(subj_age < 45){
    online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],]$findrisc_response_1 <- 0
  } else if(subj_age >= 45 & subj_age < 55){
    online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],]$findrisc_response_1 <- 2
  } else if(subj_age >= 55 & subj_age < 65){
    online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],]$findrisc_response_1 <- 3
  } else if(subj_age >= 65){
    online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],]$findrisc_response_1 <- 4
  }
  sumScore <- sum(online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],][167:174])
  online_data$questionnaire[online_data$questionnaire$subj_id == findrisc_na_id[i],]$findrisc_sumScore <- sumScore
}

### Some subjects messaged me after the study to correct answers during the questionnaire

# Subject 6154918f83e8ca861b23ea04: wrote PM instead if AM on first workdays question
# id_shuffle("6154918f83e8ca861b23ea04")
online_data$questionnaire[online_data$questionnaire$subj_id == "848619531f",]$mctq_response_raw_2 <- "12:30 AM"
online_data$questionnaire[online_data$questionnaire$subj_id == "848619531f",]$mctq_response_2 <- "00:30"

# Subject 5aff33bae55f90000139f664: wrong annual income: change 1300 to 13000
# id_shuffle("5aff33bae55f90000139f664")
online_data$demographics[online_data$demographics$subj_id == "efb5a3f53a",]$income <- "13000 GBP"

# Subject 610024d9cd49f604ad283e11: change first four SHAPS answers from strongly disagree to strongly agree
online_data$questionnaire[online_data$questionnaire$subj_id == "c0d6120d49",]$shaps_response_1 <- 0

online_data$questionnaire[online_data$questionnaire$subj_id == "c0d6120d49",]$shaps_response_2 <- 0

online_data$questionnaire[online_data$questionnaire$subj_id == "c0d6120d49",]$shaps_response_3 <- 0

online_data$questionnaire[online_data$questionnaire$subj_id == "c0d6120d49",]$shaps_response_4 <- 0

# Subject 6234a3d5cd2f63c705e5842d: change physical activity questions
# id_shuffle("6234a3d5cd2f63c705e5842d")
online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_response_1 <- "None"
online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_response_2 <- NA
online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_response_4 <- "1:30"

online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_vigorous_MET <- 0
online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_moderate_MET <- 
  4 * period_to_seconds(hm(online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_response_4))/60 * 
  as.numeric(online_data$questionnaire[online_data$questionnaire$subj_id == "c4d62a3d35",]$ipaq_response_3)


### (4) Questionnaire data processing ------------------------------------------

## BMI - unrealistic values?

# Make completely unrealistic values NA
online_data$questionnaire %<>% 
  rowwise() %>%
  mutate(across(bmi_response_1:bmi_result, ~ case_when(is.infinite(bmi_result) ~ NA,
                                                       is.nan(bmi_result) ~ NA,
                                                       bmi_result <= 12 ~ NA, 
                                                       bmi_result >= 200 ~ NA, 
                                                       bmi_response_1 > 250 ~ NA,
                                                       bmi_response_1 < 140 ~ NA,
                                                       bmi_response_2 < 20 ~ NA,
                                                       bmi_response_2 > 600 ~ NA,
                                                       .default = .))) %>%
  # turn rows_df back into normal tibble
  ungroup()

# Look at participants that have a BMI under or over 2sd from sample
online_data$questionnaire %>% 
  filter(bmi_result > (26.87537 + 3 * 6.270634)) %>% 
  select(bmi_response_1, bmi_response_2, bmi_result) %>% print(n = Inf)

## IPAQ

# Data cleaning according to manual 
# exclude outliers (combined minutes > 960)

for(subj in seq_along(online_data$questionnaire$subj_id)){
  if(all(!is.na(online_data$questionnaire[subj,c("ipaq_response_2", "ipaq_response_4", "ipaq_response_6")]))){
    
    # exclude data points indicating more than 16h of exercise per day
    if(sum(period_to_seconds(hm(online_data$questionnaire[subj,c("ipaq_response_2", "ipaq_response_4", "ipaq_response_6")])) / 60) > 960){
      online_data$questionnaire[subj,189:199] <- NA
    } else {
      for(i in c("ipaq_response_2", "ipaq_response_4", "ipaq_response_6")){
        
        # round exercise < 10 minutes down to 0
        if((period_to_seconds(hm(online_data$questionnaire[subj,i])) / 60) < 10){
          online_data$questionnaire[subj,i] <- "00:00"
          
          # round exercise > 180 minutes down to 3h
        } else if((period_to_seconds(hm(online_data$questionnaire[subj,i])) / 60) > 180){
          online_data$questionnaire[subj,i] <- "03:00"
          
          # recalculate scores
          online_data$questionnaire[subj,"ipaq_walking_MET"] <- 3.3 * as.numeric(online_data$questionnaire[subj,"ipaq_response_5"]) * (period_to_seconds(hm(online_data$questionnaire[subj,"ipaq_response_6"])) / 60)
          online_data$questionnaire[subj,"ipaq_moderate_MET"] <- 4 * as.numeric(online_data$questionnaire[subj,"ipaq_response_3"]) * (period_to_seconds(hm(online_data$questionnaire[subj,"ipaq_response_4"])) / 60)
          online_data$questionnaire[subj,"ipaq_vigorous_MET"] <- 8 * as.numeric(online_data$questionnaire[subj,"ipaq_response_1"]) * (period_to_seconds(hm(online_data$questionnaire[subj,"ipaq_response_2"])) / 60)
          online_data$questionnaire[subj,"ipaq_sumScore"] <- sum(online_data$questionnaire[subj,c("ipaq_walking_MET", "ipaq_moderate_MET", "ipaq_vigorous_MET")])
        } 
      } 
    }
  }
}


## MCTQ

# Data cleaning according to Roenneberg et al. (2012)

### MCTQ data exclusion based on Roenneberg et al. (2012)
# exclude subjects  who set an alarm on free days and who have extreme values 
# (corrected MSF after midday)

MCTQ_exclusion <- online_data$questionnaire[period_to_seconds(hm(online_data$questionnaire$mctq_MSF_SC)) > 12*60*60,1]
online_data$questionnaire[online_data$questionnaire$subj_id %in% MCTQ_exclusion$subj_id,
                          grep("mctq", colnames(online_data$questionnaire))] <- NA

### (5) Pre-registered data exclusion ------------------------------------------

exclude <- c()

# Catch questions
# "catch_question_pass_hard_1" coded wrong!!
online_data$questionnaire$catch_question_pass_hard_1 <- abs(online_data$questionnaire$catch_question_pass_hard_1-1)
catch_easy_exclude <- c()
catch_hard_exclude <- c()
for(i in 1:994){
  # both easy questions have to be passed
  if(online_data$questionnaire$catch_question_pass_easy_2[i] & online_data$questionnaire$catch_question_pass_easy_3[i]){
    catch_easy_exclude[i] <- 0
  } else {
    catch_easy_exclude[i] <- 1
  }
  # at least one hard catch question has to be passed
  if(online_data$questionnaire$catch_question_pass_hard_1[i] | online_data$questionnaire$catch_question_pass_hard_4[i]){
    catch_hard_exclude[i] <- 0
  } else {
    catch_hard_exclude[i] <- 1
  }
}

exclude <- c(exclude, online_data$questionnaire$subj_id[catch_hard_exclude == 1 | catch_easy_exclude == 1])

# English level
exclude <- c(exclude, online_data$questionnaire$subj_id[online_data$demographics$english == "A1/A2" | 
                                                          online_data$demographics$english == "B1"])

# Severe neurological condition
exclude <- c(exclude, online_data$questionnaire$subj_id[grepl("Epilepsy|Multiple Sclerosis|astrocytoma",
                                                              online_data$demographics$neurological_condition)])

# All offers rejected in task
exclude <- c(exclude, online_data$questionnaire$subj_id[aggregate(choice ~ subj_id,
                                                                  FUN = sum, data = online_data$game[online_data$game$phase == "game",])[,2] == 0])


### (6) Data monitoring --------------------------------------------------------

# Game strategy questions
online_data$questionnaire$game_response_text_1[online_data$questionnaire$game_response_1 == 1]
online_data$questionnaire$game_response_text_2[online_data$questionnaire$game_response_2 == 1]

# Final questionnaire
online_data$questionnaire$technical_comments[!is.na(online_data$questionnaire$technical_comments)]

online_data$questionnaire$layout_comments[!is.na(online_data$questionnaire$layout_comments)]


### (7) Game based exclusion ---------------------------------------------------

# Clicking capacity has to be at least 7 so that subjects need to click more than once
# one each level 
exclude <- c(exclude, unique(online_data$game$subj_id[online_data$game$clicking_calibration < 7]))

# A big difference between the minimum and maximum clicking speed during calibration
# indicates that the calibration value was miss-estimated 

cali_diff <- aggregate(clicks ~ subj_id,
                       FUN = function(x) abs(x[1] - x[2]),
                       data = online_data$game[online_data$game$phase == "calibration" & 
                                                 online_data$game$trial != 1,])

cali_diff <- cali_diff[cali_diff$clicks > (mean(cali_diff$clicks) + 3*sd(cali_diff$clicks)),]

exclude <- c(exclude, cali_diff$subj_id)

# Change in max clicking capacity
clicking_calibrations_pre_max <- aggregate(clicks ~ subj_id, FUN = function(x) max(x[1:3]), 
                                           data = online_data$game[online_data$game$phase == "calibration",])

clicking_calibrations_post_max <- aggregate(clicks ~ subj_id, FUN = function(x) max(x[4]), 
                                            data = online_data$game[online_data$game$phase == "calibration",])

change_mean <- mean(clicking_calibrations_pre_max$clicks - clicking_calibrations_post_max$clicks) 
change_sd <- sd(clicking_calibrations_pre_max$clicks - clicking_calibrations_post_max$clicks) 

exclude <- c(exclude, clicking_calibrations_pre_max[clicking_calibrations_pre_max$clicks - clicking_calibrations_post_max$clicks <
                                                      change_mean - 3*change_sd,]$subj_id)


# Data exclusion summary: 
# 36 excluded in total
# 12 due to catch questions
# 2 due to English level
# 14 due to severe neurological condition
# 3 due to game not working because of too low clicking calibration
# 3 due to too big of a discrepancy between calibration trials
# 1 due to too large of a difference in clicking capacity pre to post game

online_data_excl <- online_data
# dataset of excluded participants
online_data_excl$demographics <- online_data$demographics[online_data$demographics$subj_id %in% exclude,] %>% as_tibble()
online_data_excl$game <- online_data$game[online_data$game$subj_id %in% exclude,] %>% as_tibble()
online_data_excl$game_meta <- online_data$game_meta[online_data$game_meta$subj_id %in% exclude,] %>% as_tibble()
online_data_excl$modelling_data <- online_data$modelling_data[online_data$modelling_data$subjID %in% exclude,] %>% as_tibble()
online_data_excl$questionnaire <- online_data$questionnaire[online_data$questionnaire$subj_id %in% exclude,] %>% as_tibble()
# dataset of included participants
online_data$demographics <- online_data$demographics[!online_data$demographics$subj_id %in% exclude,] %>% as_tibble()
online_data$game <- online_data$game[!online_data$game$subj_id %in% exclude,] %>% as_tibble()
online_data$game_meta <- online_data$game_meta[!online_data$game_meta$subj_id %in% exclude,] %>% as_tibble()
online_data$modelling_data <- online_data$modelling_data[!online_data$modelling_data$subjID %in% exclude,] %>% as_tibble()
online_data$questionnaire <- online_data$questionnaire[!online_data$questionnaire$subj_id %in% exclude,] %>% as_tibble()


### (8) Save cleaned data ------------------------------------------------------
setwd(here::here())
saveRDS(online_data, "data/processed_data/main_study/online_data.RDS")
saveRDS(online_data_excl, "data/processed_data/main_study/online_data_excl.RDS")
saveRDS(meta_data, "data/processed_data/main_study/online_meta_data.RDS")


##> Circadian follow-up data ------------------------------------------------------
# (9) Read-in and parse data ------------------------------------------------------

# JATOS data

# List data file names
data_files <- list.files(path = here::here("data/raw_data/chrono_followup/main"), 
                         recursive = TRUE, pattern = ".txt", full.names = TRUE)

circadian_followup_data <- parsing_fun(file = data_files, 
                                       study_part = "chronotype_followup",
                                       raw_data = TRUE)

# add missed catch question columns with NA to match main dataset
circadian_followup_data$questionnaire <- cbind(circadian_followup_data$questionnaire, 
                                               "catch_question_pass_hard_4" = NA,
                                               "catch_question_score_hard_4" = NA)

# Prolific data

# List data file names
meta_data_files <- list.files(path = here::here("data/raw_data/chrono_followup/main"), 
                              recursive = TRUE, pattern = ".csv", full.names = TRUE)
meta_followup_data <- lapply(meta_data_files, read.csv)
meta_followup_data <- data.table::rbindlist(meta_followup_data) %>% as_tibble()

# Screening data

data_files <- list.files(path = here::here("data/raw_data/chrono_followup/screening"), 
                         recursive = TRUE, pattern = ".txt", full.names = TRUE)

circadian_screening_data <- parsing_fun(file = data_files, 
                                       study_part = "screening",
                                       raw_data = TRUE)

# Merge screening questionnaires with main data

circadian_followup_data$questionnaire %<>% 
  left_join(circadian_screening_data$questionnaire, 
            by = c("subj_id" = "subj_id"))


# (10) Questionnaire data processing  ------------------------------------------------------

## BMI - unrealistic values?
# height: 
which(circadian_followup_data$questionnaire$bmi_response_1 < 150 |
        circadian_followup_data$questionnaire$bmi_response_1 > 200)

circadian_followup_data$questionnaire %>% 
  select(bmi_response_1:bmi_result) %>% 
  filter(bmi_response_1 <  150 | bmi_response_1 > 200)

# weight: 
which(circadian_followup_data$questionnaire$bmi_response_2 < 30 |
        circadian_followup_data$questionnaire$bmi_response_2 > 150)

# any unrealistic BMIs left?
min(circadian_followup_data$questionnaire$bmi_result, na.rm = TRUE)
max(circadian_followup_data$questionnaire$bmi_result, na.rm = TRUE)

## IPAQ

# Data cleaning according to manual 
# exclude outliers (combined minutes > 960)

for(subj in seq_along(circadian_followup_data$questionnaire$subj_id)){
  if(all(!is.na(circadian_followup_data$questionnaire[subj,c("ipaq_response_2", "ipaq_response_4", "ipaq_response_6")]))){
    
    # exclude data points indicating more than 16h of exercise per day
    if(sum(period_to_seconds(hm(circadian_followup_data$questionnaire[subj,c("ipaq_response_2", "ipaq_response_4", "ipaq_response_6")])) / 60) > 960){
      circadian_followup_data$questionnaire[subj, c("ipaq_response_1", "ipaq_response_2", "ipaq_response_3", 
                                                    "ipaq_response_4", "ipaq_response_5", "ipaq_response_6", 
                                                    "ipaq_response_7", "ipaq_walking_MET", "ipaq_moderate_MET",
                                                    "ipaq_vigorous_MET", "ipaq_sumScore")] <- NA
      
    } else {
      for(i in c("ipaq_response_2", "ipaq_response_4", "ipaq_response_6")){
        
        # round exercise < 10 minutes down to 0
        if((period_to_seconds(hm(circadian_followup_data$questionnaire[subj,i])) / 60) < 10){
          circadian_followup_data$questionnaire[subj,i] <- "00:00"
          
          # round exercise > 180 minutes down to 3h
        } else if((period_to_seconds(hm(circadian_followup_data$questionnaire[subj,i])) / 60) > 180){
          circadian_followup_data$questionnaire[subj,i] <- "03:00"
          
          # recalculate scores
          circadian_followup_data$questionnaire[subj,"ipaq_walking_MET"] <- 3.3 * as.numeric(circadian_followup_data$questionnaire[subj,"ipaq_response_5"]) * (period_to_seconds(hm(circadian_followup_data$questionnaire[subj,"ipaq_response_6"])) / 60)
          circadian_followup_data$questionnaire[subj,"ipaq_moderate_MET"] <- 4 * as.numeric(circadian_followup_data$questionnaire[subj,"ipaq_response_3"]) * (period_to_seconds(hm(circadian_followup_data$questionnaire[subj,"ipaq_response_4"])) / 60)
          circadian_followup_data$questionnaire[subj,"ipaq_vigorous_MET"] <- 8 * as.numeric(circadian_followup_data$questionnaire[subj,"ipaq_response_1"]) * (period_to_seconds(hm(circadian_followup_data$questionnaire[subj,"ipaq_response_2"])) / 60)
          circadian_followup_data$questionnaire[subj,"ipaq_sumScore"] <- sum(circadian_followup_data$questionnaire[subj,c("ipaq_walking_MET", "ipaq_moderate_MET", "ipaq_vigorous_MET")])
        } 
      } 
    }
  }
}

## MCTQ

### MCTQ data exclusion based on Roenneberg et al. (2012)
# exclude subjects  who set an alarm on free days and who have extreme values 
# (corrected MSF after midday)

MCTQ_exclusion <- circadian_followup_data$questionnaire[period_to_seconds(hm(circadian_followup_data$questionnaire$mctq_MSF_SC)) > 12*60*60,1]
circadian_followup_data$questionnaire[circadian_followup_data$questionnaire$subj_id %in% MCTQ_exclusion,
                          grep("mctq", colnames(circadian_followup_data$questionnaire))] <- NA

# (11) Pre-registered data exclusion ------------------------------------------------------

exclude <- c()

# Catch questions
# "catch_question_pass_hard_1" coded wrong!!
circadian_followup_data$questionnaire$catch_question_pass_hard_1 <- abs(circadian_followup_data$questionnaire$catch_question_pass_hard_1-1)
catch_easy_exclude <- c()
catch_hard_exclude <- c()
for(i in 1:38){
  # both easy questions have to be passed
  if(circadian_followup_data$questionnaire$catch_question_pass_easy_2[i] & circadian_followup_data$questionnaire$catch_question_pass_easy_3[i]){
    catch_easy_exclude[i] <- 0
  } else {
    catch_easy_exclude[i] <- 1
  }
  # hard catch question has to be passed
  if(circadian_followup_data$questionnaire$catch_question_pass_hard_1[i]){
    catch_hard_exclude[i] <- 0
  } else {
    catch_hard_exclude[i] <- 1
  }
}

exclude <- c(exclude, circadian_followup_data$questionnaire$subj_id[catch_hard_exclude == 1 | catch_easy_exclude == 1])

# All offers rejected in task
exclude <- c(exclude, circadian_followup_data$questionnaire$subj_id[aggregate(choice ~ subj_id,
                                                                              FUN = sum, data = circadian_followup_data$game[circadian_followup_data$game$phase == "game",])[,2] == 0])

# Clicking capacity has to be at least 7 so that subjects need to click more than once
# one each level 
exclude <- c(exclude, unique(circadian_followup_data$game$subj_id[circadian_followup_data$game$clicking_calibration < 7]))

# A big difference between the minimum and maximum clicking speed during calibration
# indicates that the calibration value was miss-estimated 
cali_diff <- aggregate(clicks ~ subj_id,
                       FUN = function(x) abs(x[1] - x[2]),
                       data = circadian_followup_data$game[circadian_followup_data$game$phase == "calibration" & 
                                                             circadian_followup_data$game$trial != 1,])
# (from main data set processing)
cali_diff <- cali_diff[cali_diff$clicks >  30.2751,]

exclude <- c(exclude, cali_diff$subj_id)

# Change in max clicking capacity
clicking_calibrations_pre_max <- aggregate(clicks ~ subj_id, FUN = function(x) max(x[1:3]), 
                                           data = circadian_followup_data$game[circadian_followup_data$game$phase == "calibration",])

clicking_calibrations_post_max <- aggregate(clicks ~ subj_id, FUN = function(x) max(x[4]), 
                                            data = circadian_followup_data$game[circadian_followup_data$game$phase == "calibration",])

change_mean <- mean(clicking_calibrations_pre_max$clicks - clicking_calibrations_post_max$clicks) 
change_sd <- sd(clicking_calibrations_pre_max$clicks - clicking_calibrations_post_max$clicks) 

exclude <- c(exclude, clicking_calibrations_pre_max[clicking_calibrations_pre_max$clicks - clicking_calibrations_post_max$clicks <
                                                      -134.5161,]$subj_id)

# Make everything a tibble
circadian_followup_data$demographics <- as_tibble(circadian_followup_data$demographics)
circadian_followup_data$game <- as_tibble(circadian_followup_data$game)
circadian_followup_data$game_meta <- as_tibble(circadian_followup_data$game_meta)
circadian_followup_data$modelling_data <- as_tibble(circadian_followup_data$modelling_data)
circadian_followup_data$questionnaire <- as_tibble(circadian_followup_data$questionnaire)


# (12) Save cleaned data ------------------------------------------------------
setwd(here::here())
saveRDS(circadian_followup_data, "data/processed_data/chrono_followup/circadian_followup_data.RDS")

























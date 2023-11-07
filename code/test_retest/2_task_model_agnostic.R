##########################################################################################
###############--------------- MODEL AGNOSTIC TASK ANALYSES ---------------###############
##########################################################################################

### In this script: 
# (1) Mixed-effects ANOVA

# Set working directory
here::i_am("github/effort-study/code/test_retest/2_task_model_agnostic.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")

# source dataset
retest_data <- readRDS("data/processed_data/test_retest/retest_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate, nlme)

### (1) Mixed-effects ANOVA -----------------------------------------------

# Session 1

choice_data <- retest_data$modelling_data$session_1 %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))
  
anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = choice_data))

effort_levels <- choice_data$effort_a %>% unique() %>% sort()
amount_levels <- choice_data$amount_a %>% unique() %>% sort()

# Post hoc anovas
# per reward level
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[1])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[2])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[3])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[4])))

# per effort level
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[1])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[2])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[3])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[4])))

# Session 2

choice_data <- retest_data$modelling_data$session_2 %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))

anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = choice_data))

effort_levels <- choice_data$effort_a %>% unique() %>% sort()
amount_levels <- choice_data$amount_a %>% unique() %>% sort()

# Post hoc anovas
# per reward level
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[1])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[2])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[3])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[4])))

# per effort level
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[1])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[2])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[3])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[4])))
 










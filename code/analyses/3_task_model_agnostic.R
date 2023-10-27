##########################################################################################
###############--------------- MODEL AGNOSTIC TASK ANALYSES ---------------###############
##########################################################################################

### In this script: 
# (1) Mixed-effects ANOVA
# (2) Task descriptives
# (3) Plotting

# Set working directory
here::i_am("github/effort-study/code/main_study/3_task_model_agnostic.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/plot_funs.R")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate, nlme)

# CBU color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

### (1) Mixed-effects ANOVA -----------------------------------------------

choice_data <- main_data$modelling_data %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))
  
anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = choice_data))

# Post hoc anovas
# per reward level
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(amount_a)[1])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(amount_a)[2])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(amount_a)[3])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(amount_a)[4])))

# per effort level
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(effort_a)[1])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(effort_a)[2])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(effort_a)[3])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == sort(effort_a)[4])))


### (2) Task descriptives -----------------------------------------------

# Rate of accepted challenges
main_data$game %>% 
  filter(phase == "game") %>% 
  # accepted trials
  filter(choice == 1) %>% 
  group_by(subj_id) %>% 
  reframe(prop_accept = n() / 64) %>% 
  reframe(mean_prop_accept = mean(prop_accept)*100, 
          sd_prop_accept = sd(prop_accept)*100, 
          min_prop_accept = min(prop_accept), 
          max_prop_accept = max(prop_accept))

# Successrate of accepted challenges
main_data$game %>% 
  filter(phase == "game") %>% 
  # accepted trials
  filter(choice == 1) %>% 
  group_by(subj_id) %>% 
  reframe(prop_success = sum((goalClicks - clicks) <= 0) / n()) %>% 
  reframe(mean_prop_sucess = mean(prop_success)*100, 
          sd_prop_sucess = sd(prop_success)*100)

# Enjoyment ratings
main_data$questionnaire %>% 
  select(enjoyment) %>% 
  reframe(mean_enjoyment = mean(enjoyment), 
          sd_enjoyment = sd(enjoyment))


### (3) Plotting -----------------------------------------------

acceptance_plot <- task_plot(main_data$modelling_data, 
                             main_title = "", 
                             arrange_cols = 4, arrange_rows = 1)
acceptance_plot

pdf(file = here::here("output/figures/R_plots/task_agnostic.pdf"),  
    width = 10, # The width of the plot in cm (transformed to inches)
    height = 2.5) # The height of the plot in cm (transformed to inches)
par(mar=c(0,4,0.5,0.5))

acceptance_plot

dev.off()

# width 30,16
# height 5,36 cm



















###########################################################################################
###############--------------- ANALYSES OF CIRCADIAN EFFECTS ---------------###############
###########################################################################################

### In this script: 
# (1) Create grouping variables
# (2) Fit models
# (3) Run GLMs
# (4) Add follow-up data
# (5) Fit models with followup data
# (6) Run GLMs with followup data

# Set working directory
here::i_am("github/effort-study/code/main_study/7_circadian_analyses.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")

# adjust time stamps to summer time
main_data$game_meta %<>%
  mutate(start_time = start_time + 60*60,
         end_time = end_time + 60*60)

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, 
                 MatchIt, writexl, lubridate, magrittr, bayestestR, rstanarm)

# should models be fitted or loaded? 
run_models <- FALSE

### (1) Create grouping variables -----------------------------------------------

# Grouping by testing time: 
# am: 08:00am - 11:59am
# pm: 06:00pm - 09:59pm

main_data$game_meta %<>% 
  mutate(testing_group = case_when(update(start_time, year=2000, month=1, mday=1) > ymd_hms("2000-01-01 08:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) < ymd_hms("2000-01-01 11:59:59") ~ "am",
                                   update(start_time, year=2000, month=1, mday=1) > ymd_hms("2000-01-01 18:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) < ymd_hms("2000-01-01 21:59:59") ~ "pm", 
                                   .default = "none"))

# Grouping by chronotype
# early: MEQ > 58, MSFSC < 2:30am
# late: MEQ < 42 , MSFSC > 05:30am

main_data$questionnaire %<>% 
  mutate(chronotype = case_when(meq_sumScore > 58 & hm(mctq_MSF_SC) < hm("02:30") ~ "early",
                                meq_sumScore < 42 & hm(mctq_MSF_SC) > hm("05:30") ~ "late",
                                is.na(mctq_MSF_SC) ~ "NA",
                                   .default = "intermediate"))

main_data$questionnair %>% 
  janitor::tabyl(chronotype)

# Merge data
circadian_data <- list(main_data$demographics, main_data$game_meta, main_data$questionnaire) %>% 
  reduce(left_join, by = "subj_id") %>% 
  mutate(group =  case_when(testing_group == "am" & chronotype == "early" ~ "am_early",
                            testing_group == "am" & chronotype == "late" ~ "am_late",
                            testing_group == "pm" & chronotype == "early" ~ "pm_early",
                            testing_group == "pm" & chronotype == "late" ~ "pm_late",
                            .default = "none")) %>% 
  filter(group != "none") %>% 
  select(subj_id, age, gender, shaps_sumScore, dars_sumScore, aes_sumScore, 
         meq_sumScore, mctq_MSF_SC, findrisc_sumScore, bmi_result, ipaq_sumScore, 
         mdd_past, mdd_current, mdd_recurrent, testing_group, chronotype, group)

# Sub-sample sizes
circadian_data %>% 
  janitor::tabyl(group)

### (2) Fit models -----------------------------------------------

# Load models
m2_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m2_parabolic.stan")
m3_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m3_parabolic.stan")
m3_linear_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_linear/ed_m3_linear.stan")

### AM testing, early chronoytpe

task_am_early <- main_data$modelling_data %>% 
  filter(subjID %in% circadian_data$subj_id[circadian_data$group == "am_early"])

if(run_models){
model_dat_am_early <- model_preprocessing(raw_data = task_am_early,
                                 retest = FALSE,
                                 subjs = unique(task_am_early$subjID), 
                                 n_subj = length(unique(task_am_early$subjID)), 
                                 t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_early)[,2], 
                                 t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_early)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_am_early <- m3_parabolic_stan_model$sample(
    data = model_dat_am_early, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_am_early"
  )
  saveRDS(m3_para_fit_am_early, here::here("data/model_fits/chrono_followup/m3_para_fit_am_early.RDS"))  
} else {
  m3_para_fit_am_early <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_am_early.RDS"))
}


# Convergence check 
m3_para_check_am_early <- convergence_check(m3_para_fit_am_early, 
                                   params = c("kE", "kR", "a"), 
                                   Rhat = TRUE, ess = TRUE,
                                   trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_am_early$Rhat
m3_para_check_am_early$ess
m3_para_check_am_early$trace_plot

# Get individual parameters

params_am_early <- get_params(subj_id = unique(task_am_early$subjID), 
                              model_fit = m3_para_fit_am_early, 
                              n_subj = 63, 
                              n_params = 3, 
                              param_names = c("kE", "kR", "a"))


### PM testing, early chronoytpe

task_pm_early <- main_data$modelling_data %>% 
  filter(subjID %in% circadian_data$subj_id[circadian_data$group == "pm_early"])

if(run_models){
model_dat_pm_early <- model_preprocessing(raw_data = task_pm_early,
                                          retest = FALSE,
                                          subjs = unique(task_pm_early$subjID), 
                                          n_subj = length(unique(task_pm_early$subjID)), 
                                          t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_early)[,2], 
                                          t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_early)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_pm_early <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_early, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_pm_early"
  )
  saveRDS(m3_para_fit_pm_early, here::here("data/model_fits/chrono_followup/m3_para_fit_pm_early.RDS"))
} else {
  m3_para_fit_pm_early <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_pm_early.RDS"))
}

# Convergence check 
m3_para_check_pm_early <- convergence_check(m3_para_fit_pm_early, 
                                            params = c("kE", "kR", "a"), 
                                            Rhat = TRUE, ess = TRUE,
                                            trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_pm_early$Rhat
m3_para_check_pm_early$ess
m3_para_check_pm_early$trace_plot

# Get individual parameters

params_pm_early <- get_params(subj_id = unique(task_pm_early$subjID), 
                              model_fit = m3_para_fit_pm_early, 
                              n_subj = length(unique(task_pm_early$subjID)), 
                              n_params = 3, 
                              param_names = c("kE", "kR", "a"))


### AM testing, late chronoytpe

task_am_late <- main_data$modelling_data %>% 
  filter(subjID %in% circadian_data$subj_id[circadian_data$group == "am_late"])

if(run_models){
model_dat_am_late <- model_preprocessing(raw_data = task_am_late,
                                         retest = FALSE,
                                         subjs = unique(task_am_late$subjID), 
                                         n_subj = length(unique(task_am_late$subjID)), 
                                         t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_late)[,2], 
                                         t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_late)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_am_late <- m3_parabolic_stan_model$sample(
    data = model_dat_am_late, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_am_late"
  )
  saveRDS(m3_para_fit_am_late, here::here("data/model_fits/chrono_followup/m3_para_fit_am_late.RDS"))
} else {
  m3_para_fit_am_late <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_am_late.RDS"))
}

# Convergence check 
m3_para_check_am_late <- convergence_check(m3_para_fit_am_late, 
                                           params = c("kE", "kR", "a"), 
                                           Rhat = TRUE, ess = TRUE,
                                           trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_am_late$Rhat
m3_para_check_am_late$ess
m3_para_check_am_late$trace_plot

# Get individual parameters

params_am_late <- get_params(subj_id = unique(task_am_late$subjID), 
                             model_fit = m3_para_fit_am_late, 
                             n_subj = length(unique(task_am_late$subjID)), 
                             n_params = 3, 
                             param_names = c("kE", "kR", "a"))



### PM testing, late chronoytpe

task_pm_late <- main_data$modelling_data %>% 
  filter(subjID %in% circadian_data$subj_id[circadian_data$group == "pm_late"])

if(run_models){
model_dat_pm_late <- model_preprocessing(raw_data = task_pm_late,
                                          retest = FALSE,
                                          subjs = unique(task_pm_late$subjID), 
                                          n_subj = length(unique(task_pm_late$subjID)), 
                                          t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_late)[,2], 
                                          t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_late)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_pm_late <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_late, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_pm_late"
  )
  saveRDS(m3_para_fit_pm_late, here::here("data/model_fits/chrono_followup/m3_para_fit_pm_late.RDS"))
} else {
  m3_para_fit_pm_late <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_pm_late.RDS"))
}

# Convergence check 
m3_para_check_pm_late <- convergence_check(m3_para_fit_pm_late, 
                                            params = c("kE", "kR", "a"), 
                                            Rhat = TRUE, ess = TRUE,
                                            trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_pm_late$Rhat
m3_para_check_pm_late$ess
m3_para_check_pm_late$trace_plot

# Get individual parameters

params_pm_late <- get_params(subj_id = unique(task_pm_late$subjID), 
                             model_fit = m3_para_fit_pm_late, 
                             n_subj = length(unique(task_pm_late$subjID)), 
                             n_params = 3, 
                             param_names = c("kE", "kR", "a"))


### Merge to one dataset for analyses

params_data <- bind_rows(params_am_early$individual_params, params_pm_early$individual_params, 
                         params_am_late$individual_params, params_pm_late$individual_params) %>%
  pivot_wider(names_from = parameter, 
              values_from = c(estimate:hdi_upper)) %>% 
  # add scales mean parameter estimates for Bayesian GLMs
  mutate(estimate_kE_scaled = rescale(estimate_kE), 
         estimate_kR_scaled = rescale(estimate_kR), 
         estimate_a_scaled = rescale(estimate_a))
  
circadian_data %<>%
  left_join(params_data, 
            by = c("subj_id" = "subj_id"))


### (3) Run GLMs -----------------------------------------------

# Effort sensitivity
kE_glm <- stan_glm(estimate_kE_scaled ~ testing_group * chronotype + age + gender, data = circadian_data, 
                                iter = 40000, seed = 123)
kE_glm$coefficients
hdi(kE_glm)

# Reward sensitivity
kR_glm <- stan_glm(estimate_kR_scaled ~ testing_group * chronotype + age + gender, data = circadian_data, 
                   iter = 40000, seed = 123)
kR_glm$coefficients
hdi(kR_glm)

# Choice bias
a_glm <- stan_glm(estimate_a_scaled ~ testing_group * chronotype + age + gender, data = circadian_data, 
                   iter = 40000, seed = 123)
a_glm$coefficients
hdi(a_glm)


### (4) Add follow-up data -----------------------------------------------

followup_data <- readRDS("data/processed_data/chrono_followup/circadian_followup_data.RDS")

# Grouping by testing time: 
# am: 08:00am - 11:59am
# pm: 06:00pm - 09:59pm

# adjust time stamps to summer time
followup_data$game_meta %<>%
  mutate(start_time = start_time + 60*60,
         end_time = end_time + 60*60)

followup_data$game_meta %<>% 
  mutate(testing_group = case_when(update(start_time, year=2000, month=1, mday=1) > ymd_hms("2000-01-01 08:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) < ymd_hms("2000-01-01 11:59:59") ~ "am",
                                   update(start_time, year=2000, month=1, mday=1) > ymd_hms("2000-01-01 18:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) < ymd_hms("2000-01-01 21:59:59") ~ "pm", 
                                   .default = "none"))

# Grouping by chronotype
# early: MEQ > 58, MSFSC < 2:30am
# late: MEQ < 42 , MSFSC > 05:30am

followup_data$questionnaire %<>% 
  mutate(chronotype = case_when(meq_sumScore > 58 & hm(mctq_MSF_SC) < hm("02:30") ~ "early",
                                meq_sumScore < 42 & hm(mctq_MSF_SC) > hm("05:30") ~ "late",
                                is.na(mctq_MSF_SC) ~ "NA",
                                .default = "intermediate"))


# Merge data
circadian_data_fu <- list(followup_data$demographics, followup_data$game_meta, followup_data$questionnaire) %>% 
  reduce(left_join, by = "subj_id") %>% 
  mutate(group =  case_when(testing_group == "am" & chronotype == "early" ~ "am_early",
                            testing_group == "am" & chronotype == "late" ~ "am_late",
                            testing_group == "pm" & chronotype == "early" ~ "pm_early",
                            testing_group == "pm" & chronotype == "late" ~ "pm_late",
                            .default = "none")) %>% 
  filter(group != "none") %>% 
  select(subj_id, age, gender, shaps_sumScore, dars_sumScore, aes_sumScore, 
         meq_sumScore, mctq_MSF_SC, findrisc_sumScore, bmi_result, ipaq_sumScore, 
         mdd_past, mdd_current, mdd_recurrent, testing_group, chronotype, group)

# Make new data set with eligible participants from main study and followup

circadian_data_all <- circadian_data %>% 
  select(subj_id:group) %>% 
  bind_rows(circadian_data_fu)

# Sub-sample sizes
circadian_data_all %>% 
  janitor::tabyl(group)

### (5) Fit models with followup data -----------------------------------------------

### AM testing, early chronoytpe
# no new participants -> use fit from above 

params_am_early_all <- params_am_early

### PM testing, early chronoytpe
# no new participants -> use fit from above 

task_pm_early_all <- bind_rows(main_data$modelling_data %>% 
                             filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_early"]), 
                           followup_data$modelling_data %>% 
                             filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_early"]))

if(run_models){
model_dat_pm_early_all <- model_preprocessing(raw_data = task_pm_early_all,
                                          retest = FALSE,
                                          subjs = unique(task_pm_early_all$subjID), 
                                          n_subj = length(unique(task_pm_early_all$subjID)), 
                                          t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_early_all)[,2], 
                                          t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_early_all)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_pm_early_all <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_early_all, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_pm_early_all"
  )
  saveRDS(m3_para_fit_pm_early_all, here::here("data/model_fits/chrono_followup/m3_para_fit_pm_early_all.RDS"))
} else {
  m3_para_fit_pm_early_all <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_pm_early_fu.RDS"))
}

# Convergence check 
m3_para_check_pm_early_all <- convergence_check(m3_para_fit_pm_early_all, 
                                            params = c("kE", "kR", "a"), 
                                            Rhat = TRUE, ess = TRUE,
                                            trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_pm_early_all$Rhat
m3_para_check_pm_early_all$ess
m3_para_check_pm_early_all$trace_plot

# Get individual parameters

params_pm_early_all <- get_params(subj_id = unique(task_pm_early_all$subjID), 
                              model_fit = m3_para_fit_pm_early_all, 
                              n_subj = length(unique(task_pm_early_all$subjID)), 
                              n_params = 3, 
                              param_names = c("kE", "kR", "a"))


### AM testing, late chronoytpe
# no new participants -> use fit from above 

task_am_late_all <- bind_rows(main_data$modelling_data %>% 
                                filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "am_late"]), 
                              followup_data$modelling_data %>% 
                                filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "am_late"]))

if(run_models){
model_dat_am_late_all <- model_preprocessing(raw_data = task_am_late_all,
                                             retest = FALSE,
                                             subjs = unique(task_am_late_all$subjID), 
                                             n_subj = length(unique(task_am_late_all$subjID)), 
                                             t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_late_all)[,2], 
                                             t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_late_all)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_am_late_all <- m3_parabolic_stan_model$sample(
    data = model_dat_am_late_all, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_am_late_all"
  )
  saveRDS(m3_para_fit_am_late_all, here::here("data/model_fits/chrono_followup/m3_para_fit_am_late_all.RDS"))
} else {
  m3_para_fit_am_late_all <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_am_late_fu.RDS"))
}

# Convergence check 
m3_para_check_am_late_all <- convergence_check(m3_para_fit_am_late_all, 
                                               params = c("kE", "kR", "a"), 
                                               Rhat = TRUE, ess = TRUE,
                                               trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_am_late_all$Rhat
m3_para_check_am_late_all$ess
m3_para_check_am_late_all$trace_plot

# Get individual parameters

params_am_late_all <- get_params(subj_id = unique(task_am_late_all$subjID), 
                                 model_fit = m3_para_fit_am_late_all, 
                                 n_subj = length(unique(task_am_late_all$subjID)), 
                                 n_params = 3, 
                                 param_names = c("kE", "kR", "a"))


### PM testing, late chronoytpe
# no new participants -> use fit from above 

task_pm_late_all <- bind_rows(main_data$modelling_data %>% 
                                filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_late"]), 
                              followup_data$modelling_data %>% 
                                filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_late"]))

if(run_models){
model_dat_pm_late_all <- model_preprocessing(raw_data = task_pm_late_all,
                                             retest = FALSE,
                                             subjs = unique(task_pm_late_all$subjID), 
                                             n_subj = length(unique(task_pm_late_all$subjID)), 
                                             t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_late_all)[,2], 
                                             t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_late_all)[,2]))

# Parabolic model, 3 parameters
  m3_para_fit_pm_late_all <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_late_all, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/chrono_followup"), output_basename = "m3_para_mcmc_pm_late_all"
  )
  saveRDS(m3_para_fit_pm_late_all, here::here("data/model_fits/chrono_followup/m3_para_fit_pm_late_all.RDS"))
} else {
  m3_para_fit_pm_late_all <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_fit_pm_late_fu.RDS"))
}

# Convergence check 
m3_para_check_pm_late_all <- convergence_check(m3_para_fit_pm_late_all, 
                                               params = c("kE", "kR", "a"), 
                                               Rhat = TRUE, ess = TRUE,
                                               trace_plot = TRUE, rank_hist = FALSE)
m3_para_check_pm_late_all$Rhat
m3_para_check_pm_late_all$ess
m3_para_check_pm_late_all$trace_plot

# Get individual parameters

params_pm_late_all <- get_params(subj_id = unique(task_pm_late_all$subjID), 
                                 model_fit = m3_para_fit_pm_late_all, 
                                 n_subj = length(unique(task_pm_late_all$subjID)), 
                                 n_params = 3, 
                                 param_names = c("kE", "kR", "a"))


### Merge to one dataset for analyses

params_data_all <- bind_rows(params_am_early_all$individual_params, params_pm_early_all$individual_params, 
                         params_am_late_all$individual_params, params_pm_late_all$individual_params) %>%
  pivot_wider(names_from = parameter, 
              values_from = c(estimate:hdi_upper)) %>% 
  # add scales mean parameter estimates for Bayesian GLMs
  mutate(estimate_kE_scaled = rescale(estimate_kE), 
         estimate_kR_scaled = rescale(estimate_kR), 
         estimate_a_scaled = rescale(estimate_a))

circadian_data_all %<>%
  left_join(params_data_all, 
            by = c("subj_id" = "subj_id")) 


### (6) Run GLMs -----------------------------------------------

# Effort sensitivity
kE_glm <- stan_glm(estimate_kE_scaled ~ chronotype * testing_group + age + gender, 
                   data = circadian_data_all, 
                         iter = 40000, seed = 101)
kE_glm$coefficients
hdi(kE_glm)

# Reward sensitivity
kR_glm <- stan_glm(estimate_kR_scaled ~ chronotype * testing_group + age + gender, 
                   data = circadian_data_all, 
                         iter = 40000, seed = 102)
kR_glm$coefficients
hdi(kR_glm)

# Choice bias
a_glm <- stan_glm(estimate_a_scaled ~ chronotype * testing_group + age + gender, 
                  data = circadian_data_all, 
                   iter = 40000, seed = 103)
a_glm$coefficients
hdi(a_glm)



### (7) Run GLMs with SHAPS, DARS, and AES -----------------------------------------------

# first, scale SHAPS, DARS, and AES to be between 0 and 1 as well
circadian_data_all %<>% 
  mutate(shaps_sumScore = rescale(shaps_sumScore)) %>% 
  mutate(dars_sumScore = rescale(dars_sumScore)) %>% 
  mutate(aes_sumScore = rescale(aes_sumScore))


# shaps
a_shaps_glm <- stan_glm(estimate_a_scaled ~ shaps_sumScore + age + gender, 
                        data = circadian_data_all, 
                  iter = 40000, seed = 123)
a_shaps_glm$coefficients
hdi(a_shaps_glm)
# effect: more anhedonia predicts lower choice bias

# dars
a_dars_glm <- stan_glm(estimate_a_scaled ~ dars_sumScore + age + gender, 
                       data = circadian_data_all, 
                        iter = 40000, seed = 123)
a_dars_glm$coefficients
hdi(a_dars_glm)
# effect: more anhedonia predicts lower choice bias

# aes
a_aes_glm <- stan_glm(estimate_a_scaled ~ aes_sumScore + age + gender, 
                      data = circadian_data_all, 
                        iter = 40000, seed = 123)
a_aes_glm$coefficients
hdi(a_aes_glm)
# effect: more apathy predicts lower choice bias

## Add chronotype and time of day to the GLMs that show effects

# shaps
a_shaps_circadian_glm <- stan_glm(estimate_a_scaled ~ shaps_sumScore * chronotype * testing_group + age + gender, 
                                  data = circadian_data_all, 
                        iter = 40000, seed = 123)
a_shaps_circadian_glm$coefficients
hdi(a_shaps_circadian_glm)
# effect: more anhedonia predicts lower choice bias

# dars
a_dars_circadian_glm <- stan_glm(estimate_a_scaled ~ dars_sumScore * chronotype * testing_group + age + gender, 
                                 data = circadian_data_all, 
                                  iter = 40000, seed = 123)
a_dars_circadian_glm$coefficients
hdi(a_dars_circadian_glm)
# effect: more anhedonia predicts lower choice bias

# aes
a_aes_circadian_glm <- stan_glm(estimate_a_scaled ~ aes_sumScore * chronotype * testing_group + age + gender, 
                                data = circadian_data_all, 
                      iter = 40000, seed = 123)
a_aes_circadian_glm$coefficients
hdi(a_aes_circadian_glm)
# effect: more apathy predicts lower choice bias



















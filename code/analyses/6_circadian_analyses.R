###########################################################################################
###############--------------- ANALYSES OF CIRCADIAN EFFECTS ---------------###############
###########################################################################################

### In this script: 
# (1) Create grouping variables
# (2) Fit models
# (3) Run GLMs
# (4) Add follow-up data
# (5) Sample demographics
# (6) Fit models with followup data
# (7) Run GLMs with followup data
# (8) Run GLMs with SHAPS, DARS, and AES

# Set working directory
here::i_am("github/effort-study/code/analyses/6_circadian_analyses.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/plot_funs.R")
source("github/effort-study/code/functions/model_preprocess_fun.R")
source("github/effort-study/code/functions/model_convergence_check_fun.R")
source("github/effort-study/code/functions/parameter_estimates_fun.R")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, 
                 MatchIt, writexl, lubridate, magrittr, bayestestR, rstanarm)

# Color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")

# adjust time stamps to summer time
main_data$game_meta %<>%
  mutate(start_time = start_time + 60*60,
         end_time = end_time + 60*60)

# should models be fitted or loaded? 
run_models <- FALSE

### (1) Create grouping variables -----------------------------------------------

# Grouping by testing time: 
# am: 08:00am - 11:59am
# pm: 06:00pm - 09:59pm

main_data$game_meta %<>% 
  mutate(testing_group = case_when(update(start_time, year=2000, month=1, mday=1) >= ymd_hms("2000-01-01 08:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) <= ymd_hms("2000-01-01 11:59:59") ~ "am",
                                   update(start_time, year=2000, month=1, mday=1) >= ymd_hms("2000-01-01 18:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) <= ymd_hms("2000-01-01 21:59:59") ~ "pm", 
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
  select(subj_id, age, gender, 
         psych_neurdev, psych_neurdev_condition, antidepressant,
         shaps_sumScore, dars_sumScore, aes_sumScore, mdd_current,
         meq_sumScore, mctq_MSF_SC, findrisc_sumScore, bmi_result, ipaq_sumScore, 
         mdd_current, testing_group, chronotype, group)

# Sub-sample sizes
circadian_data %>% 
  janitor::tabyl(group)

### (2) Fit models -----------------------------------------------

# Load models
m3_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m3_parabolic.stan")

if(run_models){
  
### AM testing, early chronoytpe
  task_am_early <- main_data$modelling_data %>% 
    filter(subjID %in% circadian_data$subj_id[circadian_data$group == "am_early"])
  
  model_dat_am_early <- model_preprocessing(raw_data = task_am_early,
                                            retest = FALSE,
                                            subjs = unique(task_am_early$subjID), 
                                            n_subj = length(unique(task_am_early$subjID)), 
                                            t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_early)[,2], 
                                            t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_early)[,2]))
  
  m3_para_am_early_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_am_early, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_am_early_mcmc"
  )
  # Convergence check
  m3_para_am_early_check <- convergence_check(m3_para_am_early_fit, 
                                                  params = c("kE", "kR", "a"), 
                                                  Rhat = TRUE, ess = TRUE,
                                                  trace_plot = TRUE, rank_hist = FALSE)
  m3_para_am_early_check$trace_plot
  saveRDS(list(m3_para_am_early_check$Rhat, m3_para_am_early_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_am_early_check.RDS"))
  # LOO for model comparisons
  m3_para_am_early_loo <- m3_para_am_early_fit$loo()
  saveRDS(m3_para_am_early_loo, here::here("data/model_fits/chrono_followup/m3_para_am_early_loo.RDS"))
  # Parameter estimates
  m3_para_am_early_params <- get_params(subj_id = unique(task_am_early$subjID), 
                                        model_fit = m3_para_am_early_fit, 
                                        n_subj = length(unique(task_am_early$subjID)), 
                                        n_params = 3, 
                                        param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_am_early_params, here::here("data/model_fits/chrono_followup/m3_para_am_early_params.RDS"))
  
  
  ### PM testing, early chronoytpe
  task_pm_early <- main_data$modelling_data %>% 
    filter(subjID %in% circadian_data$subj_id[circadian_data$group == "pm_early"])
  
  model_dat_pm_early <- model_preprocessing(raw_data = task_pm_early,
                                            retest = FALSE,
                                            subjs = unique(task_pm_early$subjID), 
                                            n_subj = length(unique(task_pm_early$subjID)), 
                                            t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_early)[,2], 
                                            t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_early)[,2]))
  
  m3_para_pm_early_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_early, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_pm_early_mcmc"
  )
  # Convergence check
  m3_para_pm_early_check <- convergence_check(m3_para_pm_early_fit, 
                                              params = c("kE", "kR", "a"), 
                                              Rhat = TRUE, ess = TRUE,
                                              trace_plot = TRUE, rank_hist = FALSE)
  m3_para_pm_early_check$trace_plot
  saveRDS(list(m3_para_pm_early_check$Rhat, m3_para_pm_early_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_pm_early_check.RDS"))
  # LOO for model comparisons
  m3_para_pm_early_loo <- m3_para_pm_early_fit$loo()
  saveRDS(m3_para_pm_early_loo, here::here("data/model_fits/chrono_followup/m3_para_pm_early_loo.RDS"))
  # Parameter estimates
  m3_para_pm_early_params <- get_params(subj_id = unique(task_pm_early$subjID), 
                                        model_fit = m3_para_pm_early_fit, 
                                        n_subj = length(unique(task_pm_early$subjID)), 
                                        n_params = 3, 
                                        param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_pm_early_params, here::here("data/model_fits/chrono_followup/m3_para_pm_early_params.RDS"))

  ### AM testing, late chronoytpe
  task_am_late <- main_data$modelling_data %>% 
    filter(subjID %in% circadian_data$subj_id[circadian_data$group == "am_late"])
  
  model_dat_am_late <- model_preprocessing(raw_data = task_am_late,
                                            retest = FALSE,
                                            subjs = unique(task_am_late$subjID), 
                                            n_subj = length(unique(task_am_late$subjID)), 
                                            t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_late)[,2], 
                                            t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_late)[,2]))
  
  m3_para_am_late_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_am_late, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_am_late_mcmc"
  )
  # Convergence check
  m3_para_am_late_check <- convergence_check(m3_para_am_late_fit, 
                                              params = c("kE", "kR", "a"), 
                                              Rhat = TRUE, ess = TRUE,
                                              trace_plot = TRUE, rank_hist = FALSE)
  m3_para_am_late_check$trace_plot
  saveRDS(list(m3_para_am_late_check$Rhat, m3_para_am_late_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_am_late_check.RDS"))
  # LOO for model comparisons
  m3_para_am_late_loo <- m3_para_am_late_fit$loo()
  saveRDS(m3_para_am_late_loo, here::here("data/model_fits/chrono_followup/m3_para_am_late_loo.RDS"))
  # Parameter estimates
  m3_para_am_late_params <- get_params(subj_id = unique(task_am_late$subjID), 
                                        model_fit = m3_para_am_late_fit, 
                                        n_subj = length(unique(task_am_late$subjID)), 
                                        n_params = 3, 
                                        param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_am_late_params, here::here("data/model_fits/chrono_followup/m3_para_am_late_params.RDS"))

  ### PM testing, late chronoytpe
  task_pm_late <- main_data$modelling_data %>% 
    filter(subjID %in% circadian_data$subj_id[circadian_data$group == "pm_late"])
  
  model_dat_pm_late <- model_preprocessing(raw_data = task_pm_late,
                                            retest = FALSE,
                                            subjs = unique(task_pm_late$subjID), 
                                            n_subj = length(unique(task_pm_late$subjID)), 
                                            t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_late)[,2], 
                                            t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_late)[,2]))
  
  m3_para_pm_late_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_late, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_pm_late_mcmc"
  )
  # Convergence check
  m3_para_pm_late_check <- convergence_check(m3_para_pm_late_fit, 
                                              params = c("kE", "kR", "a"), 
                                              Rhat = TRUE, ess = TRUE,
                                              trace_plot = TRUE, rank_hist = FALSE)
  m3_para_pm_late_check$trace_plot
  saveRDS(list(m3_para_pm_late_check$Rhat, m3_para_pm_late_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_pm_late_check.RDS"))
  # LOO for model comparisons
  m3_para_pm_late_loo <- m3_para_pm_late_fit$loo()
  saveRDS(m3_para_pm_late_loo, here::here("data/model_fits/chrono_followup/m3_para_pm_late_loo.RDS"))
  # Parameter estimates
  m3_para_pm_late_params <- get_params(subj_id = unique(task_pm_late$subjID), 
                                        model_fit = m3_para_pm_late_fit, 
                                        n_subj = length(unique(task_pm_late$subjID)), 
                                        n_params = 3, 
                                        param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_pm_late_params, here::here("data/model_fits/chrono_followup/m3_para_pm_late_params.RDS"))
  
} else {
  ### AM testing, early chronoytpe
  m3_para_am_early_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_early_check.RDS"))
  m3_para_am_early_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_early_loo.RDS"))
  m3_para_am_early_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_early_params.RDS"))
  
  ### PM testing, early chronoytpe
  m3_para_pm_early_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_early_check.RDS"))
  m3_para_pm_early_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_early_loo.RDS"))
  m3_para_pm_early_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_early_params.RDS"))
  
  ### AM testing, late chronoytpe
  m3_para_am_late_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_late_check.RDS"))
  m3_para_am_late_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_late_loo.RDS"))
  m3_para_am_late_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_late_params.RDS"))
  
  ### PM testing, late chronoytpe
  m3_para_pm_late_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_late_check.RDS"))
  m3_para_pm_late_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_late_loo.RDS"))
  m3_para_pm_late_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_late_params.RDS"))
}

### Merge to one dataset for analyses

params_data <- bind_rows(m3_para_am_early_params$individual_params, m3_para_pm_early_params$individual_params, 
                         m3_para_am_late_params$individual_params, m3_para_pm_late_params$individual_params) %>%
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
                                iter = 2000, seed = 123)
kE_glm$coefficients
hdi(kE_glm)

# Reward sensitivity
kR_glm <- stan_glm(estimate_kR_scaled ~ testing_group * chronotype + age + gender, data = circadian_data, 
                   iter = 2000, seed = 123)
kR_glm$coefficients
hdi(kR_glm)

# Choice bias
a_glm <- stan_glm(estimate_a_scaled ~ testing_group * chronotype + age + gender, data = circadian_data, 
                   iter = 2000, seed = 123)
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
  mutate(testing_group = case_when(update(start_time, year=2000, month=1, mday=1) >= ymd_hms("2000-01-01 08:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) <= ymd_hms("2000-01-01 11:59:59") ~ "am",
                                   update(start_time, year=2000, month=1, mday=1) >= ymd_hms("2000-01-01 18:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) <= ymd_hms("2000-01-01 21:59:59") ~ "pm", 
                                   .default = "none"))

# Grouping by chronotype
# early: MEQ > 58, MSFSC < 2:30am
# late: MEQ < 42 , MSFSC > 05:30am

followup_data$questionnaire %<>% 
  mutate(chronotype = case_when(meq_sumScore > 58 & hm(mctq_MSF_SC) <= hm("02:30") ~ "early",
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
  select(subj_id, age, gender, 
         psych_neurdev, psych_neurdev_condition, antidepressant,
         shaps_sumScore, dars_sumScore, aes_sumScore, 
         meq_sumScore, mctq_MSF_SC, findrisc_sumScore, bmi_result, ipaq_sumScore, 
         mdd_past, mdd_current, mdd_recurrent, testing_group, chronotype, group)

# Make new data set with eligible participants from main study and followup

circadian_data_all <- circadian_data %>% 
  select(subj_id:group) %>% 
  bind_rows(circadian_data_fu)

# Sub-sample sizes
circadian_data_all %>% 
  janitor::tabyl(group)

# Sub-sample sizes
circadian_data_all %>% 
  janitor::tabyl(chronotype, testing_group) %>% 
  chisq.test()

### (5) Sample demographics -----------------------------------------------

# Sample size and age
circadian_data_all %>% 
  group_by(chronotype) %>% 
  summarise(N=n(), N_perc = (n()/197)*100, 
            mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

var.test(age ~ chronotype, 
       data = circadian_data_all)
t.test(age ~ chronotype, 
       data = circadian_data_all, var.equal = TRUE)

# Gender
circadian_data_all %>% 
  filter(chronotype == "early") %>% 
  janitor::tabyl(gender)

circadian_data_all %>% 
  filter(chronotype == "late") %>% 
  janitor::tabyl(gender)

circadian_data_all %>% 
  janitor::tabyl(chronotype, gender) %>% 
  chisq.test()

# Psychiatric comorbidity

circadian_data_all %>% 
  group_by(chronotype) %>% 
  summarise(N_psych_neurdev = sum(psych_neurdev), 
            N_psych_neurdev_prec = sum(psych_neurdev)/n()*100)

var.test(psych_neurdev ~ chronotype, 
         data = circadian_data_all)
t.test(psych_neurdev ~ chronotype, 
       data = circadian_data_all, var.equal = TRUE)


# Depression
circadian_data_all %>% 
  filter(chronotype == "early") %>%
  . $psych_neurdev_condition %>% 
  str_subset("Major depressive disorder|depression|Depression|MDD|mdd") %>% 
  length()
(4 / 102)*100

circadian_data_all %>% 
  filter(chronotype == "late") %>%
  . $psych_neurdev_condition %>% 
  str_subset("Major depressive disorder|depression|Depression|MDD|mdd") %>% 
  length()
(22 / 95)*100

matrix(data = c(4, 22, 98, 73), nrow = 2, 
       dimnames = list(c("early", "late"),
                       c("dep_yes", "dep_no"))) %>% 
  chisq.test()

# Anxiety
circadian_data_all %>% 
  filter(chronotype == "early") %>%
  . $psych_neurdev_condition %>% 
  str_subset("Social anxiety disorder|Generalised anxiety disorder") %>% 
  length()
(18 / 102)*100

circadian_data_all %>% 
  filter(chronotype == "late") %>%
  . $psych_neurdev_condition %>% 
  str_subset("Social anxiety disorder|Generalised anxiety disorder") %>% 
  length()
(24 / 95)*100

matrix(data = c(18, 24, 84, 71), nrow = 2, 
       dimnames = list(c("early", "late"),
                       c("anx_yes", "anx_no"))) %>% 
  chisq.test()

# Anti-depressant use
circadian_data_all %>% 
  group_by(chronotype) %>% 
  summarise(N_antid = sum(antidepressant, na.rm = TRUE), 
            N_antid_prec = (sum(antidepressant, na.rm = TRUE)/n())*100)

matrix(data = c(9, 26, 93, 69), nrow = 2, 
       dimnames = list(c("early", "late"),
                       c("anti_d_yes", "anti_d_no"))) %>% 
  chisq.test()

# Psychiatric questionnaire measures
circadian_data_all %>% 
  group_by(chronotype) %>% 
  summarise(shaps_mean = mean(shaps_sumScore), shaps_sd = sd(shaps_sumScore), 
            dars_mean = mean(dars_sumScore), dars_sd = sd(dars_sumScore), 
            aes_mean = mean(aes_sumScore), aes_sd = sd(aes_sumScore))

var.test(shaps_sumScore ~ chronotype, 
         data = circadian_data_all)
t.test(shaps_sumScore ~ chronotype, 
       data = circadian_data_all, var.equal = TRUE)

var.test(dars_sumScore ~ chronotype, 
         data = circadian_data_all)
t.test(dars_sumScore ~ chronotype, 
       data = circadian_data_all, var.equal = TRUE)

var.test(aes_sumScore ~ chronotype, 
         data = circadian_data_all)
t.test(aes_sumScore ~ chronotype, 
       data = circadian_data_all, var.equal = TRUE)

circadian_data_all %>% 
  filter(chronotype == "early") %>% 
  janitor::tabyl(mdd_current)

circadian_data_all %>% 
  filter(chronotype == "late") %>% 
  janitor::tabyl(mdd_current)


matrix(data = c(3, 15, 99, 80), nrow = 2, 
       dimnames = list(c("early", "late"),
                       c("mdd_d_yes", "mdd_d_no"))) %>% 
  chisq.test()


### (6) Fit models with followup data -----------------------------------------------

if(run_models){
  
  ### AM testing, early chronotype
  task_am_early_all <- bind_rows(main_data$modelling_data %>% 
                                   filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "am_early"]), 
                                 followup_data$modelling_data %>% 
                                   filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "am_early"]))
  
  model_dat_am_early_all <- model_preprocessing(raw_data = task_am_early_all,
                                                retest = FALSE,
                                                subjs = unique(task_am_early_all$subjID), 
                                                n_subj = length(unique(task_am_early_all$subjID)), 
                                                t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_early_all)[,2], 
                                                t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_early_all)[,2]))
  
  m3_para_am_early_all_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_am_early_all, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_am_early_all_check <- convergence_check(m3_para_am_early_all_fit, 
                                              params = c("kE", "kR", "a"), 
                                              Rhat = TRUE, ess = TRUE,
                                              trace_plot = TRUE, rank_hist = FALSE)
  m3_para_am_early_all_check$trace_plot
  saveRDS(list(m3_para_am_early_all_check$Rhat, m3_para_am_early_all_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_am_early_all_check.RDS"))
  # LOO for model comparisons
  m3_para_am_early_all_loo <- m3_para_am_early_all_fit$loo()
  saveRDS(m3_para_am_early_all_loo, here::here("data/model_fits/chrono_followup/m3_para_am_early_all_loo.RDS"))
  # Parameter estimates
  m3_para_am_early_all_params <- get_params(subj_id = unique(task_am_early_all$subjID), 
                                        model_fit = m3_para_am_early_all_fit, 
                                        n_subj = length(unique(task_am_early_all$subjID)), 
                                        n_params = 3, 
                                        param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_am_early_all_params, here::here("data/model_fits/chrono_followup/m3_para_am_early_all_params.RDS"))
  
  
  ### PM testing, early chronotype
  task_pm_early_all <- bind_rows(main_data$modelling_data %>% 
                                   filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_early"]), 
                                 followup_data$modelling_data %>% 
                                   filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_early"]))
  
  model_dat_pm_early_all <- model_preprocessing(raw_data = task_pm_early_all,
                                                retest = FALSE,
                                                subjs = unique(task_pm_early_all$subjID), 
                                                n_subj = length(unique(task_pm_early_all$subjID)), 
                                                t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_early_all)[,2], 
                                                t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_early_all)[,2]))
  
  m3_para_pm_early_all_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_early_all, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_pm_early_all_mcmc"
  )
  # Convergence check
  m3_para_pm_early_all_check <- convergence_check(m3_para_pm_early_all_fit, 
                                                  params = c("kE", "kR", "a"), 
                                                  Rhat = TRUE, ess = TRUE,
                                                  trace_plot = TRUE, rank_hist = FALSE)
  m3_para_pm_early_all_check$trace_plot
  saveRDS(list(m3_para_pm_early_all_check$Rhat, m3_para_pm_early_all_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_pm_early_all_check.RDS"))
  # LOO for model comparisons
  m3_para_pm_early_all_loo <- m3_para_pm_early_all_fit$loo()
  saveRDS(m3_para_pm_early_all_loo, here::here("data/model_fits/chrono_followup/m3_para_pm_early_all_loo.RDS"))
  # Parameter estimates
  m3_para_pm_early_all_params <- get_params(subj_id = unique(task_pm_early_all$subjID), 
                                            model_fit = m3_para_pm_early_all_fit, 
                                            n_subj = length(unique(task_pm_early_all$subjID)), 
                                            n_params = 3, 
                                            param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_pm_early_all_params, here::here("data/model_fits/chrono_followup/m3_para_pm_early_all_params.RDS"))
  
  
  ### AM testing, late chronotype
  task_am_late_all <- bind_rows(main_data$modelling_data %>% 
                                   filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "am_late"]), 
                                 followup_data$modelling_data %>% 
                                   filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "am_late"]))
  
  model_dat_am_late_all <- model_preprocessing(raw_data = task_am_late_all,
                                                retest = FALSE,
                                                subjs = unique(task_am_late_all$subjID), 
                                                n_subj = length(unique(task_am_late_all$subjID)), 
                                                t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_am_late_all)[,2], 
                                                t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_am_late_all)[,2]))
  
  m3_para_am_late_all_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_am_late_all, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_am_late_all_mcmc"
  )
  # Convergence check
  m3_para_am_late_all_check <- convergence_check(m3_para_am_late_all_fit, 
                                                  params = c("kE", "kR", "a"), 
                                                  Rhat = TRUE, ess = TRUE,
                                                  trace_plot = TRUE, rank_hist = FALSE)
  m3_para_am_late_all_check$trace_plot
  saveRDS(list(m3_para_am_late_all_check$Rhat, m3_para_am_late_all_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_am_late_all_check.RDS"))
  # LOO for model comparisons
  m3_para_am_late_all_loo <- m3_para_am_late_all_fit$loo()
  saveRDS(m3_para_am_late_all_loo, here::here("data/model_fits/chrono_followup/m3_para_am_late_all_loo.RDS"))
  # Parameter estimates
  m3_para_am_late_all_params <- get_params(subj_id = unique(task_am_late_all$subjID), 
                                            model_fit = m3_para_am_late_all_fit, 
                                            n_subj = length(unique(task_am_late_all$subjID)), 
                                            n_params = 3, 
                                            param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_am_late_all_params, here::here("data/model_fits/chrono_followup/m3_para_am_late_all_params.RDS"))
  
  
  ### PM testing, late chronotype
  task_pm_late_all <- bind_rows(main_data$modelling_data %>% 
                                  filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_late"]), 
                                followup_data$modelling_data %>% 
                                  filter(subjID %in% circadian_data_all$subj_id[circadian_data_all$group == "pm_late"]))
  
  model_dat_pm_late_all <- model_preprocessing(raw_data = task_pm_late_all,
                                               retest = FALSE,
                                               subjs = unique(task_pm_late_all$subjID), 
                                               n_subj = length(unique(task_pm_late_all$subjID)), 
                                               t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_pm_late_all)[,2], 
                                               t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_pm_late_all)[,2]))
  
  m3_para_pm_late_all_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_pm_late_all, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m3_para_pm_late_all_mcmc"
  )
  # Convergence check
  m3_para_pm_late_all_check <- convergence_check(m3_para_pm_late_all_fit, 
                                                 params = c("kE", "kR", "a"), 
                                                 Rhat = TRUE, ess = TRUE,
                                                 trace_plot = TRUE, rank_hist = FALSE)
  m3_para_pm_late_all_check$trace_plot
  saveRDS(list(m3_para_pm_late_all_check$Rhat, m3_para_pm_late_all_check$ess), 
          here::here("data/model_fits/chrono_followup/m3_para_pm_late_all_check.RDS"))
  # LOO for model comparisons
  m3_para_pm_late_all_loo <- m3_para_pm_late_all_fit$loo()
  saveRDS(m3_para_pm_late_all_loo, here::here("data/model_fits/chrono_followup/m3_para_pm_late_all_loo.RDS"))
  # Parameter estimates
  m3_para_pm_late_all_params <- get_params(subj_id = unique(task_pm_late_all$subjID), 
                                           model_fit = m3_para_pm_late_all_fit, 
                                           n_subj = length(unique(task_pm_late_all$subjID)), 
                                           n_params = 3, 
                                           param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_pm_late_all_params, here::here("data/model_fits/chrono_followup/m3_para_pm_late_all_params.RDS"))
  
} else {
  # AM early
  m3_para_am_early_all_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_early_all_check.RDS"))
  m3_para_am_early_all_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_early_all_loo.RDS"))
  m3_para_am_early_all_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_early_all_params.RDS"))
  
  # PM early
  m3_para_pm_early_all_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_early_all_check.RDS"))
  m3_para_pm_early_all_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_early_all_loo.RDS"))
  m3_para_pm_early_all_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_early_all_params.RDS"))
  
  # AM late
  m3_para_am_late_all_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_late_all_check.RDS"))
  m3_para_am_late_all_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_late_all_loo.RDS"))
  m3_para_am_late_all_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_am_late_all_params.RDS"))
  
  # PM late
  m3_para_pm_late_all_check <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_late_all_check.RDS"))
  m3_para_pm_late_all_loo <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_late_all_loo.RDS"))
  m3_para_pm_late_all_params <- readRDS(here::here("data/model_fits/chrono_followup/m3_para_pm_late_all_params.RDS"))
}


### Merge to one dataset for analyses

params_data_all <- bind_rows(m3_para_am_early_all_params$individual_params, m3_para_pm_early_all_params$individual_params, 
                             m3_para_am_late_all_params$individual_params, m3_para_pm_late_all_params$individual_params) %>%
  pivot_wider(names_from = parameter, 
              values_from = c(estimate:hdi_upper)) %>% 
  # add scales mean parameter estimates for Bayesian GLMs
  mutate(estimate_kE_scaled = rescale(estimate_kE), 
         estimate_kR_scaled = rescale(estimate_kR), 
         estimate_a_scaled = rescale(estimate_a))

circadian_data_all %<>%
  left_join(params_data_all, 
            by = c("subj_id" = "subj_id")) 


### (7) Run GLMs with followup data -----------------------------------------------

# Effort sensitivity
kE_glm <- stan_glm(estimate_kE_scaled ~ chronotype * testing_group + age + gender, 
                   data = circadian_data_all, 
                   iter = 2000, seed = 123)
kE_glm$coefficients
hdi(kE_glm)

# Reward sensitivity
kR_glm <- stan_glm(estimate_kR_scaled ~ chronotype * testing_group + age + gender, 
                   data = circadian_data_all, 
                   iter = 2000, seed = 123)
kR_glm$coefficients
hdi(kR_glm)

# Choice bias
a_glm <- stan_glm(estimate_a_scaled ~ chronotype * testing_group + age + gender, 
                  data = circadian_data_all, 
                  iter = 2000, seed = 123)
a_glm$coefficients
hdi(a_glm)

# Follow up GLMs on interaction
a_glm_early <- stan_glm(estimate_a_scaled ~ testing_group + age + gender, 
                  data = circadian_data_all %>% filter(chronotype == "early"), 
                  iter = 2000, seed = 123)
a_glm_early$coefficients
hdi(a_glm_early)

a_glm_late <- stan_glm(estimate_a_scaled ~ testing_group + age + gender, 
                  data = circadian_data_all %>% filter(chronotype == "late"), 
                  iter = 2000, seed = 123)
a_glm_late$coefficients
hdi(a_glm_late)


### (8) Run GLMs with SHAPS, DARS, and AES -----------------------------------------------

# first, scale SHAPS, DARS, and AES to be between 0 and 1 as well
circadian_data_all %<>% 
  mutate(shaps_sumScore = rescale(shaps_sumScore)) %>% 
  mutate(dars_sumScore = rescale(dars_sumScore)) %>% 
  mutate(aes_sumScore = rescale(aes_sumScore)) %>% 
# transform aes and dars to be interpretable in the same direction as the shaps (and in line with main result reporting)
  mutate(dars_sumScore = 1-dars_sumScore, 
         aes_sumScore = 1-aes_sumScore)

# shaps
a_shaps_glm <- stan_glm(estimate_a_scaled ~ shaps_sumScore + age + gender, 
                        data = circadian_data_all, 
                        iter = 40000, seed = 123)
a_shaps_glm$coefficients
hdi(a_shaps_glm)
a_shaps_glm_r2 <- bayes_R2(a_shaps_glm)
median(a_shaps_glm_r2)
# effect: more anhedonia predicts lower choice bias

# dars
a_dars_glm <- stan_glm(estimate_a_scaled ~ dars_sumScore + age + gender, 
                       data = circadian_data_all, 
                       iter = 40000, seed = 123)
a_dars_glm$coefficients
hdi(a_dars_glm)
a_dars_glm_r2 <- bayes_R2(a_dars_glm)
median(a_dars_glm_r2)
# effect: more anhedonia predicts lower choice bias

# aes
a_aes_glm <- stan_glm(estimate_a_scaled ~ aes_sumScore + age + gender, 
                      data = circadian_data_all, 
                      iter = 40000, seed = 123)
a_aes_glm$coefficients
hdi(a_aes_glm)
a_aes_glm_r2 <- bayes_R2(a_aes_glm)
median(a_aes_glm_r2)
# effect: more apathy predicts lower choice bias

# Does the effect of neuropsychiatric symptoms of choice bias change when accounting for chronotype and time?

# shaps
a_shaps_chrono_glm <- stan_glm(estimate_a_scaled ~ shaps_sumScore + chronotype * testing_group + age + gender , 
                        data = circadian_data_all, 
                        iter = 50000, seed = 123)
a_shaps_chrono_glm$coefficients
hdi(a_shaps_chrono_glm)

# Model comparison: does chronotype and time-of-day add predictive value above and beyond shaps?
loo_shaps <- rstanarm::loo(a_shaps_glm)
loo_shaps_chrono <- rstanarm::loo(a_shaps_chrono_glm)
loo_chrono <- loo(a_glm)
loo_compare(loo_shaps, loo_shaps_chrono, loo_chrono)

# R^2
a_shaps_rsq <- bayes_R2(a_shaps_glm)
median(a_shaps_rsq)
a_shaps_chrono_rsq <- bayes_R2(a_shaps_chrono_glm)
median(a_shaps_chrono_rsq)
a_chrono_rsq <- bayes_R2(a_glm)
median(a_chrono_rsq)

# dars
a_dars_chrono_glm <- stan_glm(estimate_a_scaled ~ dars_sumScore + chronotype * testing_group + age + gender , 
                               data = circadian_data_all, 
                               iter = 50000, seed = 1234)
a_dars_chrono_glm$coefficients
hdi(a_dars_chrono_glm)

# Model comparison: does chronotype and time-of-day add predictive value above and beyond dars?
loo_dars <- loo(a_dars_glm)
loo_dars_chrono <- loo(a_dars_chrono_glm)
loo_compare(loo_dars, loo_dars_chrono, loo_chrono)

# R^2
a_dars_rsq <- bayes_R2(a_dars_glm)
median(a_dars_rsq)
a_dars_chrono_rsq <- bayes_R2(a_dars_chrono_glm)
median(a_dars_chrono_rsq)
a_chrono_rsq <- bayes_R2(a_glm)
median(a_chrono_rsq)

# aes
a_aes_chrono_glm <- stan_glm(estimate_a_scaled ~ aes_sumScore + chronotype * testing_group + age + gender , 
                              data = circadian_data_all, 
                              iter = 40000, seed = 123)
a_aes_chrono_glm$coefficients
hdi(a_aes_chrono_glm)

# Model comparison: does chronotype and time-of-day add predictive value above and beyond aes?
loo_aes <- loo(a_aes_glm)
loo_aes_chrono <- loo(a_aes_chrono_glm)
loo_compare(loo_aes, loo_aes_chrono, loo_chrono)

# R^2
a_aes_rsq <- bayes_R2(a_aes_glm)
median(a_aes_rsq)
a_aes_chrono_rsq <- bayes_R2(a_aes_chrono_glm)
median(a_aes_chrono_rsq)
a_chrono_rsq <- bayes_R2(a_glm)
median(a_chrono_rsq)


shaps_glm <- stan_glm(estimate_a_scaled ~ shaps_sumScore + age + gender , 
                    data = circadian_data_all, 
                    iter = 40000, seed = 123)
shaps_rsq <- bayes_R2(shaps_glm)
median(shaps_rsq)
dars_glm <- stan_glm(estimate_a_scaled ~ dars_sumScore + age + gender , 
                      data = circadian_data_all, 
                      iter = 40000, seed = 123)
dars_rsq <- bayes_R2(dars_glm)
median(dars_rsq)
aes_glm <- stan_glm(estimate_a_scaled ~ aes_sumScore + age + gender , 
                             data = circadian_data_all, 
                             iter = 40000, seed = 123)
aes_rsq <- bayes_R2(aes_glm)
median(aes_rsq)

shaps_dars_glm <- stan_glm(estimate_a_scaled ~ shaps_sumScore + dars_sumScore + age + gender , 
                    data = circadian_data_all, 
                    iter = 40000, seed = 123)
shaps_dars_rsq <- bayes_R2(shaps_dars_glm)
median(shaps_dars_rsq)

### (9) Plotting -----------------------------------------------

if(plotting){
  
  # Plot reward sensitivity
  kR_circadian_plot <- raincloud_plot(dat = circadian_data_all, title = "", 
                                  xlab = "", ylab = "Reward sensitivity", 
                                  predictor_var = "testing_group", outcome_var = "estimate_kR_scaled", 
                                  predictor_tick_lab = c("morning testing", "evening testing"), col = c(color_pal[1], color_pal[2]), 
                                  include_grouping = TRUE, group_var = "chronotype", legendlab = "Chronotype",
                                  scale_seq = c(0, 1, 0.25)) + 
    theme(legend.position = "none", 
          axis.text.x = element_text(size = 10)) 
  
  pdf(file = here::here("output/figures/R_plots/circadian_kR.pdf"),  
      width = 7, # The width of the plot in cm (transformed to inches)
      height = 3) # The height of the plot in cm (transformed to inches)
  par(mar=c(0,4,0.5,0.5))
  
  kR_circadian_plot
  
  dev.off()
  
  # Plot choice bias
  a_circadian_plot <- raincloud_plot(dat = circadian_data_all, title = "", 
                                     xlab = "", ylab = "Motivational tendency", 
                                     predictor_var = "testing_group", outcome_var = "estimate_a_scaled", 
                                     predictor_tick_lab = c("morning testing", "evening testing"), col = c(color_pal[1], color_pal[2]), 
                                     include_grouping = TRUE, group_var = "chronotype", legendlab = "Chronotype",
                                     scale_seq = c(0, 1, 0.25)) + 
    theme(legend.position = "none", 
          axis.text.x = element_text(size = 10)) 
  
  pdf(file = here::here("output/figures/R_plots/circadian_a.pdf"),  
      width = 7, # The width of the plot in cm (transformed to inches)
      height = 3) # The height of the plot in cm (transformed to inches)
  par(mar=c(0,4,0.5,0.5))
  
  a_circadian_plot
  
  dev.off()
  
}








#################################################################################
###############--------------- MDD - HC COMPARISON ---------------###############
#################################################################################


### In this script: 
# (1) Prep data
# (2) Match HCs
# (3) Confirm effort discounting effects
# (4) Fit models
# (5) Run GLMs
# (6) Plotting

# Set working directory
here::i_am("github/effort-study/code/analyses/5_mdd_hc_comparison.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/model_preprocess_fun.R")
source("github/effort-study/code/functions/model_convergence_check_fun.R")
source("github/effort-study/code/functions/parameter_estimates_fun.R")
source("github/effort-study/code/functions/model_comparison_fun.R")
source("github/effort-study/code/functions/plot_funs.R")

# source datasets
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")
meta_data <- readRDS("data/processed_data/main_study/online_meta_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, 
                 writexl, lubridate, purrr, magrittr, pls, nlme, cmdstanr, rstanarm, bayestestR)

# run model fitting? 
model_fitting <- FALSE

# Color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

# plot?
plotting <- TRUE

### (1) Prep data  -----------------------------------------------

# Grouping
mdd_subj = main_data$questionnaire %>% filter(mdd_current == 1) %>% .$subj_id
hc_subj = main_data$questionnaire %>% filter(mdd_current == 0, mdd_past == 0) %>% .$subj_id


main_data$game_meta %<>% 
  mutate(testing_group = case_when(update(start_time, year=2000, month=1, mday=1) >= ymd_hms("2000-01-01 08:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) <= ymd_hms("2000-01-01 11:59:59") ~ "am",
                                   update(start_time, year=2000, month=1, mday=1) >= ymd_hms("2000-01-01 18:00:00") & 
                                     update(start_time, year=2000, month=1, mday=1) <= ymd_hms("2000-01-01 21:59:59") ~ "pm", 
                                   .default = "none"))

mdd_hc_data <- main_data$demographics %>% 
  left_join(main_data$questionnaire %>% 
              mutate("mdd_hc_group" = case_when(mdd_current == 1 ~ "mdd", 
                                                mdd_current == 0 ~ "hc")), 
            by = "subj_id") %>% 
  select(subj_id, age, gender, shaps_sumScore, dars_sumScore, dars_food_drink_sumScore, 
         dars_hobbies_sumScore, dars_social_sumScore, dars_sensory_sumScore, 
         aes_sumScore, meq_sumScore, mctq_MSF_SC, bmi_result, findrisc_sumScore,
         mdd_current, mdd_past, mdd_recurrent, mdd_hc_group) %>% 
  left_join(main_data$game_meta %>% select(subj_id, testing_group), by = "subj_id")

# Sub-sample sizes
mdd_hc_data %>% 
  janitor::tabyl(mdd_hc_group)


### (2) Match HCs  -----------------------------------------------

mdd_hc_data %<>% 
  # make gender and group factors
  mutate(gender = as_factor(gender)) %>% 
  mutate(mdd_hc_group = as_factor(mdd_hc_group)) %>% 
  mutate(mdd_hc_group = factor(mdd_hc_group, levels = c("hc", "mdd")))

mdd_hc_match_data <- matchit(mdd_hc_group ~ age + gender, data = mdd_hc_data,
                             method = "optimal", distance = "glm")
summary(mdd_hc_match_data)
mdd_hc_matched_data <- match.data(mdd_hc_match_data)

# Check balance
# Age 
# t test
t.test(age ~ mdd_hc_group, 
       data = mdd_hc_matched_data)

# Gender
mdd_hc_matched_data %>% 
  janitor::tabyl(mdd_hc_group, gender)

mdd_hc_matched_data %>% 
  janitor::tabyl(mdd_hc_group, gender) %>% 
  chisq.test()


### (3) Confirm effort discounting effects  -----------------------------------------------

# Mixed effects ANOVA

# MDD group
task_mdd <- main_data$modelling_data %>% 
  filter(subjID %in% mdd_hc_matched_data$subj_id[mdd_hc_matched_data$mdd_hc_group == "mdd"])

mdd_choice_data <- task_mdd %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))

anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = mdd_choice_data))

# HC group
task_hc <- main_data$modelling_data %>% 
  filter(subjID %in% mdd_hc_matched_data$subj_id[mdd_hc_matched_data$mdd_hc_group == "hc"])

hc_choice_data <- task_hc %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))

anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = hc_choice_data))


### (4) Fit models  -----------------------------------------------

### MDD group
if(model_fitting){
  
  # Load models
  m2_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m2_parabolic.stan")
  m3_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m3_parabolic.stan")
  m3_linear_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_linear/ed_m3_linear.stan")
  
  model_dat_mdd <- model_preprocessing(raw_data = task_mdd,
                                       retest = FALSE,
                                       subjs = unique(task_mdd$subjID), 
                                       n_subj = length(unique(task_mdd$subjID)), 
                                       t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_mdd)[,2], 
                                       t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_mdd)[,2]))
  
  ### Parabolic models
  
  # Model 2
  m2_para_mdd_fit <- m2_parabolic_stan_model$sample(
    data = model_dat_mdd, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL, output_basename = "m2_para_mdd_mcmc"
  )
  # Convergence check
  m2_para_mdd_check <- convergence_check(m2_para_mdd_fit, 
                                     params = c("kE", "kR"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m2_para_mdd_check$trace_plot
  saveRDS(list(m2_para_mdd_check$Rhat, m2_para_mdd_check$ess), 
          here::here("data/model_fits/mdd_hc_comparison/m2_para_mdd_check.RDS"))
  # LOO for model comparisons
  m2_para_mdd_loo <- m2_para_mdd_fit$loo()
  saveRDS(m2_para_mdd_loo, here::here("data/model_fits/mdd_hc_comparison/m2_para_mdd_loo.RDS"))
  # Parameter estimates
  m2_para_mdd_params <- get_params(subj_id = unique(task_mdd$subjID), 
                               model_fit = m2_para_mdd_fit, 
                               n_subj = length(unique(task_mdd$subjID)), 
                               n_params = 2, 
                               param_names = c("kE", "kR"))
  saveRDS(m2_para_mdd_params, here::here("data/model_fits/mdd_hc_comparison/m2_para_mdd_parameter.RDS"))
  
  # Model 3
  m3_para_mdd_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_mdd, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_mdd_check <- convergence_check(m3_para_mdd_fit, 
                                         params = c("kE", "kR", "a"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m3_para_mdd_check$trace_plot
  saveRDS(list(m3_para_mdd_check$Rhat, m3_para_mdd_check$ess), 
          here::here("data/model_fits/mdd_hc_comparison/m3_para_mdd_check.RDS"))
  # LOO for model comparisons
  m3_para_mdd_loo <- m3_para_mdd_fit$loo()
  saveRDS(m3_para_mdd_loo, here::here("data/model_fits/mdd_hc_comparison/m3_para_mdd_loo.RDS"))
  # Parameter estimates
  m3_para_mdd_params <- get_params(subj_id = unique(task_mdd$subjID), 
                                   model_fit = m3_para_mdd_fit, 
                                   n_subj = length(unique(task_mdd$subjID)), 
                                   n_params = 3, 
                                   param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_mdd_params, here::here("data/model_fits/mdd_hc_comparison/m3_para_mdd_parameter.RDS"))
  
  ### Linear models
  
  # Model 3
  m3_lin_mdd_fit <- m3_linear_stan_model$sample(
    data = model_dat_mdd, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/mdd_hc_comparison"), output_basename = "m3_lin_mdd_mcmc"
  )
  # Convergence check
  m3_lin_mdd_check <- convergence_check(m3_lin_mdd_fit, 
                                         params = c("kE", "kR", "a"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_mdd_check$trace_plot
  saveRDS(list(m3_lin_mdd_check$Rhat, m3_lin_mdd_check$ess), 
          here::here("data/model_fits/mdd_hc_comparison/m3_lin_mdd_check.RDS"))
  # LOO for model comparisons
  m3_lin_mdd_loo <- m3_lin_mdd_fit$loo()
  saveRDS(m3_lin_mdd_loo, here::here("data/model_fits/mdd_hc_comparison/m3_lin_mdd_loo.RDS"))
  # Parameter estimates
  m3_lin_mdd_params <- get_params(subj_id = unique(task_mdd$subjID), 
                                   model_fit = m3_lin_mdd_fit, 
                                   n_subj = length(unique(task_mdd$subjID)), 
                                   n_params = 3, 
                                   param_names = c("kE", "kR", "a"))
  saveRDS(m3_lin_mdd_params, here::here("data/model_fits/mdd_hc_comparison/m3_lin_mdd_parameter.RDS"))

  
} else {
  m3_para_mdd_check <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_para_mdd_check.RDS"))
  m3_para_mdd_loo <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_para_mdd_loo.RDS"))
  m3_para_mdd_params <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_para_mdd_parameter.RDS"))
  
  m2_para_mdd_check <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m2_para_mdd_check.RDS"))
  m2_para_mdd_loo <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m2_para_mdd_loo.RDS"))
  m2_para_mdd_params <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m2_para_mdd_parameter.RDS"))
  
  m3_lin_mdd_check <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_lin_mdd_check.RDS"))
  m3_lin_mdd_loo <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_lin_mdd_loo.RDS"))
  m3_lin_mdd_params <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_lin_mdd_parameter.RDS"))
}

### HC group
if(model_fitting){
  
  model_dat_hc <- model_preprocessing(raw_data = task_hc,
                                       retest = FALSE,
                                       subjs = unique(task_hc$subjID), 
                                       n_subj = length(unique(task_hc$subjID)), 
                                       t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_hc)[,2], 
                                       t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_hc)[,2]))
  
  ### Parabolic models
  
  # Model 2
  m2_para_hc_fit <- m2_parabolic_stan_model$sample(
    data = model_dat_hc, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_para_hc_check <- convergence_check(m2_para_hc_fit, 
                                         params = c("kE", "kR"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m2_para_hc_check$trace_plot
  saveRDS(list(m2_para_hc_check$Rhat, m2_para_hc_check$ess), 
          here::here("data/model_fits/mdd_hc_comparison/m2_para_hc_check.RDS"))
  # LOO for model comparisons
  m2_para_hc_loo <- m2_para_hc_fit$loo()
  saveRDS(m2_para_hc_loo, here::here("data/model_fits/mdd_hc_comparison/m2_para_hc_loo.RDS"))
  # Parameter estimates
  m2_para_hc_params <- get_params(subj_id = unique(task_hc$subjID), 
                                   model_fit = m2_para_hc_fit, 
                                   n_subj = length(unique(task_hc$subjID)), 
                                   n_params = 2, 
                                   param_names = c("kE", "kR"))
  saveRDS(m2_para_hc_params, here::here("data/model_fits/mdd_hc_comparison/m2_para_hc_parameter.RDS"))
  
  # Model 3

  m3_para_hc_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_hc, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_hc_check <- convergence_check(m3_para_hc_fit, 
                                         params = c("kE", "kR", "a"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m3_para_hc_check$trace_plot
  saveRDS(list(m3_para_hc_check$Rhat, m3_para_hc_check$ess),
          here::here("data/model_fits/mdd_hc_comparison/m3_para_hc_check.RDS"))
  # LOO for model comparisons
  m3_para_hc_loo <- m3_para_hc_fit$loo()
  saveRDS(m3_para_hc_loo, here::here("data/model_fits/mdd_hc_comparison/m3_para_hc_loo.RDS"))
  # Parameter estimates
  m3_para_hc_params <- get_params(subj_id = unique(task_hc$subjID), 
                                   model_fit = m3_para_hc_fit, 
                                   n_subj = length(unique(task_hc$subjID)), 
                                   n_params = 3, 
                                   param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_hc_params, here::here("data/model_fits/mdd_hc_comparison/m3_para_hc_parameter.RDS"))
  
  ### Linear models
  
  # Model 3
  m3_lin_hc_fit <- m3_linear_stan_model$sample(
    data = model_dat_hc, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/mdd_hc_comparison"), output_basename = "m3_lin_hc_mcmc"
  )
  # Convergence check
  m3_lin_hc_check <- convergence_check(m3_lin_hc_fit, 
                                        params = c("kE", "kR", "a"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_hc_check$trace_plot
  saveRDS(list(m3_lin_hc_check$Rhat, m3_lin_hc_check$ess), 
          here::here("data/model_fits/mdd_hc_comparison/m3_lin_hc_check.RDS"))
  # LOO for model comparisons
  m3_lin_hc_loo <- m3_lin_hc_fit$loo()
  saveRDS(m3_lin_hc_loo, here::here("data/model_fits/mdd_hc_comparison/m3_lin_hc_loo.RDS"))
  # Parameter estimates
  m3_lin_hc_params <- get_params(subj_id = unique(task_hc$subjID), 
                                  model_fit = m3_lin_hc_fit, 
                                  n_subj = length(unique(task_hc$subjID)), 
                                  n_params = 3, 
                                  param_names = c("kE", "kR", "a"))
  saveRDS(m3_lin_hc_params, here::here("data/model_fits/mdd_hc_comparison/m3_lin_hc_parameter.RDS"))
  
  
} else {
  m3_para_hc_check <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_para_hc_check.RDS"))
  m3_para_hc_loo <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_para_hc_loo.RDS"))
  m3_para_hc_params <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_para_hc_parameter.RDS"))
  
  m2_para_hc_check <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m2_para_hc_check.RDS"))
  m2_para_hc_loo <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m2_para_hc_loo.RDS"))
  m2_para_hc_params <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m2_para_hc_parameter.RDS"))
  
  m3_lin_hc_check <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_lin_hc_check.RDS"))
  m3_lin_hc_loo <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_lin_hc_loo.RDS"))
  m3_lin_hc_params <- readRDS(here::here("data/model_fits/mdd_hc_comparison/m3_lin_hc_parameter.RDS"))
  
}

### Model comparison
# MDD
mdd_model_comparison <- model_comparison(loo_paths = list.files(path = here::here("data/model_fits/mdd_hc_comparison"), 
                                                      pattern = "mdd_loo.RDS", full.names = TRUE), 
                               model_names = c("Parabolic model 2",
                                               "Linear model 3", "Parabolic model 3"),
                               LOO_ELPD_title = "Model comparison")
# HC
hc_model_comparison <- model_comparison(loo_paths = list.files(path = here::here("data/model_fits/mdd_hc_comparison"), 
                                                                pattern = "hc_loo.RDS", full.names = TRUE), 
                                         model_names = c("Parabolic model 2",
                                                         "Linear model 3", "Parabolic model 3"),
                                         LOO_ELPD_title = "Model comparison")

# full parabolic model wins in both groups and all comparisons

### Merge to one dataset for analyses

params_mdd_hc <- bind_rows(m3_para_mdd_params$individual_params, m3_para_hc_params$individual_params) %>%
  pivot_wider(names_from = parameter, 
              values_from = c(estimate:hdi_upper)) %>% 
  # add scaled mean parameter estimates for Bayesian GLMs
  mutate(estimate_kE_scaled = rescale(estimate_kE), 
         estimate_kR_scaled = rescale(estimate_kR), 
         estimate_a_scaled = rescale(estimate_a))

mdd_hc_matched_data %<>%
  left_join(params_mdd_hc, 
            by = c("subj_id" = "subj_id"))

### (5) Run GLMs  -----------------------------------------------

# To control for age and gender, input natal sex for non-binary individuals (due to low numbers)
nb_subj <- mdd_hc_matched_data %>% filter(gender == "Non-binary") %>% .$subj_id
for(i in seq_along(nb_subj)){
  mdd_hc_matched_data$gender[mdd_hc_matched_data$subj_id == nb_subj[i]] <- meta_data$Sex[meta_data$Participant.id == nb_subj[i]]
}

# make gender dummy variable
mdd_hc_matched_data$gender <- as.factor(mdd_hc_matched_data$gender)

# Effort sensitivity
kE_glm <- stan_glm(estimate_kE_scaled ~ mdd_hc_group, data = mdd_hc_matched_data, 
                   iter = 2000, seed = 1234)
kE_glm$coefficients
hdi(kE_glm)

# Reward sensitivity
kR_glm <- stan_glm(estimate_kR_scaled ~ mdd_hc_group, data = mdd_hc_matched_data, 
                   iter = 2000, seed = 1234)
kR_glm$coefficients
hdi(kR_glm)

# Choice bias
a_glm <- stan_glm(estimate_a_scaled ~ mdd_hc_group, data = mdd_hc_matched_data, 
                  iter = 2000, seed = 1234)
a_glm$coefficients
hdi(a_glm)



### (6) Plotting  -----------------------------------------------

if(plotting){
 
  # Plot effort sensitivity
  kE_mdd_hc_plot <- raincloud_plot(dat = mdd_hc_matched_data, title = "", 
                                   xlab = " ", ylab = "Effort sensitivity", 
                                   predictor_var = "mdd_hc_group", outcome_var = "estimate_kE_scaled", 
                                   predictor_tick_lab = c("HC", "MDD"), col = c(color_pal[4], color_pal[5]), 
                                   include_grouping = FALSE, direction = "horizontal", scale_seq = c(0, 1, 0.25)) + 
    theme(legend.position = "none", axis.title.y = element_blank()) +
    ggtitle("Effort sensitivity")
  
  
  # Plot choice bias
  a_mdd_hc_plot <- raincloud_plot(dat = mdd_hc_matched_data, title = "", 
                                   xlab = " ", ylab = "Motivational tendency", 
                                   predictor_var = "mdd_hc_group", outcome_var = "estimate_a_scaled", 
                                   predictor_tick_lab = c("HC", "MDD"), col = c(color_pal[4], color_pal[5]), 
                                   include_grouping = FALSE, direction = "horizontal", scale_seq = c(-0, 1,0.25)) + 
    theme(legend.position = "none") +
    ggtitle("Motivational tendency")
  
  
  pdf(file = here::here("output/figures/R_plots/mdd_hc_plot.pdf"),  
      width = 8.75, # The width of the plot in cm (transformed to inches)
      height = 2.25) # The height of the plot in cm (transformed to inches)
  par(mar=c(0,4,0.5,0.5))
  
  ggarrange(a_mdd_hc_plot, kE_mdd_hc_plot,
            ncol = 2, nrow = 1)
  
  dev.off()
}









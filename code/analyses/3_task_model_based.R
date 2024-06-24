#######################################################################################
###############--------------- MODEL BASED TASK ANALYSES ---------------###############
#######################################################################################

### In this script: 
# (1) Preprocess data
# (2) Model fitting
# (3) Convergence check
# (4) Model comparison
# (5) Posterior predictive checks

# Set working directory
here::i_am("github/effort-study/code/analyses/3_task_model_based.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/model_preprocess_fun.R")
source("github/effort-study/code/functions/model_convergence_check_fun.R")
source("github/effort-study/code/functions/model_comparison_fun.R")
source("github/effort-study/code/functions/parameter_estimates_fun.R")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")
task_data <- main_data$modelling_data

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate, magrittr, pushoverr)
library(cmdstanr)
# run model fitting? 
model_fitting <- FALSE

# pushoverr
pushoverUserKey <- "uw5u2amicxd317vuitzqn88z8xfxfa"
pushoverAPIKey <- "asp86ee11jemqhir5i1dau5cf16vrt"


pushoverr::pushover(message = "Exponential model 2 finished running", 
                    user = pushoverUserKey,
                    app = pushoverAPIKey)

### (1) Preprocess data -----------------------------------------------

model_dat <- model_preprocessing(raw_data = task_data, 
                                 retest = FALSE,
                                 subjs = unique(task_data$subjID), 
                                 n_subj = length(unique(task_data$subjID)), 
                                 t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_data)[,2], 
                                 t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_data)[,2]))


### (2) Model fitting -----------------------------------------------

if(model_fitting){
  # Load models
  m1_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m1_parabolic.stan")
  m2_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m2_parabolic.stan")
  m3_parabolic_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m3_parabolic.stan")
  
  m1_linear_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_linear/ed_m1_linear.stan")
  m2_linear_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_linear/ed_m2_linear.stan")
  m3_linear_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_linear/ed_m3_linear.stan")
  
  m1_exponential_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_exponential/ed_m1_exponential.stan")
  m2_exponential_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_exponential/ed_m2_exponential.stan")
  m3_exponential_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_exponential/ed_m3_exponential.stan")
  
  
  ## Parabolic discounting models
  # Model 1
  m1_para_fit <- m1_parabolic_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_para_check <- convergence_check(m1_para_fit, 
                                     params = c("kE", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m1_para_check$trace_plot
  saveRDS(list(m1_para_check$Rhat, m1_para_check$ess), 
          here::here("data/model_fits/main_study/m1_para_check.RDS"))
  # LOO for model comparisons
  m1_para_loo <- m1_para_fit$loo()
  saveRDS(m1_para_loo, 
          here::here("data/model_fits/main_study/m1_para_loo.RDS"))
  # Parameter estimates
  m1_para_params <- get_params(subj_id = unique(task_data$subjID), 
                               model_fit = m1_para_fit, 
                               n_subj = length(unique(task_data$subjID)), 
                               n_params = 2, 
                               param_names = c("kE", "a"))
  saveRDS(m1_para_params, 
          here::here("data/model_fits/main_study/m1_para_parameter.RDS"))
  
  # Model 2
  m2_para_fit <- m2_parabolic_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_para_check <- convergence_check(m2_para_fit, 
                                    params = c("kE", "kR"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m2_para_check$trace_plot
  saveRDS(list(m2_para_check$Rhat, m2_para_check$ess), 
          here::here("data/model_fits/main_study/m2_para_check.RDS"))
  # LOO for model comparisons
  m2_para_loo <- m2_para_fit$loo()
  saveRDS(m2_para_loo, here::here("data/model_fits/main_study/m2_para_loo.RDS"))
  # Parameter estimates
  m2_para_params <- get_params(subj_id = unique(task_data$subjID), 
                               model_fit = m2_para_fit, 
                               n_subj = length(unique(task_data$subjID)), 
                               n_params = 2, 
                               param_names = c("kE", "kR"))
  saveRDS(m2_para_params, here::here("data/model_fits/main_study/m2_para_parameter.RDS"))
  
  # Model 3
  m3_para_fit <- m3_parabolic_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_check <- convergence_check(m3_para_fit, 
                                     params = c("kE", "kR", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m3_para_check$trace_plot
  saveRDS(list(m3_para_check$Rhat, m3_para_check$ess), here::here("data/model_fits/main_study/m3_para_check.RDS"))
  # LOO for model comparisons
  m3_para_loo <- m3_para_fit$loo()
  saveRDS(m3_para_loo, here::here("data/model_fits/main_study/m3_para_loo.RDS"))
  # Posterior Predictions (for target model only)
  m3_para_PPC_dat <- posterior_predictions(csv_paths = m3_para_fit$output_files(),
                                           n_chains = 4,
                                           n_iter = (14*6000),
                                           n_subj = 958, 
                                           n_trials = 64,
                                           real_dat = task_data) 
  saveRDS(m3_para_PPC_dat, here::here("data/model_fits/main_study/m3_para_PPC.RDS"))
  # Parameter estimates
  m3_para_params <- get_params(subj_id = unique(task_data$subjID), 
                               model_fit = m3_para_fit, 
                               n_subj = length(unique(task_data$subjID)), 
                               n_params = 3, 
                               param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_params, here::here("data/model_fits/main_study/m3_para_parameter.RDS"))
  
  
  ## Linear discounting models
  # Model 1
  m1_lin_fit <- m1_linear_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_lin_check <- convergence_check(m1_lin_fit, 
                                    params = c("kE", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m1_lin_check$trace_plot
  saveRDS(list(m1_lin_check$Rhat, m1_lin_check$ess), 
          here::here("data/model_fits/main_study/m1_lin_check.RDS"))
  # LOO for model comparisons
  m1_lin_loo <- m1_lin_fit$loo()
  saveRDS(m1_lin_loo, here::here("data/model_fits/main_study/m1_lin_loo.RDS"))
  # Parameter estimates
  m1_lin_params <- get_params(subj_id = unique(task_data$subjID), 
                              model_fit = m1_lin_fit, 
                              n_subj = length(unique(task_data$subjID)), 
                              n_params = 2, 
                              param_names = c("kE", "a"))
  saveRDS(m1_lin_params, here::here("data/model_fits/main_study/m1_lin_parameter.RDS"))
  
  # Model 2
  m2_lin_fit <- m2_linear_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_lin_check <- convergence_check(m2_lin_fit, 
                                    params = c("kE", "kR"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m2_lin_check$trace_plot
  saveRDS(list(m2_lin_check$Rhat, m2_lin_check$ess), here::here("data/model_fits/main_study/m2_lin_check.RDS"))
  # LOO for model comparisons
  m2_lin_loo <- m2_lin_fit$loo()
  saveRDS(m2_lin_loo, here::here("data/model_fits/main_study/m2_lin_loo.RDS"))
  # Parameter estimates
  m2_lin_params <- get_params(subj_id = unique(task_data$subjID), 
                               model_fit = m2_lin_fit, 
                               n_subj = length(unique(task_data$subjID)), 
                               n_params = 2, 
                               param_names = c("kE", "kR"))
  saveRDS(m2_lin_params, here::here("data/model_fits/main_study/m2_lin_parameter.RDS"))
  
  # Model 3
  m3_lin_fit <- m3_linear_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_lin_check <- convergence_check(m3_lin_fit, 
                                    params = c("kE", "kR", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_check$trace_plot
  saveRDS(list(m3_lin_check$Rhat, m3_lin_check$ess), here::here("data/model_fits/main_study/m3_lin_check.RDS"))
  # LOO for model comparisons
  m3_lin_loo <- m3_lin_fit$loo()
  saveRDS(m3_lin_loo, here::here("data/model_fits/main_study/m3_lin_loo.RDS"))
  # Parameter estimates
  m3_lin_params <- get_params(subj_id = unique(task_data$subjID), 
                              model_fit = m3_lin_fit, 
                              n_subj = length(unique(task_data$subjID)), 
                              n_params = 3, 
                              param_names = c("kE", "kR", "a"))
  saveRDS(m3_lin_params, here::here("data/model_fits/main_study/m3_lin_parameter.RDS"))

  ## Exponential discounting models
  # Model 1
  m1_exp_fit <- m1_exponential_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_exp_check <- convergence_check(m1_exp_fit, 
                                    params = c("kE", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  
  m1_exp_check$trace_plot
  saveRDS(list(m1_exp_check$Rhat, m1_exp_check$ess), here::here("data/model_fits/main_study/m1_exp_check.RDS"))
  # LOO for model comparisons
  m1_exp_loo <- m1_exp_fit$loo()
  saveRDS(m1_exp_loo, here::here("data/model_fits/main_study/m1_exp_loo.RDS"))
  # Parameter estimates
  m1_exp_params <- get_params(subj_id = unique(task_data$subjID), 
                              model_fit = m1_exp_fit, 
                              n_subj = length(unique(task_data$subjID)), 
                              n_params = 2, 
                              param_names = c("kE", "a"))
  saveRDS(m1_exp_params, here::here("data/model_fits/main_study/m1_exp_parameter.RDS"))
  
  # Model 2
  m2_exp_fit <- m2_exponential_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  pushoverr::pushover(message = "Exponential model 2 finished running", 
                      user = pushoverUserKey,
                      app = pushoverAPIKey)
  # Convergence check
  m2_exp_check <- convergence_check(m2_exp_fit, 
                                    params = c("kE", "kR"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  
  m2_exp_check$trace_plot
  saveRDS(list(m2_exp_check$Rhat, m2_exp_check$ess), here::here("data/model_fits/main_study/m2_exp_check.RDS"))
  # LOO for model comparisons
  m2_exp_loo <- m2_exp_fit$loo()
  saveRDS(m2_exp_loo, here::here("data/model_fits/main_study/m2_exp_loo.RDS"))
  # Parameter estimates
  m2_exp_params <- get_params(subj_id = unique(task_data$subjID), 
                              model_fit = m2_exp_fit, 
                              n_subj = length(unique(task_data$subjID)), 
                              n_params = 2, 
                              param_names = c("kE", "kR"))
  saveRDS(m2_exp_params, here::here("data/model_fits/main_study/m2_exp_parameter.RDS"))

  # Model 3
  m3_exp_fit <- m3_exponential_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_exp_check <- convergence_check(m3_exp_fit, 
                                    params = c("kE", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  
  m3_exp_check$trace_plot
  saveRDS(list(m3_exp_check$Rhat, m3_exp_check$ess), here::here("data/model_fits/main_study/m3_exp_check.RDS"))
  # LOO for model comparisons
  m3_exp_loo <- m3_exp_fit$loo()
  saveRDS(m3_exp_loo, here::here("data/model_fits/main_study/m3_exp_loo.RDS"))
  # Parameter estimates
  m3_exp_params <- get_params(subj_id = unique(task_data$subjID), 
                              model_fit = m3_exp_fit, 
                              n_subj = length(unique(task_data$subjID)), 
                              n_params = 2, 
                              param_names = c("kE", "a"))
  saveRDS(m3_exp_params, here::here("data/model_fits/main_study/m3_exp_parameter.RDS"))

} else {
  ### Read in models if already fitted

  # Parabolic
  m1_para_loo <- readRDS(here::here("data/model_fits/main_study/m1_para_loo.RDS"))
  m1_para_check <- readRDS(here::here("data/model_fits/main_study/m1_para_check.RDS"))
  m1_para_params <- readRDS(here::here("data/model_fits/main_study/m1_para_parameter.RDS"))
  
  m2_para_loo <- readRDS(here::here("data/model_fits/main_study/m2_para_loo.RDS"))
  m2_para_check <- readRDS(here::here("data/model_fits/main_study/m2_para_check.RDS"))
  m2_para_params <- readRDS(here::here("data/model_fits/main_study/m2_para_parameter.RDS"))
  
  m3_para_loo <- readRDS(here::here("data/model_fits/main_study/m3_para_loo.RDS"))
  m3_para_check <- readRDS(here::here("data/model_fits/main_study/m3_para_check.RDS"))
  m3_para_params <- readRDS(here::here("data/model_fits/main_study/m3_para_parameter.RDS"))
  
  # Linear
  m1_lin_loo <- readRDS(here::here("data/model_fits/main_study/m1_lin_loo.RDS"))
  m1_lin_check <- readRDS(here::here("data/model_fits/main_study/m1_lin_check.RDS"))
  m1_lin_params <- readRDS(here::here("data/model_fits/main_study/m1_lin_parameter.RDS"))
  
  m2_lin_loo <- readRDS(here::here("data/model_fits/main_study/m2_lin_loo.RDS"))
  m2_lin_check <- readRDS(here::here("data/model_fits/main_study/m2_lin_check.RDS"))
  m2_lin_params <- readRDS(here::here("data/model_fits/main_study/m2_lin_parameter.RDS"))
  
  m3_lin_loo <- readRDS(here::here("data/model_fits/main_study/m3_lin_loo.RDS"))
  m3_lin_check <- readRDS(here::here("data/model_fits/main_study/m3_lin_check.RDS"))
  m3_lin_params <- readRDS(here::here("data/model_fits/main_study/m3_lin_parameter.RDS"))
  
  # Exponential
  m1_exp_loo <- readRDS(here::here("data/model_fits/main_study/m1_exp_loo.RDS"))
  m1_exp_check <- readRDS(here::here("data/model_fits/main_study/m1_exp_check.RDS"))
  m1_exp_params <- readRDS(here::here("data/model_fits/main_study/m1_lin_parameter.RDS"))
  
  m2_exp_loo <- readRDS(here::here("data/model_fits/main_study/m2_exp_loo.RDS"))
  m2_exp_check <- readRDS(here::here("data/model_fits/main_study/m2_exp_check.RDS"))
  m2_exp_params <- readRDS(here::here("data/model_fits/main_study/m2_exp_parameter.RDS"))
  
  m3_exp_loo <- readRDS(here::here("data/model_fits/main_study/m3_exp_loo.RDS"))
  m3_exp_check <- readRDS(here::here("data/model_fits/main_study/m3_exp_check.RDS"))
  m3_exp_params <- readRDS(here::here("data/model_fits/main_study/m3_exp_parameter.RDS"))
  
}

### (3) Convergence check -----------------------------------------------

# Rhats maximum
max(m1_para_check[[1]][6], m2_para_check[[1]][6], m3_para_check[[1]][6],
    m1_lin_check[[1]][6], m2_lin_check[[1]][6], m3_lin_check[[1]][6],
    m1_exp_check[[1]][6], m2_exp_check[[1]][6], m3_exp_check[[1]][6])

# ESS minimum
min(m1_para_check[[2]][1], m2_para_check[[2]][1], 
    m1_lin_check[[2]][1], m2_lin_check[[2]][1], m3_lin_check[[2]][1],
    m1_exp_check[[2]][1], m2_exp_check[[2]][1], m3_exp_check[[2]][1])

### (4) Model comparison -----------------------------------------------

comparison <- model_comparison(loo_paths = list.files(path = here::here("data/model_fits/main_study"), 
                                                        pattern = "loo.RDS", full.names = TRUE), 
                               model_names = c("exp 1", "lin 1", "para 1", 
                                               "exp 2", "lin 2", "para 2", 
                                               "exp 3", "lin 3", "para 3"))


pdf(file = here::here("output/figures/R_plots/model_comparison.pdf"),  
    width = 13 * 0.393701, # The width of the plot in cm (transformed to inches)
    height = 9 * 0.393701) # The height of the plot in cm (transformed to inches)
par(mar=c(0,4,0.5,0.5))

comparison$LOO_ELPD_plot +
  scale_x_discrete(labels = 
                     c(expression("Exponential" ~beta[E]~alpha), expression("Exponential" ~beta[E]~beta[R]), expression("Exponential"~beta[E]~beta[R]~alpha), 
                       expression("Linear" ~beta[E]~alpha), expression("Linear" ~beta[E]~beta[R]), expression("Linear" ~beta[E]~beta[R]~alpha), 
                       expression("Parabolic" ~beta[E]~alpha), expression("Parabolic" ~beta[E]~beta[R]), expression("Parabolic" ~beta[E]~beta[R]~alpha)))


dev.off()

comparison$loo_output

### (5) Posterior predictive checks -----------------------------------------------

# Posterior predictive check of the winning model

if(model_fitting){
  m3_para_PPC_dat <- posterior_predictions(csv_paths = m3_para_fit$output_files(),
                                           n_chains = 4,
                                           n_iter = (14*6000),
                                           n_subj = 958, 
                                           n_trials = 64,
                                           real_dat = task_data) 
  saveRDS(m3_para_PPC_dat, here::here("data/model_fits/main_study/m3_para_PPC.RDS"))
} else {
  m3_para_PPC_dat <- readRDS(here::here("data/model_fits/main_study/m3_para_PPC.RDS")) 
}

# Plot
ppc_plot <- ppc_plots(m3_para_PPC_dat, indiv_plot_title = "")


pdf(file = here::here("output/figures/R_plots/ppc.pdf"),  
    width = 14* 0.393701, # The width of the plot in cm (transformed to inches)
    height = 8 * 0.393701) # The height of the plot in cm (transformed to inches)
par(mar=c(0,4,0.5,0.5))

ppc_plot$indiv_plot

dev.off()

### (6) Plotting mean model parameters -----------------------------------------------

m3_para_params

color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

kE_group_mean <- m3_para_params$group_params %>% filter(parameter == "mu_kE") %>% .$estimate
kE_indiv_mean <- m3_para_params$individual_params %>% filter(parameter == "kE") %>% .$estimate

kR_group_mean <- m3_para_params$group_params %>% filter(parameter == "mu_kR") %>% .$estimate
kR_indiv_mean <- m3_para_params$individual_params %>% filter(parameter == "kR") %>% .$estimate

a_group_mean <- m3_para_params$group_params %>% filter(parameter == "mu_a") %>% .$estimate
a_indiv_mean <- m3_para_params$individual_params %>% filter(parameter == "a") %>% .$estimate


### By Effort ---- 

effort_a <- standardization(1:4)
amount_a <- effort_a[3]

# individual subjects
SV_para <- data.frame("subj_id" = rep(1:958, each = 8),
                      "Effort" = rep(rep(effort_a, 2), 958), 
                      "Reward" = rep(rep(amount_a, 8), 958), 
                      "effort_sensitivity" = rep(kE_indiv_mean, each = 8),
                      "reward_sensitivity" = rep(kR_indiv_mean, each = 8),
                      "SV" = rep(rep(NA, 8), 958))

for(i in 1:7664){
  SV_para[i, "SV"] <- (SV_para[i, "reward_sensitivity"] * SV_para[i, "Reward"]) - 
    (SV_para[i, "effort_sensitivity"] * SV_para[i, "Effort"]^2) 
}

SV_para %<>% 
  mutate_at(vars(Effort, Reward, effort_sensitivity), factor)

# group level 
SV_group_para <- data.frame("subj_id" = rep(0, 8),
                            "Effort" = rep(rep(effort_a, 2), 1), 
                            "Reward" = rep(rep(amount_a, 8), 1), 
                            "effort_sensitivity" = rep(kE_group_mean, each = 8),
                            "reward_sensitivity" = rep(kR_group_mean, each = 8),
                            "SV" = rep(rep(NA, 8), 1))

for(i in 1:8){
  SV_group_para[i, "SV"] <- (SV_group_para[i, "reward_sensitivity"] * SV_group_para[i, "Reward"]) - 
    (SV_group_para[i, "effort_sensitivity"] * SV_group_para[i, "Effort"]^2) 
}

SV_group_para %<>% 
  mutate_at(vars(Effort, Reward, effort_sensitivity), factor)

# Plot by Effort

kE_para_plot <- ggplot(SV_para, aes(x = Effort, y = SV, group = subj_id)) +
  geom_line(alpha = 0.1, size = 1, colour = color_pal[4]) +
  geom_point(alpha = 0.1, size = 1, colour = color_pal[4]) +
  ylab("Subjective Value") + 
  ggtitle("Effort sensitivity") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size =15),
        legend.text = element_text(size = 13)) +
  #ylim(c(-5, 15)) +
  scale_x_discrete(labels = 1:4)

kE_para_plot +
  geom_line(data = SV_group_para, aes(x = Effort, y = SV), 
            size = 2, colour = color_pal[2]) +
  geom_point(data = SV_group_para, aes(x = Effort, y = SV), 
             size = 3, colour = color_pal[2]) 
  
### By Reward ---- 

amount_a <- standardization(1:4)
effort_a <- amount_a[3]

# individual subjects
SV_para <- data.frame("subj_id" = rep(1:958, each = 8),
                      "Effort" = rep(rep(effort_a, 8), 958), 
                      "Reward" = rep(rep(amount_a, 2), 958), 
                      "effort_sensitivity" = rep(kE_indiv_mean, each = 8),
                      "reward_sensitivity" = rep(kR_indiv_mean, each = 8),
                      "SV" = rep(rep(NA, 8), 958))

for(i in 1:7664){
  SV_para[i, "SV"] <- (SV_para[i, "reward_sensitivity"] * SV_para[i, "Reward"]) - 
    (SV_para[i, "effort_sensitivity"] * SV_para[i, "Effort"]^2) 
}

SV_para %<>% 
  mutate_at(vars(Effort, Reward, effort_sensitivity), factor)

# group level 
SV_group_para <- data.frame("subj_id" = rep(0, 8),
                            "Effort" = rep(rep(effort_a, 8), 1), 
                            "Reward" = rep(rep(amount_a, 2), 1), 
                            "effort_sensitivity" = rep(kE_group_mean, each = 8),
                            "reward_sensitivity" = rep(kR_group_mean, each = 8),
                            "SV" = rep(rep(NA, 8), 1))

for(i in 1:8){
  SV_group_para[i, "SV"] <- (SV_group_para[i, "reward_sensitivity"] * SV_group_para[i, "Reward"]) - 
    (SV_group_para[i, "effort_sensitivity"] * SV_group_para[i, "Effort"]^2) 
}

SV_group_para %<>% 
  mutate_at(vars(Effort, Reward, effort_sensitivity), factor)

kR_para_plot <- ggplot(SV_para, aes(x = Reward, y = SV, group = subj_id)) +
  geom_line(alpha = 0.1, size = 1, colour = color_pal[4]) +
  geom_point(alpha = 0.1, size = 1, colour = color_pal[4]) +
  ylab("Subjective Value") + 
  ggtitle("Reward sensitivity") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size =15),
        legend.text = element_text(size = 13)) +
  #ylim(c(-5, 15)) +
  scale_x_discrete(labels = 1:4)

kR_para_plot +
  geom_line(data = SV_group_para, aes(x = Reward, y = SV), 
            size = 2, colour = color_pal[2]) +
  geom_point(data = SV_group_para, aes(x = Reward, y = SV), 
             size = 3, colour = color_pal[2]) 


# Soft max / motivational tendency

# individual level
alpha <- a_indiv_mean
SV <- seq(-10, 10, 0.5)
softmax <- data.frame("SV" = rep(SV, 958), 
                      "choice_bias" = as.factor(rep(alpha,each = length(SV))),
                      "prob_accept" = rep(NA, length(SV)*958))

for(a in seq_along(alpha)){
  for(i in seq_along(SV)){
    softmax[softmax$choice_bias == alpha[a],][i,]$prob_accept <- 1 / (1 + exp(-1*((alpha[a]) + SV[i])))
  } 
}

# group level
alpha <- a_group_mean
SV <- seq(-10, 10, 0.5)
softmax_group <- data.frame("SV" = rep(SV, 1), 
                      "choice_bias" = as.factor(rep(alpha,each = length(SV))),
                      "prob_accept" = rep(NA, length(SV)*1))

for(a in seq_along(alpha)){
  for(i in seq_along(SV)){
    softmax_group[softmax_group$choice_bias == alpha[a],][i,]$prob_accept <- 1 / (1 + exp(-1*((alpha[a]) + SV[i])))
  } 
}

softmax_plot <- ggplot(softmax, aes(x = SV, y = prob_accept, group = choice_bias)) +
  geom_line(alpha = 0.1, size = 1, colour = color_pal[4]) +
  ylab("p(accept)") + 
  xlab("Subjective value") +
  ggtitle("Motivational tendency") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size =15),
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = color_pal[1:4], name = "Motivational tendency", labels = c("low", "medium", "high")) 

softmax_plot +
  geom_line(data = softmax_group, aes(x = SV, y = prob_accept), 
            size = 2, colour = color_pal[2])





#######################################################################################
###############--------------- MODEL BASED TASK ANALYSES ---------------###############
#######################################################################################

### In this script: 
# (1) Preprocess data
# (2) Model fitting
# (3) Convergence check
# (4) Model comparison

# Set working directory
here::i_am("github/effort-study/code/test_retest/3_task_model_based.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/model_preprocess_fun.R")
source("github/effort-study/code/functions/model_convergence_check_fun.R")
source("github/effort-study/code/functions/model_comparison_fun.R")
source("github/effort-study/code/functions/parameter_estimates_fun.R")

# source dataset
retest_data <- readRDS("data/processed_data/test_retest/retest_data.RDS")
task_dat_s1 <- retest_data$modelling_data$session_1
task_dat_s2 <- retest_data$modelling_data$session_2

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate, magrittr, pushoverr)
library(cmdstanr)
# run model fitting? 
model_fitting <- FALSE

### (1) Preprocess data -----------------------------------------------

model_dat_s1 <- model_preprocessing(raw_data = task_dat_s1, 
                                    retest = FALSE,
                                    subjs = unique(task_dat_s1$subjID), 
                                    n_subj = length(unique(task_dat_s1$subjID)), 
                                    t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_dat_s1)[,2], 
                                    t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_dat_s1)[,2]))

model_dat_s2 <- model_preprocessing(raw_data = task_dat_s2, 
                                    retest = FALSE,
                                    subjs = unique(task_dat_s2$subjID), 
                                    n_subj = length(unique(task_dat_s2$subjID)), 
                                    t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_dat_s2)[,2], 
                                    t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_dat_s2)[,2]))


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
  
  
  ### SESSION 1 ---
  
  ## Parabolic discounting models
  # Model 1
  m1_para_s1_fit <- m1_parabolic_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_para_s1_check <- convergence_check(m1_para_s1_fit, 
                                     params = c("kE", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m1_para_s1_check$trace_plot
  saveRDS(list(m1_para_s1_check$Rhat, m1_para_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m1_para_s1_check.RDS"))
  # LOO for model comparisons
  m1_para_s1_loo <- m1_para_s1_fit$loo()
  saveRDS(m1_para_s1_loo, 
          here::here("data/model_fits/retest_study/s1/m1_para_s1_loo.RDS"))
  # Parameter estimates
  m1_para_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                               model_fit = m1_para_s1_fit, 
                               n_subj = length(unique(task_dat_s1$subjID)), 
                               n_params = 2, 
                               param_names = c("kE", "a"))
  saveRDS(m1_para_s1_params, 
          here::here("data/model_fits/retest_study/s1/m1_para_s1_parameter.RDS"))
  
  # Model 2
  m2_para_s1_fit <- m2_parabolic_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_para_s1_check <- convergence_check(m2_para_s1_fit, 
                                        params = c("kE", "kR"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m2_para_s1_check$trace_plot
  saveRDS(list(m2_para_s1_check$Rhat, m2_para_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m2_para_s1_check.RDS"))
  # LOO for model comparisons
  m2_para_s1_loo <- m2_para_s1_fit$loo()
  saveRDS(m2_para_s1_loo, 
          here::here("data/model_fits/retest_study/s1/m2_para_s1_loo.RDS"))
  # Parameter estimates
  m2_para_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                  model_fit = m2_para_s1_fit, 
                                  n_subj = length(unique(task_dat_s1$subjID)), 
                                  n_params = 2, 
                                  param_names = c("kE", "kR"))
  saveRDS(m2_para_s1_params, 
          here::here("data/model_fits/retest_study/s1/m2_para_s1_parameter.RDS"))
  
  # Model 3
  m3_para_s1_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_s1_check <- convergence_check(m3_para_s1_fit, 
                                        params = c("kE", "kR", "a"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m3_para_s1_check$trace_plot
  saveRDS(list(m3_para_s1_check$Rhat, m3_para_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m3_para_s1_check.RDS"))
  # LOO for model comparisons
  m3_para_s1_loo <- m3_para_s1_fit$loo()
  saveRDS(m3_para_s1_loo, 
          here::here("data/model_fits/retest_study/s1/m3_para_s1_loo.RDS"))
  # Parameter estimates
  m3_para_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                  model_fit = m3_para_s1_fit, 
                                  n_subj = length(unique(task_dat_s1$subjID)), 
                                  n_params = 3, 
                                  param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_s1_params, 
          here::here("data/model_fits/retest_study/s1/m3_para_s1_parameter.RDS"))
  
  ## Linear discounting models
  # Model 1
  m1_lin_s1_fit <- m1_linear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_lin_s1_check <- convergence_check(m1_lin_s1_fit, 
                                    params = c("kE", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m1_lin_s1_check$trace_plot
  saveRDS(list(m1_lin_s1_check$Rhat, m1_lin_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m1_lin_s1_check.RDS"))
  # LOO for model comparisons
  m1_lin_s1_loo <- m1_lin_s1_fit$loo()
  saveRDS(m1_lin_s1_loo, here::here("data/model_fits/retest_study/s1/m1_lin_s1_loo.RDS"))
  # Parameter estimates
  m1_lin_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                              model_fit = m1_lin_s1_fit, 
                              n_subj = length(unique(task_dat_s1$subjID)), 
                              n_params = 2, 
                              param_names = c("kE", "a"))
  saveRDS(m1_lin_s1_params, here::here("data/model_fits/retest_study/s1/m1_lin_s1_parameter.RDS"))
  
  # Model 2
  m2_lin_s1_fit <- m2_linear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_lin_s1_check <- convergence_check(m2_lin_s1_fit, 
                                       params = c("kE", "kR"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m2_lin_s1_check$trace_plot
  saveRDS(list(m2_lin_s1_check$Rhat, m2_lin_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m2_lin_s1_check.RDS"))
  # LOO for model comparisons
  m2_lin_s1_loo <- m2_lin_s1_fit$loo()
  saveRDS(m2_lin_s1_loo, here::here("data/model_fits/retest_study/s1/m2_lin_s1_loo.RDS"))
  # Parameter estimates
  m2_lin_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m2_lin_s1_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "kR"))
  saveRDS(m2_lin_s1_params, here::here("data/model_fits/retest_study/s1/m2_lin_s1_parameter.RDS"))
  
  # Model 2
  m3_lin_s1_fit <- m3_linear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_lin_s1_check <- convergence_check(m3_lin_s1_fit, 
                                       params = c("kE", "kR", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_s1_check$trace_plot
  saveRDS(list(m3_lin_s1_check$Rhat, m3_lin_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m3_lin_s1_check.RDS"))
  # LOO for model comparisons
  m3_lin_s1_loo <- m3_lin_s1_fit$loo()
  saveRDS(m3_lin_s1_loo, here::here("data/model_fits/retest_study/s1/m3_lin_s1_loo.RDS"))
  # Parameter estimates
  m3_lin_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m3_lin_s1_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 3, 
                                 param_names = c("kE", "kR", "a"))
  saveRDS(m3_lin_s1_params, here::here("data/model_fits/retest_study/s1/m3_lin_s1_parameter.RDS"))
  
  ## Exponential discounting models
  # Model 1
  m1_exp_s1_fit <- m1_expear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_exp_s1_check <- convergence_check(m1_exp_s1_fit, 
                                       params = c("kE", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m1_exp_s1_check$trace_plot
  saveRDS(list(m1_exp_s1_check$Rhat, m1_exp_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m1_exp_s1_check.RDS"))
  # LOO for model comparisons
  m1_exp_s1_loo <- m1_exp_s1_fit$loo()
  saveRDS(m1_exp_s1_loo, here::here("data/model_fits/retest_study/s1/m1_exp_s1_loo.RDS"))
  # Parameter estimates
  m1_exp_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m1_exp_s1_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "a"))
  saveRDS(m1_exp_s1_params, here::here("data/model_fits/retest_study/s1/m1_exp_s1_parameter.RDS"))
  
  # Model 2
  m2_exp_s1_fit <- m2_expear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_exp_s1_check <- convergence_check(m2_exp_s1_fit, 
                                       params = c("kE", "kR"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m2_exp_s1_check$trace_plot
  saveRDS(list(m2_exp_s1_check$Rhat, m2_exp_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m2_exp_s1_check.RDS"))
  # LOO for model comparisons
  m2_exp_s1_loo <- m2_exp_s1_fit$loo()
  saveRDS(m2_exp_s1_loo, here::here("data/model_fits/retest_study/s1/m2_exp_s1_loo.RDS"))
  # Parameter estimates
  m2_exp_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m2_exp_s1_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "kR"))
  saveRDS(m2_exp_s1_params, here::here("data/model_fits/retest_study/s1/m2_exp_s1_parameter.RDS"))
  
  # Model 2
  m3_exp_s1_fit <- m3_expear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_exp_s1_check <- convergence_check(m3_exp_s1_fit, 
                                       params = c("kE", "kR", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m3_exp_s1_check$trace_plot
  saveRDS(list(m3_exp_s1_check$Rhat, m3_exp_s1_check$ess), 
          here::here("data/model_fits/retest_study/s1/m3_exp_s1_check.RDS"))
  # LOO for model comparisons
  m3_exp_s1_loo <- m3_exp_s1_fit$loo()
  saveRDS(m3_exp_s1_loo, here::here("data/model_fits/retest_study/s1/m3_exp_s1_loo.RDS"))
  # Parameter estimates
  m3_exp_s1_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m3_exp_s1_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 3, 
                                 param_names = c("kE", "kR", "a"))
  saveRDS(m3_exp_s1_params, here::here("data/model_fits/retest_study/s1/m3_exp_s1_parameter.RDS"))
  
  
  ### SESSION 2 ---
  
  ## Parabolic discounting models
  # Model 1
  m1_para_s2_fit <- m1_parabolic_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_para_s2_check <- convergence_check(m1_para_s2_fit, 
                                        params = c("kE", "a"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m1_para_s2_check$trace_plot
  saveRDS(list(m1_para_s2_check$Rhat, m1_para_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m1_para_s2_check.RDS"))
  # LOO for model comparisons
  m1_para_s2_loo <- m1_para_s2_fit$loo()
  saveRDS(m1_para_s2_loo, 
          here::here("data/model_fits/retest_study/s2/m1_para_s2_loo.RDS"))
  # Parameter estimates
  m1_para_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                  model_fit = m1_para_s2_fit, 
                                  n_subj = length(unique(task_dat_s1$subjID)), 
                                  n_params = 2, 
                                  param_names = c("kE", "a"))
  saveRDS(m1_para_s2_params, 
          here::here("data/model_fits/retest_study/s2/m1_para_s2_parameter.RDS"))
  
  # Model 2
  m2_para_s2_fit <- m2_parabolic_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_para_s2_check <- convergence_check(m2_para_s2_fit, 
                                        params = c("kE", "kR"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m2_para_s2_check$trace_plot
  saveRDS(list(m2_para_s2_check$Rhat, m2_para_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m2_para_s2_check.RDS"))
  # LOO for model comparisons
  m2_para_s2_loo <- m2_para_s2_fit$loo()
  saveRDS(m2_para_s2_loo, 
          here::here("data/model_fits/retest_study/s2/m2_para_s2_loo.RDS"))
  # Parameter estimates
  m2_para_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                  model_fit = m2_para_s2_fit, 
                                  n_subj = length(unique(task_dat_s1$subjID)), 
                                  n_params = 2, 
                                  param_names = c("kE", "kR"))
  saveRDS(m2_para_s2_params, 
          here::here("data/model_fits/retest_study/s2/m2_para_s2_parameter.RDS"))
  
  # Model 3
  m3_para_s2_fit <- m3_parabolic_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_s2_check <- convergence_check(m3_para_s2_fit, 
                                        params = c("kE", "kR", "a"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m3_para_s2_check$trace_plot
  saveRDS(list(m3_para_s2_check$Rhat, m3_para_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m3_para_s2_check.RDS"))
  # LOO for model comparisons
  m3_para_s2_loo <- m3_para_s2_fit$loo()
  saveRDS(m3_para_s2_loo, 
          here::here("data/model_fits/retest_study/s2/m3_para_s2_loo.RDS"))
  # Parameter estimates
  m3_para_s2_params <- get_params(subj_id = unique(task_dat_s2$subjID), 
                                  model_fit = m3_para_s2_fit, 
                                  n_subj = length(unique(task_dat_s1$subjID)), 
                                  n_params = 3, 
                                  param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_s2_params, 
          here::here("data/model_fits/retest_study/s2/m3_para_s2_parameter.RDS"))
  
  ## Linear discounting models
  # Model 1
  m1_lin_s2_fit <- m1_linear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_lin_s2_check <- convergence_check(m1_lin_s2_fit, 
                                       params = c("kE", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m1_lin_s2_check$trace_plot
  saveRDS(list(m1_lin_s2_check$Rhat, m1_lin_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m1_lin_s2_check.RDS"))
  # LOO for model comparisons
  m1_lin_s2_loo <- m1_lin_s2_fit$loo()
  saveRDS(m1_lin_s2_loo, here::here("data/model_fits/retest_study/s2/m1_lin_s2_loo.RDS"))
  # Parameter estimates
  m1_lin_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m1_lin_s2_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "a"))
  saveRDS(m1_lin_s2_params, here::here("data/model_fits/retest_study/s2/m1_lin_s2_parameter.RDS"))
  
  # Model 2
  m2_lin_s2_fit <- m2_linear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_lin_s2_check <- convergence_check(m2_lin_s2_fit, 
                                       params = c("kE", "kR"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m2_lin_s2_check$trace_plot
  saveRDS(list(m2_lin_s2_check$Rhat, m2_lin_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m2_lin_s2_check.RDS"))
  # LOO for model comparisons
  m2_lin_s2_loo <- m2_lin_s2_fit$loo()
  saveRDS(m2_lin_s2_loo, here::here("data/model_fits/retest_study/s2/m2_lin_s2_loo.RDS"))
  # Parameter estimates
  m2_lin_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m2_lin_s2_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "kR"))
  saveRDS(m2_lin_s2_params, here::here("data/model_fits/retest_study/s2/m2_lin_s2_parameter.RDS"))
  
  # Model 3
  m3_lin_s2_fit <- m3_linear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_lin_s2_check <- convergence_check(m3_lin_s2_fit, 
                                       params = c("kE", "kR", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_s2_check$trace_plot
  saveRDS(list(m3_lin_s2_check$Rhat, m3_lin_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m3_lin_s2_check.RDS"))
  # LOO for model comparisons
  m3_lin_s2_loo <- m3_lin_s2_fit$loo()
  saveRDS(m3_lin_s2_loo, here::here("data/model_fits/retest_study/s2/m3_lin_s2_loo.RDS"))
  # Parameter estimates
  m3_lin_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m3_lin_s2_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 3, 
                                 param_names = c("kE", "kR", "a"))
  saveRDS(m3_lin_s2_params, here::here("data/model_fits/retest_study/s2/m3_lin_s2_parameter.RDS"))
  
  ## Exponential discounting models
  # Model 1
  m1_exp_s2_fit <- m1_expear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_exp_s2_check <- convergence_check(m1_exp_s2_fit, 
                                       params = c("kE", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m1_exp_s2_check$trace_plot
  saveRDS(list(m1_exp_s2_check$Rhat, m1_exp_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m1_exp_s2_check.RDS"))
  # LOO for model comparisons
  m1_exp_s2_loo <- m1_exp_s2_fit$loo()
  saveRDS(m1_exp_s2_loo, here::here("data/model_fits/retest_study/s2/m1_exp_s2_loo.RDS"))
  # Parameter estimates
  m1_exp_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m1_exp_s2_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "a"))
  saveRDS(m1_exp_s2_params, here::here("data/model_fits/retest_study/s2/m1_exp_s2_parameter.RDS"))
  
  # Model 2
  m2_exp_s2_fit <- m2_expear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_exp_s2_check <- convergence_check(m2_exp_s2_fit, 
                                       params = c("kE", "kR"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m2_exp_s2_check$trace_plot
  saveRDS(list(m2_exp_s2_check$Rhat, m2_exp_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m2_exp_s2_check.RDS"))
  # LOO for model comparisons
  m2_exp_s2_loo <- m2_exp_s2_fit$loo()
  saveRDS(m2_exp_s2_loo, here::here("data/model_fits/retest_study/s2/m2_exp_s2_loo.RDS"))
  # Parameter estimates
  m2_exp_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m2_exp_s2_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 2, 
                                 param_names = c("kE", "kR"))
  saveRDS(m2_exp_s2_params, here::here("data/model_fits/retest_study/s2/m2_exp_s2_parameter.RDS"))
  
  # Model 3
  m3_exp_s2_fit <- m3_expear_stan_model$sample(
    data = model_dat_s1, 
    refresh = 0, chains = 4,  parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_exp_s2_check <- convergence_check(m3_exp_s2_fit, 
                                       params = c("kE", "kR", "a"), 
                                       Rhat = TRUE, ess = TRUE,
                                       trace_plot = TRUE, rank_hist = FALSE)
  m3_exp_s2_check$trace_plot
  saveRDS(list(m3_exp_s2_check$Rhat, m3_exp_s2_check$ess), 
          here::here("data/model_fits/retest_study/s2/m3_exp_s2_check.RDS"))
  # LOO for model comparisons
  m3_exp_s2_loo <- m3_exp_s2_fit$loo()
  saveRDS(m3_exp_s2_loo, here::here("data/model_fits/retest_study/s2/m3_exp_s2_loo.RDS"))
  # Parameter estimates
  m3_exp_s2_params <- get_params(subj_id = unique(task_dat_s1$subjID), 
                                 model_fit = m3_exp_s2_fit, 
                                 n_subj = length(unique(task_dat_s1$subjID)), 
                                 n_params = 3, 
                                 param_names = c("kE", "kR", "a"))
  saveRDS(m3_exp_s2_params, here::here("data/model_fits/retest_study/s2/m3_exp_s2_parameter.RDS"))
  
  
} else {
  ### Read in models if already fitted

  ## Session 1
  # Parabolic
  m1_para_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m1_para_s1_loo.RDS"))
  m1_para_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m1_para_s1_check.RDS"))
  m1_para_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m1_para_s1_parameter.RDS"))
  
  m2_para_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m2_para_s1_loo.RDS"))
  m2_para_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m2_para_s1_check.RDS"))
  m2_para_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m2_para_s1_parameter.RDS"))
  
  m3_para_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m3_para_s1_loo.RDS"))
  m3_para_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m3_para_s1_check.RDS"))
  m3_para_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m3_para_s1_parameter.RDS"))
  
  # Linear
  m1_lin_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m1_lin_s1_loo.RDS"))
  m1_lin_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m1_lin_s1_check.RDS"))
  m1_lin_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m1_lin_s1_parameter.RDS"))
  
  m2_lin_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m2_lin_s1_loo.RDS"))
  m2_lin_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m2_lin_s1_check.RDS"))
  m2_lin_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m2_lin_s1_parameter.RDS"))
  
  m3_lin_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m3_lin_s1_loo.RDS"))
  m3_lin_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m3_lin_s1_check.RDS"))
  m3_lin_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m3_lin_s1_parameter.RDS"))
  
  # Exponential
  m1_exp_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m1_exp_s1_loo.RDS"))
  m1_exp_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m1_exp_s1_check.RDS"))
  m1_exp_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m1_lin_s1_parameter.RDS"))
  
  m2_exp_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m2_exp_s1_loo.RDS"))
  m2_exp_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m2_exp_s1_check.RDS"))
  m2_exp_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m2_exp_s1_parameter.RDS"))
  
  m3_exp_s1_loo <- readRDS(here::here("data/model_fits/retest_study/s1/m3_exp_s1_loo.RDS"))
  m3_exp_s1_check <- readRDS(here::here("data/model_fits/retest_study/s1/m3_exp_s1_check.RDS"))
  m3_exp_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m3_exp_s1_parameter.RDS"))
  
  ## Session 2
  # Parabolic
  m1_para_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m1_para_s2_loo.RDS"))
  m1_para_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m1_para_s2_check.RDS"))
  m1_para_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m1_para_s2_parameter.RDS"))
  
  m2_para_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m2_para_s2_loo.RDS"))
  m2_para_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m2_para_s2_check.RDS"))
  m2_para_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m2_para_s2_parameter.RDS"))
  
  m3_para_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m3_para_s2_loo.RDS"))
  m3_para_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m3_para_s2_check.RDS"))
  m3_para_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m3_para_s2_parameter.RDS"))
  
  # Linear
  m1_lin_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m1_lin_s2_loo.RDS"))
  m1_lin_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m1_lin_s2_check.RDS"))
  m1_lin_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m1_lin_s2_parameter.RDS"))
  
  m2_lin_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m2_lin_s2_loo.RDS"))
  m2_lin_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m2_lin_s2_check.RDS"))
  m2_lin_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m2_lin_s2_parameter.RDS"))
  
  m3_lin_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m3_lin_s2_loo.RDS"))
  m3_lin_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m3_lin_s2_check.RDS"))
  m3_lin_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m3_lin_s2_parameter.RDS"))
  
  # Exponential
  m1_exp_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m1_exp_s2_loo.RDS"))
  m1_exp_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m1_exp_s2_check.RDS"))
  m1_exp_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m1_lin_s2_parameter.RDS"))
  
  m2_exp_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m2_exp_s2_loo.RDS"))
  m2_exp_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m2_exp_s2_check.RDS"))
  m2_exp_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m2_exp_s2_parameter.RDS"))
  
  m3_exp_s2_loo <- readRDS(here::here("data/model_fits/retest_study/s2/m3_exp_s2_loo.RDS"))
  m3_exp_s2_check <- readRDS(here::here("data/model_fits/retest_study/s2/m3_exp_s2_check.RDS"))
  m3_exp_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m3_exp_s2_parameter.RDS"))
  
}

### (3) Convergence check -----------------------------------------------

# Session 1
# Rhats maximum
max(m1_para_s1_check[[1]][6], m2_para_s1_check[[1]][6], m3_para_s1_check[[1]][6],
    m1_lin_s1_check[[1]][6], m2_lin_s1_check[[1]][6], m3_lin_s1_check[[1]][6],
    m1_exp_s1_check[[1]][6], m2_exp_s1_check[[1]][6], m3_exp_s1_check[[1]][6])

# ESS minimum
min(m1_para_s1_check[[2]][1], m2_para_s1_check[[2]][1], m3_para_s1_check[[2]][1], 
    m1_lin_s1_check[[2]][1], m2_lin_s1_check[[2]][1], m3_lin_s1_check[[2]][1],
    m1_exp_s1_check[[2]][1], m2_exp_s1_check[[2]][1], m3_exp_s1_check[[2]][1])

# Session 2
# Rhats maximum
max(m1_para_s2_check[[1]][6], m2_para_s2_check[[1]][6], m3_para_s2_check[[1]][6],
    m1_lin_s2_check[[1]][6], m2_lin_s2_check[[1]][6], m3_lin_s2_check[[1]][6],
    m1_exp_s2_check[[1]][6], m2_exp_s2_check[[1]][6], m3_exp_s2_check[[1]][6])

# ESS minimum
min(m1_para_s2_check[[2]][1], m2_para_s2_check[[2]][1], m3_para_s2_check[[2]][1], 
    m1_lin_s2_check[[2]][1], m2_lin_s2_check[[2]][1], m3_lin_s2_check[[2]][1],
    m1_exp_s2_check[[2]][1], m2_exp_s2_check[[2]][1], m3_exp_s2_check[[2]][1])

### (4) Model comparison -----------------------------------------------

## Session 1
comparison_s1 <- model_comparison(loo_paths = list.files(path = here::here("data/model_fits/retest_study/s1"), 
                                                        pattern = "loo.RDS", full.names = TRUE), 
                               model_names = c("exp 1", "lin 1", "para 1", 
                                               "exp 2", "lin 2", "para 2", 
                                               "exp 3", "lin 3", "para 3"))


comparison_s1 <- comparison_s1$LOO_ELPD_plot +
  scale_x_discrete(labels = 
                     c(expression("Exponential" ~beta[E]~alpha), expression("Exponential" ~beta[E]~beta[R]), expression("Exponential"~beta[E]~beta[R]~alpha), 
                       expression("Linear" ~beta[E]~alpha), expression("Linear" ~beta[E]~beta[R]), expression("Linear" ~beta[E]~beta[R]~alpha), 
                       expression("Parabolic" ~beta[E]~alpha), expression("Parabolic" ~beta[E]~beta[R]), expression("Parabolic" ~beta[E]~beta[R]~alpha)))


comparison_s1$loo_output

## Session 2
comparison_s2 <- model_comparison(loo_paths = list.files(path = here::here("data/model_fits/retest_study/s2"), 
                                                         pattern = "loo.RDS", full.names = TRUE), 
                                  model_names = c("exp 1", "lin 1", "para 1", 
                                                  "exp 2", "lin 2", "para 2", 
                                                  "exp 3", "lin 3", "para 3"))


comparison_s2 <- comparison_s2$LOO_ELPD_plot +
  scale_x_discrete(labels = 
                     c(expression("Exponential" ~beta[E]~alpha), expression("Exponential" ~beta[E]~beta[R]), expression("Exponential"~beta[E]~beta[R]~alpha), 
                       expression("Linear" ~beta[E]~alpha), expression("Linear" ~beta[E]~beta[R]), expression("Linear" ~beta[E]~beta[R]~alpha), 
                       expression("Parabolic" ~beta[E]~alpha), expression("Parabolic" ~beta[E]~beta[R]), expression("Parabolic" ~beta[E]~beta[R]~alpha)))


comparison_s2$loo_output


retest_model_comp <- ggarrange(comparison_s1,
                               comparison_s2,
                               ncol = 2, nrow = 1, 
                               common.legend = TRUE, 
                               legend = "bottom")

pdf(here::here("output/figures/R_plots/retest_model_comparison.pdf"), 
    width = 7, 
    height = 3) 

retest_model_comp

dev.off()

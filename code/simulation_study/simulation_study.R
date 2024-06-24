##############################################################################
###############--------------- SIMULATION STUDY ---------------###############
##############################################################################

### In this script: 
# (1) Simulate data
# (2) Model fitting
# (3) Parameter recovery metric
# (4) Plotting

# Set working directory
here::i_am("github/effort-study/code/simulation_study/simulation_study.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/task_simulation_fun.R")
source("github/effort-study/code/functions/plot_funs.R")
source("github/effort-study/code/functions/model_preprocess_fun.R")
source("github/effort-study/code/functions/model_convergence_check_fun.R")
source("github/effort-study/code/functions/parameter_estimates_fun.R")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, ggcorrplot, magrittr, pushoverr)
library(cmdstanr)

# CBU color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

# run model fitting? 
fit_models <- FALSE

# pushoverr
pushoverUserKey <- "uw5u2amicxd317vuitzqn88z8xfxfa"
pushoverAPIKey <- "asp86ee11jemqhir5i1dau5cf16vrt"


### (1) Simulate data -----------------------------------------------

if(fit_models) {
  
  ## Parabolic discounting models
  # Model 1
  m1_para_sim <- task_simulation(n = 500, model = "m1_parabolic",
                                 kE = runif(500, 0, 1),
                                 alpha = runif(500, -7, 7))
  task_plot(m1_para_sim$simulated_task)
  saveRDS(m1_para_sim,
          here::here("data/processed_data/simulation_study/m1_para_sim.RDS"))
  
  # Model 2
  m2_para_sim <- task_simulation(n = 500, model = "m2_parabolic",
                                 kE = runif(500, 0, 1),
                                 kR = runif(500, 0, 6))
  task_plot(m2_para_sim$simulated_task)
  saveRDS(m2_para_sim,
          here::here("data/processed_data/simulation_study/m2_para_sim.RDS"))
  
  # Model 3
  m3_para_sim <- task_simulation(n = 500, model = "m3_parabolic",
                                 kE = runif(500, 0, 1),
                                 kR = runif(500, 0, 6),
                                 alpha = runif(500, -7, 7))
  task_plot(m3_para_sim$simulated_task)
  saveRDS(m3_para_sim,
          here::here("data/processed_data/simulation_study/m3_para_sim.RDS"))
  
  ## Linear discounting models
  # Model 1
  m1_lin_sim <- task_simulation(n = 500, model = "m1_linear",
                                kE = runif(500, 0, 4),
                                alpha = runif(500, -7, 7))
  task_plot(m1_lin_sim$simulated_task)
  saveRDS(m1_lin_sim,
          here::here("data/processed_data/simulation_study/m1_lin_sim.RDS"))
  
  # Model 2
  m2_lin_sim <- task_simulation(n = 500, model = "m2_linear",
                                kE = runif(500, 0, 4),
                                kR = runif(500, 0, 4))
  task_plot(m2_lin_sim$simulated_task)
  saveRDS(m2_lin_sim,
          here::here("data/processed_data/simulation_study/m2_lin_sim.RDS"))
  
  # Model 3
  m3_lin_sim <- task_simulation(n = 500, model = "m3_linear",
                                kE = runif(500, 0, 4),
                                kR = runif(500, 0, 4),
                                alpha = runif(500, -7, 7))
  task_plot(m3_lin_sim$simulated_task)
  saveRDS(m3_lin_sim,
          here::here("data/processed_data/simulation_study/m3_lin_sim.RDS"))
  
  ## Exponential discounting models
  # Model 1
  m1_exp_sim <- task_simulation(n = 500, model = "m1_exponential",
                                kE = runif(500, 0, 1),
                                alpha = runif(500, -2, 0))
  task_plot(m1_exp_sim$simulated_task)
  saveRDS(m1_exp_sim,
          here::here("data/processed_data/simulation_study/m1_exp_sim.RDS"))
  
  # Model 2
  m2_exp_sim <- task_simulation(n = 500, model = "m2_exponential",
                                kE = runif(500, 0, 1),
                                kR = runif(500, 0, 2))
  task_plot(m2_exp_sim$simulated_task)
  saveRDS(m2_exp_sim,
          here::here("data/processed_data/simulation_study/m2_exp_sim.RDS"))
  
  # Model 3
  m3_exp_sim <- task_simulation(n = 500, model = "m3_exponential",
                                kE = runif(500, 0, 1),
                                kR = runif(500, 0, 2),
                                alpha = runif(500, -2, 0))
  task_plot(m3_exp_sim$simulated_task)
  saveRDS(m3_exp_sim,
          here::here("data/processed_data/simulation_study/m3_exp_sim.RDS"))
  
} else {
  ## Parabolic discounting models
  m1_para_sim <- readRDS(here::here("data/processed_data/simulation_study/m1_para_sim.RDS"))
  m2_para_sim <- readRDS(here::here("data/processed_data/simulation_study/m2_para_sim.RDS"))
  m3_para_sim <- readRDS(here::here("data/processed_data/simulation_study/m3_para_sim.RDS"))
  
  ## Linear discounting models
  m1_lin_sim <- readRDS(here::here("data/processed_data/simulation_study/m1_lin_sim.RDS"))
  m2_lin_sim <- readRDS(here::here("data/processed_data/simulation_study/m2_lin_sim.RDS"))
  m3_lin_sim <- readRDS(here::here("data/processed_data/simulation_study/m3_lin_sim.RDS"))
  
  ## Exponential discounting models
  m1_exp_sim <- readRDS(here::here("data/processed_data/simulation_study/m1_exp_sim.RDS"))
  m2_exp_sim <- readRDS(here::here("data/processed_data/simulation_study/m2_exp_sim.RDS"))
  m3_exp_sim <- readRDS(here::here("data/processed_data/simulation_study/m3_exp_sim.RDS"))
}

### (2) Model fitting -----------------------------------------------

if(fit_models) {
  
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
  
  # Preprocess data
  m1_para_sim_dat <- model_preprocessing(raw_data = m1_para_sim$simulated_task, 
                                   retest = FALSE,
                                   subjs = unique(m1_para_sim$simulated_task$subjID), 
                                   n_subj = length(unique(m1_para_sim$simulated_task$subjID)), 
                                   t_subjs = aggregate(trial ~ subjID, FUN = max, data = m1_para_sim$simulated_task)[,2], 
                                   t_max = max(aggregate(trial ~ subjID, FUN = max, data = m1_para_sim$simulated_task)[,2]))
  
  m1_para_sim_fit <- m1_parabolic_stan_model$sample(
    data = m1_para_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_para_sim_check <- convergence_check(m1_para_sim_fit, 
                                     params = c("kE", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m1_para_sim_check$trace_plot
  saveRDS(list(m1_para_sim_check$Rhat, m1_para_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m1_para_sim_check.RDS"))
  # LOO for model comparisons
  m1_para_sim_loo <- m1_para_sim_fit$loo()
  saveRDS(m1_para_sim_loo, 
          here::here("data/model_fits/simulation_study/m1_para_sim_loo.RDS"))
  # Parameter estimates
  m1_para_sim_params <- get_params(subj_id = unique(m1_para_sim$simulated_task$subjID), 
                               model_fit = m1_para_sim_fit, 
                               n_subj = length(unique(m1_para_sim$simulated_task$subjID)), 
                               n_params = 2, 
                               param_names = c("kE", "a"))
  saveRDS(m1_para_sim_params, 
          here::here("data/model_fits/simulation_study/m1_para_sim_params.RDS"))
  
  # Model 2
  
  # Preprocess data
  m2_para_sim_dat <- model_preprocessing(raw_data = m2_para_sim$simulated_task, 
                                         retest = FALSE,
                                         subjs = unique(m2_para_sim$simulated_task$subjID), 
                                         n_subj = length(unique(m2_para_sim$simulated_task$subjID)), 
                                         t_subjs = aggregate(trial ~ subjID, FUN = max, data = m2_para_sim$simulated_task)[,2], 
                                         t_max = max(aggregate(trial ~ subjID, FUN = max, data = m2_para_sim$simulated_task)[,2]))
  
  m2_para_sim_fit <- m2_parabolic_stan_model$sample(
    data = m2_para_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_para_sim_check <- convergence_check(m2_para_sim_fit, 
                                         params = c("kE", "kR"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m2_para_sim_check$trace_plot
  saveRDS(list(m2_para_sim_check$Rhat, m2_para_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m2_para_sim_check.RDS"))
  # LOO for model comparisons
  m2_para_sim_loo <- m2_para_sim_fit$loo()
  saveRDS(m2_para_sim_loo, 
          here::here("data/model_fits/simulation_study/m2_para_sim_loo.RDS"))
  # Parameter estimates
  m2_para_sim_params <- get_params(subj_id = unique(m2_para_sim$simulated_task$subjID), 
                                   model_fit = m2_para_sim_fit, 
                                   n_subj = length(unique(m2_para_sim$simulated_task$subjID)), 
                                   n_params = 2, 
                                   param_names = c("kE", "kR"))
  saveRDS(m2_para_sim_params, 
          here::here("data/model_fits/simulation_study/m2_para_sim_params.RDS"))
  
  # Model 3
  
  # Preprocess data
  m3_para_sim_dat <- model_preprocessing(raw_data = m3_para_sim$simulated_task, 
                                         retest = FALSE,
                                         subjs = unique(m3_para_sim$simulated_task$subjID), 
                                         n_subj = length(unique(m3_para_sim$simulated_task$subjID)), 
                                         t_subjs = aggregate(trial ~ subjID, FUN = max, data = m3_para_sim$simulated_task)[,2], 
                                         t_max = max(aggregate(trial ~ subjID, FUN = max, data = m3_para_sim$simulated_task)[,2]))
  
  m3_para_sim_fit <- m3_parabolic_stan_model$sample(
    data = m3_para_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_para_sim_check <- convergence_check(m3_para_sim_fit, 
                                         params = c("kE", "kR", "a"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m3_para_sim_check$trace_plot
  saveRDS(list(m3_para_sim_check$Rhat, m3_para_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m3_para_sim_check.RDS"))
  # LOO for model comparisons
  m3_para_sim_loo <- m3_para_sim_fit$loo()
  saveRDS(m3_para_sim_loo, 
          here::here("data/model_fits/simulation_study/m3_para_sim_loo.RDS"))
  # Parameter estimates
  m3_para_sim_params <- get_params(subj_id = unique(m3_para_sim$simulated_task$subjID), 
                                   model_fit = m3_para_sim_fit, 
                                   n_subj = length(unique(m3_para_sim$simulated_task$subjID)), 
                                   n_params = 3, 
                                   param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_sim_params, 
          here::here("data/model_fits/simulation_study/m3_para_sim_params.RDS"))
  
  ## Linear discounting models
  # Model 1
  
  # Preprocess data
  m1_lin_sim_dat <- model_preprocessing(raw_data = m1_lin_sim$simulated_task, 
                                         retest = FALSE,
                                         subjs = unique(m1_lin_sim$simulated_task$subjID), 
                                         n_subj = length(unique(m1_lin_sim$simulated_task$subjID)), 
                                         t_subjs = aggregate(trial ~ subjID, FUN = max, data = m1_lin_sim$simulated_task)[,2], 
                                         t_max = max(aggregate(trial ~ subjID, FUN = max, data = m1_lin_sim$simulated_task)[,2]))
  
  m1_lin_sim_fit <- m1_linear_stan_model$sample(
    data = m1_lin_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_lin_sim_check <- convergence_check(m1_lin_sim_fit, 
                                         params = c("kE", "a"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m1_lin_sim_check$trace_plot
  saveRDS(list(m1_lin_sim_check$Rhat, m1_lin_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m1_lin_sim_check.RDS"))
  # LOO for model comparisons
  m1_lin_sim_loo <- m1_lin_sim_fit$loo()
  saveRDS(m1_lin_sim_loo, 
          here::here("data/model_fits/simulation_study/m1_lin_sim_loo.RDS"))
  # Parameter estimates
  m1_lin_sim_params <- get_params(subj_id = unique(m1_lin_sim$simulated_task$subjID), 
                                   model_fit = m1_lin_sim_fit, 
                                   n_subj = length(unique(m1_lin_sim$simulated_task$subjID)), 
                                   n_params = 2, 
                                   param_names = c("kE", "a"))
  saveRDS(m1_lin_sim_params, 
          here::here("data/model_fits/simulation_study/m1_lin_sim_params.RDS"))
  
  # Model 2
  
  # Preprocess data
  m2_lin_sim_dat <- model_preprocessing(raw_data = m2_lin_sim$simulated_task, 
                                         retest = FALSE,
                                         subjs = unique(m2_lin_sim$simulated_task$subjID), 
                                         n_subj = length(unique(m2_lin_sim$simulated_task$subjID)), 
                                         t_subjs = aggregate(trial ~ subjID, FUN = max, data = m2_lin_sim$simulated_task)[,2], 
                                         t_max = max(aggregate(trial ~ subjID, FUN = max, data = m2_lin_sim$simulated_task)[,2]))
  
  m2_lin_sim_fit <- m2_linear_stan_model$sample(
    data = m2_lin_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_lin_sim_check <- convergence_check(m2_lin_sim_fit, 
                                         params = c("kE", "kR"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m2_lin_sim_check$trace_plot
  saveRDS(list(m2_lin_sim_check$Rhat, m2_lin_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m2_lin_sim_check.RDS"))
  # LOO for model comparisons
  m2_lin_sim_loo <- m2_lin_sim_fit$loo()
  saveRDS(m2_lin_sim_loo, 
          here::here("data/model_fits/simulation_study/m2_lin_sim_loo.RDS"))
  # Parameter estimates
  m2_lin_sim_params <- get_params(subj_id = unique(m2_lin_sim$simulated_task$subjID), 
                                   model_fit = m2_lin_sim_fit, 
                                   n_subj = length(unique(m2_lin_sim$simulated_task$subjID)), 
                                   n_params = 2, 
                                   param_names = c("kE", "kR"))
  saveRDS(m2_lin_sim_params, 
          here::here("data/model_fits/simulation_study/m2_lin_sim_params.RDS"))
  
  # Model 3
  
  # Preprocess data
  m3_lin_sim_dat <- model_preprocessing(raw_data = m3_lin_sim$simulated_task, 
                                         retest = FALSE,
                                         subjs = unique(m3_lin_sim$simulated_task$subjID), 
                                         n_subj = length(unique(m3_lin_sim$simulated_task$subjID)), 
                                         t_subjs = aggregate(trial ~ subjID, FUN = max, data = m3_lin_sim$simulated_task)[,2], 
                                         t_max = max(aggregate(trial ~ subjID, FUN = max, data = m3_lin_sim$simulated_task)[,2]))
  
  m3_lin_sim_fit <- m3_linear_stan_model$sample(
    data = m3_lin_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_lin_sim_check <- convergence_check(m3_lin_sim_fit, 
                                         params = c("kE", "kR", "a"), 
                                         Rhat = TRUE, ess = TRUE,
                                         trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_sim_check$trace_plot
  saveRDS(list(m3_lin_sim_check$Rhat, m3_lin_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m3_lin_sim_check.RDS"))
  # LOO for model comparisons
  m3_lin_sim_loo <- m3_lin_sim_fit$loo()
  saveRDS(m3_lin_sim_loo, 
          here::here("data/model_fits/simulation_study/m3_lin_sim_loo.RDS"))
  # Parameter estimates
  m3_lin_sim_params <- get_params(subj_id = unique(m3_lin_sim$simulated_task$subjID), 
                                   model_fit = m3_lin_sim_fit, 
                                   n_subj = length(unique(m3_lin_sim$simulated_task$subjID)), 
                                   n_params = 3, 
                                   param_names = c("kE", "kR", "a"))
  saveRDS(m3_lin_sim_params, 
          here::here("data/model_fits/simulation_study/m3_lin_sim_params.RDS"))
  
  pushoverr::pushover(message = "Linear models finished running", 
                      user = pushoverUserKey,
                      app = pushoverAPIKey)
  
  ## Exponential discounting models
  # Model 1
  
  # Preprocess data
  m1_exp_sim_dat <- model_preprocessing(raw_data = m1_exp_sim$simulated_task, 
                                        retest = FALSE,
                                        subjs = unique(m1_exp_sim$simulated_task$subjID), 
                                        n_subj = length(unique(m1_exp_sim$simulated_task$subjID)), 
                                        t_subjs = aggregate(trial ~ subjID, FUN = max, data = m1_exp_sim$simulated_task)[,2], 
                                        t_max = max(aggregate(trial ~ subjID, FUN = max, data = m1_exp_sim$simulated_task)[,2]))
  
  m1_exp_sim_fit <- m1_exponential_stan_model$sample(
    data = m1_exp_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m1_exp_sim_check <- convergence_check(m1_exp_sim_fit, 
                                        params = c("kE", "a"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m1_exp_sim_check$trace_plot
  saveRDS(list(m1_exp_sim_check$Rhat, m1_exp_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m1_exp_sim_check.RDS"))
  # LOO for model comparisons
  m1_exp_sim_loo <- m1_exp_sim_fit$loo()
  saveRDS(m1_exp_sim_loo, 
          here::here("data/model_fits/simulation_study/m1_exp_sim_loo.RDS"))
  # Parameter estimates
  m1_exp_sim_params <- get_params(subj_id = unique(m1_exp_sim$simulated_task$subjID), 
                                  model_fit = m1_exp_sim_fit, 
                                  n_subj = length(unique(m1_exp_sim$simulated_task$subjID)), 
                                  n_params = 2, 
                                  param_names = c("kE", "a"))
  saveRDS(m1_exp_sim_params, 
          here::here("data/model_fits/simulation_study/m1_exp_sim_params.RDS"))
  
  # Model 2
  
  # Preprocess data
  m2_exp_sim_dat <- model_preprocessing(raw_data = m2_exp_sim$simulated_task, 
                                        retest = FALSE,
                                        subjs = unique(m2_exp_sim$simulated_task$subjID), 
                                        n_subj = length(unique(m2_exp_sim$simulated_task$subjID)), 
                                        t_subjs = aggregate(trial ~ subjID, FUN = max, data = m2_exp_sim$simulated_task)[,2], 
                                        t_max = max(aggregate(trial ~ subjID, FUN = max, data = m2_exp_sim$simulated_task)[,2]))
  
  m2_exp_sim_fit <- m2_exponential_stan_model$sample(
    data = m2_exp_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m2_exp_sim_check <- convergence_check(m2_exp_sim_fit, 
                                        params = c("kE", "kR"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m2_exp_sim_check$trace_plot
  saveRDS(list(m2_exp_sim_check$Rhat, m2_exp_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m2_exp_sim_check.RDS"))
  # LOO for model comparisons
  m2_exp_sim_loo <- m2_exp_sim_fit$loo()
  saveRDS(m2_exp_sim_loo, 
          here::here("data/model_fits/simulation_study/m2_exp_sim_loo.RDS"))
  # Parameter estimates
  m2_exp_sim_params <- get_params(subj_id = unique(m2_exp_sim$simulated_task$subjID), 
                                  model_fit = m2_exp_sim_fit, 
                                  n_subj = length(unique(m2_exp_sim$simulated_task$subjID)), 
                                  n_params = 2, 
                                  param_names = c("kE", "kR"))
  saveRDS(m2_exp_sim_params, 
          here::here("data/model_fits/simulation_study/m2_exp_sim_params.RDS"))
  
  # Model 3
  
  # Preprocess data
  m3_exp_sim_dat <- model_preprocessing(raw_data = m3_exp_sim$simulated_task, 
                                        retest = FALSE,
                                        subjs = unique(m3_exp_sim$simulated_task$subjID), 
                                        n_subj = length(unique(m3_exp_sim$simulated_task$subjID)), 
                                        t_subjs = aggregate(trial ~ subjID, FUN = max, data = m3_exp_sim$simulated_task)[,2], 
                                        t_max = max(aggregate(trial ~ subjID, FUN = max, data = m3_exp_sim$simulated_task)[,2]))
  
  m3_exp_sim_fit <- m3_exponential_stan_model$sample(
    data = m3_exp_sim_dat, 
    refresh = 0, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  # Convergence check
  m3_exp_sim_check <- convergence_check(m3_exp_sim_fit, 
                                        params = c("kE", "kR", "a"), 
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE)
  m3_exp_sim_check$trace_plot
  saveRDS(list(m3_exp_sim_check$Rhat, m3_exp_sim_check$ess), 
          here::here("data/model_fits/simulation_study/m3_exp_sim_check.RDS"))
  # LOO for model comparisons
  m3_exp_sim_loo <- m3_exp_sim_fit$loo()
  saveRDS(m3_exp_sim_loo, 
          here::here("data/model_fits/simulation_study/m3_exp_sim_loo.RDS"))
  # Parameter estimates
  m3_exp_sim_params <- get_params(subj_id = unique(m3_exp_sim$simulated_task$subjID), 
                                  model_fit = m3_exp_sim_fit, 
                                  n_subj = length(unique(m3_exp_sim$simulated_task$subjID)), 
                                  n_params = 3, 
                                  param_names = c("kE", "kR", "a"))
  saveRDS(m3_exp_sim_params, 
          here::here("data/model_fits/simulation_study/m3_exp_sim_params.RDS"))
} else {
  
  ### Read in models if already fitted
  
  # Parabolic
  m1_para_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m1_para_sim_loo.RDS"))
  m1_para_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m1_para_sim_check.RDS"))
  m1_para_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m1_para_sim_params.RDS"))
  
  m2_para_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m2_para_sim_loo.RDS"))
  m2_para_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m2_para_sim_check.RDS"))
  m2_para_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m2_para_sim_params.RDS"))
  
  m3_para_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m3_para_sim_loo.RDS"))
  m3_para_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m3_para_sim_check.RDS"))
  m3_para_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m3_para_sim_params.RDS"))
  
  # Linear
  m1_lin_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m1_lin_sim_loo.RDS"))
  m1_lin_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m1_lin_sim_check.RDS"))
  m1_lin_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m1_lin_sim_params.RDS"))
  
  m2_lin_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m2_lin_sim_loo.RDS"))
  m2_lin_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m2_lin_sim_check.RDS"))
  m2_lin_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m2_lin_sim_params.RDS"))
  
  m3_lin_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m3_lin_sim_loo.RDS"))
  m3_lin_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m3_lin_sim_check.RDS"))
  m3_lin_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m3_lin_sim_params.RDS"))
  
  # Exponential
  m1_exp_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m1_exp_sim_loo.RDS"))
  m1_exp_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m1_exp_sim_check.RDS"))
  m1_exp_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m1_exp_sim_params.RDS"))
  
  m2_exp_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m2_exp_sim_loo.RDS"))
  m2_exp_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m2_exp_sim_check.RDS"))
  m2_exp_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m2_exp_sim_params.RDS"))
  
  m3_exp_sim_loo <- readRDS(here::here("data/model_fits/simulation_study/m3_exp_sim_loo.RDS"))
  m3_exp_sim_check <- readRDS(here::here("data/model_fits/simulation_study/m3_exp_sim_check.RDS"))
  m3_exp_sim_params <- readRDS(here::here("data/model_fits/simulation_study/m3_exp_sim_params.RDS"))
  
}

### (3) Parameter recovery metric -----------------------------------------------

## Parabolic discounting models
# Model 1
m1_para_recovery <- setNames(c(cor(m1_para_sim$simulation_parameters$kE,
                                   m1_para_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m1_para_sim$simulation_parameters$a,
                                   m1_para_sim_params$individual_params %>% 
                                     filter(parameter == "a") %>% .$estimate)), 
                             c("kE", "a"))

# Model 2
m2_para_recovery <- setNames(c(cor(m2_para_sim$simulation_parameters$kE,
                                   m2_para_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m2_para_sim$simulation_parameters$kR,
                                   m2_para_sim_params$individual_params %>% 
                                     filter(parameter == "kR") %>% .$estimate)), 
                             c("kE", "kR"))

# Model 3 
m3_para_recovery <- setNames(c(cor(m3_para_sim$simulation_parameters$kE,
                                   m3_para_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m3_para_sim$simulation_parameters$kR,
                                   m3_para_sim_params$individual_params %>% 
                                     filter(parameter == "kR") %>% .$estimate),
                               cor(m3_para_sim$simulation_parameters$a,
                                   m3_para_sim_params$individual_params %>% 
                                     filter(parameter == "a") %>% .$estimate)), 
                             c("kE", "kR", "a"))

## Linear discounting models
# Model 1
m1_lin_recovery <- setNames(c(cor(m1_lin_sim$simulation_parameters$kE,
                                   m1_lin_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m1_lin_sim$simulation_parameters$a,
                                   m1_lin_sim_params$individual_params %>% 
                                     filter(parameter == "a") %>% .$estimate)), 
                             c("kE", "a"))

# Model 2
m2_lin_recovery <- setNames(c(cor(m2_lin_sim$simulation_parameters$kE,
                                   m2_lin_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m2_lin_sim$simulation_parameters$kR,
                                   m2_lin_sim_params$individual_params %>% 
                                     filter(parameter == "kR") %>% .$estimate)), 
                             c("kE", "kR"))

# Model 3
m3_lin_recovery <- setNames(c(cor(m3_lin_sim$simulation_parameters$kE,
                                   m3_lin_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m3_lin_sim$simulation_parameters$kR,
                                   m3_lin_sim_params$individual_params %>% 
                                     filter(parameter == "kR") %>% .$estimate),
                               cor(m3_lin_sim$simulation_parameters$a,
                                   m3_lin_sim_params$individual_params %>% 
                                     filter(parameter == "a") %>% .$estimate)), 
                             c("kE", "kR", "a"))


## Exponential discounting models
# Model 1
m1_exp_recovery <- setNames(c(cor(m1_exp_sim$simulation_parameters$kE,
                                   m1_exp_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m1_exp_sim$simulation_parameters$a,
                                   m1_exp_sim_params$individual_params %>% 
                                     filter(parameter == "a") %>% .$estimate)), 
                             c("kE", "a"))

# Model 2
m2_exp_recovery <- setNames(c(cor(m2_exp_sim$simulation_parameters$kE,
                                   m2_exp_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m2_exp_sim$simulation_parameters$kR,
                                   m2_exp_sim_params$individual_params %>% 
                                     filter(parameter == "kR") %>% .$estimate)), 
                             c("kE", "kR"))

# Model 3
m3_exp_recovery <- setNames(c(cor(m3_exp_sim$simulation_parameters$kE,
                                   m3_exp_sim_params$individual_params %>% 
                                     filter(parameter == "kE") %>% .$estimate), 
                               cor(m3_exp_sim$simulation_parameters$kR,
                                   m3_exp_sim_params$individual_params %>% 
                                     filter(parameter == "kR") %>% .$estimate),
                               cor(m3_exp_sim$simulation_parameters$a,
                                   m3_exp_sim_params$individual_params %>% 
                                     filter(parameter == "a") %>% .$estimate)), 
                             c("kE", "kR", "a"))


### (4) Plotting -----------------------------------------------

# Parameter recvery per parameter

recovery_data_kE <- tibble("real" = m3_para_sim$simulation_parameters$kE,
                           "recovered" = m3_para_sim_params$individual_params %>% 
                             filter(parameter == "kE") %>% .$estimate)
recovery_plot_kE <- params_recovery_plot(recovery_data = recovery_data_kE,
                                         plot_title = "Effort sensitivity",
                                         col = color_pal[1])

recovery_data_kR <- tibble("real" = m3_para_sim$simulation_parameters$kR,
                           "recovered" = m3_para_sim_params$individual_params %>% 
                             filter(parameter == "kR") %>% .$estimate)
recovery_plot_kR <- params_recovery_plot(recovery_data = recovery_data_kR,
                                         plot_title = "Reward sensitivity",
                                         col = color_pal[2])

recovery_data_a <- tibble("real" = m3_para_sim$simulation_parameters$a,
                           "recovered" = m3_para_sim_params$individual_params %>% 
                             filter(parameter == "a") %>% .$estimate)
recovery_plot_a <- params_recovery_plot(recovery_data = recovery_data_a,
                                         plot_title = "Motivational tendency",
                                         col = color_pal[3])

# Correlation matrix between parameters

cormat <- matrix(data = c(cor(m3_para_sim_params$individual_params %>% filter(parameter == "kE") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$a), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "kR") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$a), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "a") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$a), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "kE") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$kR), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "kR") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$kR), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "a") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$kR), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "kE") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$kE), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "kR") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$kE), 
                          cor(m3_para_sim_params$individual_params %>% filter(parameter == "a") %>% .$estimate, 
                              m3_para_sim$simulation_parameters$kE)),
                 byrow = TRUE, ncol = 3)
                          
colnames(cormat) = c("rec_kE", "rec_kR", "rec_a")
rownames(cormat) = c("under_a", "under_kR", "under_kE")

m3_para_correlation_plot <- ggcorrplot(cormat, type = "full", ggtheme = ggplot2::theme_gray,
                                       colors = c("#C21A00", "white", "#005BB0"), digits = 3,
                                       lab = TRUE, lab_size = 3) +
  ggplot2::labs(title = "Pearson's correlations", y = "Recovered parameter estimates", x = "Underlying parameters") +
  ggplot2::scale_x_discrete(labels=c("under_kE" = expression(beta[E]), 
                                     "under_kR" = expression(beta[R]), 
                                     "under_a" = expression(alpha))) +
  ggplot2::scale_y_discrete(labels=c("rec_kE" = expression(beta[E]), 
                                     "rec_kR" = expression(beta[R]), 
                                     "rec_a" = expression(alpha))) +
  theme(legend.position = "none", 
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) 

fig_S1 <- ggarrange(recovery_plot_kE,
                    recovery_plot_kR,
                    recovery_plot_a,
                    m3_para_correlation_plot,
                    ncol = 2, nrow = 2)

pdf(here::here("output/figures/R_plots/parameter_recov_plot.pdf"), 
    width = 6, 
    height = 6) 

fig_S1

dev.off()





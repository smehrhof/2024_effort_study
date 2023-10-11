#######################################################################################
###############--------------- MODEL BASED TASK ANALYSES ---------------###############
#######################################################################################

### In this script: 
# (1) Compiling models
# (2) Model fitting
# (3) Convergence check
# (4) Model comparison
# (5) Model validation
# (6) Posterior predictive checks
# (7) Individual parameter estimation 

# Set working directory
here::i_am("github/effort-study/code/main_study/4_task_model_based.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/model_preprocess_fun.R")
source("github/effort-study/code/functions/model_convergence_check_fun.R")
source("github/effort-study/code/functions/model_comparison_fun.R")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")
task_data <- main_data$modelling_data

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate)

# run model fitting? 
model_fitting <- FALSE

### (1) Compiling models -----------------------------------------------

# Preprocess data
model_dat <- model_preprocessing(raw_data = task_data, 
                                 retest = FALSE,
                                 subjs = unique(task_data$subjID), 
                                 n_subj = length(unique(task_data$subjID)), 
                                 t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_data)[,2], 
                                 t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_data)[,2]))

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


### (2) Model fitting -----------------------------------------------

if(model_fitting){
  
  ## Parabolic discounting models
  # Model 1
  m1_para_fit <- m1_parabolic_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 20, iter_sampling = 60, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m1_para_mcmc"
  )
  saveRDS(m1_para_fit, here::here("data/model_fits/main_study/m1_para_fit.RDS"))
  # Model 2
  m2_para_fit <- m2_parabolic_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m2_para_mcmc"
  )
  saveRDS(m2_para_fit, here::here("data/model_fits/main_study/m2_para_fit.RDS"))
  # Model 3
  m3_para_fit <- m3_parabolic_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m3_para_mcmc"
  )
  saveRDS(m3_para_fit, here::here("data/model_fits/main_study/m3_para_fit.RDS"))
  
  ## Linear discounting models
  # Model 1
  m1_lin_fit <- m1_linear_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m1_lin_mcmc"
  )
  saveRDS(m1_lin_fit, here::here("data/model_fits/main_study/m1_lin_fit.RDS"))
  # Model 2
  m2_lin_fit <- m2_linear_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m2_lin_mcmc"
  )
  saveRDS(m2_lin_fit, here::here("data/model_fits/main_study/m2_lin_fit.RDS"))
  # Model 3
  m3_lin_fit <- m3_linear_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m3_lin_mcmc"
  )
  saveRDS(m3_lin_fit, here::here("data/model_fits/main_study/m3_lin_fit.RDS"))
  
  ## Exponential discounting models
  # Model 1
  m1_exp_fit <- m1_exponential_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m1_exp_mcmc"
  )
  saveRDS(m1_exp_fit, here::here("data/model_fits/main_study/m1_exp_fit.RDS"))
  # Model 2
  m2_exp_fit <- m2_exponential_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m2_exp_mcmc"
  )
  saveRDS(m2_exp_fit, here::here("data/model_fits/main_study/m2_exp_fit.RDS"))
  # Model 3
  m3_exp_fit <- m3_exponential_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = here::here("data/model_fits/main_study"), output_basename = "m3_exp_mcmc"
  )
  saveRDS(m3_exp_fit, here::here("data/model_fits/main_study/m3_exp_fit.RDS"))
  
} else {
  ### Read in models if already fitted
  # Parabolic
  m1_para_fit <- readRDS(here::here("data/model_fits/main_study/m1_para_fit.RDS"))
  m2_para_fit <- readRDS(here::here("data/model_fits/main_study/m2_para_fit.RDS"))
  m3_para_fit <- readRDS(here::here("data/model_fits/main_study/m3_para_fit.RDS"))
  
  # Linear
  m1_lin_fit <- readRDS(here::here("data/model_fits/main_study/m1_lin_fit.RDS"))
  m2_lin_fit <- readRDS(here::here("data/model_fits/main_study/m2_lin_fit.RDS"))
  m3_lin_fit <- readRDS(here::here("data/model_fits/main_study/m3_lin_fit.RDS"))
  
  # Exponential
  m1_exp_fit <- readRDS(here::here("data/model_fits/main_study/m1_exp_fit.RDS"))
  m2_exp_fit <- readRDS(here::here("data/model_fits/main_study/m2_exp_fit.RDS"))
  m3_exp_fit <- readRDS(here::here("data/model_fits/main_study/m3_exp_fit.RDS"))
}

### (3) Convergence check -----------------------------------------------

if(model_fitting){
  
  ## Parabolic discounting models
  
  m1_para_check <- convergence_check(m1_para_fit, 
                                     params = c("kE", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m1_para_check$Rhat
  m1_para_check$ess
  m1_para_check$trace_plot
  
  m2_para_check <- convergence_check(m2_para_fit, 
                                     params = c("kE", "kR"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m2_para_check$Rhat
  m2_para_check$ess
  m2_para_check$trace_plot
  
  m3_para_check <- convergence_check(m3_para_fit, 
                                     params = c("kE", "kR", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m3_para_check$Rhat
  m3_para_check$ess
  m3_para_check$trace_plot
  
  ## Linear discounting models
  
  m1_lin_check <- convergence_check(m1_lin_fit, 
                                    params = c("kE", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m1_lin_check$Rhat
  m1_lin_check$ess
  m1_lin_check$trace_plot
  
  m2_lin_check <- convergence_check(m2_lin_fit, 
                                    params = c("kE", "kR"),
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m2_lin_check$Rhat
  m2_lin_check$ess
  m2_lin_check$trace_plot
  
  m3_lin_check <- convergence_check(m3_lin_fit, 
                                    params = c("kE", "kR", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m3_lin_check$Rhat
  m3_lin_check$ess
  m3_lin_check$trace_plot
  
  
  ## Exponential discounting models
  
  m1_exp_check <- convergence_check(m1_exp_fit, 
                                    params = c("kE", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m1_exp_check$Rhat
  m1_exp_check$ess
  m1_exp_check$trace_plot
  
  m2_exp_check <- convergence_check(m2_exp_fit, 
                                    params = c("kE", "kR"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m2_exp_s1_check$Rhat
  m2_exp_s1_check$ess
  m2_exp_s1_check$trace_plot
  
  m3_exp_check <- convergence_check(m3_exp_fit, 
                                    params = c("kE", "kR", "a"), 
                                    Rhat = TRUE, ess = TRUE,
                                    trace_plot = TRUE, rank_hist = FALSE)
  m3_exp_check$Rhat
  m3_exp_check$ess
  m3_exp_check$trace_plot
  
}

### (4) Model comparison -----------------------------------------------

comparison <- model_comparison(model_paths = list.files(path = here::here("data/model_fits/main_study"), 
                                                        pattern = ".RDS", full.names = TRUE), 
                               model_names = c("Exponential model 1", "Linear model 1", "Parabolic model 1",
                                               "Exponential model 2", "Linear model 2", "Parabolic model 2",
                                               "Exponential model 3", "Linear model 3", "Parabolic model 3"),
                               LOO_ELPD_title = "Model comparison")

fig_3B <- annotate_figure(comparison$LOO_ELPD_plot, top = text_grob("Model comparison", 
                                                                    face = "bold", size = 10),
                          fig.lab = c("B"),
                          fig.lab.pos = c("top.left"))

### (5) Posterior predictive checks -----------------------------------------------

# Posterior predictive check of the winning model

m3_para_PPC_dat <- posterior_predictions(csv_paths = m3_para_fit$output_files(),
                                         n_chains = 4,
                                         n_iter = (14*6000),
                                         n_subj = 958, 
                                         n_trials = 64,
                                         real_dat = task_data) 

saveRDS(m3_para_PPC_dat, here::here("data/model_fits/main_study/m3_para_PPC_dat.RDS"))

### (6) Individual parameter estimation -----------------------------------------------

### Parabolic model 3

m3_para_params_group <- data.frame("parameter" = c("kE", "kR", "a"), 
                                   "estimate" = as.vector(colMeans(m3_para_fit$draws(variables = c("mu_kE", "mu_kR", "mu_a"), 
                                                                                     format = "df", inc_warmup = FALSE)))[1:3])

m3_para_params <- as.vector(colMeans(m3_para_fit$draws(variables = c("kE", "kR", "a"), 
                                                       format = "df", inc_warmup = FALSE)))

m3_para_params_hdi <- apply(m3_para_fit$draws(variables = c("kE", "kR", "a"), 
                                              format = "df", inc_warmup = FALSE), 
                            FUN = hBayesDM::HDIofMCMC, MARGIN = 2)

m3_para_params_dat <- data.frame("subj_id" = rep(unique(task_dat$subjID), 3),
                                 "parameter" = rep(c("kE", "kR", "a"), each = 955),
                                 "estimates" = m3_para_params[1:(length(m3_para_params)-3)], 
                                 "hdi_lower" = as.vector(m3_para_params_hdi[1,1:2865]),
                                 "hdi_upper" = as.vector(m3_para_params_hdi[2,1:2865]))

if(FALSE){
  params_estimate_plot(m3_para_params_dat[m3_para_params_dat$parameter == "kE",],
                       parameter = "kE", col = color_pal[1])
  
  params_estimate_plot(m3_para_params_dat[m3_para_params_dat$parameter == "kR",],
                       parameter = "kR", col = color_pal[2])
  
  params_estimate_plot(m3_para_params_dat[m3_para_params_dat$parameter == "a",],
                       parameter = "a", col = color_pal[3])  
}




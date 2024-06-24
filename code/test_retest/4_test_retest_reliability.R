#####################################################################################
###############--------------- TEST RETEST RELIABILITY ---------------###############
#####################################################################################

### In this script: 
# (1) Intra-class correlations
# (2) Correlations 
# (3) Posterior predictions

# Set working directory
here::i_am("github/effort-study/code/test_retest/3_task_model_based.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/plot_funs.R")
source("github/effort-study/code/functions/model_comparison_fun.R")
source("github/effort-study/code/functions/extract_posterior_predictions_fun.R")
source("github/effort-study/code/functions/retest_predictive_performance_fun.R")

library(ggpubr)

# CBU color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

# source data 
retest_data <- readRDS("data/processed_data/test_retest/retest_data.RDS")
task_dat_s1 <- retest_data$modelling_data$session_1
task_dat_s2 <- retest_data$modelling_data$session_2

# source parameter estimates
m3_para_s1_params <- readRDS(here::here("data/model_fits/retest_study/s1/m3_para_s1_parameter.RDS"))
m3_para_s2_params <- readRDS(here::here("data/model_fits/retest_study/s2/m3_para_s2_parameter.RDS"))

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate, magrittr, pushoverr)

# run model fitting? 
model_fitting <- FALSE

### (1) Intra-class correlations -----------------------------------------------

m3_para_s1_params$individual_params %<>% 
  select(subj_id, parameter, estimate) %>% 
  pivot_wider(names_from = parameter, values_from = estimate)
m3_para_s2_params$individual_params %<>% 
  select(subj_id, parameter, estimate) %>% 
  pivot_wider(names_from = parameter, values_from = estimate)

m3_para_all_params <- left_join(m3_para_s1_params$individual_params, 
                                m3_para_s2_params$individual_params, 
                                by="subj_id", 
                                suffix = c("_s1", "_s2"))

### kE
# ICC
irr::icc(m3_para_all_params %>% select(kE_s1, kE_s2), model = "twoway", "consistency", "single")

### kR
# ICC
irr::icc(m3_para_all_params %>% select(kR_s1, kR_s2), model = "twoway", "consistency", "single")

### a
# ICC
irr::icc(m3_para_all_params %>% select(a_s1, a_s2), model = "twoway", "consistency", "single")

### (2) Correlations -----------------------------------------------

# Normal Pearson's correlation between mean posterior estimates
### kE
# Pearson's correlation
cor.test(m3_para_all_params$kE_s1, m3_para_all_params$kE_s2, method = "pearson") 
# Plot
icc_kE_plot <- ggplot(m3_para_all_params, aes(x=kE_s1, y=kE_s2)) + 
  geom_point(colour = color_pal[1], size = 0.75) + 
  labs(title = "Effort sensitivity",
       x = "Session 1", y = "Session 2") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  geom_abline(color = "darkgrey", alpha = 0.75) +
  geom_smooth(fullrange=TRUE, method='lm', formula=y~x,
              colour = color_pal[1], fill = color_pal[1], 
              alpha = 0.25, size = 0.75)

### kR
# Pearson's correlation
cor.test(m3_para_all_params$kR_s1, m3_para_all_params$kR_s2, method = "pearson") 
# Plot
icc_kR_plot <- ggplot(m3_para_all_params, aes(x=kR_s1, y=kR_s2)) + 
  geom_point(colour = color_pal[2], size = 0.75) + 
  labs(title = "Reward sensitivity",
       x = "Session 1", y = "Session 2") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  geom_abline(color = "darkgrey", alpha = 0.75) +
  geom_smooth(fullrange=TRUE, method='lm', formula=y~x,
              colour = color_pal[2], fill = color_pal[2], 
              alpha = 0.25, size = 0.75)

### a
# Pearson's correlation
cor.test(m3_para_all_params$a_s1, m3_para_all_params$a_s2, method = "pearson") 
# plot
icc_a_plot <- ggplot(m3_para_all_params, aes(x=a_s1, y=a_s2)) + 
  geom_point(colour = color_pal[3], size = 0.75) + 
  labs(title = "Motivational tendency",
       x = "Session 1", y = "Session 2") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  geom_abline(color = "darkgrey", alpha = 0.75) +
  geom_smooth(fullrange=TRUE, method='lm', formula=y~x,
              colour = color_pal[3], fill = color_pal[3], 
              alpha = 0.25, size = 0.75)

retest_iccs <- ggarrange(icc_kE_plot,
                         icc_kR_plot,
                         icc_a_plot, 
                         ncol = 3, nrow = 1)

pdf(here::here("output/figures/R_plots/retest_iccs.pdf"), 
    width = 7, 
    height = 3) 

retest_iccs

dev.off()

# Correlations from embedded matrix

if(model_fitting){
  
  # pre-process data
  model_dat <- model_preprocessing(list(task_dat_s1, task_dat_s2), 
                                   retest = TRUE,
                                   subj = unique(task_dat_s1$subjID), 
                                   n_subj = 30,
                                   t_max = 64)
  
  # compile model
  m3_parabolic_retest_stan_model <- cmdstanr::cmdstan_model("github/effort-study/code/stan/models_parabolic/ed_m3_parabolic_retest.stan")

  # fit model
  m3_para_retest_fit <- m3_parabolic_retest_stan_model$sample(
    data = model_dat, 
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 6000, 
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE, 
    output_dir = NULL
  )
  
  # extract draws estimating correlations
  m3_para_R <- m3_para_retest_fit$draws(format = "df", variables = c("R_kE", "R_kR", "R_a"),
                                        inc_warmup = FALSE)
  saveRDS(m3_para_R, here::here("data/model_fits/retest_study/retest_correlation/m3_para_R.RDS"))
  
} else {
  m3_para_R <- readRDS(here::here("data/model_fits/retest_study/retest_correlation/m3_para_R.RDS"))
}

# kE
mean(m3_para_R$`R_kE[2,1]`)
hBayesDM::HDIofMCMC(m3_para_R$`R_kE[2,1]`)

# kR
mean(m3_para_R$`R_kR[2,1]`)
hBayesDM::HDIofMCMC(m3_para_R$`R_kR[2,1]`)

# a
mean(m3_para_R$`R_a[2,1]`)
hBayesDM::HDIofMCMC(m3_para_R$`R_a[2,1]`)


### (3) Posterior predictions -----------------------------------------------

## Subject wise prediction
# Session 1 predicting session 1
m3_para_posterior_pred11_subj <- retest_predictive_peformance(predicted_session_data = task_dat_s1,
                                                              prediction_parameters = m3_para_s1_params$individual_params, 
                                                              model = "m3_parabolic", 
                                                              prediction = "subject_level")

# Session 1 predicting session 2
m3_para_posterior_pred12_subj <- retest_predictive_peformance(predicted_session_data = task_dat_s2,
                                                              prediction_parameters = m3_para_s1_params$individual_params, 
                                                              model = "m3_parabolic", 
                                                              prediction = "subject_level")
t.test(m3_para_posterior_pred12_subj$prediction_accuracy, mu=0.5, alternative = "greater")

# Session 2 predicting session 1
m3_para_posterior_pred21_subj <- retest_predictive_peformance(predicted_session_data = task_dat_s1,
                                                              prediction_parameters = m3_para_s2_params$individual_params, 
                                                              model = "m3_parabolic", 
                                                              prediction = "subject_level")
t.test(m3_para_posterior_pred21_subj$prediction_accuracy, mu=0.5, alternative = "greater")

# Session 2 predicting session 2
m3_para_posterior_pred22_subj <- retest_predictive_peformance(predicted_session_data = task_dat_s2,
                                                              prediction_parameters = m3_para_s2_params$individual_params, 
                                                              model = "m3_parabolic", 
                                                              prediction = "subject_level")
## Plots
m3_para_posterior_pred11_subj_sum <- aggregate(prediction_accuracy ~ subj, 
                                               data = m3_para_posterior_pred11_subj, FUN = mean)
m3_para_posterior_pred12_subj_sum <- aggregate(prediction_accuracy ~ subj, 
                                               data = m3_para_posterior_pred12_subj, FUN = mean)
m3_para_posterior_pred21_subj_sum <- aggregate(prediction_accuracy ~ subj, 
                                               data = m3_para_posterior_pred21_subj, FUN = mean)
m3_para_posterior_pred22_subj_sum <- aggregate(prediction_accuracy ~ subj, 
                                               data = m3_para_posterior_pred22_subj, FUN = mean)

m3_para_posterior_pred_subj_all <- data.frame("comparison" = rep(c("s1s1", "s1s2", "s2s1", "s2s2"),
                                                                 each = 30), 
                                              rbind(m3_para_posterior_pred11_subj_sum, m3_para_posterior_pred12_subj_sum, 
                                                    m3_para_posterior_pred21_subj_sum, m3_para_posterior_pred22_subj_sum))

subject_chance_plot <- ggplot(data = m3_para_posterior_pred_subj_all, 
                              aes(x = prediction_accuracy, y = comparison, fill = comparison)) + 
  geom_violin( alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) + 
  guides(fill = "none", color = "none") +
  scale_fill_manual(values = color_pal[1:4]) +
  xlim(c(0.5, 1)) +
  geom_vline(xintercept = 0.5, col = "darkgrey", linetype="dotted") +
  labs(title = "Subject-level posterior prediction vs. chance", y = "Prediction direction", x = "Predictive accuracy") +
  coord_flip() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) 



## Group wise prediction
# Session 1 predicting session 1
m3_para_posterior_pred11_group <- retest_predictive_peformance(predicted_session_data = task_dat_s1,
                                                               prediction_parameters = m3_para_s1_params$group_params, 
                                                               model = "m3_parabolic", 
                                                               prediction = "group_level")

# Session 1 predicting session 2
m3_para_posterior_pred12_group <- retest_predictive_peformance(predicted_session_data = task_dat_s2,
                                                               prediction_parameters = m3_para_s1_params$group_params, 
                                                               model = "m3_parabolic", 
                                                               prediction = "group_level")
t.test(m3_para_posterior_pred12_group$prediction_accuracy, mu=0.5, alternative = "greater")

# Session 2 predicting session 1
m3_para_posterior_pred21_group <- retest_predictive_peformance(predicted_session_data = task_dat_s1,
                                                               prediction_parameters = m3_para_s2_params$group_params, 
                                                               model = "m3_parabolic", 
                                                               prediction = "group_level")
t.test(m3_para_posterior_pred21_group$prediction_accuracy, mu=0.5, alternative = "greater")

# Session 2 predicting session 2
m3_para_posterior_pred22_group <- retest_predictive_peformance(predicted_session_data = task_dat_s2,
                                                               prediction_parameters = m3_para_s2_params$group_params, 
                                                               model = "m3_parabolic", 
                                                               prediction = "group_level")

## Do the subject wise parameter estimates outperform the group level parameters?
t.test(m3_para_posterior_pred12_subj$prediction_accuracy, 
       m3_para_posterior_pred12_group$prediction_accuracy, 
       alternative = "greater")

t.test(m3_para_posterior_pred21_subj$prediction_accuracy, 
       m3_para_posterior_pred21_group$prediction_accuracy, 
       alternative = "greater")


## Plots
m3_para_posterior_pred11_group_sum <- aggregate(prediction_accuracy ~ subj, 
                                                data = m3_para_posterior_pred11_group, FUN = mean)
m3_para_posterior_pred12_group_sum <- aggregate(prediction_accuracy ~ subj, 
                                                data = m3_para_posterior_pred12_group, FUN = mean)
m3_para_posterior_pred21_group_sum <- aggregate(prediction_accuracy ~ subj, 
                                                data = m3_para_posterior_pred21_group, FUN = mean)
m3_para_posterior_pred22_group_sum <- aggregate(prediction_accuracy ~ subj, 
                                                data = m3_para_posterior_pred22_group, FUN = mean)

m3_para_posterior_pred_group_all <- data.frame("comparison" = rep(c("s1s1", "s1s2", "s2s1", "s2s2"),
                                                                  each = 30), 
                                               rbind(m3_para_posterior_pred11_group_sum, m3_para_posterior_pred12_group_sum, 
                                                     m3_para_posterior_pred21_group_sum, m3_para_posterior_pred22_group_sum))

m3_para_posterior_pred_subj_vs_group_all <- data.frame("comparison" = m3_para_posterior_pred_subj_all$comparison, 
                                                       "subj" = m3_para_posterior_pred_subj_all$subj,
                                                       "prediction_accuracy_diff" =
                                                         m3_para_posterior_pred_subj_all$prediction_accuracy -
                                                         m3_para_posterior_pred_group_all$prediction_accuracy)

subject_vs_group <- ggplot(data = m3_para_posterior_pred_subj_vs_group_all, 
                           aes(x = prediction_accuracy_diff, y = comparison, fill = comparison)) + 
  geom_violin( alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) + 
  guides(fill = "none", color = "none") +
  scale_fill_manual(values = color_pal[1:4]) +
  geom_vline(xintercept = 0, col = "darkgrey", linetype="dotted") +
  labs(title = "Subject- vs. group-level posterior prediction", y = "Prediction direction", x = "Predictive accuracy") +
  coord_flip() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) 


post_preds_plots <- ggarrange(subject_chance_plot, subject_vs_group,
                              widths = c(10, 10),
                              ncol = 2, nrow = 1)
post_preds_plots

pdf(here::here("output/figures/R_plots/retest_post_preds.pdf"), 
    width = 7,
    height = 2.75) 

post_preds_plots

dev.off()



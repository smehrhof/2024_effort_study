###########################################################################
###############--------------- MAIN ANALYSES ---------------###############
###########################################################################

### In this script: 
# (1) Prep data
# (2) Partial least squared regression
# (3) General linear models
# (4) Plotting

# Set working directory
here::i_am("github/effort-study/code/analyses/5_main_analyses.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")

# source datasets
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")
parameter_estimated <- readRDS("data/model_fits/main_study/m3_para_parameter.RDS")
meta_data <- readRDS("data/processed_data/main_study/online_meta_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, 
                 writexl, lubridate, purrr, magrittr, pls, rstanarm, bayestestR)

# Color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

# plot?
plotting <- TRUE

### (1) Prep data  -----------------------------------------------

# Merge datasets for analyses
data <- main_data$demographics %>% 
  select(subj_id, age, gender) %>% 
  left_join(main_data$questionnaire %>% 
              select(subj_id, shaps_sumScore, dars_sumScore, dars_food_drink_sumScore, 
                     dars_hobbies_sumScore, dars_social_sumScore, dars_sensory_sumScore, 
                     aes_sumScore, meq_sumScore, mctq_MSF_SC, bmi_result, findrisc_sumScore), 
            by = "subj_id") %>% 
  left_join(parameter_estimated$individual_params %>% 
              pivot_wider(id_cols = subj_id, names_from = parameter, 
                          values_from = c(estimate, hdi_lower, hdi_upper)), 
            by = "subj_id") %>% 
  # make MCTQ result numeric (minutes since 00:00)
  mutate(mctq_MSF_SC = period_to_seconds(hm(mctq_MSF_SC))/60) %>% 
  left_join(meta_data %>% select(Participant.id, Sex), by = c("subj_id" = "Participant.id"))

# Standardize questionnaire data (to be between 0 and 1) 
data %<>% 
  mutate_at(colnames(data)[4:14], rescale)

# Visualize distributions
ggplot(gather(data %>% select(c(shaps_sumScore:findrisc_sumScore))) %>% na.omit(), 
       aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# Standardize and normalize questionnaire data (to be between 0 and 1) 
# (depending on distribution)

data %<>% 
  # positively skewed
  mutate_at(c("shaps_sumScore", "mctq_MSF_SC", "findrisc_sumScore"), sqrt) %>% 
  # negatively skewed
  mutate_at(c("dars_sumScore", "dars_food_drink_sumScore", "dars_hobbies_sumScore", "dars_social_sumScore", 
              "dars_sensory_sumScore", "aes_sumScore"), norm_neg_skew)

# Visualize distributions again
ggplot(gather(data %>% select(c(shaps_sumScore:findrisc_sumScore))) %>% na.omit(), 
       aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# Standardize task parameters (to be between 0 and 1) 
data %<>% 
  mutate_at(colnames(data)[15:23], rescale)

# Transform aes and dars to be in the same direction as shaps
data %<>% 
  mutate(aes_sumScore = 1-aes_sumScore) %>% 
  mutate(dars_sumScore = 1-dars_sumScore) %>%
  mutate(dars_food_drink_sumScore = 1-dars_food_drink_sumScore) %>% 
  mutate(dars_hobbies_sumScore = 1-dars_hobbies_sumScore) %>% 
  mutate(dars_social_sumScore = 1-dars_social_sumScore) %>% 
  mutate(dars_sensory_sumScore = 1-dars_sensory_sumScore) 
  
# Transform meq to be in the same direction as mctq
data %<>% 
  mutate(meq_sumScore = 1-meq_sumScore)


### (2) Partial least squared regression -----------------------------------------------

### Effort sensitivity

set.seed(1)
all <- 1:length(data$subj_id)
# splitting (complete) data into training and testing sets
all_complete <- all[complete.cases(data)]
training <- sample(all_complete, length(all_complete) * 0.75)
training <- c(training, all[!complete.cases(data)])
testing <- all_complete[! all_complete %in% training]

set.seed(2)
kE_x_train <- data[training, c("estimate_kE", 
                               "shaps_sumScore", "dars_sumScore", "aes_sumScore", "meq_sumScore", 
                               "mctq_MSF_SC", "bmi_result", "findrisc_sumScore")]
kE_y_test <- data[testing, c("estimate_kE")]
kE_x_test <- data[testing, c("shaps_sumScore", "dars_sumScore", "aes_sumScore", "meq_sumScore", 
                             "mctq_MSF_SC", "bmi_result", "findrisc_sumScore")]

#use model to make predictions on a test set
kE_pls_model <- plsr(estimate_kE ~ shaps_sumScore + dars_sumScore + aes_sumScore + meq_sumScore +
                     mctq_MSF_SC + bmi_result + findrisc_sumScore, data = kE_x_train, validation = "CV")
summary(kE_pls_model)
validationplot(kE_pls_model)



### Reward sensitivity
set.seed(3)
# splitting (complete) data into training and testing sets
all_complete <- all[complete.cases(data)]
training <- sample(all_complete, length(all_complete) * 0.75)
training <- c(training, all[!complete.cases(data)])
testing <- all_complete[! all_complete %in% training]

set.seed(4)
kR_x_train <- data[training, c("estimate_kR", 
                               "shaps_sumScore", "dars_sumScore", "aes_sumScore", "meq_sumScore", 
                               "mctq_MSF_SC", "bmi_result", "findrisc_sumScore")]
kR_y_test <- data[testing, c("estimate_kR")]
kR_x_test <- data[testing, c("shaps_sumScore", "dars_sumScore", "aes_sumScore", "meq_sumScore", 
                             "mctq_MSF_SC", "bmi_result", "findrisc_sumScore")]

#use model to make predictions on a test set
kR_pls_model <- plsr(estimate_kR ~ shaps_sumScore + dars_sumScore + aes_sumScore + meq_sumScore +
                       mctq_MSF_SC + bmi_result + findrisc_sumScore, data = kR_x_train, validation = "CV")
summary(kR_pls_model)
validationplot(kR_pls_model)

### Choice bias

set.seed(5)
# splitting (complete) data into training and testing sets
all_complete <- all[complete.cases(data)]
training <- sample(all_complete, length(all_complete) * 0.75)
training <- c(training, all[!complete.cases(data)])
testing <- all_complete[! all_complete %in% training]

set.seed(6)
a_x_train <- data[training, c("estimate_a", 
                              "shaps_sumScore", "dars_sumScore", "aes_sumScore", "meq_sumScore", 
                              "mctq_MSF_SC", "bmi_result", "findrisc_sumScore")]
a_y_test <- data[testing, c("estimate_a")]
a_x_test <- data[testing, c("shaps_sumScore", "dars_sumScore", "aes_sumScore", "meq_sumScore", 
                            "mctq_MSF_SC", "bmi_result", "findrisc_sumScore")]

#use model to make predictions on a test set
a_pls_model <- plsr(estimate_a~shaps_sumScore+dars_sumScore+aes_sumScore+meq_sumScore+
                      mctq_MSF_SC+bmi_result+findrisc_sumScore, data=a_x_train, validation="CV")
summary(kR_pls_model)
validationplot(a_pls_model)

# model with one component has best predictive value

# predictions
a_pls_preds <- predict(a_pls_model, a_x_test, ncomp=1)
a_pls_preds <- a_pls_preds[,,1] %>% unname()

#calculate RMSE
RMSE_a <- sqrt(mean((a_pls_preds - a_y_test$estimate_a)^2))
RMSE_a
# R^2
alpha_Rs <- cor(a_pls_preds, a_y_test$estimate_a)^2
alpha_Rs 


# Permutation test: permute y labels of training set, fit model again an test on testing set
RMSE_a_perm <- c()

for(i in 1:1000){
  
  a_x_train_perm <- a_x_train
  a_x_train_perm$estimate_a <- sample(a_x_train_perm$estimate_a)
  
  a_pls_model_perm <- plsr(estimate_a~shaps_sumScore+dars_sumScore+aes_sumScore+meq_sumScore+
                             mctq_MSF_SC+bmi_result+findrisc_sumScore,
                           data = a_x_train_perm, validation="CV")
  
  a_pls_preds_perm <- predict(a_pls_model_perm, a_x_test, ncomp=1)
  a_pls_preds_perm <- a_pls_preds_perm[,,1] %>% unname()
  
  #calculate RMSE
  RMSE_a_perm[i] <- sqrt(mean((a_pls_preds_perm - a_y_test$estimate_a)^2))
}

hist(RMSE_a_perm, breaks = 40, 
     xlab = "RMSE", main = "Distribution of RMSEs under the null hypotehsis")  
abline(v = RMSE_a, col = "red", lwd = 2)
abline(v = quantile(RMSE_a_perm, probs = c(0.05)), col = "blue", lwd = 2, lty = 2)

# P value
pValue_alpha <- sum(RMSE_a_perm <= RMSE_a) / 1000
pValue_alpha

# factor loadings

a_factor_loadings <- loadings(a_pls_model)
a_factor_loadings_df <- tibble("variable" = names(a_factor_loadings[,1]),
                                   "loadings" = as.numeric(a_factor_loadings[,1]))


### Factor loadings plot

if(plotting){
  
  a_factor_loadings_df$variable <- factor(a_factor_loadings_df$variable,
                                          levels = c("shaps_sumScore", "aes_sumScore", "dars_sumScore", "meq_sumScore", 
                                                     "mctq_MSF_SC", "bmi_result", "findrisc_sumScore"))
  
  a_factor_loadings_plot <- ggplot(a_factor_loadings_df[1:5,], 
                                   aes(x=variable, y=loadings, fill=variable)) +
    geom_bar(stat="identity", alpha = 0.8) +
    scale_fill_manual(values = c(color_pal[3], color_pal[1], color_pal[5], color_pal[2], 
                                 color_pal[6], color_pal[7], color_pal[8])) +
    theme(legend.position = "none") +
    ylab("Loadings") + 
    xlab(" ") +
    scale_x_discrete(labels = c("Anhedonia", "Apathy", "Anhedonia", "Chronotype", 
                                "Chronotype", "BMI", "Metabolic health")) +
    scale_y_continuous(breaks = seq(-0.8, 0.2, 0.2), limits = c(-0.8, 0.05)) +
    theme(axis.text.x = element_text(size = 15, angle = 30, hjust = 1),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 12))
  
  pdf(file = here::here("output/figures/R_plots/PLS_plot_ppt.pdf"),  
      width = 12.75 * 0.393701, # The width of the plot in cm (transformed to inches)
      height = 11  * 0.393701) # The height of the plot in cm (transformed to inches)
  par(mar=c(0,4,0.5,0.5))
  
  a_factor_loadings_plot
  
  dev.off()
  
}


### (3) General linear models  -----------------------------------------------

# To control for age and gender, input natal sex for non-binary individuals (due to low numbers)

nb_subj <- data$subj_id[data$gender == "Non-binary"]
for(i in seq_along(nb_subj)){
  data$gender[data$subj_id == nb_subj[i]] <- meta_data$Sex[meta_data$Participant.id == nb_subj[i]]
}

# make gender dummy variable
data$gender <- as.factor(data$gender)

# SHAPS
shaps_a_glm <- stan_glm(estimate_a ~ shaps_sumScore + age + gender, data = data, 
                        iter = 1000, seed = 12345)
shaps_a_glm$coefficients
hdi(shaps_a_glm)

# DARS
dars_a_glm <- stan_glm(estimate_a ~ dars_sumScore + age + gender, data = data, 
                       iter = 1000, seed = 12345)
dars_a_glm$coefficients
hdi(dars_a_glm)

# AES
aes_a_glm <- stan_glm(estimate_a ~ aes_sumScore + age + gender, data = data, 
                        iter = 1000, seed = 12345)
aes_a_glm$coefficients
hdi(aes_a_glm)

# MEQ
meq_a_glm <- stan_glm(estimate_a ~ meq_sumScore + age + gender, data = data, 
                      iter = 1000, seed = 12345)
meq_a_glm$coefficients
hdi(meq_a_glm)

# MCTQ
mctq_a_glm <- stan_glm(estimate_a ~ mctq_MSF_SC + age + gender, data = data, 
                      iter = 1000, seed = 12345)
mctq_a_glm$coefficients
hdi(mctq_a_glm)

# BMI
bmi_a_glm <- stan_glm(estimate_a ~ bmi_result + age + gender, data = data, 
                       iter = 1000, seed = 12345)
bmi_a_glm$coefficients
hdi(bmi_a_glm)

# FINDRISC
findrisc_a_glm <- stan_glm(estimate_a ~ findrisc_sumScore + age + gender, data = data, 
                       iter = 1000, seed = 12345)
findrisc_a_glm$coefficients
hdi(findrisc_a_glm)

# DARS follow-ups
dars_food_drink_a_glm <- stan_glm(estimate_a ~ dars_food_drink_sumScore + age + gender, data = data, 
                       iter = 1000, seed = 1)
dars_food_drink_a_glm$coefficients
hdi(dars_food_drink_a_glm)

dars_hobbies_a_glm <- stan_glm(estimate_a ~ dars_hobbies_sumScore + age + gender, data = data, 
                       iter = 1000, seed = 2)
dars_hobbies_a_glm$coefficients
hdi(dars_hobbies_a_glm)

dars_social_a_glm <- stan_glm(estimate_a ~ dars_social_sumScore + age + gender, data = data, 
                       iter = 1000, seed = 3)
dars_social_a_glm$coefficients
hdi(dars_social_a_glm)

dars_sensory_a_glm <- stan_glm(estimate_a ~ dars_sensory_sumScore + age + gender, data = data, 
                       iter = 1000, seed = 4)
dars_sensory_a_glm$coefficients
hdi(dars_sensory_a_glm)


### (4) Plotting -----------------------------------------------

### GLMs plot

if(plotting){
  
  # SHAPS
  a_shaps_plot <- ggplot(data, aes(x = shaps_sumScore, y = estimate_a)) + 
    geom_point(color = color_pal[3], alpha = 0.5, size = 1.25) +
    geom_smooth(method = glm, color = color_pal[3], fill = color_pal[3]) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_x_continuous(breaks = c(0.13, 0.87), limits = c(0, 1), 
                       labels = c("low\nanhedonia", "high\nanhedonia")) +
    labs(y = "Choice bias") +
    theme(plot.title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.ticks.x = element_blank()) +
    ggtitle("SHAPS")
  
  # DARS
  a_dars_plot <- ggplot(data, aes(x = dars_sumScore, y = estimate_a)) + 
    geom_point(color = color_pal[1], alpha = 0.5, size = 1.25) +
    geom_smooth(method = glm, color = color_pal[1], fill = color_pal[1]) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_x_continuous(breaks = c(0.13, 0.87), limits = c(0, 1), 
                       labels = c("low\nanhedonia", "high\nanhedonia")) +
    labs(y = "Choice bias") +
    theme(plot.title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.ticks.x = element_blank()) +
    ggtitle("DARS")
  
  # AES
  a_aes_plot <- ggplot(data, aes(x = aes_sumScore, y = estimate_a)) + 
    geom_point(color = color_pal[2], alpha = 0.5, size = 1.25) +
    geom_smooth(method = glm, color = color_pal[2], fill = color_pal[2]) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_x_continuous(breaks = c(0.13, 0.87), limits = c(0, 1), 
                       labels = c("low\nanhedonia", "high\nanhedonia")) +
    labs(y = "Choice bias") +
    theme(plot.title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.ticks.x = element_blank()) +
    ggtitle("AES")
  
  pdf(file = here::here("output/figures/R_plots/psych_glm_plot.pdf"),  
      width = 9, # The width of the plot in cm (transformed to inches)
      height = 4) # The height of the plot in cm (transformed to inches)
  par(mar=c(0,4,0.5,0.5))

  ggarrange(a_shaps_plot, a_dars_plot, a_aes_plot, 
            ncol = 3, nrow = 1)
  
  dev.off()
  
}















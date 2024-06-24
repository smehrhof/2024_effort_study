##########################################################################################
###############--------------- MODEL AGNOSTIC TASK ANALYSES ---------------###############
##########################################################################################

### In this script: 
# (1) Mixed-effects ANOVA
# (2) Task descriptives
# (3) Plotting
# (4) Finger switching comparison
# (5) Model agnostic effects of neuropsychiatric symptoms
# (6) "Extra effort"

# Set working directory
here::i_am("github/effort-study/code/analyses/2_task_model_agnostic.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")
source("github/effort-study/code/functions/plot_funs.R")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, magrittr, purrr, here, janitor, MatchIt, writexl,
                 lubridate, nlme, reshape2)

# CBU color pallet 
color_pal <- c("#E94D36", "#5B9BD5", "#71AB48", "#FDC219", "#8456B8", "#FF7236", "#1FD5B3", "#F781BE")

### (1) Mixed-effects ANOVA -----------------------------------------------

choice_data <- main_data$modelling_data %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))
  
anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = choice_data))

effort_levels <- choice_data$effort_a %>% unique() %>% sort()
amount_levels <- choice_data$amount_a %>% unique() %>% sort()

# Post hoc anovas
# per reward level
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[1])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[2])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[3])))
anova(lme(mean_choice ~ effort_a, random= ~1|subjID, 
          data = choice_data %>% filter(amount_a == amount_levels[4])))

# per effort level
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[1])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[2])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[3])))
anova(lme(mean_choice ~ amount_a, random= ~1|subjID, 
          data = choice_data %>% filter(effort_a == effort_levels[4])))


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

# acceptec challenges plot
acceptance_plot <- task_plot(main_data$modelling_data, 
                             main_title = "", 
                             plot_type = "line_plot",
                             arrange_cols = 0, arrange_rows = 0)
acceptance_plot

# cummulative offer plot

trial_wise <-  main_data$modelling_data %>%
  mutate(effort_a = round(effort_a),
         amount_a = round(amount_a)) %>%
  group_by(trial) %>%
  summarise("mean_effort" = mean(effort_a),
            "se_effort" = se(effort_a),
            "mean_reward" = mean(amount_a),
            "se_reward" = se(amount_a)) 

dat_trial <- trial_wise %>%
  pivot_longer(
    cols = c(mean_effort, mean_reward), names_to = "outcome", values_to = "values"
  )
 
dat_trial %>%
  ggplot(aes(x = trial, y = values, colour = outcome)) +
  geom_line() + geom_point(aes(shape = outcome)) +

  scale_color_manual(name = " ",
                     values = c(color_pal[3], color_pal[1]), 
                     labels = c("Reward", "Effort")) +
  
  scale_shape_manual(name = " ",
                     values = c(15, 16), 
                     labels = c("Reward", "Effort")) +
  
  scale_fill_manual(values = c(color_pal[3], color_pal[1]), name = " ",
                    labels = c("Reward", "Effort"), guide = "none") +
  
  labs(title = "", x = "Trial", y = "Effort / Reward level (SE)") +
  ylim(1.5, 3.5) +
  
  
  theme(
    text = element_text(size = 12),
    plot.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)) +

    # set aspect ratio for plot output
   # aspect.ratio= 1/3) +
  
  # Format axes and axis lines
  scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  scale_y_continuous(breaks = seq(1.5, 3.5, .5))# +
  #annotate(x = 0, xend = 70, y = -Inf, yend = -Inf, geom = "segment", lwd = .75) +
  #annotate(x = -Inf, xend = -Inf, y = 1.5, yend = 3.5, geom = "segment", lwd = .75)





### (4) Finger switching comparison -----------------------------------------------

finger_switch_id <- main_data$questionnaire %>% 
  filter(game_response_1 == 1) %>% 
  .$subj_id

main_data$modelling_data %<>% 
  mutate(finger_switching = case_when(subjID %in% finger_switch_id ~ 1, 
                                      .default = 0))

# Are main and interaction effects effort and reward on trail acceptance present in 
# participants that report finger switching? 

choice_data_fs <- main_data$modelling_data %>% 
  filter(finger_switching == 1) %>% 
  # acceptance probability per subject, effort level and reward level
  group_by(subjID, effort_a, amount_a) %>% 
  summarize(mean_choice = mean(choice))

anova(lme(mean_choice ~ effort_a * amount_a, random= ~1|subjID, 
          data = choice_data_fs))

# Is there a difference in overall number of accepted trials?
var.test(mean_choice ~ finger_switching, 
         data = main_data$modelling_data %>% 
           group_by(subjID, finger_switching) %>% 
           summarize(mean_choice = mean(choice)))
t.test(mean_choice ~ finger_switching, 
       data = main_data$modelling_data %>% 
         group_by(subjID, finger_switching) %>% 
         summarize(mean_choice = mean(choice)), 
       var.equal = TRUE)

### (5) Model agnostic effects of neuropsychiatric symptoms -----------------------------------------------

### Plotting

# Staircasing development

# number trials by staircase 
# 16 staircases, 4 trials each
main_data$game %<>% 
  filter(phase == "game") %>% 
  group_by(trialType, subj_id) %>% 
  mutate(staircase_num = row_number()) 

# plot by shaps score

main_data$questionnaire %>%
  .$shaps_sumScore %>% 
  quantile(probs = seq(0, 1, 0.25))

# low shaps
low_shaps_ids <- main_data$questionnaire %>% 
  filter(shaps_sumScore <= 4) %>% 
  .$subj_id

low_shaps_plots <- list()

for(i in 2:4){
  
  low_shaps <- main_data$game %>% 
    filter(subj_id %in% low_shaps_ids) %>% 
    filter(staircase_num == i) %>% 
    tabyl(offerEffort, offerReward, subj_id) %>% 
    unname %>%  
    map(as.matrix)
  
  low_shaps <- apply(simplify2array(low_shaps), c(1,2), mean)
  
  low_shaps %<>%
    as.data.frame() %>%
    melt(id.vars = "offerEffort") 
  
  low_shaps_plots[[i]] <- ggplot(low_shaps, aes(x = variable, y = offerEffort, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "#E8EEE4", high = "darkgreen", limits = c(0, 4.572614108)) +
    labs(x = "Reward", y = "Effort", fill = "Mean count") +
    ggtitle("Low SHAPS") +
    theme(legend.position = "none",
          plot.title = element_text(size = 13),
          axis.title = element_text(size = 13),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13)) 
}

low_shaps_plot <- ggarrange(low_shaps_plots[[2]], low_shaps_plots[[3]], low_shaps_plots[[4]],
                            ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")

low_shaps_plot <- annotate_figure(low_shaps_plot,
                                  top = text_grob("Low SHAPS", hjust = 3.75,
                                                  color = "black", size = 14))


# high shaps
high_shaps_ids <- main_data$questionnaire %>% 
  filter(shaps_sumScore > 13) %>% 
  .$subj_id

high_shaps_plots <- list()

for(i in 2:4){
  
  high_shaps <- main_data$game %>% 
    filter(subj_id %in% high_shaps_ids) %>% 
    filter(staircase_num == i) %>% 
    tabyl(offerEffort, offerReward, subj_id) %>% 
    unname %>%  
    map(as.matrix)
  
  high_shaps <- apply(simplify2array(high_shaps), c(1,2), mean)
  
  high_shaps %<>%
    as.data.frame() %>%
    melt(id.vars = "offerEffort") 
  
  high_shaps_plots[[i]] <- ggplot(high_shaps, aes(x = variable, y = offerEffort, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "#E8EEE4", high = "darkgreen", limits = c(0, 4.572614108)) +
    ggtitle("High SHAPS") +
    labs(x = "Reward", y = "Effort", fill = "Mean count") +
    theme(legend.position = "none",
          plot.title = element_text(size = 13),
          axis.title = element_text(size = 13),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          legend.title = element_text(size =13),
          legend.text = element_text(size = 13)) 
  
}

high_shaps_plot <- ggarrange(high_shaps_plots[[2]], high_shaps_plots[[3]], high_shaps_plots[[4]],
                             ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")

high_shaps_plot <- annotate_figure(high_shaps_plot,
                                   top = text_grob("High SHAPS", hjust = 3.75,
                                                   color = "black", size = 14))

ggarrange(low_shaps_plot, high_shaps_plot,
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")

staircase_plot <- ggarrange(low_shaps_plots[[4]], high_shaps_plots[[4]],
                            ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

staircase_plot <- annotate_figure(staircase_plot,
                                  top = text_grob("Staircasing development", hjust = 1.35,
                                                  color = "black", size = 14))

# Acceptance proportion 

se <- function(x) {(sd(x) / sqrt(length(x))) * 100}

# Across effort levels

choice_effort_low_subj <- main_data$modelling_data %>% 
  filter(subjID %in% low_shaps_ids) %>% 
  group_by(subjID, effort_a) %>% 
  summarise(mean_accept = mean(choice) * 100,
            se_accept = se(choice))

choice_effort_low <- choice_effort_low_subj %>% 
  group_by(effort_a) %>% 
  summarise(mean_accept = mean(mean_accept),
            se_accept = mean(se_accept)) %>% 
  add_column(shaps = "low")

choice_effort_high_subj <- main_data$modelling_data %>% 
  filter(subjID %in% high_shaps_ids) %>% 
  group_by(subjID, effort_a) %>% 
  summarise(mean_accept = mean(choice) * 100,
            se_accept = se(choice))

choice_effort_high <- choice_effort_high_subj %>% 
  group_by(effort_a) %>% 
  summarise(mean_accept = mean(mean_accept),
            se_accept = mean(se_accept)) %>% 
  add_column(shaps = "high")

choice_effort <- rbind(choice_effort_low, choice_effort_high) %>% 
  mutate_at(vars(effort_a, shaps), factor)

choice_effort_plot <- ggplot(choice_effort, aes(x = effort_a, y = mean_accept, group = shaps)) +
  geom_line(aes(color = shaps), size = 1, position = position_dodge(.1)) +
  geom_point(aes(color = shaps), size = 1, position = position_dodge(.1)) +
  geom_errorbar(aes(ymin = mean_accept - se_accept, ymax = mean_accept + se_accept, color = shaps),
                width = .1, position = position_dodge(.1)) +
  scale_color_manual(values = c(color_pal[5], color_pal[4]), name = "SHAPS", labels = c("high", "low")) + 
  ylab("% Accepted (SE)") + 
  xlab("Effort") + 
  ggtitle("Across effort levels") +
  theme(legend.position = "none",
        plot.title = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  scale_x_discrete(labels = 1:4, expand = c(0.05, 0.05))
choice_effort_plot

# Across effort levels

choice_reward_low_subj <- main_data$modelling_data %>% 
  filter(subjID %in% low_shaps_ids) %>% 
  group_by(subjID, amount_a) %>% 
  summarise(mean_accept = mean(choice) * 100,
            se_accept = se(choice))

choice_reward_low <- choice_reward_low_subj %>% 
  group_by(amount_a) %>% 
  summarise(mean_accept = mean(mean_accept),
            se_accept = mean(se_accept)) %>% 
  add_column(shaps = "low")

choice_reward_high_subj <- main_data$modelling_data %>% 
  filter(subjID %in% high_shaps_ids) %>% 
  group_by(subjID, amount_a) %>% 
  summarise(mean_accept = mean(choice) * 100,
            se_accept = se(choice))

choice_reward_high <- choice_reward_high_subj %>% 
  group_by(amount_a) %>% 
  summarise(mean_accept = mean(mean_accept),
            se_accept = mean(se_accept)) %>% 
  add_column(shaps = "high")

choice_reward <- rbind(choice_reward_low, choice_reward_high) %>% 
  mutate_at(vars(amount_a, shaps), factor)

choice_reward_plot <- ggplot(choice_reward, aes(x = amount_a, y = mean_accept, group = shaps)) +
  geom_line(aes(color = shaps), size = 1, position = position_dodge(.1)) +
  geom_point(aes(color = shaps), size = 1, position = position_dodge(.1)) +
  geom_errorbar(aes(ymin = mean_accept - se_accept, ymax = mean_accept + se_accept, color = shaps),
                width = .1, position = position_dodge(.1)) +
  scale_color_manual(values = c(color_pal[5], color_pal[4]), name = "SHAPS", labels = c("high", "low")) + 
  ylab("% Accepted (SE)") + 
  xlab("Reward") + 
  ggtitle("Across reward levels") +
  theme(legend.position = "none",
        plot.title = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size =13),
        legend.text = element_text(size = 13)) +
  scale_x_discrete(labels = 1:4, expand = c(0.05, 0.05))
choice_reward_plot

accept_plot <- ggarrange(choice_effort_plot, choice_reward_plot,
                         ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

accept_plot <- annotate_figure(accept_plot, 
                               top = text_grob("Proportion of accepted trials", hjust = 0,
                                               color = "black", size = 14))

model_agnostic_plot <- ggarrange(accept_plot, staircase_plot,
                                 ncol = 1, nrow = 2, common.legend = FALSE)


### Testing

# across all effort levels
mean_accept_data <- main_data$modelling_data %>% 
  group_by(subjID, effort_a, amount_a) %>% 
  summarise(prop_accept = mean(choice)) %>% 
  filter(effort_a < 2)

mean_accept_data <- mean_accept_data %>% 
  group_by(subjID) %>% 
  summarise(prop_accept = mean(prop_accept))

shaps_demog <- right_join(main_data$questionnaire %>% 
                            select(subj_id, shaps_sumScore, aes_sumScore), 
                          main_data$demographics %>% 
                            select(subj_id, age, gender),
                          by="subj_id") %>% 
  rename(subjID = subj_id) %>% 
  # rescale questionnaires to be betweeh 0 and 1
  mutate_at(c("shaps_sumScore", "aes_sumScore"), rescale) %>% 
  # positively skewed
  mutate_at(c("shaps_sumScore"), sqrt) %>% 
  # negatively skewed
  mutate_at(c("aes_sumScore"), norm_neg_skew) %>% 
  # transform aes to be interpretable in the same direction as the shaps (and in line with main result reporting)
  mutate(aes_sumScore = 1-aes_sumScore)

# To control for age and gender, input natal sex for non-binary individuals (due to low numbers)

nb_subj <- shaps_demog$subjID[shaps_demog$gender == "Non-binary"]
for(i in seq_along(nb_subj)){
  shaps_demog$gender[shaps_demog$subjID == nb_subj[i]] <- meta_data$Sex[meta_data$Participant.id == nb_subj[i]]
}

mean_accept_data <- right_join(mean_accept_data, shaps_demog,
                               by="subjID")

mean_accept_shaps_glm <- stan_glm(prop_accept ~ shaps_sumScore + age + gender, data = mean_accept_data, 
                                  iter = 10000, seed = 12345)
mean_accept_shaps_glm$coefficients
hdi(mean_accept_shaps_glm)

mean_accept_aes_glm <- stan_glm(prop_accept ~ aes_sumScore + age + gender, data = mean_accept_data, 
                                iter = 10000, seed = 12345)
mean_accept_aes_glm$coefficients
hdi(mean_accept_aes_glm)

# per effort level

effort_levels <- main_data$modelling_data$effort_a %>% unique() %>% sort()
effect_by_effort_m <- list()
effect_by_effort_hdi <- list()

for(j in 1:4){
  
  # across all effort levels
  mean_accept_data <- main_data$modelling_data %>% 
    filter(effort_a == effort_levels[j]) %>%
    group_by(subjID) %>% 
    summarise(prop_accept = mean(choice)*100)
  
  shaps_demog <- right_join(main_data$questionnaire %>% 
                              select(subj_id, shaps_sumScore), 
                            main_data$questionnaire %>% 
                              select(subj_id, aes_sumScore), 
                            main_data$demographics %>% 
                              select(subj_id, age, gender),
                            by="subj_id") %>% 
    rename(subjID = subj_id)
  
  # To control for age and gender, input natal sex for non-binary individuals (due to low numbers)
  
  nb_subj <- shaps_demog$subjID[shaps_demog$gender == "Non-binary"]
  for(i in seq_along(nb_subj)){
    shaps_demog$gender[shaps_demog$subjID == nb_subj[i]] <- meta_data$Sex[meta_data$Participant.id == nb_subj[i]]
  }
  
  mean_accept_data <- right_join(mean_accept_data, shaps_demog,
                                 by="subjID")
  
  mean_accept_glm <- stan_glm(prop_accept ~ shaps_sumScore + age + gender, data = mean_accept_data, 
                              iter = 1000, seed = 12345)
  effect_by_effort_m[[j]] <- mean_accept_glm$coefficients
  effect_by_effort_hdi[[j]] <- hdi(mean_accept_glm)[2,3:4]
  
}

# per reward level

reward_levels <- main_data$modelling_data$amount_a %>% unique() %>% sort()
effect_by_reward_m <- list()
effect_by_reward_hdi <- list()


for(j in 1:4){
  
  # across all reward levels
  mean_accept_data <- main_data$modelling_data %>% 
    filter(amount_a == reward_levels[j]) %>%
    group_by(subjID) %>% 
    summarise(prop_accept = mean(choice)*100)
  
  shaps_demog <- right_join(main_data$questionnaire %>% 
                              select(subj_id, shaps_sumScore), 
                            main_data$demographics %>% 
                              select(subj_id, age, gender),
                            by="subj_id") %>% 
    rename(subjID = subj_id)
  
  # To control for age and gender, input natal sex for non-binary individuals (due to low numbers)
  
  nb_subj <- shaps_demog$subjID[shaps_demog$gender == "Non-binary"]
  for(i in seq_along(nb_subj)){
    shaps_demog$gender[shaps_demog$subjID == nb_subj[i]] <- meta_data$Sex[meta_data$Participant.id == nb_subj[i]]
  }
  
  mean_accept_data <- right_join(mean_accept_data, shaps_demog,
                                 by="subjID")
  
  mean_accept_glm <- stan_glm(prop_accept ~ shaps_sumScore + age + gender, data = mean_accept_data, 
                              iter = 1000, seed = 12345)
  effect_by_reward_m[[j]] <- mean_accept_glm$coefficients
  effect_by_reward_hdi[[j]] <- hdi(mean_accept_glm)[2,3:4]
  
}


### (6) "Extra effort" -----------------------------------------------

extra_click_data <- main_data$game %>%
  filter(phase == "game") %>% 
  filter(choice == 1) %>% 
  #filter(clicks >= goalClicks) %>% 
  # exclude 3 participants with faulty goal clicks parameter
  filter(goalClicks != 0) %>% 
  select(subj_id, offerEffort, offerReward, choice, clicks, goalClicks, clicking_calibration) %>% 
  add_column("extra_clicks" = (.$clicks / .$goalClicks)) %>% 
  #clicking per subject, effort level and reward level
  group_by(subj_id, offerEffort, offerReward) %>% 
  summarize(mean_extra = mean(extra_clicks)) 

# effect of effort and reward on extra clicks
anova(lme(mean_extra ~ offerEffort * offerReward, random= ~1|subj_id, 
          data = extra_click_data))

# plot
choicePlot <- list()
for(i in 1:4){
  
  rewards <- sort(unique(extra_click_data$offerReward))
  
  choicePlot[[i]] <- ggplot(data = extra_click_data %>% 
                              mutate(offerEffort = as.factor(offerEffort)) %>% 
                              filter(offerReward == rewards[i]), 
                            aes(x=offerEffort, y=mean_extra, fill=offerEffort)) +
    geom_bar(stat="summary", fun = "mean", alpha = 0.8) +
    scale_fill_manual(values=c('#E94D36','#5B9BD5','#71AB48','#FFBF00')) +
    theme(legend.position = "none", 
          title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.ticks.x = element_blank()) +
    ggtitle(paste("Reward Level ", i)) +
    scale_x_discrete(labels = c("1", "2", "3", "4"))
  
}  

ggarrange(choicePlot[[1]], choicePlot[[2]], choicePlot[[3]], choicePlot[[4]],
          ncol = 4, nrow = 1, common.legend = TRUE)

# symptom effect? 

extra_click_data %<>% 
  group_by(subj_id) %>% 
  summarize(mean_extra = mean(mean_extra)) 

extra_click_data <- right_join(extra_click_data, shaps_demog %>% rename(subj_id = subjID),
                               by="subj_id")

extra_click_glm <- stan_glm(mean_extra ~ shaps_sumScore + age + gender, 
                            data = extra_click_data, 
                            iter = 4000, seed = 42345)
extra_click_glm$coefficients
hdi(extra_click_glm)

### (7) Accepted but failed trials -----------------------------------------------


failed_trial_data <- main_data$game %>%
  filter(phase == "game") %>% 
  filter(choice == 1) %>% 
  # exclude 3 participants with faulty goal clicks parameter
  filter(goalClicks != 0) %>% 
  select(subj_id, offerEffort, offerReward, choice, clicks, goalClicks, clicking_calibration) %>% 
  add_column("trial_succeed" = ifelse(.$clicks >= .$goalClicks, 1, 0)) %>% select(trial_succeed) %>% 
  group_by(subj_id) %>%
  summarise(fail_prop = mean(trial_succeed)) 

failed_trial_data <- right_join(failed_trial_data, shaps_demog %>% rename(subj_id = subjID),
                                by="subj_id")

failed_trial_glm <- stan_glm(fail_prop ~ shaps_sumScore + age + gender, 
                             data = failed_trial_data, 
                             iter = 4000, seed = 42345)
failed_trial_glm$coefficients
hdi(failed_trial_glm)[2,3]



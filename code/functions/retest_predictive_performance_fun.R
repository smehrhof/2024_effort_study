################################################################################
###############-- TEST-RETEST POSTERIOR PREDICTIVE PERFORMANCE --###############
################################################################################

## Prediction session a choice data from session b parameters ------------------
# @ predicted_session_data: data of the sessions to be predicted
# @ prediction_parameters: parameter estimates based on other session; can either be dataframe with individual parameter estimates or group level estimates vector
# @ prediction: are predictions made on subject level parameters (default), or group level? 

retest_predictive_peformance <- function(predicted_session_data,
                                         prediction_parameters, 
                                         model = "m3_parabolic", 
                                         prediction = "subject_level") {
  
  if(prediction == "group_level"){
    kE <- prediction_parameters %>% 
      filter(parameter == "mu_kE") %>% .$estimate
    
    if(grepl("m1", model)){
      kR <- 1
    } else {
      kR <- prediction_parameters %>% 
        filter(parameter == "mu_kR") %>% .$estimate
    }
    
    if(grepl("m2", model)){
      a <- 0
    } else {
      a <- prediction_parameters %>% 
        filter(parameter == "mu_a") %>% .$estimate
    }
  }
  
  all_prediction_acc <- c()
  
  for(subj in unique(predicted_session_data$subjID)){
    
    if(prediction == "subject_level"){
      kE <- prediction_parameters %>% 
        filter(subj_id == subj) %>% .$kE
      
      if(grepl("m1", model)){
        kR <- 1
      } else {
        kR <- prediction_parameters %>% 
          filter(subj_id == subj) %>% .$kR
      }
      if(grepl("m2", model)){
        a <- 0
      } else {
        a <- prediction_parameters %>% 
          filter(subj_id == subj) %>% .$a
      }
      
    }
    
    subj_dat <- predicted_session_data[predicted_session_data$subjID == subj,]
    prediction_acc <- c()
    
    for(t in subj_dat$trial){
      
      if(grepl("linear", model)){
        SV <- kR * subj_dat[t,"amount_a"] - kE * subj_dat[t,"effort_a"]
        prob_A <- 1 / (1 + exp(-1*(a + SV)))
      } else if(grepl("parabolic", model)){
        SV <- kR * subj_dat[t,"amount_a"] - kE * subj_dat[t,"effort_a"]^2
        prob_A <- 1 / (1 + exp(-1*(a + SV)))
      } else if(grepl("exponential", model)){
        SV <- kR * subj_dat[t,"amount_a"] * exp(-1 * kE * subj_dat[t,"effort_a"])
        prob_A <- 1 / (1 + exp(-5 * ((alpha[subj]) + SV)))
      }
      
      model_choice <- sample(0:1, 1, prob = c(1-prob_A, prob_A))
      prediction_acc[t] <- 1 - abs(subj_dat[t,"choice"] - model_choice)
    }
    all_prediction_acc <- c(all_prediction_acc, prediction_acc)
  }
  
  all_prediction_res <- data.frame("subj" = predicted_session_data$subjID,
                                   "trial" = rep(1:64, 30),
                                   "prediction_accuracy" = all_prediction_acc)
  return(all_prediction_res)
}



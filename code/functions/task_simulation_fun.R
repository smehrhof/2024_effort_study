################################################################################
###############----------- TASK SIMULATION FUNCTION -------------###############
################################################################################

## Simulates task data given a set of parameters  ------------------------------
# @ n: number of agents to simulate
# @ model: model to use for simulation
# @ kE: vector of effort sensitivity parameters
# @ kR: vector of reward sensitivity parameters
# @ alpha: vector of alpha parameters

task_simulation <- function(n = 1,
                            model = "m3_parabolic",
                            kE = 1,
                            kR = 1, 
                            alpha = 0){
  
  simulated_task <- data.frame()

  # Save simulation parameters
  simulation_parameters <- data.frame("subj" = 1:n,
                                      "model" = rep(model, n),
                                      "kE" = kE, 
                                      "kR" = kR, 
                                      "a" = alpha)
  
  if(grepl("m1", model)){
    kR <- rep(1, n)
  }
  if(grepl("m2", model)){
    
    if(grepl("exponential", model)){
      alpha <- rep(-0.5, n)  
    } else {
      alpha <- rep(0, n)  
    }
    
  }
  
  effort_levels <- reward_levels <- standardization(1:4)
  
  for(subj in 1:n){
    
    # Staircases: (effort, reward)
    staircases <- list(c(1, 1), c(1, 2), c(1, 3), c(1, 4),
                       c(2, 1), c(2, 2), c(2, 3), c(2, 4),
                       c(3, 1), c(3, 2), c(3, 3), c(3, 4), 
                       c(4, 1), c(4, 2), c(4, 3), c(4, 4))
    
    trial_order <- sample(rep(1:16, 4))
    
    agent_dat <- matrix(NA, ncol = 9, nrow = 64)
    colnames(agent_dat) <- c("subjID", "trial", "trial_type", 
                             "effort_a", "amount_a", "choice", 
                             "effort_b", "amount_b", "SV")
    
    for(t in 1:64){
      trial_type <- trial_order[t]
      
      agent_dat[t, "subjID"] <- subj
      agent_dat[t, "trial"] <- t
      # create offer
      agent_dat[t, "trial_type"] <- trial_type
      agent_dat[t, "effort_a"] <- effort_levels[staircases[trial_type][[1]][1]]
      agent_dat[t, "amount_a"] <- reward_levels[staircases[trial_type][[1]][2]]
      
      agent_dat[t, "effort_b"] <- 0
      agent_dat[t, "amount_b"] <- 0
      
      ## Linear
      # Compute subjective value
      if(grepl("linear", model)) {
        S_E <- kE[subj] * agent_dat[t, "effort_a"]
        S_R <- kR[subj] * agent_dat[t, "amount_a"]
        SV <- S_R - S_E
      # Choice probability
        prob_A <- 1 / (1 + exp(-1*((alpha[subj]) + SV)))
        
      ## Parabolic
      # Compute subjective value
      } else if(grepl("parabolic", model)) {
        S_E <- kE[subj] * agent_dat[t, "effort_a"]^2
        S_R <- kR[subj] * agent_dat[t, "amount_a"]
        SV <- S_R - S_E
      # Choice probability
        prob_A <- 1 / (1 + exp(-1*((alpha[subj]) + SV)))
        
      ## Exponential
      # Compute subjective value
      } else if(grepl("exponential", model)) {
        S_Ea <- exp(-1 * kE[subj] * agent_dat[t, "effort_a"])
        S_Ra <- kR[subj] * agent_dat[t, "amount_a"]
        SV <- S_Ra * S_Ea
      # Choice probability
        prob_A <- 1 / (1 + exp(-5 * ((alpha[subj]) + SV)))
      }
      
      # Sample choice
      agent_dat[t, "SV"] <- SV
      agent_dat[t, "choice"] <- sample(1:0, 1, prob = c(prob_A, 1-prob_A))
  
      # Update for next staircase iteration
      # Accept
      if(agent_dat[t, "choice"] == 1){
        # no updating 
        if(staircases[trial_type][[1]][1] == 4 
           & staircases[trial_type][[1]][2] == 1){
          staircases[trial_type][[1]] <- staircases[trial_type][[1]]
          
        # update reward (decrease)
        } else if(staircases[trial_type][[1]][1] == 4){
          staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]-1
          
        # update effort (increase)
        } else if(staircases[trial_type][[1]][2] == 1){
          staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]+1
          
        # random update choice (increase effort or decrease reward)
        } else {
          random_update <- sample(1:2, 1)
          if(random_update == 1){
            staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]+1
          } else {
            staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]-1
          }
        }
      # Reject
      } else if(agent_dat[t, "choice"] == 0){
        # no updating 
        if(staircases[trial_type][[1]][1] == 1 & staircases[trial_type][[1]][2] == 4){
          staircases[trial_type][[1]] <- staircases[trial_type][[1]]
          
          # update reward (increase)
        } else if(staircases[trial_type][[1]][1] == 1){
          staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]+1
          
          # update effort (decrease)
        } else if(staircases[trial_type][[1]][2] == 4){
          staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]-1
          
          # random update choice (decrease effort or increase reward)
        } else {
          random_update <- sample(1:2, 1)
          if(random_update == 1){
            staircases[trial_type][[1]][1] <- staircases[trial_type][[1]][1]-1
          } else {
            staircases[trial_type][[1]][2] <- staircases[trial_type][[1]][2]+1
          }
        }
      }
    }
    
    agent_dat <- as.data.frame(agent_dat)
    simulated_task <- rbind(simulated_task, agent_dat)
  }
  
  return(list("simulation_parameters" = simulation_parameters, 
              "simulated_task" = simulated_task))
}


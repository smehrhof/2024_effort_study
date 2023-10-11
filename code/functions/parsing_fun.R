################################################################################
###############---------------- PARSING FUNCTION ----------------###############
################################################################################

## Parsing study data from JATOS files -----------------------------------------
# @ file: relative path to .txt data file or existing R object
# @ study_part: parse data for online, retest, screening, or chronotype study? 
# @ demographic_dat: include demographic data in output?
# @ task_dat: include task data in output?
# @ modelling_dat: include task data formatted for modelling?
# @ questionnaire_dat: include questionnaire data in output?
# @ raw_data: include raw questionnaire data?
# @ exclude_subjs: subjects to be excluded due to faulty data


parsing_fun <- function(file, 
                        study_part = "online",
                        demographic_dat = TRUE,
                        task_dat = TRUE,
                        modelling_dat = TRUE,
                        questionnaire_dat = TRUE,
                        raw_data = FALSE,
                        exclude_subjs = c("5a47c383e074d600013912cc|369450")){
  
  require(lubridate) 
  require(jsonlite)
  require(ggplot2)
  require(ggpubr)
  
  # Does data still have to be imported?
  if(class(file) == "list"){
    raw_dat <- file
  } else if(class(file) == "character") {
    
    if(length(file) == 1){
      raw_dat <- jsonlite::fromJSON(sprintf('[%s]', paste(readLines(file, encoding="UTF-8", warn=F), collapse = ',')))
    } else if(length(file) > 1){
      raw_dat <- list()
      for(i in 1:length(file)){
        raw_dat <- append(raw_dat,  jsonlite::fromJSON(sprintf('[%s]', paste(readLines(file[i], encoding="UTF-8", warn=F), collapse = ','))))
      }
    }
    
  }
  
  
  if(study_part == "screening"){
    demographic_dat <- TRUE
    task_dat <- FALSE
    modelling_dat <- FALSE
    questionnaire_dat <- TRUE
  }
  
  ### COMPONENT 1: Demographic data ------------------------------------------
  
  if(study_part == "online" | study_part == "chronotype_followup"){
    res_comp_1 <- which(lapply(1:length(raw_dat), 
                               function (i) any(grepl("Welcome to the experiment.", raw_dat[[i]], fixed=T)) & 
                                 !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
  } else if(study_part == "re_test"){
    res_comp_1 <- which(lapply(1:length(raw_dat), 
                               function (i) any(grepl("Welcome to the experiment.", raw_dat[[i]], fixed=T)) &
                                 raw_dat[[i]]$session[1] == 1) %in% TRUE)
  } else if(study_part == "screening"){
    res_comp_1 <- which(lapply(1:length(raw_dat), 
                               function (i) any(grepl("Welcome to the study.", raw_dat[[i]], fixed=T))) %in% TRUE)
  }
  
  if(study_part == "online" | study_part == "re_test" | study_part == "chronotype_followup"){
    comp_1_df <- matrix(ncol = 24, nrow = (length(res_comp_1)))   
    comp_1_df <- as.data.frame(comp_1_df)
    colnames(comp_1_df) <- c("subj_id",
                             "age", "gender", "ethnicity", "ses", "income", "english", "neurological", "neurological_condition", 
                             "psych_neurdev", "psych_neurdev_condition", "psych_neurdev_condition_time", "medication", 
                             "contraceptive", "antidepressant", "antidepressant_type", "other_medication", "periods", "period_days", 
                             "current_covid", "past_covid", "long_covid", "long_covid_symptoms", "computergames")
  } else if(study_part == "screening"){
    comp_1_df <- matrix(ncol = 6, nrow = (length(res_comp_1)))
    comp_1_df <- as.data.frame(comp_1_df)
    colnames(comp_1_df) <- c("subj_id",
                             "age", "gender", "english", "neurological", "neurological_condition")
  }
  
  for(comp_i in seq_along(res_comp_1)) {
    
    demographics <- sapply(raw_dat[[res_comp_1[comp_i]]], na.omit)
    
    if(study_part == "online" | study_part == "chronotype_followup"){
      comp_1_df[comp_i, "subj_id"] <- id_shuffle(unique(demographics$prolific_id))
    } else if(study_part == "re_test"){
      comp_1_df[comp_i, "subj_id"] <- id_shuffle(unique(demographics$subj_id))
    } else if(study_part == "screening"){
      comp_1_df[comp_i, "subj_id"] <- id_shuffle(unique(demographics$prolific_id))
    }
    
    comp_1_df[comp_i, "age"] <- as.numeric(demographics$age)
    comp_1_df[comp_i, "gender"] <- demographics$gender[1]
    comp_1_df[comp_i, "english"] <- demographics$english[1]
    
    if(demographics$neurological[1] == 1){
      if (demographics$neurological_condition[1] == "None") {
        comp_1_df[comp_i, "neurological"] <- 0
        comp_1_df[comp_i, "neurological_condition"] <- NA
      } else {
        comp_1_df[comp_i, "neurological"] <- 1
        if(demographics$neurological_condition_other[1]){
          comp_1_df[comp_i, "neurological_condition"] <- paste(c(unlist(parse_json(demographics$neurological_condition[1])),
                                                                 demographics$neurological_condition_other_type[1]), collapse =', ')
        } else {
          comp_1_df[comp_i, "neurological_condition"] <- paste(unlist(parse_json(demographics$neurological_condition[1])), collapse =', ')
        }
      }
    } else {
      comp_1_df[comp_i, "neurological"] <- 0
      comp_1_df[comp_i, "neurological_condition"] <- NA
    }
    
    if(study_part != "screening"){
      comp_1_df[comp_i, "ethnicity"] <- demographics$ethnicity[1]
      comp_1_df[comp_i, "ses"] <- as.numeric(demographics$ses)
      comp_1_df[comp_i, "income"] <- paste(demographics$income[1], demographics$income_currency[1])
      if(demographics$psych_neurdev[1] == 1){
        if (demographics$psych_neurdev_condition[1] == "None") {
          comp_1_df[comp_i, "psych_neurdev"] <- 0
          comp_1_df[comp_i, "psych_neurdev_condition"] <- NA
          comp_1_df[comp_i, "psych_neurdev_condition_time"] <- NA
        } else {
          comp_1_df[comp_i, "psych_neurdev"] <- 1
          if(demographics$psych_neurdev_condition_other){
            comp_1_df[comp_i, "psych_neurdev_condition"] <- paste(c(unlist(parse_json(demographics$psych_neurdev_condition[1])),
                                                                    demographics$psych_neurdev_condition_other_type), collapse =', ')
            comp_1_df[comp_i, "psych_neurdev_condition"] <- sub("psych 16valuePast", "", comp_1_df[comp_i, "psych_neurdev_condition"])
          } else {
            comp_1_df[comp_i, "psych_neurdev_condition"] <- paste(unlist(parse_json(demographics$psych_neurdev_condition[1])), collapse =', ')
          }
          comp_1_df[comp_i, "psych_neurdev_condition_time"] <- paste(demographics$psych_neurdev_condition_time[1], collapse =', ')
        }
      } else {
        comp_1_df[comp_i, "psych_neurdev"] <- 0
        comp_1_df[comp_i, "psych_neurdev_condition"] <- NA
        comp_1_df[comp_i, "psych_neurdev_condition_time"] <- NA
      }
      comp_1_df[comp_i, "medication"] <- demographics$medication[1]
      if(demographics$medication[1] == 0){
        comp_1_df[comp_i, "contraceptive"] <- NA
        comp_1_df[comp_i, "antidepressant"] <- NA
        comp_1_df[comp_i, "antidepressant_type"] <- NA
        comp_1_df[comp_i, "other_medication"] <- NA
      } else {
        comp_1_df[comp_i, "contraceptive"] <- demographics$contraceptive[1]
        comp_1_df[comp_i, "antidepressant"] <- demographics$antidepressant[1]
        if(demographics$antidepressant[1] == 0){
          comp_1_df[comp_i, "antidepressant_type"] <- NA
        } else {
          if(demographics$antidepressant_other[1]){
            comp_1_df[comp_i, "antidepressant_type"] <- paste(c(unlist(parse_json(demographics$antidepressant_type[1])),
                                                                demographics$antidepressant_other_type), collapse =', ')
          } else {
            comp_1_df[comp_i, "antidepressant_type"] <- paste(unlist(parse_json(demographics$antidepressant_type)), collapse =', ')
          }
        }
        if(demographics$other_medication[1] == 0){
          comp_1_df[comp_i, "other_medication"] <- NA
        } else {
          comp_1_df[comp_i, "other_medication"] <- paste(unlist(parse_json(demographics$other_medication_type))[unlist(parse_json(demographics$other_medication_type)) != ""], collapse =', ')
        }
      }
      comp_1_df[comp_i, "periods"] <- demographics$period[1]
      if(demographics$period[1] == 0){
        comp_1_df[comp_i, "period_days"] <- NA
      } else {
        comp_1_df[comp_i, "period_days"] <- as.numeric(demographics$days_since_last_period)
      }
      comp_1_df[comp_i, "current_covid"] <- demographics$current_covid[1]
      comp_1_df[comp_i, "past_covid"] <- demographics$past_covid[1]
      if(demographics$past_covid[1] == 0){
        comp_1_df[comp_i, "long_covid"] <- NA
        comp_1_df[comp_i, "long_covid_symptoms"] <- NA
      } else {
        if(demographics$long_covid == "No"){
          comp_1_df[comp_i, "long_covid"] <- 0
          comp_1_df[comp_i, "long_covid_symptoms"] <- NA
        } else if(demographics$long_covid_symptoms == "None"){
          comp_1_df[comp_i, "long_covid"] <- 0
          comp_1_df[comp_i, "long_covid_symptoms"] <- NA
        } else {
          if(demographics$long_covid == "Maybe"){
            comp_1_df[comp_i, "long_covid"] <- 1
          } else if(demographics$long_covid == "Yes"){
            comp_1_df[comp_i, "long_covid"] <- 1
          }
          if(demographics$other_long_covid_symptom[1]){
            comp_1_df[comp_i, "long_covid_symptoms"] <- paste(c(unlist(parse_json(demographics$long_covid_symptoms)),
                                                                demographics$long_covid_other_symptom_text), collapse =', ')
          } else {
            comp_1_df[comp_i, "long_covid_symptoms"] <- paste(unlist(parse_json(demographics$long_covid_symptoms)), collapse =', ')
          }
        }
      }
      comp_1_df[comp_i, "computergames"] <- demographics$games[1]
    }
    
    
  }
  
  comp_1_df <- as.data.frame(comp_1_df)
  comp_1_df$age <- as.numeric(comp_1_df$age)
  comp_1_df$neurological <- as.numeric(comp_1_df$neurological)
  
  if(study_part != "screening"){
    comp_1_df$ses <- as.numeric(comp_1_df$ses)
    comp_1_df$psych_neurdev <- as.numeric(comp_1_df$psych_neurdev)
    comp_1_df$medication <- as.numeric(comp_1_df$medication)
    comp_1_df$contraceptive <- as.numeric(comp_1_df$contraceptive)
    comp_1_df$antidepressant <- as.numeric(comp_1_df$antidepressant)
    comp_1_df$periods <- as.numeric(comp_1_df$periods)
    comp_1_df$period_days <- as.numeric(comp_1_df$period_days)
    comp_1_df$current_covid <- as.numeric(comp_1_df$current_covid)
    comp_1_df$past_covid <- as.numeric(comp_1_df$past_covid)
    comp_1_df$long_covid <- as.numeric(comp_1_df$long_covid)    
  }
  
  ### COMPONENT 2: Task data ---------------------------------------------------
  
  if(task_dat){
    if(study_part == "online" | study_part == "chronotype_followup"){
      
      res_comp_2 <- which(lapply(1:length(raw_dat), 
                                 function (i) any(grepl("This is the start of the game", raw_dat[[i]], fixed=T)) & 
                                   !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
      
      comp_2_df <- data.frame(matrix(ncol = 14, nrow = 0))
      comp_2_meta_df <- data.frame(matrix(ncol = 5, nrow = 0))
      
    } else if(study_part == "re_test"){
      
      comp_2_df <- data.frame(matrix(ncol = 16, nrow = 0))
      comp_2_meta_df <- data.frame(matrix(ncol = 5, nrow = 0))
      
      res_comp_2 <- which(lapply(1:length(raw_dat), 
                                 function (i) any(grepl("This is the start of the game", raw_dat[[i]], fixed=T))) %in% TRUE)
    }
    
    
    for(comp_i in seq_along(res_comp_2)) {
      
      if(length(raw_dat[[res_comp_2[comp_i]]]$rt) != length(raw_dat[[res_comp_2[comp_i]]]$phase)){
        warning(paste("Double clicking error in subject number", comp_1_df[comp_i,"subj_id"], sep = " "))
        
        del <- which(abs(c(diff(raw_dat[[res_comp_2[comp_i]]]$rt), 0)) <= 5 & raw_dat[[res_comp_2[comp_i]]]$rt != 999)
        raw_dat[[res_comp_2[comp_i]]]$rt <- raw_dat[[res_comp_2[comp_i]]]$rt[-del]
        raw_dat[[res_comp_2[comp_i]]]$choice <- raw_dat[[res_comp_2[comp_i]]]$choice[-del]
      }
      
      if(study_part == "online" | study_part == "chronotype_followup"){
        
        task_meta_data <- data.frame("subj_id" = id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id)), 
                                     "game_id" = raw_dat[[res_comp_2[comp_i]]]$subjID,
                                     "start_time" = as_datetime(raw_dat[[res_comp_2[comp_i]]]$date[1]), 
                                     "end_time" = as_datetime(raw_dat[[res_comp_2[comp_i]]]$date[2]),
                                     "clicking_calibration" = mean(raw_dat[[res_comp_2[comp_i]]]$clicks[raw_dat[[res_comp_2[comp_i]]]$phase == "calibration"][2:3]))
        
        task_data <- data.frame("subj_id" = rep(id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id)), 
                                                length(raw_dat[[res_comp_2[comp_i]]]$phase)), 
                                "game_id" = raw_dat[[res_comp_2[comp_i]]]$subjID,
                                "start_time" = rep(raw_dat[[res_comp_2[comp_i]]]$date[1], length(raw_dat[[res_comp_2[comp_i]]]$phase)), 
                                "end_time" = rep(raw_dat[[res_comp_2[comp_i]]]$date[2], length(raw_dat[[res_comp_2[comp_i]]]$phase)),
                                raw_dat[[res_comp_2[comp_i]]][4:12], 
                                "clicking_calibration" = rep(mean(raw_dat[[res_comp_2[comp_i]]]$clicks[raw_dat[[res_comp_2[comp_i]]]$phase == "calibration"][2:3]),
                                                             length(raw_dat[[res_comp_2[comp_i]]]$phase)))  
        
      } else if(study_part == "re_test"){
        task_data <- data.frame("subj_id" = rep(id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$subj_id)), 
                                                length(raw_dat[[res_comp_2[comp_i]]]$phase)), 
                                "game_id" = raw_dat[[res_comp_2[comp_i]]]$subjID,
                                "session" = raw_dat[[res_comp_2[comp_i]]]$session, "room" = raw_dat[[res_comp_2[comp_i]]]$room,
                                "start_time" = rep(raw_dat[[res_comp_2[comp_i]]]$date[1], length(raw_dat[[res_comp_2[comp_i]]]$phase)), 
                                "end_time" = rep(raw_dat[[res_comp_2[comp_i]]]$date[2], length(raw_dat[[res_comp_2[comp_i]]]$phase)),
                                raw_dat[[res_comp_2[comp_i]]][4:12], 
                                "clicking_calibration" = rep(mean(raw_dat[[res_comp_2[comp_i]]]$clicks[raw_dat[[res_comp_2[comp_i]]]$phase == "calibration"][2:3]),
                                                             length(raw_dat[[res_comp_2[comp_i]]]$phase)))    
      }
      
      comp_2_df <- rbind(comp_2_df, task_data)
      
      comp_2_meta_df <- rbind(comp_2_meta_df, task_meta_data)
      
    }
    
    # Formatting data for modelling
    
    if(modelling_dat){
      
      modelling_df <- list()
      
      if(study_part == "online" | study_part == "chronotype_followup"){
        data_modelling <- comp_2_df[comp_2_df$phase == "game", c(1, 2, 6, 8, 9, 10)]
        data_modelling$offerEffort <- standardization(data_modelling$offerEffort, 0, unique(data_modelling$offerEffort))
        data_modelling$offerReward <- standardization(data_modelling$offerReward, 0, unique(data_modelling$offerReward))
        colnames(data_modelling) <- c("subjID", "gameID", "trial", "effort_a", "amount_a", "choice")
        data_modelling <- cbind(data_modelling, "effort_b" = rep(0, dim(data_modelling)[1]), "amount_b" = rep(0, dim(data_modelling)[1]))
        modelling_df <- data_modelling
        
      } else if(study_part == "re_test"){
        data_modelling <- comp_2_df[comp_2_df$phase == "game", c(1, 2, 3, 8, 10, 11, 12)]
        data_modelling$offerEffort <- standardization(data_modelling$offerEffort, 0, unique(data_modelling$offerEffort))
        data_modelling$offerReward <- standardization(data_modelling$offerReward, 0, unique(data_modelling$offerReward))
        colnames(data_modelling) <- c("subjID", "gameID", "session", "trial", "effort_a", "amount_a", "choice")
        data_modelling <- cbind(data_modelling, "effort_b" = rep(0, dim(data_modelling)[1]), "amount_b" = rep(0, dim(data_modelling)[1]))
        modelling_df$session_1 <- data_modelling[data_modelling$session == 1,]
        modelling_df$session_2 <- data_modelling[data_modelling$session == 2,]
      }
      
    } 
  }
  
  
  ### COMPONENT 3: Questionnaire data ------------------------------------------
  
  if(study_part == "online" | study_part == "chronotype_followup"){
    res_comp_3 <- which(lapply(1:length(raw_dat), 
                               function (i) any(grepl("Well done for completing the game!", raw_dat[[i]], fixed=T)) & 
                                 !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
    
  } else if(study_part == "re_test"){
    res_comp_3 <- which(lapply(1:length(raw_dat), 
                               function (i) any(grepl("Well done for completing the game!", raw_dat[[i]], fixed=T))) %in% TRUE)
    res_comp_3_s2 <- which(lapply(1:length(raw_dat), 
                                  function (i) any(grepl("Thank you! You have completed the game.", raw_dat[[i]], fixed=T))) %in% TRUE)  
    
  } else if(study_part == "screening"){
    res_comp_3 <- which(lapply(1:length(raw_dat), 
                               function (i) any(grepl("For each of the following questions", raw_dat[[i]], fixed=T))) %in% TRUE)
  }
  
  
  comp_3_df <- data.frame(matrix(ncol = 0, nrow = 0))
  
  for(comp_i in seq_along(res_comp_3)) {
    
    questionnaires <- raw_dat[[res_comp_3[comp_i]]]
    
    if(study_part != "chronotype_followup"){
      
      # MEQ
      responseCols <- c("meq_response", "meq_response_raw")
      
      meq_data <- questionnaires[questionnaires$questionnaire == "MEQ",][,c("questionnaire", "question_no", responseCols)]
      meq_data <- meq_data[!is.na(meq_data$questionnaire),]
      
      meq <- matrix(ncol=dim(meq_data)[1]*length(responseCols), nrow=1)
      colnames(meq) <- rep("", dim(meq_data)[1]*length(responseCols))
      c <- 0
      for(i in which(colnames(meq_data) %in% responseCols)){
        for(j in 1:dim(meq_data)[1]){
          c <- c+1
          meq[,c] <- meq_data[,i][j]
          colnames(meq)[c] <- paste(colnames(meq_data)[i], meq_data$question_no[j], sep = "_")
        }
      }
      
      # scoring mistake in data from online study and two subjects in retest study
      if(study_part == "online"){
        meq[,"meq_response_8"] <- 4 - meq[, "meq_response_raw_8"]
        meq[,"meq_response_9"] <- 4 - meq[, "meq_response_raw_9"]
        meq[,"meq_response_13"] <- 4 - meq[, "meq_response_raw_13"]
        meq[,"meq_response_15"] <- 4 - meq[, "meq_response_raw_15"]
        
      } else if(study_part == "re_test"){
        if(comp_i == 28 | comp_i == 29){
          meq[,"meq_response_8"] <- 4 - meq[, "meq_response_raw_8"]
          meq[,"meq_response_9"] <- 4 - meq[, "meq_response_raw_9"]
          meq[,"meq_response_13"] <- 4 - meq[, "meq_response_raw_13"]
          meq[,"meq_response_15"] <- 4 - meq[, "meq_response_raw_15"]
        }
      }
      
      if(raw_data) {
        meq <- data.frame(meq, "meq_sumScore" = sum(meq[,1:19]))
      } else {
        meq <- data.frame(meq, "meq_sumScore" = sum(meq[,1:19]))
        meq <- meq[,c(1:19, 39)]
      }
      
      rm(meq_data)
      
      # MCTQ
      if(raw_data) {
        responseCols <- c("mctq_response", "mctq_response_raw")
      } else {
        responseCols <- c("mctq_response")
      }
      
      mctq_data <- questionnaires[questionnaires$questionnaire == "MCTQ",][,c("questionnaire", "subscale", "question_no", responseCols)]
      mctq_data <- mctq_data[!is.na(mctq_data$questionnaire),]
      
      if(study_part == "online"){
        if(comp_i == 141){
          mctq_data$mctq_response_raw[2] <- "10:00 PM"  
        }
        
        for(i in c(2, 3, 5, 9, 10, 12)){
          if(strftime(as.POSIXct(mctq_data$mctq_response_raw[i], format="%H:%M"), format = "%H") == "10"){
            
            if(endsWith(mctq_data$mctq_response_raw[i], "AM")){
              mctq_data$mctq_response[i] <- paste0("10:", 
                                                   strftime(as.POSIXct(mctq_data$mctq_response_raw[i], format="%H:%M"), format = "%M"))
            } else if(endsWith(mctq_data$mctq_response_raw[i], "PM")){
              mctq_data$mctq_response[i] <- paste0("22:", 
                                                   strftime(as.POSIXct(mctq_data$mctq_response_raw[i], format="%H:%M"), format = "%M"))
            }
          }
        }
      }
      
      mctq_data$question_no <- as.numeric(mctq_data$question_no)
      # remove corrected answers
      if(raw_data == FALSE){
        mctq_data_clean <- as.data.frame(matrix(data = NA, nrow = 16, ncol = 4)) 
      } else {
        mctq_data_clean <- as.data.frame(matrix(data = NA, nrow = 16, ncol = 5))
      }
      colnames(mctq_data_clean) <- colnames(mctq_data)
      duplicates <- data.frame(table(mctq_data$question_no))
      for(i in 1:16){
        mctq_data_clean[i,] <- mctq_data[mctq_data$question_no == i,][duplicates$Freq[i],]
      }
      
      mctq <- matrix(ncol=(dim(mctq_data_clean)[1]*length(responseCols)), nrow=1)
      colnames(mctq) <- rep("", dim(mctq_data_clean)[1]*length(responseCols))
      c <- 0
      for(i in which(colnames(mctq_data_clean) %in% responseCols)){
        for(j in 1:dim(mctq_data_clean)[1]){
          c <- c+1
          mctq[,c] <- mctq_data_clean[,i][j]
          colnames(mctq)[c] <- paste(colnames(mctq_data_clean)[i], mctq_data_clean$question_no[j], sep = "_")
        }
      }
      
      mctq <- data.frame(mctq)
      
      # scoring
      mctq_SO_w <- strftime(as.POSIXct(mctq$mctq_response_3, format="%H:%M") + 
                              as.numeric(mctq$mctq_response_4)*60, format="%H:%M")
      mctq_SO_f <- strftime(as.POSIXct(mctq$mctq_response_10, format="%H:%M") + 
                              as.numeric(mctq$mctq_response_11)*60, format="%H:%M")
      
      mctq_GU_w <- strftime(as.POSIXct(mctq$mctq_response_5, format="%H:%M") + 
                              as.numeric(mctq$mctq_response_6)*60, format="%H:%M")
      mctq_GU_f <- strftime(as.POSIXct(mctq$mctq_response_12, format="%H:%M") + 
                              as.numeric(mctq$mctq_response_13)*60, format="%H:%M")
      
      mctq_SD_w <- strftime(as.POSIXct(mctq$mctq_response_5, format="%H:%M") - 
                              period_to_seconds(hm(mctq_SO_w)), format="%H:%M")
      mctq_SD_f <- strftime(as.POSIXct(mctq$mctq_response_12, format="%H:%M") - 
                              period_to_seconds(hm(mctq_SO_f)), format="%H:%M")
      
      mctq_TBT_w <- strftime(as.POSIXct(mctq_GU_w, format="%H:%M") - 
                               period_to_seconds(hm(mctq$mctq_response_2)), format="%H:%M")
      mctq_TBT_f <- strftime(as.POSIXct(mctq_GU_f, format="%H:%M") - 
                               period_to_seconds(hm(mctq$mctq_response_9)), format="%H:%M")
      
      mctq_MS_w <- strftime(as.POSIXct(mctq_SO_w, format="%H:%M") + 
                              period_to_seconds(hm(mctq_SD_w)) / 2, format="%H:%M")
      mctq_MS_f <- strftime(as.POSIXct(mctq_SO_f, format="%H:%M") + 
                              period_to_seconds(hm(mctq_SD_f)) / 2, format="%H:%M")
      
      mctq_SD_week <- as.character(seconds_to_period(round((period_to_seconds(hm(mctq_SD_w)) * as.numeric(mctq$mctq_response_1) 
                                                            + period_to_seconds(hm(mctq_SD_f)) * (7-as.numeric(mctq$mctq_response_1)))/7)))
      
      mctq_MSF_SC <- ifelse(period_to_seconds(hm(mctq_SD_f)) <= period_to_seconds(hm(mctq_SD_w)), 
                            mctq_MS_f, strftime(as.POSIXct(mctq_MS_f, format="%H:%M") - 
                                                  (period_to_seconds(hm(mctq_SD_f)) - period_to_seconds(hm(mctq_SD_w)))/2, format="%H:%M"))
      
      mctq <- cbind(mctq, data.frame(mctq_SO_w, mctq_SO_f, mctq_GU_w, mctq_GU_f, mctq_SD_w, mctq_SD_f, 
                                     mctq_TBT_w, mctq_TBT_f, mctq_MS_w, mctq_MS_f, mctq_SD_week, mctq_MSF_SC))
      rm(mctq_data, mctq_SO_w, mctq_SO_f, mctq_GU_w, mctq_GU_f, mctq_SD_w, mctq_SD_f, 
         mctq_TBT_w, mctq_TBT_f, mctq_MS_w, mctq_MS_f, mctq_SD_week, mctq_MSF_SC)   
      
    }
    
    if(study_part != "screening"){
      # GAME
      game_data <- questionnaires[questionnaires$questionnaire == "GAME",][,c("questionnaire", "question_no", "game_response", "game_response_text")]
      game_data <- game_data[!is.na(game_data$questionnaire),]
      
      game <- matrix(ncol=(dim(game_data)[1]*2), nrow=1)
      colnames(game) <- rep("", dim(game_data)[1]*2)
      c <- 0
      for(i in which(colnames(game_data) %in% c("game_response", "game_response_text"))){
        for(j in 1:dim(game_data)[1]){
          c <- c+1
          game[,c] <- game_data[,i][j]
          colnames(game)[c] <- paste(colnames(game_data)[i], game_data$question_no[j], sep = "_")
        }
      }
      game <- data.frame(game)
      game$game_response_text_1 <- ifelse(game$game_response_text_1 == "NA", NA,  game$game_response_text_1)
      if(study_part == "re_test" & comp_i == 29){
        game$game_response_text_2 <- "I tried to minimise the amount of clicks on the lesser effort games"
      }
      
      rm(game_data)
      
      # SHAPS
      shaps_data <- questionnaires[questionnaires$questionnaire == "SHAPS",][,c("questionnaire", "question_no", "shaps_response_raw")]
      shaps_data <- shaps_data[!is.na(shaps_data$questionnaire),]
      colnames(shaps_data) <- c("questionnaire", "question_no", "shaps_response")
      
      shaps <- matrix(ncol=(dim(shaps_data)[1]), nrow=1)
      colnames(shaps) <- rep("", dim(shaps_data)[1])
      c <- 0
      for(i in which(colnames(shaps_data) %in% c("shaps_response"))){
        for(j in 1:dim(shaps_data)[1]){
          c <- c+1
          shaps[,c] <- shaps_data[,i][j]
          colnames(shaps)[c] <- paste(colnames(shaps_data)[i], shaps_data$question_no[j], sep = "_")
        }
      }
      shaps <- data.frame(shaps, "shaps_sumScore" = sum(shaps_data$shaps_response))
      rm(shaps_data)
      
      
      # DARS
      dars_data <- questionnaires[questionnaires$questionnaire == "DARS",][,c("questionnaire", "subscale", "question_no", "dars_response")]
      dars_data <- dars_data[!is.na(dars_data$question_no),]
      dars_data$subscale[16] <- "sensory"
      dars <- matrix(ncol=(dim(dars_data)[1]), nrow=1)
      colnames(dars) <- rep("", dim(dars_data)[1])
      c <- 0
      for(i in which(colnames(dars_data) %in% c("dars_response"))){
        for(j in 1:dim(dars_data)[1]){
          c <- c+1
          dars[,c] <- dars_data[,i][j]
          colnames(dars)[c] <- paste(colnames(dars_data)[i], dars_data$subscale[j], dars_data$question_no[j], sep = "_")
        }
      }
      
      dars_list_data <- questionnaires[questionnaires$questionnaire == "DARS",][,c("questionnaire", "subscale", "question_no", "dars_list")]
      dars_list_data <- dars_list_data[!is.na(dars_list_data$dars_list),]
      dars_list <- matrix(ncol=4, nrow=1)
      colnames(dars_list) <- rep("", 4)
      for(i in 1:4){
        dars_list[,i] <- paste(dars_list_data$dars_list[i], collapse = ", ")
        try(dars_list[,i] <- paste(unlist(parse_json(dars_list_data$dars_list[i])), collapse = ", "),
            silent=TRUE)
        colnames(dars_list)[i] <- paste("dars_list", dars_list_data$subscale[i], sep = "_")
      }
      
      dars <- data.frame(dars, dars_list, 
                         "dars_hobbies_sumScore" = sum(dars_data[dars_data$subscale == "hobbies",]$dars_response),
                         "dars_food_drink_sumScore" = sum(dars_data[dars_data$subscale == "food_drink",]$dars_response),
                         "dars_social_sumScore" = sum(dars_data[dars_data$subscale == "social",]$dars_response),
                         "dars_sensory_sumScore" = sum(dars_data[dars_data$subscale == "sensory",]$dars_response),
                         "dars_sumScore" = sum(dars_data$dars_response))
      rm(dars_data)
      rm(dars_list_data)
      
      
      # AES
      if(raw_data){
        responseCols <- c("aes_response", "aes_response_raw")
      } else {
        responseCols <- c("aes_response")
      }
      
      aes_data <- questionnaires[questionnaires$questionnaire == "AES",][,c("questionnaire", "question_no", responseCols)]
      aes_data <- aes_data[!is.na(aes_data$questionnaire),]
      
      aes <- matrix(ncol=dim(aes_data)[1]*length(responseCols), nrow=1)
      colnames(aes) <- rep("", dim(aes_data)[1]*length(responseCols))
      c <- 0
      for(i in which(colnames(aes_data) %in% responseCols)){
        for(j in 1:dim(aes_data)[1]){
          c <- c+1
          aes[,c] <- aes_data[,i][j]
          colnames(aes)[c] <- paste(colnames(aes_data)[i], aes_data$question_no[j], sep = "_")
        }
      }
      
      aes <- data.frame(aes, "aes_sumScore" = sum(aes_data$aes_response))
      rm(aes_data)
      
      # FINDRISC
      if(raw_data) {
        responseCols <- c("findrisc_response", "findrisc_response_raw")
      } else {
        responseCols <- c("findrisc_response")
      }
      
      findrisc_data <- questionnaires[questionnaires$questionnaire == "FINDRISC",][,c("questionnaire", "question_no", responseCols)]
      findrisc_data <- findrisc_data[!is.na(findrisc_data$questionnaire),]
      
      findrisc <- matrix(ncol=(dim(findrisc_data)[1]*length(responseCols)), nrow=1)
      colnames(findrisc) <- rep("", dim(findrisc_data)[1]*length(responseCols))
      c <- 0
      for(i in which(colnames(findrisc_data) %in% responseCols)){
        for(j in 1:dim(findrisc_data)[1]){
          c <- c+1
          findrisc[,c] <- findrisc_data[,i][j]
          colnames(findrisc)[c] <- paste(colnames(findrisc_data)[i], findrisc_data$question_no[j], sep = "_")
        }
      }
      findrisc <- data.frame(findrisc, "findrisc_sumScore" = sum(findrisc_data$findrisc_response))
      findrisc[1:8] <- as.numeric(findrisc[1:8])
      rm(findrisc_data)
      
      
      # BMI
      if(raw_data) {
        responseCols <- c("bmi_response", "bmi_response_raw")
      } else {
        responseCols <- c("bmi_response")
      }
      
      bmi_data <- questionnaires[questionnaires$questionnaire == "BMI",][,c("questionnaire", "question_no", responseCols)]
      bmi_data <- bmi_data[!is.na(bmi_data$questionnaire),]
      
      bmi <- matrix(ncol=(dim(bmi_data)[1]*length(responseCols)), nrow=1)
      colnames(bmi) <- rep("", dim(bmi_data)[1]*length(responseCols))
      c <- 0
      for(i in which(colnames(bmi_data) %in% responseCols)){
        for(j in 1:dim(bmi_data)[1]){
          c <- c+1
          bmi[,c] <- bmi_data[,i][j]
          colnames(bmi)[c] <- paste(colnames(bmi_data)[i], bmi_data$question_no[j], sep = "_")
        }
      }
      bmi <- data.frame(bmi)
      bmi[,1:2] <- as.numeric(bmi[,1:2])
      
      bmi <- cbind(bmi, "bmi_result" = as.numeric(bmi$bmi_response_2) / ((as.numeric(bmi$bmi_response_1))/100)^2)
      rm(bmi_data)
      
      
      # IPAQ
      ipaq_data <- questionnaires[questionnaires$questionnaire == "IPAQ",][,c("questionnaire", "question_no", "ipaq_response")]
      ipaq_data <- ipaq_data[!is.na(ipaq_data$questionnaire),]
      
      ipaq <- matrix(ncol=(dim(ipaq_data)[1]), nrow=1)
      colnames(ipaq) <- rep("", dim(ipaq_data)[1])
      c <- 0
      for(i in which(colnames(ipaq_data) %in% c("ipaq_response"))){
        for(j in 1:dim(ipaq_data)[1]){
          c <- c+1
          ipaq[,c] <- ipaq_data[,i][j]
          colnames(ipaq)[c] <- paste(colnames(ipaq_data)[i], ipaq_data$question_no[j], sep = "_")
        }
      }
      ipaq <- data.frame(ipaq)
      
      ipaq_walking_MET <- ifelse(ipaq$ipaq_response_5 == "None", 0, 3.3 * period_to_seconds(hm(ipaq$ipaq_response_6))/60 * as.numeric(ipaq$ipaq_response_5))
      ipaq_moderate_MET <- ifelse(ipaq$ipaq_response_3 == "None", 0, 4 * period_to_seconds(hm(ipaq$ipaq_response_4))/60 * as.numeric(ipaq$ipaq_response_3))
      ipaq_vigorous_MET <- ifelse(ipaq$ipaq_response_1 == "None", 0, 8 * period_to_seconds(hm(ipaq$ipaq_response_2))/60 * as.numeric(ipaq$ipaq_response_1))
      
      ipaq <- data.frame(ipaq, ipaq_walking_MET, ipaq_moderate_MET, ipaq_vigorous_MET, "ipaq_sumScore" = sum(ipaq_walking_MET, ipaq_moderate_MET, ipaq_vigorous_MET))
      rm(ipaq_data, ipaq_walking_MET, ipaq_moderate_MET, ipaq_vigorous_MET)
      
      
      # MDD
      mdd_data <- questionnaires[questionnaires$questionnaire == "MDD",][,c("questionnaire", "subscale", "question_no", "mdd_response")]
      mdd_data <- mdd_data[!is.na(mdd_data$question_no),]
      
      mdd <- matrix(ncol=(dim(mdd_data)[1]), nrow=1)
      colnames(mdd) <- rep("", dim(mdd_data)[1])
      c <- 0
      for(i in which(colnames(mdd_data) %in% c("mdd_response"))){
        for(j in 1:dim(mdd_data)[1]){
          c <- c+1
          mdd[,c] <- mdd_data[,i][j]
          colnames(mdd)[c] <- paste(colnames(mdd_data)[i], mdd_data$question_no[j], sep = "_")
        }
      }
      
      mdd_followup_data <- questionnaires[questionnaires$questionnaire == "MDD",][,c("questionnaire", "subscale", "mdd_response_followup")]
      mdd_followup_data <- mdd_followup_data[!is.na(mdd_followup_data$mdd_response_followup),]
      mdd_followup <- matrix(ncol=4, nrow=1)
      colnames(mdd_followup) <- rep("", 4)
      for(i in 1:4){
        mdd_followup[,i] <- mdd_followup_data$mdd_response_followup[i]
        colnames(mdd_followup)[i] <- paste("mdd_followup", i, sep = "_")
      }
      
      mdd <- data.frame(mdd, mdd_followup) 
      
      if(mdd$mdd_response_1 == 0 & mdd$mdd_response_3 == 0){
        mdd_past <- 0
        mdd_current <- 0
        mdd_recurrent <- 0
      } else {
        mdd_past <- ifelse(sum(mdd_data[mdd_data$subscale %in% c("A1", "A2", "A3_past"),]$mdd_response, na.rm = TRUE) >= 5 
                           & mdd_data[mdd_data$subscale == "A4_past",]$mdd_response == 1, 1, 0)
        mdd_current <- ifelse(sum(mdd_data[mdd_data$subscale %in% c("A1_current", "A2_current", "A3_current"),]$mdd_response, na.rm = TRUE) >= 5 
                              & mdd_data[mdd_data$subscale == "A4_current",]$mdd_response == 1, 1, 0)
        mdd_recurrent <- mdd_data[mdd_data$subscale == "A5",]$mdd_response
      }
      
      mdd <- data.frame(mdd, mdd_past, mdd_current, mdd_recurrent)
      rm(mdd_data, mdd_followup_data, mdd_past, mdd_current, mdd_recurrent)
      
      # CATCH QUESTIONS
      catch_questions_data <- questionnaires[questionnaires$questionnaire == "catch_questions",][,c("questionnaire", "subscale", "question_no", "catch_question_score", "catch_question_pass")]
      catch_questions_data <- catch_questions_data[!is.na(catch_questions_data$questionnaire),]
      
      catch_questions <- matrix(ncol=(dim(catch_questions_data)[1]*2), nrow=1)
      colnames(catch_questions) <- rep("", dim(catch_questions_data)[1]*2)
      c <- 0
      for(i in which(colnames(catch_questions_data) %in% c("catch_question_score", "catch_question_pass"))){
        for(j in 1:dim(catch_questions_data)[1]){
          c <- c+1
          catch_questions[,c] <- catch_questions_data[,i][j]
          colnames(catch_questions)[c] <- paste(colnames(catch_questions_data)[i], catch_questions_data$subscale[j], catch_questions_data$question_no[j], sep = "_")
          
        }
      }
      catch_questions <- data.frame(catch_questions, "catch_question_pass" = ifelse(all(catch_questions_data[catch_questions_data$subscale == "easy",]$catch_question_pass == TRUE) & 
                                                                                      sum(catch_questions_data[catch_questions_data$subscale == "hard",]$catch_question_pass == TRUE) > 0, TRUE, FALSE))
      catch_questions <- catch_questions[ , order(colnames(catch_questions))]
      rm(catch_questions_data)
      
      # FINAL SURVEY
      
      if(study_part == "online" | study_part == "chronotype_followup"){
        final_survey <- data.frame("enjoyment" = as.numeric(na.omit(questionnaires$enjoyment)), 
                                   "concentration" = as.numeric(na.omit(questionnaires$concentration)),
                                   "technical" = as.numeric(na.omit(questionnaires$technical)),
                                   "technical_comments" = ifelse(length(na.omit(questionnaires$technical_comments)) != 0, na.omit(questionnaires$technical_comments)[1], 'NA'),
                                   "layout" = as.numeric(na.omit(questionnaires$layout)),
                                   "layout_comments" = ifelse(length(na.omit(questionnaires$layout_comments)) != 0, na.omit(questionnaires$layout_comments)[1], 'NA'),
                                   "other" = as.numeric(na.omit(questionnaires$other)),
                                   "other_comments" = ifelse(length(na.omit(questionnaires$other_comments)) != 0, na.omit(questionnaires$other_comments)[1], 'NA'))
        
        if(study_part == "online"){
          comp_3_one_df <- cbind("subj_id" = id_shuffle(questionnaires$prolific_id[1]),
                                 game, shaps, dars, aes, meq, mctq, findrisc, bmi, ipaq, mdd, catch_questions, final_survey)
        } else {
          comp_3_one_df <- cbind("subj_id" = id_shuffle(questionnaires$prolific_id[1]),
                                 game, shaps, dars, aes, findrisc, bmi, ipaq, mdd, catch_questions, final_survey)
        }
        
        
      } else if(study_part == "re_test"){
        final_survey <- data.frame("enjoyment_session_1" = as.numeric(na.omit(questionnaires$enjoyment)), 
                                   "concentration_session_1" = as.numeric(na.omit(questionnaires$concentration)),
                                   "technical_session_1" = as.numeric(na.omit(questionnaires$technical)),
                                   "technical_comments_session_1" = ifelse(length(na.omit(questionnaires$technical_comments)) != 0, na.omit(questionnaires$technical_comments)[1], 'NA'),
                                   "layout_session_1" = as.numeric(na.omit(questionnaires$layout)),
                                   "layout_comments_session_1" = ifelse(length(na.omit(questionnaires$layout_comments)) != 0, na.omit(questionnaires$layout_comments)[1], 'NA'),
                                   "other_session_1" = as.numeric(na.omit(questionnaires$other)),
                                   "other_comments_session_1" = ifelse(length(na.omit(questionnaires$other_comments)) != 0, na.omit(questionnaires$other_comments)[1], 'NA'))
        
        comp_3_one_df <- cbind("subj_id" = id_shuffle(questionnaires$subj_id[1]),
                               game, shaps, dars, aes, meq, mctq, findrisc, bmi, ipaq, mdd, catch_questions, final_survey)    
        
      }
    } else {
      comp_3_one_df <- cbind("subj_id" = id_shuffle(questionnaires$prolific_id[1]),
                             meq, mctq)    
    }
    
    
    if(study_part != "screening"){
      comp_3_df <- rbind(comp_3_df, comp_3_one_df) 
    } else {
      comp_3_df <- rbind(comp_3_df, comp_3_one_df)
    }
    
  }
  
  # Add final survey from second session
  if(study_part == "re_test"){
    final_survey_session_2 <- data.frame(matrix(ncol = 0, nrow = 0))
    
    for(comp_i in seq_along(res_comp_3_s2)) {
      questionnaires <- raw_dat[[res_comp_3_s2[comp_i]]]
      
      # FINAL SURVEY
      if(comp_i == 28 | comp_i == 29){
        final_survey <- data.frame("subj_id" = unique(questionnaires$subj_id),
                                   "enjoyment_session_2" = 'NA', 
                                   "concentration_session_2" = 'NA',
                                   "technical_session_2" = 'NA',
                                   "technical_comments_session_2" = 'NA',
                                   "layout_session_2" = 'NA',
                                   "layout_comments_session_2" = 'NA',
                                   "other_session_2" = 'NA',
                                   "other_comments_session_2" = 'NA')
        
      } else {
        final_survey <- data.frame("subj_id" = unique(questionnaires$subj_id),
                                   "enjoyment_session_2" = as.numeric(na.omit(questionnaires$enjoyment)), 
                                   "concentration_session_2" = as.numeric(na.omit(questionnaires$concentration)),
                                   "technical_session_2" = as.numeric(na.omit(questionnaires$technical)),
                                   "technical_comments_session_2" = ifelse(length(na.omit(questionnaires$technical_comments)) != 0, na.omit(questionnaires$technical_comments)[1], 'NA'),
                                   "layout_session_2" = as.numeric(na.omit(questionnaires$layout)),
                                   "layout_comments_session_2" = ifelse(length(na.omit(questionnaires$layout_comments)) != 0, na.omit(questionnaires$layout_comments)[1], 'NA'),
                                   "other_session_2" = as.numeric(na.omit(questionnaires$other)),
                                   "other_comments_session_2" = ifelse(length(na.omit(questionnaires$other_comments)) != 0, na.omit(questionnaires$other_comments)[1], 'NA'))
        
      }
      
      final_survey_session_2 <- rbind(final_survey_session_2, final_survey)
    } 
    
    final_survey_session_2 <- final_survey_session_2[order(match(final_survey_session_2[,1],comp_3_df[,1])),]
    comp_3_df <- cbind(comp_3_df, final_survey_session_2[2:9])
  }
  
  # OUTPUT
  
  results <- list()
  
  if(demographic_dat){
    results <- append(results, list("demographics" = comp_1_df))
  }
  if(task_dat){
    results <- append(results, list("game" = comp_2_df))
    results <- append(results, list("game_meta" = comp_2_meta_df))
  }
  if(modelling_dat){
    results <- append(results, list("modelling_data" = modelling_df))
  }
  if(questionnaire_dat){
    comp_3_df[comp_3_df=="NA"] <- NA
    results <- append(results, list("questionnaire" = comp_3_df))
  }
  return(results)
}




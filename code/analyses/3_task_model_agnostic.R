##########################################################################################
###############--------------- MODEL AGNOSTIC TASK ANALYSES ---------------###############
##########################################################################################

### In this script: 
# (1) 

# Set working directory
here::i_am("github/effort-study/code/main_study/3_task_model_agnostic.R")
setwd(here::here())

# source functions
source("github/effort-study/code/functions/helper_funs.R")

# source dataset
main_data <- readRDS("data/processed_data/main_study/online_data.RDS")

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, writexl, lubridate)

### (1)  -----------------------------------------------

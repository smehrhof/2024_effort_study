# **Overlapping effects of circadian rhythm and neuropsychiatric symptoms on effort-based decision making**

Sara Mehrhof & Camilla Nord

# Data

All data used in our study can be found in the `/data` directory.

### Processed data

The cleaned, raw data resulting from our study can be found in the `/processed_data` directory. Data processing includes cleaning of individual questionnaires according to questionnaire manuals and applying pre-registered exclusion criteria.   

### Model fit

An empty folder structure for model fit objects produced by scripts in the `/analyses` directory. 

# Code

All code used to run analyses for our study can be found in the `/code` directory. Some scripts may call supporting functions and stan scripts found in the `/functions` and `/stan` directories.  

### Analyses

All code used for our main study analyses can be found in the `/analyses` directory. 
The analyses steps are structured as follows:

#### 1 Descriptives

Descriptive data analyses including demographics, psychiatric comorbidities, task metrics, and questionnaire measures. 

#### 2 Task - model agnostic

Model agnostic analyses of task data. 

#### 3 Task - model based

Model based analyses of task data, includes model fitting, checking for model convergence, model comparison, model validation, and posterior predictive checks.

#### 4 Main analyses

Main analyses as described in the first pre-registration (see https://osf.io/2x3au). 

#### 5 MDD-HC comparison

Exploratory analyses comparing subjects meeting criteria for current major depressive episode to age and gender matched healthy controls. 

#### 6 Circadian analyses 

Main analyses investigating circadian effects, as described in the second pre-registration (see https://osf.io/y4fbe).

### Test retest 

All code used for our analyses of our in person test-retest reliability study can be found in the `/test_retest` directory.  
The analyses steps are structured as follows:

#### 1 Descriptives

Descriptive data analyses including demographics and questionnaire measures. 

#### 2 Task - model agnostic

Model agnostic analyses of task data.

#### 3 Task - model based

Model based analyses of task data, includes model fitting, checking for model convergence, model comparison.

#### 4 Test retest reliability

Analyses of test-retest reliability, includes intra-class correlations, Pearson's correlations, and posterior predictions. 

### Simulation study

All code used for our simulation study can be found in the `/simulation_study` directory. Results are presented in the Supplementary material 1. 

### Task

All code used to run the novel, online effort expenditure task can be found in the `/task` directory.  









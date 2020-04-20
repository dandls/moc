#################################
### Main script iterated racing 
#################################

#--- Setup ----
# if (!"rstudioapi" %in% installed.packages()) {
#   install.packages("rstudioapi")
# }
# require("rstudioapi")
# current_dir = dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(current_dir)
# source("../helpers/libs.R")


# TRAIN_MODEL = FALSE 
# GET_EVALS = FALSE
# RUN_IRACE = FALSE
# GET_GENERATIONS = FALSE
# PARALLEL = FALSE
# TUNE_KERAS = TRUE
# TUNING = TRUE
# names_models = c("ranger", "xgboost", "svm", "logreg", "neuralnet")

#--- Train benchmark models on benchmark tasks ----
if (TRAIN_MODEL) {
    task_ids = c(14965, 37, 3917, 219, 52945, 3483, 3822, 3586)
  source("../helpers/train_models_mlr3.R")
  tasks_irace = tasks_trained
} else {
  models_irace = readRDS("../saved_objects/models_irace.rds")
}


#--- Get number evals to for targetRunner of irace ----
devtools::load_all("../iml")
library("mosmafs")

tasks_irace_s = tasks_irace
tasks_irace_s[[2]] = NULL
tasks_irace_s[[3]] = NULL
tasks_irace_s[[4]] = NULL
tasks_irace_s[[4]] = NULL

if (GET_EVALS) {
  source("get_evals.R")
} else {
  evals = 15000
}


#--- Run iterated racing ----
if (RUN_IRACE) {
  source("run_irace.R")
} else {
  irace_results = readRDS("../saved_objects/irace_results.rds")
}


#--- Get default parameter for generation ----
if (GET_GENERATIONS) {
  SAVEBESTCONFIG = FALSE
  source("get_generations.R")
} 


#--- Have a look on the tuned parameterset ----
best_config = readRDS("../saved_objects/best_config.rds")
print(best_config)


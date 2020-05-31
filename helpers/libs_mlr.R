# --- Needed R packages ----
packages = c("checkmate",  "devtools", "Rcpp", "ParamHelpers", "mlr", "ecr",
  "OpenML", "magrittr", "data.table", "farff", "ranger", "mlrCPO", "parallelMap",
  "xgboost", "BBmisc", "rjson", "Metrics", "foreach", "prediction", "rgl",
  "randomForest", "pracma", "parallelMap", "keras", "irace", "ggplot2",
  "plot3Drgl", "latex2exp", "scatterplot3d", "ggrepel", "reticulate",
  "datarium", "dplyr", "roxygen2", "gridExtra", "Formula", "StatMatch",
  "keras", "purrr", "e1071", "stringr", "mosmafs",
  "xtable", "ggpubr", "tidyr", # Packages for evaluation of benchmark results
  "GGally", "fmsb", "ggExtra", "metR", "mvtnorm") # Packages for study and applications

source("../helpers/keras_mlr_learner.R")

new.packages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# #--- Load respositories iml ----
# load `iml` and `counterfactuals` like "normal" packages.
# in the future this would just be library("counterfactuals").
devtools::load_all("../iml", export_all = FALSE)
devtools::load_all("../counterfactuals", export_all = FALSE)

library("mlr")
library("mlrCPO")


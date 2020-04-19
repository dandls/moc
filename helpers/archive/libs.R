##############################
### libs 
##############################

libs.path = tryCatch(dirname(sys.frame(1)$ofile),
  error = function(e) dirname(rstudioapi::getSourceEditorContext()$path))

#--- Install packages if not already installed ----
# packages = c("checkmate",  "devtools", "Rcpp", "ParamHelpers", "mlr", "ecr", "OpenML", "magrittr", 
#   "data.table", "farff", "ranger", "mlrCPO", "parallelMap", "xgboost", 
#   "Metrics", "foreach", "prediction", "rgl", "randomForest", "pracma", 
#   "irace", "ggplot2", "plot3Drgl", "latex2exp", "scatterplot3d", "ggrepel",
#   "datarium", "dplyr", "roxygen2", "gridExtra", "Formula", "StatMatch", 
#   "mlr3")

packages = c("ranger", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3keras", "OpenML", 
  "mlr3misc", "mlr3tuning", "e1071", "stringr", "parallelMap",
  "paradox", "checkmate", "keras", "rjson", "pracma", "irace")

new.packages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(packages, require, character.only = TRUE)

# #--- Load respositories mosmafs and iml ----
# if ("mosmafs" %in% installed.packages()) {
#   library("mosmafs")
# } else {
#   devtools::install_github("compstat-lmu/mosmafs", ref = "mosmafs-package")
# }
# 
# load_all(file.path(libs.path, "../iml"))




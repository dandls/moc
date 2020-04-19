###############################
### RUN MOC
##############################

#--- Setup ----
source("../helpers/libs_mlr.R")
source("random_search.R")
source("study_design.R")
source("helpers_evaluate.R")

args = commandArgs(trailingOnly=TRUE)
data.dir = args[[6]]
library(parallel)
cpus = 4L
PARALLEL = FALSE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

#--- Create tuning instances---
instances = readRDS(args[[2]])

#--- Get tuned Parameterset ----
best.config = readRDS(args[[4]])

#--- Run Benchmark ----
if (PARALLEL) {
    parallelStartMulticore(cpus = cpus, load.balancing = TRUE)
}
bench.list = parallelMap(fun = study_design, inst = instances, 
    more.args = list(best.config = best.config, save.dir = data.dir))
if (PARALLEL) {
    parallelStop() 
}

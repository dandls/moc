###############################
### RUN MOC
##############################

#--- Setup ----
source("../helpers/libs_mlr.R")
source("random_search.R")
source("study_design.R")
source("helpers_evaluate.R")

args = commandArgs(trailingOnly=TRUE)
data.dir = args[[3]]
library(parallel)
cpus = 20L
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

#--- Create tuning instances---
instances = readRDS(args[[1]])

#--- Get tuned Parameterset ----
best.config = readRDS(args[[2]])

#--- Run Benchmark ----
if (PARALLEL) {
    parallelMap::parallelStartMulticore(cpus = cpus, load.balancing = TRUE)
}
# best.config$generations = 2L
# best.config$mu = 10L
bench.list = parallelMap::parallelMap(fun = study_design, inst = instances, 
    more.args = list(best.config = best.config, save.dir = data.dir))
if (PARALLEL) {
    parallelMap::parallelStop() 
}

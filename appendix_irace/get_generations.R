###############################
### Get default for generations 
###############################

#--- Setup ----
source("../helpers/libs_mlr.R")
args = commandArgs(trailingOnly=TRUE)
models_irace = readRDS(args[[1]])
models.5 = rep(models_irace, 5)
savedir = args[[2]]
irace_results = readRDS(args[[3]])
savedirgen = args[[4]]
cpus = 5L
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)


#--- Extract best parameterset ----
# Get best parameters from irace, only keep best and round to 2 digits
best_params = irace::removeConfigurationsMetaData(irace_results)[1,]
num_id = sapply(best_params, is.numeric)
best_params[num_id] = round(best_params[num_id], 2)

#--- Conduct experiment ----
# Sample datapoint, extract target and find number of generations 
# the hypervolume did not increase for 10 generations
if (PARALLEL) {
  parallelStartMulticore(cpus = cpus, load.balancing = TRUE)
}
generations = parallelMap(function(inst){
  message(paste(inst$task.id, inst$learner.id, sep = " / "))
  inst = initialize_instance(inst, "data_irace")
  x.interest = inst$x.interest
  target = inst$target
  pred = inst$predictor
  best_params = best_params
  cf = Counterfactuals$new(predictor = pred, target = target, 
    mu = best_params$mu, x.interest = x.interest, p.mut = best_params$p.mut, 
    p.rec = best_params$p.rec, p.mut.gen = best_params$p.mut.gen, 
    p.mut.use.orig = best_params$p.mut.use.orig, 
    p.rec.gen = best_params$p.rec.gen, 
    p.rec.use.orig = best_params$p.rec.use.orig, 
    generations = list(mosmafsTermStagnationHV(10), 
      mosmafsTermGenerations(400))) 
  return(nrow(cf$log) - 10)
}, models.5)

if (PARALLEL) {
  parallelStop()
}

#--- Extract median generations ----
allgen = unlist(generations)
generations = as.integer(quantile(allgen, 0.8, na.rm = TRUE)) 
saveRDS(allgen, savedirgen)
# Save best parameter set 
best_config = cbind(generations, best_params)
saveRDS(best_config, savedir)
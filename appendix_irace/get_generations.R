###############################
### Get default for generations 
###############################

#--- Setup ----
source("../helpers/libs_mlr.R")
args = commandArgs(trailingOnly=TRUE)
models_irace = readRDS(args[[1]])
data_dir = args[[2]]
irace_results = readRDS(args[[3]])
savedir = args[[4]]


cpus = parallel::detectCores()
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

models_irace.10 = flatten_instances(models_irace)

#--- Extract best parameterset ----
# Get best parameters from irace, only keep best and round to 2 digits
best_params = irace::removeConfigurationsMetaData(irace_results)[1,]
num_id = sapply(best_params, is.numeric)
best_params[num_id] = round(best_params[num_id], 3)

#--- Conduct experiment ----
# Sample datapoint, extract target and find number of generations 
# the hypervolume did not increase for 10 generations
if (PARALLEL) {
  parallelMap::parallelStartSocket(cpus = 20, load.balancing = TRUE) # ParallelStartMulticore does not work for xgboost
  parallelMap::parallelSource("../helpers/libs_mlr.R", master = FALSE)
  parallelMap::parallelExport("best_params", "data_dir")
  
}
tryCatch({
  set.seed(1234)
generations = parallelMap::parallelMap(function(inst){
  gc()
  # Extract info 
  inst = initialize_instance(inst, data_dir)
  x.interest = inst$x.interest
  target = inst$target
  pred = inst$predictor$clone()
  if (best_params$conditional) {
    conditionals = readRDS(file.path(data_dir, inst$task.id, "conditional.rds"))
    pred$conditionals = conditionals
  } 
  # Receive counterfactuals
  cf = Counterfactuals$new(predictor = pred, target = target, 
    mu = best_params$mu, x.interest = x.interest, p.mut = best_params$p.mut, 
    p.rec = best_params$p.rec, p.mut.gen = best_params$p.mut.gen, 
    p.mut.use.orig = best_params$p.mut.use.orig, 
    p.rec.gen = best_params$p.rec.gen, 
    p.rec.use.orig = best_params$p.rec.use.orig,
    initialization = best_params$initialization,
    generations = list(mosmafs::mosmafsTermStagnationHV(10),
      mosmafs::mosmafsTermGenerations(400))) #SD
  # Save number of generations
  cat(sprintf("finished: %s/%s\n", inst$learner.id, inst$task.id))
  return(nrow(cf$log) - 10)
}, models_irace.10)
}, finally = {
  if (PARALLEL) {
    parallelMap::parallelStop()
  }
})
#--- Extract median generations ----
allgen = unlist(generations)
generations = as.integer(quantile(allgen, 0.9, na.rm = TRUE)) 
saveRDS(allgen, file.path(data_dir, "max_generations.rds"))
# Save best parameter set 
best_config = cbind(generations, best_params)
saveRDS(best_config, savedir)

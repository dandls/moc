#################################
### Get number evals used for irace
#################################

#---Setup---
source("../helpers/libs_mlr.R")

args = commandArgs(trailingOnly=TRUE)
models_irace = readRDS(args[[1]])
data_dir = args[[2]]
save_dir = args[[3]]
PARALLEL = TRUE
mu = 50
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)


cpus = parallel::detectCores()

#--- Use sampled observations ----
# 10 for each dataset and model
models_irace.10 = flatten_instances(models_irace)

if (PARALLEL) {
  parallelMap::parallelStartSocket(cpus = 20, load.balancing = TRUE) # ParallelStartMulticore does not work for xgboost
  parallelMap::parallelSource("../helpers/libs_mlr.R", master = FALSE)
  parallelMap::parallelLibrary("pracma")
  parallelMap::parallelExport("mu", "data_dir")
  
  ## parallelExport(
  ##   "Counterfactuals", "Predictor", "Conditional", "InterpretationMethod",
  ##   "make_paramlist", "mu",
  ##   "data_dir", "initialize_instance",
  ##   "char_to_factor",
  ##   "transform_to_orig",
  ##   "sdev_to_list", "select_nondom", "select_diverse",
  ##   "fitness_fun", "computeCrowdingDistanceR", "get_diff",
  ##   "load_keras_model")
  ## parallelLibrary("keras", "pracma", "iml")
  ## parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
  ##   "trans_target", "invoke", "get_keras_model", "predict_proba")
  ## parallelLibrary("mosmafs", "ParamHelpers")
}
tryCatch({
  set.seed(1234)
  get_nr_generations = parallelMap::parallelMap(function(inst){
    gc()
    # Sample data point as x.interest
    inst = initialize_instance(inst, data_dir)
    x.interest = inst$x.interest
    target = inst$target
    conditionals <<- tryCatch(conditionals, error = function(e) readRDS(file.path(data_dir, inst$task.id, "conditional.rds")))
    pred = inst$predictor$clone()
    pred$conditionals = conditionals
    # Receive counterfactuals
    cf = Counterfactuals$new(predictor = pred, target = target,
      x.interest = x.interest, mu = mu, epsilon = 0,
      generations = list(mosmafs::mosmafsTermStagnationHV(10),
        mosmafs::mosmafsTermGenerations(400))) #SD
    # Save number of generations
    cat(sprintf("finished: %s/%s\n", inst$learner.id, inst$task.id))
    
    nrow(cf$log) - 1  # number of generations excludes the 0th generation
  }, models_irace.10) #SD
}, finally = {
  if (PARALLEL) {
    parallelMap::parallelStop()
  }
})
#--- Save number of generations ----
saveRDS(unlist(get_nr_generations) * mu, save_dir)

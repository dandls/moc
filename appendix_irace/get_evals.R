####################################
### Get number evals used for irace
####################################

#---Setup---
source("../helpers/libs_mlr.R")
source("helpers.R")

args = commandArgs(trailingOnly=TRUE)
models_irace = readRDS(args[[1]]) # models_irace = readRDS("../saved_objects/irace/models_irace.rds")
data_dir = args[[2]] # data_dir = "../saved_objects/irace"
save_dir = args[[3]] # save_dir = "../saved_objects_rerun/irace/max_eval.rds"
PARALLEL = TRUE
mu = 50L
n_generations = 300L
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)


cpus = parallel::detectCores()

#--- Use sampled observations ----
# 10 for each dataset and model
models_irace.10 = flatten_instances(models_irace)

if (PARALLEL) {
  parallelMap::parallelStartSocket(cpus = cpus, load.balancing = TRUE) # ParallelStartMulticore does not work for xgboost
  parallelMap::parallelSource("../helpers/libs_mlr.R", master = FALSE)
  parallelMap::parallelSource("helpers.R", master = FALSE)
  parallelMap::parallelLibrary("pracma")
  parallelMap::parallelExport("mu", "n_generations", "data_dir")
}
tryCatch({
  set.seed(1234)
  get_nr_generations = parallelMap::parallelMap(function(inst){
    print(paste(inst$learner.id, "on", inst$task.id))
    # Sample data point as x.interest
    inst = initialize_instance(inst, data_dir)
    x.interest = inst$x.interest
    target = inst$target
    # Receive counterfactuals
    moccf = MOCClassif$new(predictor = inst$predictor, epsilon = 0, mu = mu, 
      n_generations = n_generations, p_rec = 0.9, p_rec_gen = 0.7, use_conditional_mutator = FALSE,
      p_mut = 0.2, p_mut_gen = 0.5, p_mut_use_orig = 0.2, k = 1L, quiet = TRUE)
    cf = moccf$find_counterfactuals(x_interest = x.interest, 
      desired_class = make.names(inst$predictor$class), desired_prob = target)
    hv = moccf$get_dominated_hv()$hv
    return(getTermGenerations(hv))
  }, models_irace.10) 
}, finally = {
  if (PARALLEL) {
    parallelMap::parallelStop()
  }
})

#--- Save number of generations ----
saveRDS(unlist(get_nr_generations) * mu, save_dir)

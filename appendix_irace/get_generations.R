###############################
### Get default for generations 
###############################

#--- Setup ----
source("../helpers/libs_mlr.R")
source("helpers.R")
args = commandArgs(trailingOnly=TRUE)
models_irace = readRDS(args[[1]]) # models_irace = readRDS("../saved_objects/irace/models_irace.rds")
data_dir = args[[2]] # data_dir = "../saved_objects/irace"
irace_results = readRDS(args[[3]]) # irace_results = readRDS("../saved_objects/irace/irace_results.rds")
savedir = args[[4]] # savedir = " ../saved_objects/best_configs.rds"


cpus = parallel::detectCores()
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

models_irace.10 = flatten_instances(models_irace)

n_generations = 300L

#--- Extract best parameterset ----
# Get best parameters from irace, only keep best and round to 2 digits
best_params = irace::removeConfigurationsMetaData(irace_results)[1,]
num_id = sapply(best_params, is.numeric)
best_params[num_id] = round(best_params[num_id], 3)
best_params$conditional = as.logical(best_params$conditional)
# names(best_params) = gsub('\\.', '_', names(best_params))
# names(best_params)[names(best_params) == "initialization"] = "init_strategy"

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
    # Receive counterfactuals
    moccf = MOCClassif$new(predictor = pred, epsilon = 0, mu = best_params$mu, 
      n_generations = n_generations, init_strategy = best_params$init_strategy,
      use_conditional_mutator = best_params$conditional,
      p_rec = best_params$p_rec, p_rec_gen = best_params$p_rec_gen,
      p_mut = best_params$p_mut, p_mut_gen = best_params$p_mut_gen, p_mut_use_orig = best_params$p_mut_use_orig,
      quiet = TRUE)
    cf = moccf$find_counterfactuals(x_interest = x.interest, 
      desired_class = make.names(inst$predictor$class), desired_prob = target)
    hv = moccf$get_dominated_hv()$hv
    # Save number of generations
    cat(sprintf("finished: %s/%s\n", inst$learner.id, inst$task.id))
    return(getTermGenerations(hv)) 
  }, models_irace.10)
}, finally = {
  if (PARALLEL) {
    parallelMap::parallelStop()
  }
})
#--- Extract median generations ----
allgen = unlist(generations)
generations = as.integer(max(allgen)) 
saveRDS(allgen, file.path(data_dir, "max_generations.rds"))

# Save best parameter set 
best_config = cbind(generations, best_params)
saveRDS(best_config, savedir)

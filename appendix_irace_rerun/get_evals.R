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


#--- Sample new observations ----
# 5 for each dataset and model 
models_irace.5 = rep(models_irace, 5)

if (PARALLEL) {
  parallelStartSocket(cpus = 20) # ParallelStartMulticore does not work for xgboost
  
  parallelExport(
    "Counterfactuals", "Predictor", "Conditional", "InterpretationMethod",
    "make_paramlist", "mu",
    "data_dir", "initialize_instance",
    "char_to_factor",
    "transform_to_orig",
    "sdev_to_list", "select_nondom", "select_diverse", 
    "fitness_fun", "computeCrowdingDistanceR", "get_diff",
    "load_keras_model")
  parallelLibrary("keras", "pracma", "iml")
  parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
    "trans_target", "invoke", "get_keras_model", "predict_proba")
  parallelLibrary("mosmafs", "ParamHelpers")
}

set.seed(1234)
get_nr_generations = parallelMap(function(inst){
    message("next")
    # Sample data point as x.interest 
    inst = initialize_instance(inst, data_dir)
    x.interest = inst$x.interest
    target = inst$target
    # Receive counterfactuals 
    cf = Counterfactuals$new(predictor = inst$predictor, target = target,
      x.interest = x.interest, mu = mu,
      generations = list(mosmafsTermStagnationHV(10),
        mosmafsTermGenerations(400))) #SD
    # Save number of generations 
    gen = nrow(cf$log)-1
    print(paste("finished: ", inst$learner.id, "/", inst$task.id, sep = ""))
    return(gen)
}, models_irace.5) #SD

if (PARALLEL) {
  parallelStop()
}

#--- Save number of generations ----
saveRDS(unlist(get_nr_generations)*mu, save_dir)

  



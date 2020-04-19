#################################
### IRACE 
#################################

#--- Setup ----
source("../helpers/libs_mlr.R")
library("irace")

args = commandArgs(trailingOnly=TRUE)
read_dir = args[[2]]
save_dir = args[[4]]
evals = 200*50
cpus = 5L 
evals = max(evals)
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

#--- Create tuning instances----
models_irace = readRDS(read_dir)
instances = rep(models_irace, 5)

# Check for NULL Entries
detect_null = lapply(instances, function(inst) {
    lapply(inst, function(el) {if (length(el) == 0) stop("")})
})

#--- Irace Setup ----
ps = pSS(
  mu : integer[20, 100],
  p.mut : numeric[0.05, 0.8], 
  p.rec : numeric[0.3, 1], 
  p.mut.gen : numeric[0.05, 0.8],
  p.mut.use.orig : numeric[0.05, 0.5], 
  p.rec.gen : numeric[0.3, 1], 
  p.rec.use.orig : numeric[0.3, 1] 
)

targetRunnerParallel = function(experiment, exec.target.runner, scenario, target.runner) {
  inst = experiment[[1]]$instance
  inst = initialize_instance(inst, "data_irace")
  x.interest = inst$x.interest
  target = inst$target
  message(inst$learner.id)
  hv_auc = parallelMap(function(curexp) {
    if (inst$learner.id %in% c("logreg", "neuralnet")) {
      inst = load_keras_model(inst, "data_irace")
    }
    pred = inst$predictor
    pars = curexp$configuration
    cf = Counterfactuals$new(predictor = pred, target = target, 
      mu = pars$mu, x.interest = x.interest, p.mut = pars$p.mut, 
      p.rec = pars$p.rec, p.mut.gen = pars$p.mut.gen, 
      p.mut.use.orig = pars$p.mut.use.orig, 
      p.rec.gen = pars$p.rec.gen, 
      p.rec.use.orig = pars$p.rec.use.orig, 
      generations = list(mosmafsTermEvals(evals)))
    integral(approxfun(c(0, cf$log$evals), c(0, cf$log$fitness.domHV)), 
      xmin = 0, xmax = evals)
  }, experiment)
  res = lapply(hv_auc, function(y) list(cost = y*-1, time = NA_real_))
  return(res)
}
extra.args = list()
extra.args$maxExperiments = 3000
extra.args$firstTest = 15

tuner.config = c(list(targetRunnerParallel = targetRunnerParallel, 
  instances = instances), extra.args)

#--- Run Irace ----
library(parallel)
if (PARALLEL) {
  parallelStartSocket(cpus = cpus) # ParallelStartMulticore does not work for xgboost
  parallelExport("Counterfactuals", "Predictor",
    "make_paramlist", "evals",
    "char_to_factor", "transform_to_orig",
    "sdev_to_list", "select_nondom", "select_diverse",
    "fitness_fun", "computeCrowdingDistanceR_ver1", "get_diff",
    "compute_diversity", "load_keras_model")
  parallelLibrary("keras", "pracma")
  parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
    "trans_target", "invoke", "get_keras_model", "predict_proba")
  parallelLibrary("mosmafs", "ParamHelpers")
}

irace_results = irace(scenario = tuner.config, 
  parameters = convertParamSetToIrace(ps))

if (PARALLEL) {
  parallelStop()
}

saveRDS(irace_results, save_dir)

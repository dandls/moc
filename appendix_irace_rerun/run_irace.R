#################################
### IRACE 
#################################

#--- Setup ----
source("../helpers/libs_mlr.R")
library("irace")

args = commandArgs(trailingOnly=TRUE)
read_dir = args[[1]]
save_dir = args[[2]]
data_dir = args[[4]]
evals = 200*50
evals = 2L #SD
cpus = 20L 
evals = max(evals)
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

#--- Create tuning instances----
instances = readRDS(read_dir)

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
  p.rec.use.orig : numeric[0.3, 1], 
  initialization : discrete[random, sd, icecurve, traindata], 
  conditional : logical
)

targetRunnerParallel = function(experiment, exec.target.runner, scenario, target.runner) {
  inst = experiment[[1]]$instance
  cond = any(unlist(lapply(experiment, function(exp) as.logical(exp$configuration$conditional))))
  inst = initialize_instance(inst, data_dir)
  pred = inst$predictor
  if (cond) {
    conditional = readRDS(file.path(data_dir, inst$task.id, "conditional.rds"))
  }
  x.interest = inst$x.interest
  target = inst$target
  message(inst$learner.id)
  hv_auc = parallelMap(function(curexp) {
    pars = curexp$configuration
    if (inst$learner.id %in% c("logreg", "neuralnet")) {
      inst = load_keras_model(inst, data_dir)
    }
    if (is.logical(pars$conditional)) {
      pred = inst$predictor$clone()
      pred$conditionals = conditional
    } else {
      pred = inst$predictor
    }
    cf = Counterfactuals$new(predictor = pred, target = target, 
      mu = pars$mu, x.interest = x.interest, p.mut = pars$p.mut, 
      p.rec = pars$p.rec, p.mut.gen = pars$p.mut.gen, 
      p.mut.use.orig = pars$p.mut.use.orig, 
      p.rec.gen = pars$p.rec.gen, 
      p.rec.use.orig = pars$p.rec.use.orig, 
      initialization = pars$initialization,
      generations = list(mosmafsTermEvals(evals)))
    integral(approxfun(c(0, cf$log$evals), c(0, cf$log$fitness.domHV)), 
      xmin = 0, xmax = evals)
  }, experiment)
  res = lapply(hv_auc, function(y) list(cost = y*-1, time = NA_real_))
  return(res)
}
extra.args = list()
extra.args$maxExperiments = 1000 #3000
extra.args$firstTest = 15

tuner.config = c(list(targetRunnerParallel = targetRunnerParallel, 
  instances = instances), extra.args)

#--- Run Irace ----
library(parallel)
if (PARALLEL) {
  parallelStartSocket(cpus = cpus) # ParallelStartMulticore does not work for xgboost
  parallelExport(
  "Counterfactuals", "Predictor", "Conditional", "InterpretationMethod",
  "make_paramlist",
    "evals", "data_dir",
  "char_to_factor",
   "transform_to_orig",
   "sdev_to_list", "select_nondom", "select_diverse", "get_ICE_var", "get_ice_curve",
   "fitness_fun", "computeCrowdingDistanceR", "get_diff",
    "load_keras_model", "get.grid.1D")
  parallelLibrary("keras", "pracma", "iml")
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

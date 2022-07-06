#################################
### IRACE
#################################

#--- Setup ----
source("../helpers/libs_mlr.R")
library("irace")
library("pracma") # for integral

args = commandArgs(trailingOnly=TRUE)
read_dir = args[[1]] # read_dir = "../saved_objects/irace/models_irace.rds"
save_dir = args[[2]] # save_dir = "../saved_objects/irace/irace_results.rds"
data_dir = args[[4]] # data_dir = "../saved_objects/irace"
all_evals = readRDS(args[[3]]) # all_evals = readRDS("../saved_objects/irace/max_eval.rds")
evals = (ceiling(quantile(all_evals, probs = 0.9)/1000)*1000)[[1]]

cpus = 20
PARALLEL = TRUE
SAVE = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

#--- Create tuning instances----
instances = readRDS(read_dir)
rmid = unlist(lapply(instances, function(inst) inst$task.id %in% c("nursery", "kc1")))
instances = instances[!rmid]
instances = flatten_instances(instances)
instances = instances[sample.int(length(instances))]

# Check for NULL Entries
detect_null = lapply(instances, function(inst) {
  lapply(inst, function(el) stopifnot(length(el) > 0))
})

#--- Irace Setup ----
ps = pSS(
  mu : integer[20, 100],
  p_mut : numeric[0.05, 0.8],
  p_rec : numeric[0.3, 1],
  p_mut_gen : numeric[0.05, 0.8],
  p_mut_use_orig : numeric[0.05, 0.5],
  p_rec_gen : numeric[0.3, 1]
)

targetRunnerParallel = function(experiment, exec.target.runner, scenario, target.runner) {
  
  # we are assuming here that we have the same instance in every experiment:
  stopifnot(length(unique(vapply(experiment, `[[`, numeric(1), "id.instance"))) == 1)
  inst = experiment[[1]]$instance

  message(inst$learner.id)
  message(inst$task.id)
  gc()
  if (PARALLEL) {
    parallelMap::parallelStartSocket(cpus = min(cpus, length(experiment)),
      load.balancing = length(experiment) > cpus) # ParallelStartMulticore does not work for xgboost
    parallelMap::parallelSource("../helpers/libs_mlr.R", master = FALSE)
    parallelMap::parallelLibrary("pracma")
    parallelMap::parallelExport("evals", "data_dir", "inst")
  }
  tryCatch({
    hv_auc = parallelMap::parallelMap(function(curexp) {
      gc()

      inst = initialize_instance(inst, data_dir)
      
      pars = curexp$configuration
      
      pred = inst$predictor
      
      if (inst$task.id == "mammography") {
        declass = pred$class
      } else {
        declass = make.names(pred$class)
      }

      set.seed(curexp$seed)
      moccf = MOCClassif$new(predictor = pred, epsilon = 0, mu = pars$mu, 
        n_generations = ceiling(evals/pars$mu), init_strategy = "icecurve",
        use_conditional_mutator = FALSE,
        p_rec = pars$p_rec, p_rec_gen = pars$p_rec_gen,
        p_mut = pars$p_mut, p_mut_gen = pars$p_mut_gen, p_mut_use_orig = pars$p_mut_use_orig, quiet = TRUE)
      cf = moccf$find_counterfactuals(x_interest = inst$x.interest, 
        desired_class = declass, desired_prob = inst$target)
      
      # compute integral under hv curve over evaluations
      hv = moccf$get_dominated_hv()
      hv$evals = hv$generation * pars$mu
      integral(approxfun(c(0, hv$evals), c(0, hv$hv)),
        xmin = 0, xmax = evals)
    }, experiment)
  }, finally = {
    if (PARALLEL) {
      parallelMap::parallelStop()
    }
  })
  
  lapply(hv_auc, function(y) list(cost = y * -1, time = NA_real_))
}
extra.args = list()
extra.args$maxExperiments = 2500
extra.args$firstTest = 15

tuner.config = c(list(targetRunnerParallel = targetRunnerParallel,
  instances = instances), extra.args)

#--- Run Irace ----
set.seed(12345)
irace_results = irace(scenario = tuner.config,
  parameters = convertParamSetToIrace(ps))

if (SAVE) saveRDS(irace_results, save_dir)

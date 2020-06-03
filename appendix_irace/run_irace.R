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
evals = readRDS(args[[3]])
evals = (ceiling(quantile(evals, probs = 0.95)/100)*100)[[1]]
#evals = 2L #SD

cpus = 20
evals = max(evals)
PARALLEL = TRUE
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

#--- Create tuning instances----
instances = readRDS(read_dir)
instances = flatten_instances(instances)
instances = instances[sample.int(length(instances))]

# Check for NULL Entries
detect_null = lapply(instances, function(inst) {
  lapply(inst, function(el) stopifnot(length(el) > 0))
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
  initialization : discrete[random, icecurve],  #SD evtl traindata raus???
  conditional : logical
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

      if (as.logical(pars$conditional)) {
        conditionals <<- tryCatch(conditionals, error = function(e) readRDS(file.path(data_dir, inst$task.id, "conditional.rds")))
        pred = inst$predictor$clone()
        pred$conditionals = conditionals
      } else {
        pred = inst$predictor
      }
      
      set.seed(curexp$seed)
      cf = Counterfactuals$new(predictor = pred, target = inst$target,
        mu = pars$mu, x.interest = inst$x.interest, p.mut = pars$p.mut,
        epsilon = 0, 
        p.rec = pars$p.rec, p.mut.gen = pars$p.mut.gen,
        p.mut.use.orig = pars$p.mut.use.orig,
        p.rec.gen = pars$p.rec.gen,
        p.rec.use.orig = pars$p.rec.use.orig,
        initialization = pars$initialization,
        generations = list(mosmafs::mosmafsTermEvals(evals)))


      integral(approxfun(c(0, cf$log$evals), c(0, cf$log$fitness.domHV)),
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
extra.args$maxExperiments = 3000 #SD 1000
extra.args$firstTest = 15

tuner.config = c(list(targetRunnerParallel = targetRunnerParallel,
  instances = instances), extra.args)

#--- Run Irace ----
set.seed(12345)
irace_results = irace(scenario = tuner.config,
  parameters = convertParamSetToIrace(ps))

saveRDS(irace_results, save_dir)

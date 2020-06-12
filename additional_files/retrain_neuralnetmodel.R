# Retrain model 
source("../helpers/keras_mlr_learner.R")

inst = subset_instances(instances = readRDS("../saved_objects_rerun/benchmark/models_benchmark.rds"), task = "plasma_retinol", 
  learner = "neuralnet")
lrn = makeLearner("classif.keraslogreg", predict.type = "prob")
lrn = setHyperPars(lrn, epochs = 1000L)


train.task = makeClassifTask(data = data.frame(inst$predictor$data$get.xy()), target = inst$predictor$data$y.names)
cat(sprintf("Resampling task %s x learner %s\n", inst$task.id, inst$learner.id))
lrn = cpoScaleRange() %>>% cpoDummyEncode(reference.cat = FALSE) %>>%
  cpoFixNames() %>>% lrn
par.set = pSS(
  lr: numeric[0.0005, 0.1],
  layer_size: integer[1, 6]
)
TUNEITERS = 100L
EVALUATE = TRUE
RESAMPLING =cv5

if (PARALLEL) {
  set.seed(123456, "L'Ecuyer-CMRG")
  parallelMap::parallelStartSocket(20L, level = "mlr.tuneParams")
  parallelMap::parallelSource("../helpers/libs_mlr.R", level = "mlr.tuneParams", master = FALSE)
}

ctrl = makeTuneControlRandom(maxit = TUNEITERS * length(par.set$pars))
if (EVALUATE) {
  lrn.tuning = makeTuneWrapper(lrn, RESAMPLING, list(mlr::acc), par.set, ctrl, show.info = FALSE) #SD
}
res = tuneParams(lrn, train.task, RESAMPLING, par.set = par.set, control = ctrl,
  show.info = FALSE)
if (EVALUATE) {
  performance = resample(lrn.tuning, train.task[[row]], RESAMPLING, list(mlr::acc))$aggr #SD
} else {
  performance = NA
}
if (PARALLEL) {
  parallelMap::parallelStop()
}
list(
  performance = performance,
  paramvals = list(res$x))

####################################
## Train Models
####################################

#---Setup----
source("../helpers/libs_mlr.R")
set.seed(1234)

# Collect arguments
args = commandArgs(trailingOnly=TRUE)
SAVE_KERAS = as.logical(args[[1]])
task_ids = readRDS(args[[2]])
save_dir = args[[3]]
folder = args[[4]]
dir.create(path = "../saved_objects_rerun", showWarnings = FALSE)

PARALLEL = TRUE
cpus = parallel::detectCores()
current_dir = getwd()
data_dir = file.path(current_dir, folder)
dir.create(path = data_dir, showWarnings = FALSE)
names_models = c("randomforest", "xgboost", "svm", "logreg", "neuralnet")

cpoFixNames = makeCPO("fixnames",
  cpo.train = NULL,
  cpo.retrafo = {
    colnames(data) <- make.names(colnames(data))
    data
  })

# --- Task design ----
checkmate::assert_double(task_ids)
task_list = lapply(task_ids, function(x) {
  OpenML::getOMLTask(task.id = x)
})
names(task_list) = task_ids
task_list = lapply(task_list, function(task.oml) {
  t = OpenML::convertOMLTaskToMlr(task.oml)$mlr.task
  # if (task.oml$task.id == 52945) {
  #   df = getTaskData(t)
  #   df$age = factor(str_replace(df$age, pattern = "-", "."))
  #   df$inv.nodes = factor(str_replace(df$inv.nodes, pattern = "-", "."))
  #   df$node.caps = factor(str_replace(df$node.caps, pattern = "-", "."))
  #   df$tumor.size = factor(str_replace(df$tumor.size, pattern = "-", "."))
  #   t = changeData(t, df)
  # }
  if (SAVE_KERAS) {
    dir.create(path = file.path(data_dir, t$task.desc$id), showWarnings = FALSE)
  }
  t
})

# ---Extract info from tasks ----
# Necessary to read into Python
sampled.rows = lapply(task_list, function(onetask) {
  task.id = onetask$task.desc$id
  sampled.rows = sample.int(onetask$task.desc$size, size = 10, replace = FALSE)
  if (SAVE_KERAS) {
    dir_name = file.path(data_dir, task.id)
    # Save sampled rows
    write(sampled.rows, file = file.path(dir_name, "sampled_ids.txt"), ncolumns = 1)
    # Save original data
    dat = getTaskData(onetask)
    dat[[getTaskTargetNames(onetask)]] = trans_target(dat[[getTaskTargetNames(onetask)]])
    write.csv(dat, file = file.path(dir_name, "data_orig.csv"), row.names = FALSE)
    # Encode features with enc
    # Different handling of binary features (due to recourse!)
     map = cpoScaleRange() %>>% cpoDummyEncode()
    nw.onetask = applyCPO(map, onetask)
    dat_encoded = getTaskData(nw.onetask)
    dat_encoded[[getTaskTargetNames(nw.onetask)]] = trans_target(dat_encoded[[getTaskTargetNames(nw.onetask)]])
    write.csv(dat_encoded, file = file.path(dir_name, "data_encoded.csv"), row.names = FALSE)
    if (!task.id %in% c("cmc", "tic-tac-toe", "plasma_retinol", "kr-vs-kp")) {
        map = cpoScaleRange() %>>% cpoDummyEncode(reference.cat = TRUE)
        nw.onetask = applyCPO(map, onetask)
        dat_encoded = getTaskData(nw.onetask)
        dat_encoded[[getTaskTargetNames(nw.onetask)]] = trans_target(dat_encoded[[getTaskTargetNames(nw.onetask)]])
        write.csv(dat_encoded, file = file.path(dir_name, "data_encoded_refcat.csv"), row.names = FALSE)
    }
    # Save feature types
    col_info = sapply(dat[,getTaskFeatureNames(onetask)], class)
    col_info["target"] = getTaskTargetNames(onetask)
    feature.types = rjson::toJSON(col_info)
    write(feature.types, file = file.path(dir_name, "feature_types.json"))
    # Save scale and center
    state = getCPOTrainedState(retrafo(onetask %>>% cpoScale()))
    center = rjson::toJSON(state$control$center)
    scale = rjson::toJSON(state$control$scale)
    write(center, file = file.path(dir_name, "feature_center.json"))
    write(scale, file = file.path(dir_name, "feature_scale.json"))
  }
  return(sampled.rows)
})

# --- Algorithm design ----
lrn.list = makeLearners(c("randomForest", "xgboost", "svm", "keraslogreg",  "keraslogreg"),
  type = "classif", predict.type = "prob")

lrn.list[[2]] = setHyperPars(lrn.list[[2]], nthread = 1)
lrn.list[[4]] = setHyperPars(lrn.list[[4]], layer_size = 0, lr = 0.001, epochs = 1000L)
lrn.list[[5]] = setHyperPars(lrn.list[[4]], epochs = 1000L)

names(lrn.list) = names_models

hyper.pars = list(
  randomforest = pSS(
    ntree : numeric[0, log(1000)]  [[trafo = function(x) round(exp(x))]]),
  xgboost = pSS(
    nrounds: numeric[0, log(1000)] [[trafo = function(x) round(exp(x))]]
  ),
  svm = pSS(
    cost: numeric[0.01, 1]
  ),
  logreg = pSS(
    lr: numeric[0.0005, 0.1]),
  neuralnet = pSS(
    lr: numeric[0.0005, 0.1],
    layer_size: integer[1, 6]
  )
)

#--- Tune and train models ----
grid = expand.grid(task.id = task_ids,
  lrn.ind = names_models)

subset.id = which((grid$lrn.ind == "logreg") & (grid$task.id %in% c(3846, 145804, 3778, 3)))
if (length(subset.id) > 0) {
  grid = grid[-subset.id, ]
}

print(nrow(grid))
#grid = grid[c(20:21),] #SD
#grid = grid[c(1:4, 20), ] #SD

stopifnot(identical(names(lrn.list), names(hyper.pars)))
stopifnot(identical(names(task_list), names(sampled.rows)))

learners = data.table::data.table(learner.id = names(lrn.list),
  learner = lrn.list, searchspace = hyper.pars, key = "learner.id")

tasks = data.table::data.table(openml.id = as.numeric(names(task_list)),
  task = task_list, sampled.rows = sampled.rows, key = "openml.id")

grid = data.table::as.data.table(grid)

tasks[,
  task.id := vapply(task, function(x) x$task.desc$id, character(1))][,
  # preproc.cpo: these should be applied to the task
  # (because some of then need outcome class balancing oversampling)
  task.preproc.cpo := lapply(task.id, function(task.nam) {
    if (task.nam %in% c("tic-tac-toe", "diabetes")) {
      cpoOversample(rate = 2L)
    } else if (task.nam %in% c("ilpd", "kc2")) {
      cpoOversample(rate = 3L)
    } else if (task.nam %in% "pc1") {
      cpoOversample(rate = 5L)
    } else {
      NULLCPO
    }
  })][,
  # train.task: has the evaluation points removed
  train.task := Map(function(t, s) subsetTask(t, subset = seq_len(getTaskSize(t))[-s]),
    task, sampled.rows)]

learners[,
  learner.preproc.cpo := lapply(learner.id, function(lrn)
    (if (lrn == "randomforest") cpoScale() else cpoScaleRange()) %>>%
    cpoDummyEncode(reference.cat = (lrn == "logreg")) %>>%
    cpoFixNames())]

task.learner.grid = tasks[learners[grid, on = c(learner.id = "lrn.ind")], on = c(openml.id = "task.id")]

### Evaluate Performance
if (PARALLEL) {
  set.seed(123456, "L'Ecuyer-CMRG")
  parallelMap::parallelStartSocket(cpus, level = "mlr.tuneParams")
  parallelMap::parallelSource("../helpers/libs_mlr.R")
}
tryCatch({
  task.learner.grid[,
    c("performance", "paramvals") := data.table::rbindlist(lapply(seq_len(nrow(task.learner.grid)), function(row) {

      cat(sprintf("Resampling task %s x learner %s\n", task.id[[row]], learner.id[[row]]))
      lrn = task.preproc.cpo[[row]] %>>% learner.preproc.cpo[[row]] %>>% learner[[row]]
      par.set = searchspace[[row]]
      ctrl = makeTuneControlRandom(maxit = 100 * length(par.set$pars))
      lrn.tuning = makeTuneWrapper(lrn, cv5, list(mlr::acc), par.set, ctrl, show.info = FALSE)
      res = tuneParams(lrn, train.task[[row]], cv5, par.set = par.set, control = ctrl,
        show.info = FALSE)
      list(performance = resample(lrn.tuning, train.task[[row]], cv5, list(mlr::acc))$aggr,
        paramvals = list(res$x))

    }))]
}, finally = {
  if (PARALLEL) {
    parallelMap::parallelStop()
  }
})

if (PARALLEL) {
  set.seed(123456, "L'Ecuyer-CMRG")
  parallelMap::parallelStartSocket(cpus, load.balancing = TRUE)
  parallelMap::parallelSource("../helpers/libs_mlr.R")
  parallelMap::parallelExport("task.learner.grid")
}
tryCatch({
  models_trained = parallelMap::parallelLapply(seq_len(nrow(task.learner.grid)), function(row) {
    with(task.learner.grid, {
      dir_name = file.path(data_dir, task.id[[row]])

      # Conditional
      ctr = partykit::ctree_control(maxdepth = 5L)
      con = fit_conditionals(getTaskData(train.task[[row]])[, getTaskFeatureNames(train.task[[row]]), drop = FALSE], ctrl = ctr)
      saveRDS(object = con, file = file.path(dir_name, paste0("conditional_", as.character(learner.id[[row]]), ".rds")))

      lrn = task.preproc.cpo[[row]] %>>% learner.preproc.cpo[[row]] %>>% learner[[row]]
      lrn = setHyperPars(lrn, par.vals = paramvals[[row]])

      mod = mlr::train(lrn, train.task[[row]])

      pred = Predictor$new(mod, data = getTaskData(train.task[[row]]),
        class = getTaskDesc(train.task[[row]])$positive)
      # Save keras models --> read into python
      if ((learner.id[[row]] == "logreg") && SAVE_KERAS) {
        keras.mod =  mod$learner.model$next.model$learner.model$model
        save_model_hdf5(keras.mod, filepath = file.path(dir_name, "logreg.h5"))
      }
      if (learner.id[[row]] == "neuralnet" && SAVE_KERAS) {
        keras.mod =  mod$learner.model$next.model$learner.model$model
        save_model_hdf5(keras.mod, filepath = file.path(dir_name, "neuralnet.h5"))
      }
      list(predictor = pred, task.id = task.id[[row]],
        learner.id = learner.id[[row]],
        sampled.rows = getTaskData(task[[row]], subset = sampled.rows[[row]], target.extra = TRUE)$data,
        performance = performance[[row]])
      })
  })
}, finally = {
  if (PARALLEL) {
    parallelMap::parallelStop()
  }
})

saveRDS(models_trained, save_dir)

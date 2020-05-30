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
cpus = 20L
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

if (PARALLEL) {
  set.seed(123456, "L'Ecuyer-CMRG")
  parallelMap::parallelStartSocket(cpus, level = "mlr.tuneParams")
  parallelMap::parallelSource("../helpers/libs_mlr.R", level = "mlr.tuneParams")
  parallelMap::parallelLibrary("keras", level = "mlr.tuneParams")
  parallelMap::parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
    "trans_target", "get_keras_model", level = "mlr.tuneParams")
}
models_trained = lapply(seq_len(nrow(grid)), function(i) {
  task.id = grid$task.id[i]
  task = task_list[[as.character(task.id)]]
  task.nam = task$task.desc$id
  n = getTaskSize(task)
  train.set = sample.int(n, size = n*0.8)
  test.set = setdiff(seq_len(n), -train.set)
  train.task = subsetTask(task, subset = train.set)
  test.task = subsetTask(task, subset = test.set)

  if (task.nam %in% c("tic-tac-toe", "diabetes")) {
    train.task = mlr::oversample(train.task, rate = 2L)
  } else if (task.nam %in% c("ilpd", "kc2")) {
    train.task = mlr::oversample(train.task, rate = 3L)
  } else if (task.nam %in% c("pc1")) {
    train.task = mlr::oversample(train.task, rate = 5L)
  }

  # Train the learner
  dir_name = file.path(data_dir, task$task.desc$id)

  lrn.id = grid$lrn.ind[i]
  print(as.character(lrn.id))

  # Conditional
  ctr = partykit::ctree_control(maxdepth = 5L)
  con = fit_conditionals(getTaskData(train.task)[, getTaskFeatureNames(train.task)], ctrl = ctr)
  saveRDS(object = con, file = file.path(dir_name, paste0("conditional_", as.character(lrn.id), ".rds")))

  # Train the learner
  # Different handling if solely binary features (due to recourse)

  lrn = (if (lrn.id == "randomforest") cpoScale() else cpoScaleRange()) %>>%
    cpoDummyEncode(reference.cat = (lrn.id == "logreg")) %>>%
    cpoFixNames() %>>%
    lrn.list[[lrn.id]]

  par.set = hyper.pars[[grid$lrn.ind[i]]]
  ctrl = makeTuneControlRandom(maxit = 100L*length(par.set$pars))
  if (!is.null(par.set)) { #SD & FALSE
    res = tuneParams(lrn, train.task, cv5, par.set = par.set, control = ctrl,
      show.info = FALSE)
    lrn = setHyperPars2(lrn, res$x) # evtl exp
  }
  mod = mlr::train(lrn, train.task)
  p = predict(mod, test.task)
  perf = performance(p, measures = acc)
  pred = Predictor$new(mod, data = getTaskData(task),
    class = getTaskDesc(task)$positive)
  # Save keras models --> read into python
  if ((lrn.id == "logreg") && (SAVE_KERAS)) {
    keras.mod =  mod$learner.model$next.model$learner.model$model
    save_model_hdf5(keras.mod, filepath = file.path(dir_name, "logreg.h5"))
  }
  if (lrn.id == "neuralnet" && SAVE_KERAS) {
    keras.mod =  mod$learner.model$next.model$learner.model$model
    save_model_hdf5(keras.mod, filepath = file.path(dir_name, "neuralnet.h5"))
  }
  list(predictor = pred, task.id = task.nam,
    learner.id = lrn.id, sampled.rows = sampled.rows[[as.character(task.id)]],
    performance = perf)
})
if (PARALLEL) {
  parallelStop()
}

saveRDS(models_trained, save_dir)

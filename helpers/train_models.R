####################################
## Train Models  
####################################

#---Setup----
source("../helpers/libs_mlr.R")
set.seed(1234)

# Collect arguments
args = commandArgs(trailingOnly=TRUE)
SAVE_KERAS = as.logical(args[[2]])
task_ids = readRDS(args[[4]])
save_dir = args[[6]]
folder = args[[8]]
dir.create(path = "../saved_objects_rerun", showWarnings = FALSE)

PARALLEL = TRUE
cpus = 20L
current_dir = getwd()
data_dir = file.path(current_dir, folder)
dir.create(path = data_dir, showWarnings = FALSE)
names_models = c("randomforest", "xgboost", "svm", "logreg", "neuralnet")

# --- Task design ----
checkmate::assert_double(task_ids)
task_list = lapply(task_ids, function(x) {
  OpenML::getOMLTask(task.id = x)
})
names(task_list) = task_ids
task_list = lapply(task_list, function(task.oml) {
  t = OpenML::convertOMLTaskToMlr(task.oml)$mlr.task
  if (SAVE_KERAS) {
    dir.create(path = file.path(data_dir, t$task.desc$id), showWarnings = FALSE)
  }
  t
})

# ---Extract info from tasks ----
# Necessary to read into Python
sampled.rows = lapply(task_list, function(onetask) {
  task.id = onetask$task.desc$id
  sampled.rows = sample(seq_len(onetask$task.desc$size), size = 10, replace = FALSE)
  if (SAVE_KERAS) {
    dir_name = file.path(data_dir, task.id)
    # Save sampled rows
    write(sampled.rows, file = paste(dir_name, "/sampled_ids.txt", sep = ""), ncolumns = 1)
    # Save original data
    dat = getTaskData(onetask)
    dat[[getTaskTargetNames(onetask)]] = trans_target(dat[[getTaskTargetNames(onetask)]])
    write.csv(dat, file = paste(dir_name, "/data_orig.csv", sep = ""), row.names = FALSE)
    # Encode features with enc
    # Different handling of binary features (due to recourse!)
     map = mlrCPO::cpoScaleRange() %>>% mlrCPO::cpoDummyEncode()
    nw.onetask = mlrCPO::applyCPO(map, onetask)
    dat_encoded = getTaskData(nw.onetask)
    dat_encoded[[getTaskTargetNames(nw.onetask)]] = trans_target(dat_encoded[[getTaskTargetNames(nw.onetask)]])
    write.csv(dat_encoded, file = paste(dir_name, "/data_encoded.csv", sep = ""), row.names = FALSE)
    if (!task.id %in% c("cmc", "tic-tac-toe", "plasma_retinol", "kr-vs-kp")) {
        map = mlrCPO::cpoScaleRange() %>>% mlrCPO::cpoDummyEncode(reference.cat = TRUE)
        nw.onetask = mlrCPO::applyCPO(map, onetask)
        dat_encoded = getTaskData(nw.onetask)
        dat_encoded[[getTaskTargetNames(nw.onetask)]] = trans_target(dat_encoded[[getTaskTargetNames(nw.onetask)]])
        write.csv(dat_encoded, file = paste(dir_name, "/data_encoded_refcat.csv", sep = ""), row.names = FALSE)
    }
    # Save feature types
    col_info = sapply(dat[,getTaskFeatureNames(onetask)], class)
    col_info["target"] = getTaskTargetNames(onetask)
    feature.types = rjson::toJSON(col_info)
    write(feature.types, file = paste(dir_name, "/feature_types.json", sep = ""))
    # Save scale and center
    state = mlrCPO::getCPOTrainedState(retrafo(onetask %>>% cpoScale()))
    center = toJSON(state$control$center)
    scale = toJSON(state$control$scale)
    write(center, file = paste(dir_name, "/feature_center.json", sep = ""))
    write(scale, file = paste(dir_name, "/feature_scale.json", sep = ""))
    # Conditional
    ctr = ctree_control(maxdepth = 5L)
    con = fit_conditionals(getTaskData(onetask), ctrl = ctr)
    saveRDS(object = con, file = paste(dir_name, "/conditional.rds", sep = ""))
  }
  return(sampled.rows)
})

# --- Algorithm design ----
lrn.list = makeLearners(c("randomForest", "xgboost", "svm", "keraslogreg",  "keraslogreg"),
  type = "classif", predict.type = "prob")

lrn.list[[2]] = setHyperPars2(lrn.list[[2]], list(nthread = 1))
lrn.list[[4]] = setHyperPars2(lrn.list[[4]], list(layer_size = 0, lr = 0.001, epochs = 1000L))
lrn.list[[5]] = setHyperPars2(lrn.list[[4]], list(epochs = 1000L))

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

grid = grid[-which((grid$lrn.ind == "logreg") & (grid$task.id %in% c(3846, 145804, 3778, 3))), ]
# grid = grid[which(grid$lrn.ind %in% c("neuralnet", "logreg")), ]
print(nrow(grid))

if (PARALLEL) {
  parallelStartSocket(cpus, level = c("mlr.tuneParams"))
  parallelLibrary("keras", level = "mlr.tuneParams")
  parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
    "trans_target", "invoke", "get_keras_model", level = "mlr.tuneParams")
}
models_trained = lapply(seq_row(grid), function(i) {
  task.id = grid$task.id[i]
  task = task_list[[as.character(task.id)]]
  task.nam = task$task.desc$id
  n = getTaskSize(task)
  train.set = sample(n, size = n*0.8)
  test.set = seq(1, n)[-train.set]
  train.task = subsetTask(task, subset = train.set)
  test.task = subsetTask(task, subset = test.set)

  if (task.nam %in% c("tic-tac-toe", "diabetes")) {
      train.task= mlr::oversample(train.task, rate = 2L)
  } else if (task.nam %in% c("ilpd", "kc2")) {
      train.task= mlr::oversample(train.task, rate = 3L)
  } else if (task.nam %in% c("pc1")) {
      train.task= mlr::oversample(train.task, rate = 5L)
  }

  # Train the learner
  dir_name = file.path(data_dir, task$task.desc$id)
  lrn.id = grid$lrn.ind[i]
  print(as.character(lrn.id))
  # Different handling if solely binary features (due to recourse)
  if (lrn.id == "logreg") {
      lrn = cpoScaleRange() %>>% cpoDummyEncode(reference.cat = TRUE) %>>% lrn.list[[lrn.id]]
  } else if (lrn.id == "randomforest") {
      lrn = cpoScale() %>>% cpoDummyEncode() %>>% lrn.list[[lrn.id]]
  } else {
      lrn = cpoScaleRange() %>>% cpoDummyEncode() %>>% lrn.list[[lrn.id]]
  }
  par.set = hyper.pars[[grid$lrn.ind[i]]]
  ctrl = makeTuneControlRandom(maxit = 100L*length(par.set$pars))
  if (!is.null(par.set)) {
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
  if ((lrn.id == "logreg") & (SAVE_KERAS)) {
    keras.mod =  mod$learner.model$next.model$learner.model$model
    save_model_hdf5(keras.mod, filepath = paste(dir_name, "/logreg.h5", sep = ""))
  }
  if (lrn.id == "neuralnet" & SAVE_KERAS) {
    keras.mod =  mod$learner.model$next.model$learner.model$model
    save_model_hdf5(keras.mod, filepath = paste(dir_name, "/neuralnet.h5", sep = ""))
  }
  return(list(predictor = pred, task.id = task.nam,
    learner.id = lrn.id, sampled.rows = sampled.rows[[as.character(task.id)]],
      performance = perf))
  })
if (PARALLEL) {
  parallelStop()
}

#--- Save trained models as .rds ----
saveRDS(models_trained, save_dir)

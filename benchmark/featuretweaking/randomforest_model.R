####################################
## Train Models for iterated racing 
####################################

library(RWeka)
library(mlr)
library(ParamHelpers)
library(randomForest)
library(OpenML)
library(mlrCPO)
library(parallelMap)
library(mosmafs)
devtools::load_all("../../iml")
#---Setup---
#source("../../helpers/libs_mlr.R")

# Collect arguments
#args = commandArgs(trailingOnly=TRUE)
SAVE_KERAS = FALSE
task_ids = readRDS("../task_ids.rds")
save_dir = "randomforests.rds"
folder = NULL

# Setup
PARALLEL = TRUE
resolution = 20L
names_models = c("randomforest")

# --- Task design ----
checkmate::assert_double(task_ids)
task_list = lapply(task_ids, function(x) {
    OpenML::getOMLTask(task.id = x)
})
names(task_list) = task_ids
task_list = lapply(task_list, function(task.oml) {
    t = OpenML::convertOMLTaskToMlr(task.oml)$mlr.task
    if (task.oml$task.id %in% c(167141, 3872)) {
        t = t %>>% cpoSelector(c(FALSE, rep(TRUE, getTaskNFeats(t)-1)))
    }
    if (SAVE_KERAS) {
        dir.create(path = file.path(data_dir, t$task.desc$id), showWarnings = FALSE)
    }
    t
})


# --- Algorithm design ----
# lrn.list = makeLearners(c("ranger", "xgboost", "svm", "keraslogreg",  "keraslogreg"),
#     type = "classif", predict.type = "prob")
lrn.list = makeLearners(c("classif.randomForest"), predict.type = "prob")

names(lrn.list) = names_models

hyper.pars = list(
    randomforest.pars = pSS(
        ntree : numeric[0, log(1000)]  [[trafo = function(x) round(exp(x))]]
    ))
#--- Fit models ----
grid = expand.grid(task.id = task_ids,
    lrn.ind = seq_along(lrn.list))
ctrl = makeTuneControlGrid(resolution = resolution)

if (PARALLEL) {
    parallelStartMulticore(5)
    # parallelStartSocket(5, level = c("mlr.tuneParams"))
    # parallelLibrary("keras", level = "mlr.tuneParams")
    # parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
    #     "trans_target", "invoke", "get_keras_model", level = "mlr.tuneParams")
}
models_trained = lapply(seq_row(grid), function(i) {
    task.id = grid$task.id[i]
    task = task_list[[as.character(task.id)]]
    lrn.id = names_models[grid$lrn.ind[i]]
    print(grid$lrn.ind[i])
    print(lrn.id)
    lrn = cpoScale() %>>% cpoDummyEncode() %>>% lrn.list[[lrn.id]]
    par.set = hyper.pars[[grid$lrn.ind[i]]]
    if (!is.null(par.set)) {
        res = tuneParams(lrn, task, cv3, par.set = par.set, control = ctrl,
            show.info = FALSE)
        lrn = setHyperPars2(lrn, res$x) # evtl exp
    }
    mod = train(lrn, task)
    pred = Predictor$new(mod, data = getTaskData(task),
        class = getTaskDesc(task)$positive)
    return(list(predictor = pred, task.id = task$task.desc$id,
        learner.id = lrn.id))
})
if (PARALLEL) {
    parallelStop()
}

saveRDS(models_trained, save_dir)

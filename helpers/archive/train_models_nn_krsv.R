####################################
## Train Models for iterated racing 
####################################

#---Setup---
source("../helpers/libs_mlr.R")

# Collect arguments
args = commandArgs(trailingOnly=TRUE)
SAVE_KERAS = FALSE
task_ids = readRDS("task_ids.rds")
task_ids = c(9971, 3913, 3918, 3718, 3749, 145976)
save_dir = NULL
folder = "data"

# Setup
PARALLEL = TRUE
resolution = 20L
current_dir = getwd()
data_dir = file.path(current_dir, "..", "saved_objects", folder)
dir.create(path = data_dir, showWarnings = FALSE)
names_models = c("neuralnet")
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

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
    # if (SAVE_KERAS) {
    #     dir.create(path = file.path(data_dir, t$task.desc$id), showWarnings = FALSE)
    # }
    t
})

sampled.rows = lapply(task_list, function(onetask) {
    task.id = onetask$task.desc$id
    sampled.rows = sample(seq_len(onetask$task.desc$size), size = 10, replace = FALSE)
    if (SAVE_KERAS) {
        dir_name = file.path(data_dir, task.id)
        # Save sampled rows
        # write(sampled.rows, file = paste(dir_name, "/sampled_ids.txt", sep = ""), ncolumns = 1)
        # Save original data
        dat = getTaskData(onetask)
        # dat[[getTaskTargetNames(onetask)]] = trans_target(dat[[getTaskTargetNames(onetask)]])
        # write.csv(dat, file = paste(dir_name, "/data_orig.csv", sep = ""), row.names = FALSE)
        # Encode features with enc
        if (task.id %in% c("cmc", "tic-tac-toe", "plasma_retinol", "kr-vs-kp")) {
            map = mlrCPO::cpoScaleRange() %>>% mlrCPO::cpoDummyEncode()
        } else {
            # map = mlrCPO::cpoScaleRange() %>>% mlrCPO::cpoDummyEncode(reference.cat = TRUE)
            map = mlrCPO::cpoScaleRange() %>>% mlrCPO::cpoDummyEncode()
        }
        nw.onetask = mlrCPO::applyCPO(map, onetask)
        dat_encoded = getTaskData(nw.onetask)
        dat_encoded[[getTaskTargetNames(nw.onetask)]] = trans_target(dat_encoded[[getTaskTargetNames(nw.onetask)]])
        write.csv(dat_encoded, file = paste(dir_name, "/data_encoded_neuralnet.csv", sep = ""), row.names = FALSE)
        
        # Save feature types
        # col_info = sapply(dat[,getTaskFeatureNames(onetask)], class)
        # col_info["target"] = getTaskTargetNames(onetask)
        # feature.types = rjson::toJSON(col_info)
        # write(feature.types, file = paste(dir_name, "/feature_types.json", sep = ""))
        # # Save scale and center
        # state = mlrCPO::getCPOTrainedState(retrafo(onetask %>>% cpoScale()))
        # center = toJSON(state$control$center)
        # scale = toJSON(state$control$scale)
        # write(center, file = paste(dir_name, "/feature_center.json", sep = ""))
        # write(scale, file = paste(dir_name, "/feature_scale.json", sep = ""))
        # # Conditional 
        # lrn = makeLearner("classif.logreg", predict.type = "prob")
        # mod = train(lrn, onetask)
        # pred = Predictor$new(mod, data = getTaskData(onetask), y = getTaskTargetNames(onetask), conditional = TRUE)
        # con = pred$conditional
        # saveRDS(object = con, file = paste(dir_name, "/conditional.rds", sep = ""))
    }
    return(sampled.rows)
})

# --- Algorithm design ----
lrn.list = makeLearners(c("keraslogreg"),
    type = "classif", predict.type = "prob")

#lrn.list[[1]] = setHyperPars2(lrn.list[[1]], list(layer_size = 0, lr = 0.001, epochs = 2000L))
lrn.list[[1]] = setHyperPars2(lrn.list[[1]], list(epochs = 2000L))
#lrn.list[[3]] = setHyperPars2(lrn.list[[3]], list(tolerance = 0.005))

names(lrn.list) = names_models

hyper.pars = list(
    kerasneuralnet.pars = pSS(
        lr: numeric[0.0001, 0.1],
        layer_size: integer[2, 6]
    )
)

#--- Fit models ----
grid = expand.grid(task.id = task_ids,
    lrn.ind = seq_along(lrn.list))
ctrl = makeTuneControlGrid(resolution = resolution)

if (PARALLEL) {
    parallelStartSocket(resolution, level = c("mlr.tuneParams"))
    parallelLibrary("keras", level = "mlr.tuneParams")
    parallelExport("trainLearner.classif.keraslogreg", "predictLearner.classif.keraslogreg",
        "trans_target", "invoke", "get_keras_model", level = "mlr.tuneParams")
}
models_trained = lapply(seq_row(grid), function(i) {
    task.id = grid$task.id[i]
    task = task_list[[as.character(task.id)]]
    print(task$task.desc$id)
    dir_name = file.path(data_dir, task$task.desc$id)
    lrn.id = names_models[grid$lrn.ind[i]]
    print(lrn.id)
    if (task$task.desc$id %in% c("cmc", "tic-tac-toe", "plasma_retinol")) {
        lrn = cpoScaleRange() %>>% cpoDummyEncode() %>>% lrn.list[[lrn.id]]
    } else {
        # lrn = cpoScaleRange() %>>% cpoDummyEncode(reference.cat = TRUE) %>>% lrn.list[[lrn.id]]
        lrn = cpoScaleRange() %>>% cpoDummyEncode() %>>% lrn.list[[lrn.id]]
    }
    #task = mlr::oversample(task, rate = 2L)
    par.set = hyper.pars[[grid$lrn.ind[i]]]
    if (!is.null(par.set)) {
        res = tuneParams(lrn, task, cv3, par.set = par.set, control = ctrl,
            show.info = FALSE)
        lrn = setHyperPars2(lrn, res$x) # evtl exp
    }
    mod = mlr::train(lrn, task)
    pred = Predictor$new(mod, data = getTaskData(task),
        class = getTaskDesc(task)$positive)
    if ((lrn.id == "logreg") & (SAVE_KERAS)) {
        keras.mod =  mod$learner.model$next.model$learner.model$model
        save_model_hdf5(keras.mod, filepath = paste(dir_name, "/logreg.h5", sep = ""))
    }
    if (lrn.id == "neuralnet" & SAVE_KERAS) {
        keras.mod =  mod$learner.model$next.model$learner.model$model
        save_model_hdf5(keras.mod, filepath = paste(dir_name, "/neuralnet.h5", sep = ""))
    }
    return(list(predictor = pred, task.id = task$task.desc$id,
        learner.id = lrn.id, sampled.rows = sampled.rows[[as.character(task.id)]]))
})
if (PARALLEL) {
    parallelStop()
}

r = lapply(models_trained, function(m) {
    browser()
    range(m$predictor$predict(m$predictor$data$get.x()))
})

# saveRDS(models_trained, "../saved_objects/models_benchmark_nn.rds")
# 
# 
# instances = readRDS("../saved_objects/models_benchmark.rds")
# 
# newinstances = lapply(instances, function(inst){
#     if (inst$learner.id %in% c("neuralnet", "logreg")) {
#         newinst = subset_instances(models_trained, task = inst$task.id, learner = inst$learner.id)
#         inst$predictor = newinst[[1]]$predictor
#     }
#     return(inst)
# })
# 
# saveRDS(newinstances, "../saved_objects/models_benchmark.rds")

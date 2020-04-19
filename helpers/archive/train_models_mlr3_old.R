####################################
## Train Models  
####################################

n_evals = 20L

#--- Install packages if not already installed ----
packages = c("rstudioapi", "ranger", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3keras", "OpenML", 
  "mlr3misc", "mlr3tuning", "e1071", "stringr", "gbm",
  "paradox", "checkmate", "keras", "rjson", "parallelMap")
sapply(packages, require, character.only = TRUE)

#--- Set path ---- 
current_dir = getwd()
data_dir = file.path(current_dir, "..", "saved_objects", "data")
dir.create(path = data_dir, showWarnings = FALSE)

#--- Task design ----
task_list = lapply(task_ids, function(x) {
  t = getOMLTask(task.id = x)
  df = t$input$data.set$data
  target = t$input$target.features
  positive = as.character(unique(t$input$data.set$data[, target][1]))
  
  task = mlr3::TaskClassif$new(id = as.character(t$task.id), backend = df, 
    target = target, positive = positive)
})

names(task_list) = task_ids

#--- Hyperpars----
hyper.pars = list(
  ranger= paradox::ParamSet$new(list(
    paradox::ParamDbl$new("num.trees", lower = 1, upper = log(1000)))),
  svm= paradox::ParamSet$new(list(
    paradox::ParamDbl$new("classif.svm.cost", lower = 0.01, upper = 1)))
)
hyper.pars$ranger$trafo = function(x, param_set) {
  x$num.trees = round(exp(x$num.trees))
  return(x) 
}


# For Keras tune over different architectures and learning rates  
if (TUNE_KERAS) {
  get_keras_model = function(arch = "arch1", lr = 3*10^-4) {
    if (arch == "arch1") {
      ffnet= keras_model_sequential()
      ffnet%>%
        layer_dense(units = 2^4, activation = "relu") %>%
        layer_dense(units = 2^4, activation = "relu") %>%
        layer_dense(units = 1L, activation = "sigmoid")
      
    } else if (arch == "arch2") {
      ffnet= keras_model_sequential()
      ffnet%>%
        layer_dense(units = 2^5, activation = "relu") %>%
        layer_dense(units = 2^5, activation = "relu") %>%
        layer_dense(units = 1L, activation = "sigmoid")
      
    } else if (arch == "arch3") {
      ffnet= keras_model_sequential()
      ffnet%>%
        layer_dense(units = 2^6, activation = "relu") %>%
        layer_dense(units = 2^6, activation = "relu") %>%
        layer_dense(units = 1L, activation = "sigmoid")
    } else if (arch == "arch4") {
      ffnet= keras_model_sequential()
      ffnet%>%
        layer_dense(units = 2^7, activation = "relu") %>%
        layer_dense(units = 2^7, activation = "relu") %>%
        layer_dense(units = 1L, activation = "sigmoid")
    }
    ffnet %>%
      compile(optimizer = optimizer_adam(lr),
        loss = "binary_crossentropy",
        metrics = "accuracy")
  }
  hyper.pars$keras = paradox::ParamSet$new(list(
    paradox:: ParamFct$new("classif.keras.model", levels = c("arch1", "arch2", "arch3", "arch4"), 
      tags = "train"), 
    paradox::ParamDbl$new("classif.keras.lr", lower = 10^-5, upper = 10^-1, tags = "train")
  ))
  
  hyper.pars$keras$trafo = function(x, param_set) {
    x$classif.keras.model = get_keras_model(x$classif.keras.model, x$classif.keras.lr)
    x$classif.keras.lr = NULL
    return(x)
  }
}


#--- Train Models ----
if (PARALLEL) {
  parallelStartMulticore(cpus = 20, load.balancing = TRUE) 
}
tasks_trained = parallelMap(function(task) {
  
  # Feedforward NN
  ffnet = keras_model_sequential()
  ffnet %>%
    layer_dense(units = 10, activation = "relu") %>%
    layer_dense(units = 10, activation = "relu") %>%
    layer_dense(units = 1L, activation = "sigmoid")
  compile(ffnet, optimizer = "adam", loss = "binary_crossentropy", metrics = "accuracy")
  
  
  # Define learner list
  lrn.list = list(lrn("classif.ranger", predict_type = "prob"), 
    lrn("classif.svm", predict_type = "prob", type = "C-classification"), 
    lrn("classif.kerasff", predict_type = "prob", layer_units = integer(),
      epochs = 100L, activation = "sigmoid", use_batchnorm = FALSE,
      optimizer = "adam", loss = "binary_crossentropy", metrics = "accuracy", 
      callbacks = list(cb_es(5))),
    lrn("classif.keras", model = ffnet, epochs = 100L, predict_type = "prob", 
      callbacks = list(cb_es(5))))
  lrn.list[[3]]$man = "mlr3keras::mlr_learners_classif.kerasff"
  
  # Loop over learners
  trained_lrn = lapply(lrn.list, function(lrn) {
    lrn.nam = str_extract(string = lrn$man, pattern = "[[:lower:]]*$")
    message(lrn.nam)
    par.set = hyper.pars[[lrn.nam]]
    
    if (lrn.nam %in% c("svm", "keras", "kerasff", "xgboost")) {
      enc = po("encode")
      sca = po("scale")
      mod = sca %>>% 
        enc %>>%
        lrn
      mod$keep_results = TRUE
      mod = GraphLearner$new(mod)
    } else {
      mod = lrn
    }
    
    if (!is.null(par.set) & TUNING) {
      # Tuning Paras
      resampling = rsmp("cv", folds = 3)
      measure = msr("classif.ce")
      tuner = tnr("grid_search", resolution = n_evals)
      terminator = term("evals", n_evals = n_evals)
      
      instance = TuningInstance$new(
        task = task, 
        learner = mod, 
        resampling = resampling, 
        measures = measure, 
        param_set = par.set, 
        terminator = terminator
      )
      res = tuner$tune(instance)
      mod$param_set$values = instance$result$params
    }
    mod$train(task)
    return(lrn.nam = mod)
  })
  
  sampled.rows = sample(seq_len(task$nrow), size = 10, replace = FALSE)
  
  if (SAVE_KERAS) {
    
    # Save needed info to transfer to python to a single folder
    dir_name = file.path(data_dir, task$id)
    dir.create(path = dir_name, showWarnings = FALSE)
    # Save sampled rows
    write(sampled.rows, file = paste(dir_name, "/sampled_ids.txt", sep = ""), ncolumns = 1)
    # Save original data
    dat = task$data()
    dat[[task$target_names]] = as.numeric(dat[[task$target_names]])
    write.csv(dat, file = paste(dir_name, "/data_orig.csv", sep = ""), row.names = FALSE)
    # Encode features with enc
    enc = po("encode")
    enc_trained = enc$train(list(task))
    dat_encoded = enc_trained[[1]]$data()
    dat_encoded[[task$target_names]] = as.numeric(dat_encoded[[task$target_names]])
    write.csv(dat_encoded, file = paste(dir_name, "/data_encoded.csv", sep = ""), row.names = FALSE)
    # Save feature types
    col_info = task$col_info[-1,]
    col_info$target = task$target_names
    feature.types = toJSON(col_info)
    write(feature.types, file = paste(dir_name, "/feature_types.json", sep = ""))
    # Save trained models
    keras1 = trained_lrn[[4]]$model$classif.keras$model$model 
    keras2 = trained_lrn[[5]]$model$classif.keras$model$model 
    save_model_hdf5(keras1, filepath = paste(dir_name, "/logreg.h5", sep = ""))
    save_model_hdf5(keras2, filepath = paste(dir_name, "/neuralnet.h5", sep = ""))
    # Save mu and sd 
    center = trained_lrn[[4]]$model$scale$center 
    scale = trained_lrn[[4]]$model$scale$scale
    write(center, file = paste(dir_name, "/feature_center.json", sep = ""))
    write(scale, file = paste(dir_name, "/feature_scale.json", sep = ""))
  }
  # Return trained lrn, sampled rows
  return(list(task = task, models = trained_lrn, row.ids = sampled.rows))
}, task_list)

if (PARALLEL) {
  parallelStop()
}








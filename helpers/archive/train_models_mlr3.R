####################################
## Train Models  
####################################
n_evals = 20L

#--- Install packages if not already installed ----
packages = c("rstudioapi", "ranger", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3keras", "OpenML", 
  "mlr3misc", "mlr3tuning", "e1071", "stringr", "future.apply",
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

#--- Utility functions ----
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


#--- Train Models ----
# Loop over learners

resampling = rsmp("cv", folds = 3)
measure = msr("classif.ce")
tuner = tnr("grid_search", resolution = n_evals)
terminator = term("evals", n_evals = n_evals)

learners = list(
  GraphLearner$new(
    { 
      mod = po("scale") %>>%
        po("encode") %>>%
        po(lrn("classif.ranger", predict_type = "prob"))
      mod$keep_results = TRUE
      tune_ps =  paradox::ParamSet$new(list(
        paradox::ParamDbl$new("classif.ranger.num.trees", lower = 1, upper = log(1000))))
      tune_ps$trafo = function(x, param_set) {
        x$classif.ranger.num.trees = round(exp(x$classif.ranger.num.trees))
        return(x) 
      }
      mod = GraphLearner$new(mod)
      AutoTuner$new(mod, resampling, measure, tune_ps, terminator, tuner)
    }),
  GraphLearner$new(
    {
      mod = po("scale") %>>%
        po("encode") %>>%
        po(lrn("classif.xgboost", predict_type = "prob", nthread = 1L))
      mod$keep_results = TRUE
      tune_ps = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("classif.xgboost.nrounds", lower = 1,
          upper = log(1000))))
      tune_ps$trafo = function(x, param_set) {
        x$classif.xgboost.nrounds = round(exp(x$classif.xgboost.nrounds))
        return(x)
      }
      mod = GraphLearner$new(mod)
      AutoTuner$new(mod, resampling, measure, tune_ps, terminator, tuner)
    }
  ),
  GraphLearner$new(
    {
      mod = po("scale") %>>%
        po("encode") %>>%
        po(lrn("classif.svm", predict_type = "prob", type = "C-classification"))
      mod$keep_results = TRUE
      tune_ps = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("classif.svm.cost", lower = 0.01, upper = 1)))
      mod = GraphLearner$new(mod)
      AutoTuner$new(mod, resampling, measure, tune_ps, terminator, tuner)
    }
  ),
  GraphLearner$new(
    {
      mod = po("scale") %>>%
        po("encode") %>>%
        po(lrn("classif.kerasff", predict_type = "prob", layer_units = integer(),
          epochs = 100L, activation = "sigmoid", use_batchnorm = FALSE,
          optimizer = "adam", loss = "binary_crossentropy", metrics = "accuracy",
          callbacks = list(cb_es(5))))
      mod$keep_results = TRUE
      mod
    }
  ),
  GraphLearner$new(
    {
      mod = po("scale") %>>%
        po("encode") %>>%
        po(lrn("classif.keras", model = NULL, epochs = 10L, predict_type = "prob",
          callbacks = list(cb_es(5))))
      mod$keep_results = TRUE
      tune_ps = paradox::ParamSet$new(list(
        paradox:: ParamFct$new("classif.keras.model", levels = c("arch1", "arch2", "arch3", "arch4"),
          tags = "train"),
        paradox::ParamDbl$new("classif.keras.lr", lower = 10^-5, upper = 10^-1, tags = "train")
      ))
      
      tune_ps$trafo = function(x, param_set) {
        x$classif.keras.model = get_keras_model(x$classif.keras.model, x$classif.keras.lr)
        x$classif.keras.lr = NULL
        return(x)
      }
      mod = GraphLearner$new(mod)
      AutoTuner$new(mod, resampling, measure, tune_ps, terminator, tuner)
    }
  )
)

plan(list(sequential, sequential, multicore))

tasks_trained = future_lapply(X = task_list, function(onetask) {
  learners = sapply(learners, function(x) x$clone())
  design = benchmark_grid(
    tasks = list(onetask),
    learners = learners,
    resamplings = rsmp("holdout", ratio = .99)
  )
  br = benchmark(design, store_model = TRUE)
  
  trained_lrn = br$learners$learner
  names(trained_lrn) = names_models
  
  sampled.rows = sample(seq_len(onetask$nrow), size = 10, replace = FALSE)
  
  if (SAVE_KERAS) {
    # Save needed info to transfer to python to a single folder
    dir_name = file.path(data_dir, onetask$id)
    dir.create(path = dir_name, showWarnings = FALSE)
    # Save sampled rows
    write(sampled.rows, file = paste(dir_name, "/sampled_ids.txt", sep = ""), ncolumns = 1)
    # Save original data
    dat = onetask$data()
    dat[[onetask$target_names]] = as.numeric(dat[[onetask$target_names]])
    write.csv(dat, file = paste(dir_name, "/data_orig.csv", sep = ""), row.names = FALSE)
    # Encode features with enc
    enc = po("encode")
    enc_trained = enc$train(list(onetask))
    dat_encoded = enc_trained[[1]]$data()
    dat_encoded[[onetask$target_names]] = as.numeric(dat_encoded[[onetask$target_names]])
    write.csv(dat_encoded, file = paste(dir_name, "/data_encoded.csv", sep = ""), row.names = FALSE)
    # Save feature types
    col_info = onetask$col_info[-1,]
    col_info$target = onetask$target_names
    feature.types = toJSON(col_info)
    write(feature.types, file = paste(dir_name, "/feature_types.json", sep = ""))
    # Save trained models
    keras1 = trained_lrn$logreg$model$classif.keras$model$model
    keras2 = trained_lrn$neuralnet$model$scale.encode.classif.keras.tuned$model$learner$model$classif.keras$model$model
    save_model_hdf5(keras1, filepath = paste(dir_name, "/logreg.h5", sep = ""))
    save_model_hdf5(keras2, filepath = paste(dir_name, "/neuralnet.h5", sep = ""))
    # Save mu and sd 
    center = trained_lrn$kerasff$model$scale$center 
    scale = trained_lrn$kerasff$model$scale$scale
    write(center, file = paste(dir_name, "/feature_center.json", sep = ""))
    write(scale, file = paste(dir_name, "/feature_scale.json", sep = ""))
  }
  # Return trained lrn, sampled rows
  return(list(task = onetask, models = trained_lrn, row.ids = sampled.rows))
  return(br)
})








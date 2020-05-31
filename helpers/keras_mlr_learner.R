#####################################
## KERAS mlr learner
#####################################
#--- Definition of learner ----
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2) #switch off messages
library("keras")

get_keras_model = function(layer_size = 0, lr = 3*10^-4, input_shape) {
  ffnet = keras_model_sequential()
  if (layer_size == 0) {
    layer_dense(ffnet, units = 1L, input_shape = input_shape,
        activation = "sigmoid")
  } else {
    layer_dense(ffnet, units = 2^layer_size, input_shape = input_shape,
      activation = "relu")
    #layer_dense(ffnet, units = 2^layer_size, activation = "relu") %>%
    layer_dense(ffnet, units = 1L, activation = "sigmoid")
  }
  compile(ffnet, optimizer = optimizer_adam(lr),
      loss = "binary_crossentropy",
      metrics = "accuracy")
}

makeRLearner.classif.keraslogreg = function() {
  makeRLearnerClassif(
    cl = "classif.keraslogreg",
    package = "keras",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "layer_size", lower = 0, upper = 6, default = 0),
      makeNumericLearnerParam(id = "lr", lower = 10^-5, upper = 1, default = 0.001),
      makeIntegerLearnerParam(id = "epochs", default = 30L, lower = 1L),
      makeIntegerLearnerParam(id = "batch_size", default = 32L, lower = 1L)),
    par.vals = list(epochs = 30L, lr = 0.001, layer_size = 0, batch_size = 32L),
    properties = c("twoclass", "numerics", "factors", "prob", "ordered"),
    name = "Keras Logistic Regression",
    short.name = "keraslogreg",
    note = "Learner param 'predict.method' maps to 'method' in predict.keraslogreg."
  )
}

trans_target = function(target) {
  keras::to_categorical(as.integer(target) - 1)[, 1, drop = FALSE]
}

trainLearner.classif.keraslogreg = function(.learner, .task, .subset, .weights = NULL, ...) {
  dt = getTaskData(.task)
  x = as.matrix(dt[, getTaskFeatureNames(.task)])
  target = dt[, getTaskTargetNames(.task)]
  y = trans_target(target)
  input_shape = sum(.task$task.desc$n.feat)
  target_labels = getTaskClassLevels(.task)

  model = get_keras_model(layer_size = .learner$par.vals$layer_size,
    lr = .learner$par.vals$lr, input_shape = input_shape)
  es = callback_early_stopping(monitor='val_loss', patience=5L)
  history = invoke(keras::fit,
    object = model,
    x = x,
    y = y,
    epochs = .learner$par.vals$epochs,
    batch_size = .learner$par.vals$batch_size,
    validation_split =0.2,
    callbacks = list(es), verbose = 0L)
  list(model = model, history = history, target_labels = target_labels)
}



predictLearner.classif.keraslogreg = function(.learner, .model, .newdata) {
  if (.learner$predict.type == "response") {
    p = .model$learner.model$model %>% predict_classes(x = as.matrix(.newdata))
    p = factor(ifelse(p == 1, .model$learner.model$target_labels[1],
      .model$learner.model$target_labels[2]), levels = .model$learner.model$target_labels)
  }
  if (.learner$predict.type == "prob") {
    p = .model$learner.model$model %>% predict_proba(x = as.matrix(.newdata))
    if (ncol(p) == 1L) p = cbind(p, 1-p)
    colnames(p) = .model$learner.model$target_labels
  }
  return(p)
}

#--- Load keras model from h5 format and initalize it ----
# weights saved as h5 should be in folder [data.dir]

# Only load model and initialize predictor:
load_keras_model = function(instance, data.dir) {
  instance$predictor$model$learner.model$next.model$learner.model$model =
    load_model_hdf5(file.path(data.dir, instance$task.id, paste(instance$learner.id, ".h5", sep = "")))
  newpred = Predictor$new(model = instance$predictor$model, data = data.frame(instance$predictor$data$get.xy()),
    y = instance$predictor$data$y.names, class = instance$predictor$class)
  instance$predictor = newpred
  return(instance)
}

# Load model, initialize predictor and sample new x.interest
initialize_instance = function(inst, data.dir) {
  if (inst$learner.id %in% c("logreg", "neuralnet")) {
    inst = load_keras_model(inst, data.dir)
  }
  sample.id = sample.int(nrow(inst$predictor$data$get.x()), 1)
  x.interest = as.data.frame(inst$predictor$data$get.x()[sample.id, ])
  inst$predictor$predict(newdata = x.interest)
  target = ifelse(inst$predictor$predict(newdata = x.interest) < 0.5, 1, 0)

  list(predictor = inst$predictor, task.id = inst$task.id,
    learner.id = inst$learner.id,x.interest = x.interest,
    point.id = sample.id,
    target = target)
}

# turn instances with `sampled.rows` vector into more instances, each with a single `sampled.rows` value.
flatten_instances <- function(instances) {
  unlist(lapply(instances, function(inst) {
    lapply(inst$sampled.rows, function(sr) {
      inst$sampled.rows <- sr  # copy semantics means this does *not* change the 'inst' var of the outer fun
      inst
    })
  }), recursive = FALSE)
}

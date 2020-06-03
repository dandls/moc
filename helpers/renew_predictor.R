renew_predictor = function(inst, data.dir) {
  if (inst$learner.id %in% c("logreg", "neuralnet")) {
    inst$predictor$model$learner.model$next.model$learner.model$model = 
      load_model_hdf5(file.path(data.dir, inst$task.id, paste(inst$learner.id, ".h5", sep = "")))
  }
    newpred = Predictor$new(model = inst$predictor$model, data = data.frame(inst$predictor$data$get.xy()),
      y = inst$predictor$data$y.names, class = inst$predictor$class)
    inst$predictor = newpred
    return(inst)
}

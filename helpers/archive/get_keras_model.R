## Tuning over model architectures
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

# hyper.pars$keras = paradox::ParamSet$new(list(
#   paradox::ParamFct$new("classif.keras.model", 
#     levels = c("a", "b", "c", "d", "e", "f")
#   )))
# hyper.pars$keras$trafo = function(x, param_set) {
#   if (x == "a") {
#     ffnet= keras_model_sequential()
#     ffnet%>%
#       layer_dense(units = 2^4, activation = "relu") %>%
#       layer_dense(units = 2^4, activation = "relu") %>%
#       layer_dense(units = 1L, activation = "sigmoid")
#     compile(ffnet, optimizer = "adam", loss = "binary_crossentropy", metrics = "accuracy")
#     x$classif.keras.model = ffnet
#   }
#   if (x == "b") {
#     ffnet = keras_model_sequential()
#     ffnet%>%
#       layer_dense(units = 2^4, activation = "relu") %>%
#       layer_dense(units = 2^4, activation = "relu") %>%
#       layer_dense(units = 1L, activation = "sigmoid")
#     compile(ffnet, optimizer = "sgd", loss = "binary_crossentropy", metrics = "accuracy")
#     x$classif.keras.model = ffnet
#   }
#   if (x == "c") {
#     ffnet = keras_model_sequential()
#     ffnet%>%
#       layer_dense(units = 2^5, activation = "relu") %>%
#       layer_dense(units = 2^5, activation = "relu") %>%
#       layer_dense(units = 1L, activation = "sigmoid")
#     compile(ffnet, optimizer = "adam", loss = "binary_crossentropy", metrics = "accuracy")
#     x$classif.keras.model = ffnet
#   }
#   if (x == "d") {
#     ffnet = keras_model_sequential()
#     ffnet%>%
#       layer_dense(units = 2^5, activation = "relu") %>%
#       layer_dense(units = 2^5, activation = "relu") %>%
#       layer_dense(units = 1L, activation = "sigmoid")
#     compile(ffnet, optimizer = "sgd", loss = "binary_crossentropy", metrics = "accuracy")
#     x$classif.keras.model = ffnet
#   }
#   if (x == "e") {
#     ffnet = keras_model_sequential()
#     ffnet%>%
#       layer_dense(units = 2^7, activation = "relu") %>%
#       layer_dense(units = 2^7, activation = "relu") %>%
#       layer_dense(units = 1L, activation = "sigmoid")
#     compile(ffnet, optimizer = "adam", loss = "binary_crossentropy", metrics = "accuracy")
#     x$classif.keras.model = ffnet
#   }
#   if (x == "f") {
#     ffnet = keras_model_sequential()
#     ffnet%>%
#       layer_dense(units = 2^7, activation = "relu") %>%
#       layer_dense(units = 2^7, activation = "relu") %>%
#       layer_dense(units = 1L, activation = "sigmoid")
#     compile(ffnet, optimizer = "sgd", loss = "binary_crossentropy", metrics = "accuracy")
#     x$classif.keras.model = ffnet
#   }
#   return(x)
# }
```
```{r}
library("mlr3tuning")
learner = lrn("regr.keras", callbacks = list(cb_es(3)))
task = mlr_tasks$get("mtcars")
resampling = rsmp("holdout")
measure = msr("regr.mse")
tuner = tnr("grid_search", resolution = 2)
terminator = term("evals", n_evals = 2)
instance = TuningInstance$new(
  task = task,
  learner = learner,
  resampling = resampling,
  measures = measure,
  param_set = ps,
  terminator = terminator
)
tuner$tune(instance)
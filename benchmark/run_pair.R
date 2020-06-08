##########################################
### RUN Pair
#########################################
#--- Setup ----
source("../helpers/libs_mlr.R")
args = commandArgs(trailingOnly=TRUE)
instances = readRDS(args[[1]])
save.dir = args[[2]]
library(parallel)
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)

# --- Compute counterfactuals -----
cfexps = lapply(instances, function(inst) {
  flatten.inst = flatten_instances(list(inst))
  message(paste(inst$task.id, inst$learner.id, sep = "/"))
  path = file.path(save.dir, inst$task.id)
  
  # Get data 
  dt = data.frame(inst$predictor$data$get.x())
  
  # Targets 
  if (inst$learner.id %in% c("logreg", "neuralnet")) {
    inst = load_keras_model(instance = inst, save.dir)
  }
  pred.dt = inst$predictor$predict(dt)[,1]
  dt_0 = dt[(pred.dt >= 0) & (pred.dt <= 0.5),]
  dt_1 = dt[(pred.dt > 0.5) & (pred.dt <= 1),]
  
  cfexps = mapply(FUN = function(oneinst) {
    
    # Get xinterest/xorig as list
    oneinst = initialize_instance(oneinst, save.dir)
    target = oneinst$target
    x.interest = oneinst$x.interest
    
    # Get ranges of numeric features for gower distance
    param.set = ParamHelpers::makeParamSet(
      params = counterfactuals:::make_paramlist(rbind(dt, 
        oneinst$x.interest[oneinst$predictor$data$feature.names])))
    
    ranges = ParamHelpers::getUpper(param.set) - 
      ParamHelpers::getLower(param.set)
    ranges[ParamHelpers::getParamIds(param.set)
      [ParamHelpers::getParamTypes(param.set) == "discrete"]]  = NA
    ranges = ranges[oneinst$predictor$data$feature.names]
    
    
    
    # Get nearest data point with desired target
    if (1 %in% target) {
      pool = dt_1
    } else {
      pool = dt_0
    }
    g.dist = StatMatch::gower.dist(x.interest, pool, rngs = ranges)
    cf = pool[which.min(g.dist), ]
    cf$row_ids = as.numeric(row.names(oneinst$x.interest))+1L
    oneinst$learner.id = oneinst$learner.id
    return(cf)
  }, flatten.inst, SIMPLIFY = FALSE)
  
  res.cf = as.data.frame(do.call("rbind", cfexps)) 
  if (nrow(res.cf) < 10) {
    print(paste("max:", max(pred.dt), "min :", min(pred.dt)))
  }
  # Save results
  name.file = paste("cf", "whatif", inst$learner.id, sep = "-")
  pathtofile = file.path(path, paste(name.file, ".csv", sep = ""))
  write.csv(res.cf, pathtofile, row.names = FALSE)
  return(res.cf)
})

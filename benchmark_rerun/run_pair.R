##########################################
### RUN Pair
#########################################
#--- Setup ----
source("../helpers/libs_mlr.R")
args = commandArgs(trailingOnly=TRUE)
instances = readRDS(args[[2]])
save.dir = args[[4]]
library(parallel)
Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2)


# --- Compute counterfactuals -----
cfexps = lapply(instances, function(inst) {
    message(paste(inst$task.id, inst$learner.id, sep = "/"))
    
    # Get data 
    dt = inst$predictor$data$get.x()
    
    # Get xinterest/xorig as list
    path = file.path("../saved_objects", save.dir, inst$task.id)
    sampled.rows = read.delim(file.path(path, "sampled_ids.txt"), header = FALSE)[,1]
    dt.x.interests = dt[sampled.rows,]
    list.x.interests = split(as.data.frame(dt.x.interests), seq(length(sampled.rows)))
    
    # Load keras model
    if (inst$learner.id %in% c("logreg", "neuralnet")) {
        inst = load_keras_model(inst, save.dir)
    }
    
    # Get ranges of numeric features for gower distance
    param.set = ParamHelpers::makeParamSet(
        params = make_paramlist(inst$predictor$data$get.x()))
    
    ranges = ParamHelpers::getUpper(param.set) - 
        ParamHelpers::getLower(param.set)
    ranges[ParamHelpers::getParamIds(param.set)
        [ParamHelpers::getParamTypes(param.set) == "discrete"]]  = NA
    ranges = ranges[inst$predictor$data$feature.names]
    
    # Targets 
    pred.dt = inst$predictor$predict(dt)[,1]
    targets = ifelse(pred.dt[sampled.rows] < 0.5, 1, 0)
    dt_0 = dt[(pred.dt >= 0) & (pred.dt <= 0.5),]
    dt_1 = dt[(pred.dt >= 0.5) & (pred.dt <= 1),]
    
    # Get nearest data point with desired target
    cfexps = mapply(FUN = function(x.interest, target, row.id) {
        if (target == 1) {
            target = c(0.5, 1)
            pool = dt_1
        } else {
            target = c(0, 0.5)
            pool = dt_0
        }
        g.dist = StatMatch::gower.dist(x.interest, pool, rngs = ranges)
        cf = pool[which.min(g.dist), ]
        cf$row_ids = row.id
        return(cf)
    }, list.x.interests, targets, sampled.rows, SIMPLIFY = FALSE)
    res.cf = as.data.frame(do.call("rbind", cfexps)) 
    if (nrow(res.cf) < 10) {
        print(paste("max:", max(pred.dt), "min :", min(pred.dt)))
    }
    
    # Save results
    name.file = paste("cf", "pair", inst$learner.id, sep = "-")
    pathtofile = file.path(path, paste(name.file, ".csv", sep = ""))
    write.csv(res.cf, pathtofile, row.names = FALSE)
    return(res.cf)
})


##########################################
### BENCHMARK DESIGN
##########################################

study_design = function(inst, best.config, save.dir = NULL) {
  message(paste(inst$task.id, inst$learner.id, sep = "/"))
  
  # Get data 
  dt = inst$predictor$data$get.x()
  
  # Get xinterest/xorig as list
  path = file.path(save.dir, inst$task.id)
  dir.create(file.path(path, "moc"), showWarnings = FALSE)
  flat.inst = flatten_instances(list(inst))
  
  # Load keras model from .h5 file if necessary
  if (inst$learner.id %in% c("logreg", "neuralnet")) {
    inst = load_keras_model(inst, save.dir)
  }
  
  # Define predictor with conditional (load from save.dir)
  pred = inst$predictor
  pred.con = pred$clone()
  pred.con$conditionals = readRDS(file.path(path, "conditional.rds")) 
  
  # Calculate counterfactuals for each data point
  results_pred = mapply(FUN = function(oneinst) {
    oneinst = initialize_instance(oneinst, save.dir)
    x.interest = oneinst$x.interest
    target = oneinst$target
    epsilon = 0

    ## MOC without modifications
    # as recommended by parameter tuning
    cf.irace = Counterfactuals$new(predictor = pred, x.interest = x.interest, 
      target = target, mu = best.config$mu, epsilon = epsilon,  
      generations = best.config$generations, p.mut = best.config$p.mut,
      p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen, 
      p.mut.use.orig = best.config$p.mut.use.orig, 
      p.rec.gen = best.config$p.rec.gen, 
      p.rec.use.orig = best.config$p.rec.use.orig, 
      initialization = "random")
    
    ## Random Search 
    ref.point = cf.irace$.__enclos_env__$private$ref.point
    range = cf.irace$.__enclos_env__$private$range
    train.data = as.data.frame(dt)
    param.set = cf.irace$.__enclos_env__$private$param.set
    obj.nam = cf.irace$.__enclos_env__$private$obj.names
    
    random = random_search(predictor = pred, x.interest = x.interest, 
        target = target, mu = best.config$mu, ref.point = ref.point, param.set = param.set, 
        range = range, max.iterations = best.config$generations, 
        train.data = train.data, obj.nam = obj.nam)
    
    # Extract counterfactuals 
    res = random$cf
    if (nrow(res) >= 1) {
        res$method = "random"
    } else {
        res$method = character()
    }
    res = add_rows(res, cf.irace$results$counterfactuals, "moc")
    
    df = cf.irace$log[, c("generation", "evals")]
    df = add_columns(df, cf.irace, "moc")
    df = cbind(df, random$log)
    
    rm(cf.irace)
    rm(random)
    gc()
    
    
    ## MOC with ice curve variance 
    cf.irace.ice = Counterfactuals$new(predictor = pred, x.interest = x.interest,
      target = target, mu = best.config$mu, epsilon = epsilon,
      generations = best.config$generations, p.mut = best.config$p.mut,
      p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen,
      p.mut.use.orig = best.config$p.mut.use.orig,
      p.rec.gen = best.config$p.rec.gen,
      p.rec.use.orig = best.config$p.rec.use.orig,
      initialization = "icecurve")

    res = add_rows(res, cf.irace.ice$results$counterfactuals, "mocice")
    df = add_columns(df, cf.irace.ice, "mocice")

    rm(cf.irace.ice)
    gc()
    
    ## MOC with modified mutator
    if (is.list(pred.con$conditionals)) {
      cf.irace.con = Counterfactuals$new(predictor = pred.con, x.interest = x.interest,
        target = target, mu = best.config$mu, epsilon = epsilon,
        generations = best.config$generations, p.mut = best.config$p.mut,
        p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen,
        p.mut.use.orig = best.config$p.mut.use.orig,
        p.rec.gen = best.config$p.rec.gen,
        p.rec.use.orig = best.config$p.rec.use.orig,
        initialization = "random")
      
      df = add_columns(df, cf.irace.con, "moccond")
      res = add_rows(res, cf.irace.con$results$counterfactuals, 
          "moccond")
      
      rm(cf.irace.con)
      gc()

      # MOC with modified mutator and ice curve variance #SD
      cf.irace.con.ice = Counterfactuals$new(predictor = pred.con,
        x.interest = x.interest,
        target = target, mu = best.config$mu, epsilon = epsilon,
        generations = best.config$generations, p.mut = best.config$p.mut,
        p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen,
        p.mut.use.orig = best.config$p.mut.use.orig,
        p.rec.gen = best.config$p.rec.gen,
        p.rec.use.orig = best.config$p.rec.use.orig,
        initialization = "icecurve")

      df = add_columns(df, cf.irace.con.ice, "mocmod")
      res = add_rows(res, cf.irace.con.ice$results$counterfactuals,
          "mocmod")

      rm(cf.irace.con.ice)
      gc()
    }
    
    row.id = as.numeric(row.names(x.interest)) + 1
    res$row_ids = row.id
    df$row_ids = row.id
    return(list(res, df))
    
  }, flat.inst, SIMPLIFY = FALSE)
  res.cf = do.call("rbind", lapply(results_pred, function(x) x[[1]]))
  res.log = do.call("rbind", lapply(results_pred, function(x) x[[2]]))
  
  # Save results as .csv
  for (nam in c("cf", "log")) {
    name.file = paste(nam, "moc", inst$learner.id, sep = "-")
    pathtofile = file.path(path, "moc", paste(name.file, ".csv", sep = ""))
    if (nam == "cf") {
      write.csv(res.cf, pathtofile, row.names = FALSE)
    } else {
      write.csv(res.log, pathtofile, row.names = FALSE)
    }
  }
  return(list(cf = res.cf, log = res.log))
}

# Helper to add row to data frame of results (counterfactuals)
add_rows = function(orig.df, add.df, name) {
  add.df$method = name
  add.df$pred = NULL
  df = rbind(orig.df, add.df)
  return(df)
}

# Helper to add column to data frame of results (hypervolume)
add_columns = function(df, rescf, name) {
  nams = paste(c("hv_", "div_"), name, sep = "")
  df[nams[1]] = c(rescf$log$fitness.domHV)
  df[nams[2]] = c(rescf$log$population.div)
  return(df)
}

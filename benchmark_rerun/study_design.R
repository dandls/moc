##########################################
### BENCHMARK DESIGN
##########################################

study_design = function(inst, best.config, save.dir = NULL) {
  message(paste(inst$task.id, inst$learner.id, sep = "/"))
  
  # Get data 
  dt = inst$predictor$data$get.x()
  
  # Get xinterest/xorig as list
  path = file.path("../saved_objects", save.dir, inst$task.id)
  sampled.rows = read.delim(file.path(path, "sampled_ids.txt"), header = FALSE)[,1]
  dt.x.interests = dt[sampled.rows,]
  list.x.interests = split(as.data.frame(dt.x.interests), seq(length(sampled.rows)))
  
  # Load keras model from .h5 file if necessary
  if (inst$learner.id %in% c("logreg", "neuralnet")) {
    inst = load_keras_model(inst, save.dir)
  }
  
  # Define predictor with conditional (load from save.dir)
  pred = inst$predictor
  pred.con = pred$clone()
  pred.con$conditional = readRDS(file.path(path, "conditional.rds"))
  
  # Define targets 
  targets = ifelse(inst$predictor$predict(newdata = as.data.frame(dt.x.interests)) < 0.5, 1, 0)[,1]
  
  # Calculate counterfactuals for each data point
  results_pred = mapply(FUN = function(x.interest, target, row.id) {
    if (target == 1) {
      target = c(0.5, 1)
    } else {
      target = c(0, 0.5)
    }
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
      use.ice.curve.var = FALSE)
    
    ## Random Search 
    ref.point = cf.irace$.__enclos_env__$private$ref.point
    range = cf.irace$.__enclos_env__$private$range
    train.data = as.data.frame(dt)
    obj.nam = cf.irace$.__enclos_env__$private$obj.names
    
    random = random_search(predictor = pred, x.interest = x.interest, 
        target = target, mu = best.config$mu, ref.point = ref.point, 
        range = range, epsilon = epsilon, max.iterations = best.config$generations, 
        train.data = train.data, obj.nam = obj.nam)
    
    # Extract counterfactuals 
    res = random$cf
    if (nrow(res) >= 1) {
        res$method = "random"
    } else {
        res$method = character()
    }
    res = add_rows(res, cf.irace$results$counterfactuals, "nsga2")
    
    df = cf.irace$log[, c("generation", "evals")]
    df = add_columns(df, cf.irace, "nsga2")
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
      use.ice.curve.var = TRUE)

    res = add_rows(res, cf.irace.ice$results$counterfactuals, "nsga2ice")
    df = add_columns(df, cf.irace.ice, "nsga2ice")
    
    rm(cf.irace.ice)
    gc()
    
    ## MOC with modified mutator
    if (class(pred.con$conditional) == "R6") {
      cf.irace.con = Counterfactuals$new(predictor = pred.con, x.interest = x.interest,
        target = target, mu = best.config$mu, epsilon = epsilon,
        generations = best.config$generations, p.mut = best.config$p.mut,
        p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen,
        p.mut.use.orig = best.config$p.mut.use.orig,
        p.rec.gen = best.config$p.rec.gen,
        p.rec.use.orig = best.config$p.rec.use.orig,
        use.ice.curve.var = FALSE)
      
      df = add_columns(df, cf.irace.con, "nsga2cond")
      res = add_rows(res, cf.irace.con$results$counterfactuals, 
          "nsga2cond")
      
      rm(cf.irace.con)
      gc()

      # MOC with modified mutator and ice curve variance 
      cf.irace.con.ice = Counterfactuals$new(predictor = pred.con,
        x.interest = x.interest,
        target = target, mu = best.config$mu, epsilon = epsilon,
        generations = best.config$generations, p.mut = best.config$p.mut,
        p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen,
        p.mut.use.orig = best.config$p.mut.use.orig,
        p.rec.gen = best.config$p.rec.gen,
        p.rec.use.orig = best.config$p.rec.use.orig,
        use.ice.curve.var = TRUE)
      
      df = add_columns(df, cf.irace.con.ice, "nsga2condice")
      res = add_rows(res, cf.irace.con.ice$results$counterfactuals, 
          "nsga2condice")
      
      rm(cf.irace.con.ice)
      gc()
    }

    res$row_ids = row.id
    df$row_ids = row.id
    return(list(res, df))
    
  }, list.x.interests, targets, sampled.rows, SIMPLIFY = FALSE)
  res.cf = do.call("rbind", lapply(results_pred, function(x) x[[1]]))
  res.log = do.call("rbind", lapply(results_pred, function(x) x[[2]]))
  
  # Save results as .csv
  for (nam in c("cf", "log")) {
    name.file = paste(nam, "nsga2", inst$learner.id, sep = "-")
    pathtofile = file.path(path, "nsga2", paste(name.file, ".csv", sep = ""))
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

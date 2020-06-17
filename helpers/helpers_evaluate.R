########################################################
#### Helper functions to evaluate counterfactuals
########################################################

# Subset object of trained benchmark models by specific task and learner names
subset_instances = function(instances, task = NULL, learner = NULL) {
  if (!is.null(task)) {
    instances = Filter(function(x) x$task.id %in% task, instances)
  }
  if (!is.null(learner)) {
    instances = Filter(function(x) x$learner.id %in% learner, instances)
  }
  return(instances)
}

# Evaluate counterfactuals of dice, recourse and tweaking
# Remove dominated counterfactuals from set if wanted (default FALSE)
evaluate_cfexp = function(cf, instance, id = "dice", remove.dom = FALSE, data.dir) {
  # Get training data
  train.data = data.frame(instance$predictor$data$get.x())
  
  # Load keras model if necessary
  if (instance$learner.id %in% c("logreg", "neuralnet")) {
    instance = load_keras_model(instance, data.dir = data.dir)
  }
  # Revert dummmy
  if (id %in% c("dice", "recourse", "tweaking")) {
    dum.vars = names(which(instance$predictor$data$feature.types == "categorical"))
    dum.vars = dum.vars[!dum.vars %in% names(cf)]
    for (var in dum.vars) {
      cf = revert_dummy(var, cf, as.character(unique(train.data[,var])))
    }
  }
  pred = instance$predictor
  
  # Get information from predictor to calculate fitness
  row_ids = unique(cf$row_ids)
  flat.inst = flatten_instances(list(instance))
  
  # Transform entries of counterfactuals that should be discrete to characters
  id.char = unlist(lapply(instance$predictor$data$feature.types, FUN = function(par) par == "categorical"))
  cf[names(id.char)[id.char]] = data.frame(lapply(cf[names(id.char)[id.char]], as.character), 
    stringsAsFactors = FALSE)
  
  id.int = which(sapply(cf[, instance$predictor$data$feature.names], is.integer))
  cf[, id.int] = sapply(cf[, id.int],  as.numeric)
  
  cf = cf[, !(names(cf) %in% pred$data$y.names)]
  cf$prediction = NULL
  
  # Calculate fitness for each x.interest & target
  res.row = mapply(function(oneinst) {
    row_id = as.numeric(row.names(oneinst$sampled.rows)) + 1L
    x = cf[cf$row_ids == (row_id), names(cf) != "row_ids"]
    # Remove duplicated rows
    dup.idx = which(duplicated(x))
    if (length(dup.idx) > 1L) {
      x = x[-dup.idx,]
    }
    oneinst = initialize_instance(oneinst, data.dir = data.dir)
    x.interest = oneinst$x.interest
    target = oneinst$target
    
    # Parameter set
    param.set = ParamHelpers::makeParamSet(
      params = counterfactuals:::make_paramlist(rbind(train.data, x.interest)))
    range = ParamHelpers::getUpper(param.set) -
      ParamHelpers::getLower(param.set)
    range[ParamHelpers::getParamIds(param.set)
      [ParamHelpers::getParamTypes(param.set) == "discrete"]]  = NA
    range = range[pred$data$feature.names]
    
    # Convert characters to factors
    x.list = split(x, seq(nrow(x)))
    x.list = lapply(x.list, function(x) {
      x = as.list(x)
      x$use.orig = rep(FALSE, length(x))
      return(x)
    })
    x.list <- lapply(x.list, function(obs) {
      obstest <- mosmafs::valuesFromNames(param.set, obs)
      ParamHelpers::trafoValue(param.set, obs)
    })
    x <- listToDf(x.list, param.set)
    
    # Calculate objective values
    fitness = counterfactuals:::fitness_fun(x = x,
      x.interest = x.interest, target = target,
      predictor = pred, train.data = train.data,
      range = range, identical.strategy = TRUE)
    
    # only keep nondominated solutions if remove.dom TRUE
    if (ncol(fitness) > 1 & remove.dom) {
      nondom.id = nondominated(fitness)
    } else {
      nondom.id = seq_len(ncol(fitness))
    }
    nondom.fitness = data.frame(t(fitness[,nondom.id]))
    names(nondom.fitness) = obj.nams
    x[, grep("use.orig", names(x))] = NULL
    nondom = cbind(x[nondom.id, ], nondom.fitness)
    nondom$row_ids = row_id
    return(nondom)
  }, flat.inst, SIMPLIFY = FALSE)
  return(do.call(rbind, res.row))
}


# Revert dummy encoding
revert_dummy <-function(var,indata, origvar){
  nam = names(indata)[grepl(var, names(indata))]
  col = indata[, nam]
  if (length(nam) > 1) {
    nams = str_remove(nam, var)
    val = nam[apply(col, 1, function(i) which.max(i))]
    val = str_remove(val, var)
  } else {
    nams = origvar
    val = ifelse(col == 1, nams[2], nams[1])
  }
  indata[, nam[1]] = factor(val, levels = nams)
  names(indata)[names(indata) == nam[1]] <- var
  if (length(nam) > 1) {
    indata[, nam[2:length(nam)]] = NULL
  }
  return(indata)
}


# Plot performance curves
# Works for ranks and hypervolumes
plot_results = function(df, type = "hv", methods = NULL, subset.col = "learner",
  pdf.file = NULL, ylim = NULL, width = 6, height = 2.7, xlim = c(0, 40),
  ylab = "dominated hypervolume", line.width = 0.4, ncol = 2) {
  
  assert_true(type %in% c("hv", "div", "rank"))
  assert_character(methods, null.ok = TRUE)
  assert_character(subset.col, null.ok = TRUE)
  
  # Extract info given by type and methods
  gen = df$generation
  by.subset_vector = !is.null(subset.col)
  if (by.subset_vector) {
    pred = df[, subset.col]
    if (is.data.frame(pred)) {
      pred = paste(pred[, 1], pred[, 2], sep = " / ")
    }
  }
  df = df[, grepl(type, names(df))]
  needed.cols = paste(type, "_", methods, sep = "")
  if (!is.null(methods)) {
    df = df[, needed.cols]
    assert_true(ncol(df) == length(methods))
  }
  names(df) = str_remove(names(df), needed.cols)
  df$generation = gen
  
  # Seperate by subset vector if given (e.g. by dataset or predictor type)
  if (by.subset_vector) {
    df$subset_vector = pred
    id.vars = c("generation", "subset_vector")
    
  } else {
    id.vars = c("generation")
  }
  
  # Prepare for plotting
  df.melt <- reshape2::melt(df, id.vars=id.vars)
  names(df.melt) = c(id.vars, "method", "value")
  ylab.name = switch(type, hv = "relnondom",
    div = "diversity")
  if (by.subset_vector) {
    by.list = list(method = df.melt$method,
      subset_vector = df.melt$subset_vector, generation = df.melt$generation)
  } else {
    by.list = list(method = df.melt$method, generation = df.melt$generation)
  }
  df.agg = aggregate(df.melt[, "value"], by = by.list,
    FUN = mean, na.rm = TRUE) ## watch out!!
  df.agg$method = as.character(df.agg$method)
  df.agg = df.agg[order(df.agg$method),]
  # Plot
  p = ggplot(data = df.agg, aes(x=generation, y=x)) +
    geom_line(aes(colour=method, linetype = method), size = line.width) +
    ylab(ylab.name) +
    #ylim(ylim) +
    xlim(xlim) +
    ylab(ylab) +
    scale_colour_manual(values=c("grey10", "grey33", "grey53", "black", "grey73")) +
    scale_linetype_manual(values=c( "dotdash",
      "dashed", "dotted", "solid", "solid")) +
    theme_bw()
  if (by.subset_vector) {
    p  = p + facet_wrap(~ subset_vector, scales = "free", ncol = ncol) +
      theme(legend.position="bottom", legend.spacing=unit(-.1,"cm"))
  }
  
  # Save if info given
  if (!is.null(pdf.file)) {
    ggsave(filename = pdf.file, p,
      width = width, height = height)
  }
  return(p)
}

# Subset number of solutions of MOC
subset_results = function(cfexps, nr.solutions, strategy = "random", epsilon = 0) {
  assert_character(strategy)
  assert_true(strategy %in% c("random", "hvcontr"))
  if (nr.solutions > nrow(cfexps)) {
    return(cfexps)
  }
  assert_integerish(nr.solutions, lower = 1)
  feas.id = which(cfexps$dist.target <= epsilon)
  best = c()
  if (length(feas.id) > 0) {
    left = nr.solutions - length(feas.id)
    if (left == 0) {
      return(cfexps[feas.id,])
    } else if (left > 0) {
      nr.solutions = left
      best = feas.id
    } else if (left < 0) {
      cfexps = cfexps[feas.id,]
    }
  } 
  if (strategy == "hvcontr") {
    idx = hv_contribution(t(cfexps[, obj.nams]), nr.solutions = nr.solutions, 
      best = best)
  } else if (strategy == "random") {
    remaining = seq_len(nrow(cfexps))
    if (length(best) > 0) remaining = remaining[-best]
    idx = c(best, sample(remaining, nr.solutions))
  }
  return(cfexps[idx,])
}

hv_contribution = function(fitness, nr.solutions, best) {
  ref.point = c(0.5, 1, max(fitness[3,])+1, 1)
  for (i in seq_len(nr.solutions)) {
    best = c(best, which.max(apply(fitness, 2, 
      function(obs) computeHV(cbind(fitness[,best], obs), ref.point = ref.point)
      )))
  }
  return(best)
}


# Calculate relative coverage of pf2 (MOC) over pf1(other methods)
relative_coverage = function(pf1, pf2) {
  
  assertTRUE(all(class(pf1) == class(pf2)))
  if(is.data.frame(pf2) && is.data.frame(pf2)) {
    pf1 = as.matrix(t(pf1))
    pf2 = as.matrix(t(pf2))
  }
  n1 = ncol(pf1)
  ranking = ecr::doNondominatedSorting(cbind(pf1, pf2))$ranks
  rank1 = ranking[1:n1]
  return(vapply(rank1, FUN.VALUE = logical(1), function(x) all(x > 1)))
}

# binomial test
biotest = function(x, n) {
  p = binom.test(x = round(x*n), n = n, p = 0.5,alternative = "greater")$p.value
  if (p < 0.05) return("*")
  return("")
}

# combine boxplots
combine_plots = function(plist, shared.y = FALSE) {
  id = seq(1, length(plist), 2)
  for (i in id) {
    #plist[[i]] = plist[[i]] + theme(axis.text.x = element_blank(), 
    #  axis.ticks.x = element_blank())
    plist[[i]] = plist[[i]] + theme(strip.text.y = element_blank())
  }
  if (shared.y) {
    id.y = seq(1, length(plist)- 1, 2)+1
    for (j in id.y) {
      plist[[j]] = plist[[j]] + theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
      #plist[[j]] = plist[[j]] + theme(strip.text.x = element_blank())
    }
  }
  return(plist)
}




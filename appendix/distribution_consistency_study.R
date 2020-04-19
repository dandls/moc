#####################################################
### Distribution Consistency Study
#####################################################
#--- Setup ----
source("../helpers/libs_mlr.R")
obj.nams = c("dist.target", "dist.x.interest", "nr.changed", "dist.train")
USE_SAVED = TRUE # use saved objects or rerun experiment

#--- Define dataset structure ----
ncol = 3
sigma = matrix(c(
  1, 0.9, 0.9, 
  0.9, 1, 0.9, 
  0.9, 0.9, 1), ncol = ncol)
mean = rep(0, ncol)

#--- Helpers ----
get_dens = function(cfexp){
  dmvnorm(x = cfexp[, seq_len(length(mean))], mean = mean, 
    sigma = sigma)
}

#--- Run experiment ----
if (USE_SAVED) {
  cfexps = readRDS("counterfactuals_correlated_data.rds")
} else {
  # Run experiment: 
  # Setup
  best.config = readRDS("../saved_objects/best_configs.rds")
  parallelMap::parallelStartMulticore(cpus = 10L)
  set.seed(1000)
  cfexps = parallelMap(function(i) {
      message(i)
      
      
      x <- rmvnorm(n = 600, mean = mean, sigma = sigma)
      data = as.data.frame(x)
      mean.dens = mean(dmvnorm(x = data[, -4], mean = mean,
          sigma = sigma))
      sums = rowSums(data) + rnorm(n = 600, mean = 0, sd = 0.2)
      data$y = ifelse(sums > 3, 1, 0)
      c = cor(data)
      data$y = as.character(data$y)
      
      # Train model
      task = mlr::makeClassifTask(data = data, target = "y")
      n = getTaskSize(task)
      train.set = sample(n, size = n*0.8)
      test.set = seq(1, n)[-train.set]
      train.task = subsetTask(task, subset = train.set)
      test.task = subsetTask(task, subset = test.set)
      lrn = mlr::makeLearner("classif.ranger", predict.type = "prob")
      mod = mlr::train(lrn, train.task)
      probs = predict(mod, task = test.task)
      perf = performance(probs)
      
      # Compute Coutnerfactuals with and without modified mutator
      pred.cond = Predictor$new(mod, data = data, conditional = TRUE, 
          type = "prob", class = "1")
      pred = Predictor$new(mod, data = data, type = "prob", class = "1")
      
      row.id = sample(seq_len(nrow(data)), size = 1)
      x.interest = data[row.id, seq_len(length(mean))]
      current.pred = pred$predict(x.interest)
      target = ifelse(current.pred < 0.5, 1, 0)
      
      # Original/without modified mutator
      cfexp.orig = Counterfactuals$new(predictor = pred, x.interest = x.interest, 
          target = target, mu = best.config$mu, epsilon = 0.3,  
          generations = best.config$generations, p.mut = best.config$p.mut,
          p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen, 
          p.mut.use.orig = best.config$p.mut.use.orig, 
          p.rec.gen = best.config$p.rec.gen, 
          p.rec.use.orig = best.config$p.rec.use.orig)
      
      # Modified mutator 
      cfexp.cond = Counterfactuals$new(predictor = pred.cond, x.interest = x.interest, 
          target = target, mu = best.config$mu, epsilon = 0.3,
          generations = best.config$generations, p.mut = best.config$p.mut,
          p.rec = best.config$p.rec, p.mut.gen = best.config$p.mut.gen, 
          p.mut.use.orig = best.config$p.mut.use.orig, 
          p.rec.gen = best.config$p.rec.gen, 
          p.rec.use.orig = best.config$p.rec.use.orig)
      
      list(cf.orig = cfexp.orig$results$counterfactuals, x.interest = cfexp.orig$x.interest,
          ref.point = cfexp.orig$.__enclos_env__$private$ref.point,
          cf.cond = cfexp.cond$results$counterfactuals, corr = c, target = cfexp.orig$target,
          performance = perf, log.orig = cfexp.orig$log, log.cond = cfexp.cond$log)
  }, 1:30)
  parallelStop()
}

#--- Evaluate mean misclassification rate ----
mean(unlist(lapply(cfexps, function(cf) {
  print(cf$corr[1:3, 1:3])
  cf$performance
})))

# ---Evaluate min distance to training data points over generations----
perf.list = lapply(cfexps, function(cfexp) {
    df1 = cfexp$log.orig[,c("generation", "fitness.domHV", paste(obj.nams, "mean", sep = "."))]
    names(df1)[2] = "hv"
    names(df1) = paste(names(df1), "original", sep = "_")
    df2 = cfexp$log.cond[, c("fitness.domHV",  paste(obj.nams, "mean", sep = "."))]
    names(df2)[1] = "hv"
    names(df2) = paste(names(df2), "conditional", sep = "_")
    df = cbind(df1, df2)
    names(df)[1] = "generation"
    return(df)
})

perf.df = do.call("rbind", perf.list)
perf.agg = aggregate(perf.df, by = list(perf.df$generation), FUN = mean)
obj.agg = perf.agg[, c("generation", "dist.train.mean_original", "dist.train.mean_conditional")]
names(obj.agg) = c("generation", "original", "conditional")
df.melt <- reshape2::melt(obj.agg, 
    id.vars = "generation")
names(df.melt) = c("generation", "method", "value")

ggplot(data = df.melt, aes(x=generation, y=value)) + 
    geom_line(aes(colour=method), size = 1) + 
    ylab("dist.train") +
    scale_colour_grey(start = 0.3, end = 0.6) +
    theme_bw() +
   theme(legend.position = c(0.7, 0.7))

#--- Evaluate density ----
dens.orig = unlist(lapply(cfexps, function(cfexp) {
  log(prod(get_dens(cfexp$cf.orig)))
}))
dens.cond = unlist(lapply(cfexps, function(cfexp) {
  log(prod(get_dens(cfexp$cf.cond)))
}))
mean(dens.orig)
mean(dens.cond)
std(dens.orig)
std(dens.cond)

#--- Evaluate hypervolume ----
hvs = lapply(cfexps, function(cf) {
    r = cf$ref.point[-4]
    hv.orig = ecr::computeHV(t(as.matrix(cf$cf.orig[, c("dist.target", 
        "dist.x.interest", "nr.changed")])), 
        ref.point = r)/
        ecr::computeHV(matrix(c(0, 0, 0)), 
            ref.point = r)
    
    hv.cond = ecr::computeHV(t(as.matrix(cf$cf.cond[, c("dist.target", 
        "dist.x.interest", "nr.changed")])), 
        ref.point = r)/
        ecr::computeHV(matrix(c(0, 0, 0)), 
            ref.point = r)
    return(list(original = hv.orig, conditional = hv.cond))
})

mean(unlist(lapply(hvs, function(x) x[1])))
mean(unlist(lapply(hvs, function(x) x[2])))
sd(unlist(lapply(hvs, function(x) x[1])))
sd(unlist(lapply(hvs, function(x) x[2])))



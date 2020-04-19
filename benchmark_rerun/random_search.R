###########################################
### Random SEARCH 
###########################################

random_search = function(predictor, x.interest, target, mu,
  ref.point, range, epsilon = NULL, max.iterations = 500, train.data,
    obj.nam, param.set.init = NULL) {
  
  # Extract info from predictor 
  param.set = ParamHelpers::makeParamSet(
    params = make_paramlist(predictor$data$get.x()))
  x.interest = x.interest[setdiff(colnames(x.interest), 
    predictor$data$y.names)]
  
  range = ParamHelpers::getUpper(param.set) - 
    ParamHelpers::getLower(param.set)
  range[ParamHelpers::getParamIds(param.set)
    [ParamHelpers::getParamTypes(param.set) == "discrete"]]  = NA
  range = range[predictor$data$feature.names]
  
  i = sapply(x.interest, is.factor)
  x.interest.char = x.interest
  x.interest.char[i] = lapply(x.interest.char[i], as.character)
  
  elite = list()
  fitness.elite = matrix(NA, nrow = 4, ncol = 0)
  hv = c()
  div = c()
  
  # Sample mu * (nr.generations + 1) candidates from set of possible values
  candidates = ParamHelpers::sampleValues(par = param.set, n = mu*max.iterations, 
    discrete.names = TRUE)
  
  if (!is.null(param.set.init)) {
    candidates.init = ParamHelpers::sampleValues(
      par = param.set.init, n = mu, 
      discrete.names = TRUE)
  } else {
    candidates.init = ParamHelpers::sampleValues(
      par = param.set, n = mu, 
      discrete.names = TRUE)
  }
  candidates = c(candidates.init, candidates)
  max.iterations = max.iterations + 1
  
  candidates = lapply(candidates, function(x) {
    x = transform_to_orig(x, x.interest.char, delete.use.orig = FALSE)
  })
  
  # Evaluate candidates
  fn = smoof::makeMultiObjectiveFunction(
    has.simple.signature = FALSE, par.set = param.set, n.objectives = 4, 
    noisy = TRUE, ref.point = ref.point,
    fn = function(x, fidelity = NULL) {
      fitness_fun(x, x.interest = x.interest, target = target, 
        predictor = predictor, train.data = train.data, range = range)
    })
  fn = mosmafs::setMosmafsVectorized(fn)
  res  = initEcr(fitness.fun = fn, population = candidates)
  
  # Match counterfactuals and fitness values 
  nondom.fitness = res$pareto.front
  names(nondom.fitness) = obj.nam
  nondom.pop = listToDf(res$pareto.set, param.set)
  nondom.pop[, grep("use.orig", names(nondom.pop))] = NULL
  nondom = cbind(nondom.pop, nondom.fitness)
  if (!is.null(epsilon)) {
    nondom = nondom[nondom$dist.target <= epsilon,]
  }
  
  # Calculate hypervolume and diversity over generations
  pop = listToDf(res$last.population, param.set)
  pop[, grep("use.orig", names(pop))] = NULL
  fitness = getPopulations(res$log)[[1]]$fitness
  len = ncol(fitness)
  folds = seq(mu, len, len/max.iterations)
  for (fold in folds) {
    hv = c(hv, ecr::computeHV(fitness[, 1:fold], ref.point)/
        ecr::computeHV(matrix(rep(0, nrow(fitness))), ref.point))
    div = c(div, compute_diversity(pop[1:fold, ], range))
  }
  
  return(list(cf = nondom, log = data.frame(hv_random = hv, div_random = div)))
}

context("Counterfactuals")

data("Boston", package  = "MASS")
Boston$chas = as.factor(Boston$chas)

set.seed(1000)
rf =  randomForest::randomForest(medv ~ ., data = Boston)
X = Boston[,-which(names(Boston) == "medv")]
mod = Predictor$new(rf, data = X)

# Explain first instance
x.interest = X[1,]
pred  = mod$predict(x.interest)[[1]]
target = 30
generations = 10

set.seed(100)
cf = Counterfactuals$new(mod, x.interest = x.interest, target = target,
  mu = 30, generations = generations)


test_that("Counterfactuals has correct output", {
  results.sub = cf$subset_results(10)
  p = plot(cf, labels = TRUE, decimal.points = 5, nr.solutions = 10)
  p_stats = cf$plot_statistics()
  p_search = cf$plot_search()
  p_hv = cf$plot_hv()
  p_parallel = cf$plot_parallel()
  pdf(tf <- paste0(tempfile(), ".pdf"))
  p_surface = cf$plot_surface(features = c("crim", "lstat"))
  p_surface_cat = cf$plot_surface(features = c("crim", "chas"))
  dev.off()
  file.remove(tf)
  hv = cf$get_hv()
  p_nr = plot(cf, labels = TRUE, nr.changed = 2)
  #div = cf$calculate_diversity()

  expect_list(cf$results, any.missing = FALSE, len = 2, unique = TRUE)
  expect_data_frame(cf$log, any.missing = FALSE, nrows = generations + 1,
    ncols = 13)
  expect_list(results.sub, len = 2, unique = TRUE)
  expect_data_frame(results.sub$counterfactuals, min.rows = 1,
    ncols = ncol(X) + 5)
  expect_data_frame(results.sub$counterfactuals.diff, min.rows = 1,
    ncols = ncol(X) + 5)
  expect_s3_class(p, c("gg", "ggplot"))
  expect_s3_class(p_hv, c("gg", "ggplot"))
  expect_s3_class(p_nr, c("gg", "ggplot"))
  expect_list(p_stats, len = 3)
  expect_s3_class(p_search, c("gg", "ggplot"))
  expect_s3_class(p_parallel, c("gg", "ggplot"))
  expect_s3_class(p_surface, c("gg", "ggExtraPlot"))
  expect_s3_class(p_surface_cat, c("gg", "ggExtraPlot"))
  expect_number(hv, lower = 0, finite = TRUE)
  # expect_number(div, lower = 0, finite = TRUE)
  diff = cf$results$counterfactuals.diff
  diff_1 = diff[diff$nr.changed == 1, names(X)]
  expect_true(all(rowSums(diff_1 != 0) == 1))

  # continue search
  cf$continue_search(5L)
  expect_list(cf$results, any.missing = FALSE, len = 2, unique = TRUE)
  expect_data_frame(cf$log, any.missing = FALSE, nrows = generations + 1L + 5L,
    ncols = 13)

})

test_that("Counterfactuals only one solutions if target equal prediction", {

  cf$explain(x.interest, target = pred)

  diff = cf$results$counterfactuals.diff

  expect_data_frame(cf$results$counterfactuals, nrows = 1, ncols = ncol(X) + 5)
  expect_data_frame(cf$results$counterfactuals.diff, nrows = 1, ncols = ncol(X) + 5)
  expect_equal(cf$results$counterfactuals$dist.target, 0)
  expect_equal(cf$results$counterfactuals$dist.x.interest, 0)
  expect_true(all(diff[, names(diff) != "pred"] == 0))
  expect_identical(cf$get_hv(), 0)
  expect_warning(cf$subset_results(10),
    "nr.solutions out of range")
  expect_list(cf$plot_statistics(), len = 3)
  expect_s3_class(cf$plot_search(), c("gg", "ggplot"))
  expect_s3_class(plot(cf, labels = TRUE, decimal.points = 3),
    c("gg", "ggplot"))
})

test_that("colnames of x.interest not identical to training data names", {
  names(x.interest) = c("crim2", names(x.interest)[2:length(names(x.interest))])
  expect_error(Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 50, generations = generations),
    "colnames of x.interest must be identical to observed data")

})



test_that("Counterfactual solutions if target in infeasible space", {

  set.seed(100)
  target = -100
  cf$explain(x.interest, target = target)
  diff = cf$results$counterfactuals.diff

  expect_data_frame(cf$results$counterfactuals, min.rows = 1, ncols = ncol(X) + 5)
  expect_data_frame(cf$results$counterfactuals.diff, min.rows = 1, ncols = ncol(X) + 5)
  expect_numeric(cf$results$counterfactuals$dist.target,
    upper = abs(target) + pred)
  expect_list(cf$plot_statistics(), len = 3)
  expect_s3_class(cf$plot_search(), c("gg", "ggplot"))
  expect_s3_class(plot(cf, labels = TRUE, decimal.points = 3),
    c("gg", "ggplot"))
  expect_number(cf$get_hv(), lower = 0)
  expect_true(all(diff(cf$log$fitness.domHV) >= 0))
  expect_true(all(diff(cf$log$dist.target.min) <= 0))
})



test_that("no good solution can be found by one feature", {
  cols.sub = c("crim", "chas", "tax")
  set.seed(10)
  rf =  randomForest::randomForest(medv ~ ., data = Boston[, c(cols.sub, "medv")])
  mod = Predictor$new(rf, data = X[, cols.sub])

  x.interest = X[1, cols.sub]
  pred  = mod$predict(x.interest)[[1]]
  target = 30
  generations = 30

  set.seed(100)
  cf = Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 50, generations = generations, fixed.features = c("chas", "tax"))
  expect_true(all(cf$results$counterfactuals.diff[, c("chas", "tax")] == 0))
  expect_numeric(cf$results$counterfactuals$pred, upper = 27,
    any.missing = FALSE)
  expect_true(all(rowSums(cf$results$counterfactuals.diff[, cols.sub] != 0) %in% c(1, 0)))
})

test_that("fixed features works with numbers and lower and upper specified", {

  test.df = Boston[, c("lstat", "rm", "medv")]
  lm =  lm(medv ~ ., data = test.df)
  mod = Predictor$new(lm, data = test.df)
  test.df = test.df[, names(test.df) %in% c("lstat", "rm")]
  x.interest = test.df[5,]

  cf.lm = Counterfactuals$new(predictor = mod, x.interest = x.interest,
    target = 20, fixed.features = 1, generations = 10)
  expect_true(all(cf.lm$results$counterfactuals.diff$lstat == 0))

  # works although ordering of x.interest columns changed
  x.interest = x.interest[, c("rm", "lstat")]
  cf.lm = expect_warning(Counterfactuals$new(predictor = mod, x.interest = x.interest,
    target = 20, fixed.features = 1, generations = 10))
  expect_true(all(cf.lm$results$counterfactuals.diff$lstat == 0))
})

test_that("works with lower and upper specified", {
  lower = 4.9
  upper = 5.1
  names(lower) = "lstat"
  names(upper) = "lstat"
  lm =  Predictor$new(lm(medv ~ lstat, data = Boston))
  x.interest = Boston[1,-which(names(Boston) == "medv")]
  cf.lm.range = Counterfactuals$new(predictor = mod, x.interest = x.interest,
    target = 20, generations = 10,
    lower = lower, upper = upper)

  set.seed(1234)
  freq_2 = calculate_frequency_wrapper(cf.lm.range, obs = x.interest)
  expect_numeric(freq_2, len = ncol(x.interest), lower = 0, upper = 1)

  cf.lm.range$target = NULL
  expect_error(calculate_frequency_wrapper(cf.lm.range, obs = x.interest),
    "target not specified")

  pdf(tf <- paste0(tempfile(), ".pdf"))
  set.seed(1234)
  freq_3 = calculate_frequency_wrapper(cf.lm.range, row.ids = 1,
    target = 20, plot = TRUE)

  expect_equal(freq_2, freq_3)

  p_freq = cf.lm.range$get_frequency(plot = TRUE)
  expect_numeric(p_freq)
  dev.off()
  file.remove(tf)

  # x.interest not in range lower to upper
  lower["lstat"] = 5.4
  expect_error(Counterfactuals$new(predictor = mod, x.interest = x.interest,
    target = 20, generations = 10,
    lower = lower, upper = upper), "'upper' is smaller than the corresponding one in 'lower'")

  # lower > upper
  lower["lstat"] = 6
  expect_error(Counterfactuals$new(predictor = mod, x.interest = x.interest,
    target = 20, generations = 10,
    lower = lower, upper = upper),
    "some component of 'upper' is smaller than the corresponding one in 'lower'")



  # value of x.interest not in lower/upper
  lower.2 = 5.3
  upper.2 = 5.6
  names(lower.2) = "lstat"
  names(upper.2) = "lstat"
  expect_error(Counterfactuals$new(predictor = lm, x.interest = x.interest,
    target = 20, generations = 10,
    lower = lower.2, upper = upper.2), "Feature values of x.interest outside range of observed data")

})


test_that("if target infinite, error", {
  target = c(Inf, Inf)
  expect_error(Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 30, generations = generations),
    "One element of target must be finite")
  cf = Counterfactuals$new(mod, x.interest = x.interest,
    mu = 30, generations = generations)
  expect_error(cf$explain(x.interest, target = Inf),
    "One element of target must be finite")

})

test_that("refpoint correct if Inf in target", {
  cf1 = Counterfactuals$new(mod, x.interest = x.interest, target = c(-Inf, 20),
    mu = 20, generations = 10)
  expect_equal(cf1$.__enclos_env__$private$ref.point[1], mod$predict(x.interest)[[1]] - 20)
  cf2 = Counterfactuals$new(mod, x.interest = x.interest, target = c(27, Inf),
    mu = 20, generations = 10)
  expect_equal(cf2$.__enclos_env__$private$ref.point[1], 27 - mod$predict(x.interest)[[1]])
})

test_that("different initialization strategies", {
  # ice curve variance
  set.seed(1234)
  cfice = Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 20, generations = 0, initialization = "icecurve")
  icevar = get_ICE_var(x.interest, mod, cfice$.__enclos_env__$private$param.set)
  icevar = icevar[order(icevar, decreasing = TRUE)]
  freq = cfice$get_frequency()
  freq = freq[order(freq, decreasing = TRUE)]
  expect_equal(names(icevar)[1:2], names(freq)[1:2])

  # train data
  cftrain = Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 50, generations = 0, initialization = "traindata")
  res = cftrain$results$counterfactuals[, names(X)]

  # sd
  mod = Predictor$new(lm(medv ~ lstat + age + indus, data = Boston[, c( "medv", "lstat", "age", "indus")]))
  x.interest = Boston[1, c("lstat", "age", "indus")]
  cfsd = Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = nrow(Boston), generations = 0, initialization = "sd")
  t = unlist(lapply(cfsd$.__enclos_env__$private$ecrresults$last.population, function(c) {
    ParamHelpers::isFeasible(cfsd$.__enclos_env__$private$param.set.init, c)
  }))
  expect_true(all(t))
})

test_that("conditional transformation trees mutator works", {
  mod_cond = mod$clone()
  ctr = partykit::ctree_control(maxdepth = 2L)
  mod_cond$conditionals = fit_conditionals(mod_cond$data$get.x(), ctrl = ctr)
  set.seed(100)
  cf_cond = Counterfactuals$new(mod_cond, x.interest = x.interest, target = target,
    mu = 30, generations = generations)
  expect_false(all(do.call(paste0, cf_cond$results$counterfactuals) %in% do.call(paste0, cf$results$counterfactuals)))
  # expr_cond = system.time(Counterfactuals$new(mod_cond, x.interest = x.interest, target = target,
  #   mu = 10, generations = 3))
  # expr_orig = system.time(Counterfactuals$new(mod, x.interest = x.interest, target = target,
  #   mu = 10, generations = 3))
  # expect_true(expr_cond[3]>expr_orig[3])
})

test_that("do not use x.interest as part of observed data", {
  mod = Predictor$new(rf, data = X[-1,])
  
  # Explain first instance
  x.interest = X[1,]
  x.interest$crim = max(X$crim+0.25)
  pred  = mod$predict(x.interest)[[1]]

  cf = Counterfactuals$new(mod, x.interest = x.interest, target = target,
    mu = 30, generations = generations)
})

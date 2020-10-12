#' Counterfactual Explanations
#'
#' \code{Counterfactuals} are calculated based on the NSGA-II with mixed integer evolutionary strategies by Li et al. (2013).
#' The method is available in the package mosmafs, which is based on the package ecr.
#'
#' @format \code{\link{R6Class}} object.
#' @name Counterfactuals
#' @section Usage:
#' \preformatted{
#' cf = Counterfactuals$new(predictor, x.interest = NULL, target = NULL,
#' epsilon = NULL, fixed.features = NULL, max.changed = NULL,
#' mu = 59, generations = 60, p.mut = 0.7, p.rec = 0.6, p.mut.gen = 0.25,
#' p.mut.use.orig = 0.2, p.rec.gen = 0.6, p.rec.use.orig = 0.7,
#' k = 1L, weights = NULL, lower = NULL, upper = NULL, initialization = "random",
#' track.infeas = TRUE)
#'
#' plot(cf)
#' cf$results
#' cf$log
#' print(cf)
#' cf$explain(x.interest, target)
#' cf$subset_results(nr.solutions = 10)
#' cfs$continue_search(generations)
#' cf$plot_search()
#' cf$plot_parallel(features, row.ids)
#' cf$plot_surface(features, grid.size)
#' cf$plot_hv()
#' cf$plot_statistics()
#' cf$get_hv()
#' cf$get_frequency(plot = FALSE, subset.zero = FALSE)
#' }
#'
#' @section Arguments:
#' For Counterfactuals$new()
#' \describe{
#' \item{predictor: }{(Predictor)\cr
#' The object (created with Predictor$new()) holding the machine learning model and the data.}
#' \item{x.interest: }{(data.frame)\cr  Single row with the instance to be explained.}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a single numeric or
#' a vector of two numerics, to define a desired interval as outcome.}
#' \item{epsilon: }{(numeric(1))\cr Soft constraint. If chosen, candidates, whose
#' distance between their prediction and target exceeds epsilon, are penalized.
#' Default is NULL.}
#' \item{fixed.features: }{(character|numeric)\cr
#' Name or index of feature(s), which are not allowed to be changed.
#' Index refers to ordering of feature names of data used to initialize predictor.
#' Default is NULL.}
#' \item{max.changed: }{integer(1)\cr Maximum number of features that can be changed.
#' Default is NULL.}
#' \item{mu: }{(integer(1))\cr Population size.
#' Default is 59.}
#' \item{generations: }{(integer(1))\cr Number of generations. Default is 60.}
#' \item{p.mut: }{numeric(1)\cr Probability a child is chosen to be mutated.
#' Default is 0.7.}
#' \item{p.rec: }{numeric(1)\cr Probability a pair of parents is chosen to recombine.
#' Default is 0.6.}
#' \item{p.mut.gen:}{numeric(1)\cr Probability one feature/gene is mutated.
#' Default is 0.25.}
#' \item{p.mut.use.orig:}{numeric(1)\cr Probability an element of the indicator
#' to use the feature value of x.interest is mutated. As hamming weight
#' bitflip mutation is used, only a probability between 0 and 0.5 is allowed.
#' Default is 0.2.}
#' \item{p.rec.gen:}{numeric(1)\cr Probability one feature/gene is recombined.
#' Default is 0.6.}
#' \item{p.rec.use.orig:}{numeric(1)\cr Probability an elment of the indicator
#' to use the feature values of x.interest is recombined.
#' Default is 0.7.}
#' \item{lower:}{numeric\cr Vector of minimal values for numeric features. If NULL
#' (default) lower is extracted from input data specified in field 'data' of 'predictor'.}
#' \item{upper: }{numeric\cr Vector of maximal values for numeric features.
#' If NULL (default) upper is extracted from input data specified in field 'data' of
#' 'predictor'.}
#' \item{initialization: }{character(1)\cr Which initialization strategy should
#' be used, either 'random', 'icecurve', 'sd' or 'traindata'. See details for an
#' overview and explanation. Default is 'random'.}
#' \item{track.infeas: }{logical(1)\cr Whether to add a fourth objective that
#' evaluates the minimum Gower distance to observed data set. How many
#' nearest neighbors are considered is specified in the k and how
#' the distance to each neighbors is weighted is specified in weights. Default is TRUE.}
#' \item{k: }{integer(1)\cr How many nearest neighbors should be considered
#' for the fourth objective. See track.infeas for details. It is only used if
#' track.infeas is TRUE. Default is 1L.}
#' \item{weights: }{numeric(1)|numeric(k)\cr How the k nearest neighhbors of the observed dataset
#' should be weighted. The first value corresponds to the nearest neighbor.
#' It is either a single value or a vector of length k. The values should sum up to 1.
#' See track.infeas for details. It is only used if
#' track.infeas is TRUE. Default is NULL which means all neighbors are weighted equally as 1/k.}
#' \item{nr.solutions: }{integer(1)\cr How many 'best' counterfactuals should be kept.
#' Ranking of the counterfactuals is done by nondominated sorting and crowding distance sorting.
#' Default 10L.}
#' \item{features: }{character\cr Which features should be plotted.}
#' \item{row.ids: }{numeric\cr Which counterfactuals (row number) should be plotted.}
#' \item{grid.size: }{integer\cr The size of the grid for evaluating the predictions. Default 50L.}
#' \item{plot: }{logical(1)\cr Whether frequencies of feature changes should be plotted.}
#' \item{subset.zero: }{logical(1)\cr Whether features with no proposed changes should be omited.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{\code{clone()}}{[internal] Method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] Method to initialize the R6 object.}
#' \item{\code{subset_results(nr.solutions)}}{Returns a subset of Counterfactuals
#' as in Counterfactuals$results of the size of nr. solutions.}
#' \item{\code{explain(x.interest, target)}}{Method to set a new data point which to explain.}
#' \item{\code{plot()}}{Method to plot the Pareto front in 2-D. See \link{plot.Counterfactuals.}}
#' \item{\code{continue_search(generations)}}{Method to continue search
#' after run was already finished. Results are automatically updated in
#' Counterfactuals$results.}
#' \item{\code{get_hv()}}{Get dominated hypervolume of Counterfactual set
#' equal to fitness.dominatedHV of last row in cf$log.}
#' \item{\code{get_frequency()}}{Calculate frequency a feature got changed
#' over the returned set of Counterfactuals.}
#' \item{\code{plot_statistics()}}{Method to plot information of Counterfactuals$log
#' for evaluation of algorithm.}
#' \item{\code{plot_surface(features, row.ids)}}{Method to plot prediction surface given two features and
#' mark \code{x.interest} and its corresponding counterfactuals on the plot. If row.ids is NULL all
#' counterfactuals are plotted.}
#' \item{\code{plot_parallel(features, row.ids)}}{Method to plot a parallel plot of the feature changes.
#' If features is NULL all features are plotted and if row.ids is NULL all counterfactuals are plotted.}
#' }
#'
#' @details
#' Different initialization strategies for the candidates of the first population exist:
#' \describe{
#' \item{\code{random}}{randomly sample from numerical feature ranges and
#' discrete feature values of the given training data and randomly set some
#' feature values to the values of 'x.interest'.}
#' \item{\code{icecurve}}{Randomly sample the feature values of the initial
#' candidates as in the strategy 'random' but use the ice curve variance to
#' determine which feature values are the same as for \code{x.interest}.
#' A lower ICE curve variance of one feature results in a higher probability for keeping the
#' features values fixed to the value of \code{x.interest}.}
#' \item{\code{sd}}{Randomly sample numerical features from the feature ranges
#' that are limited by the standard deviation extracted from observed dataset. Discrete features
#' are sampled randomly as for the \code{random} strategy and some feature are randomly set to the
#' values of \code{x.interest}.}
#' \item{\code{traindata}}{Initialize the first population as the observed training data
#' points are best according to their ranks determined by nondominated sorting and crowding distance sorting.}
#'}
#'
#' @references
#' \describe{
#' \item{Dandl, S., Molnar, C., Binder, M., Bischl, B. (2020). Multi-Objective Counterfactual Explanations. Preprint on ArXiv.}
#' \item{Li, R., Emmerich, M.T., Eggermont, J., Bäck, T., Schütz, M., Dijkstra, J., Reiber, J.H. (2013).
#'  Mixed Integer Evolution Strategies for Parameter Optimization. Evolutionary Computation 21(1): 29–64}
#' \item{Binder, M., Moosbauer, J., Thomas, J., Bischl, B. (2019).
#' Multi-Objective Hyperparameter Tuning and Feature Selection using Filter Ensembles (2019), accepted at GECCO 2020}
#' \item{Bossek, J. (2017). ecr 2.0: A modular framework for evolutionary computation in r,
#' Proceedings of the Genetic and Evolutionary Computation Conference Companion,
#' GECCO '17, pp. 1187-1193.}{}
#' \item{Deb, K., Pratap, A., Agarwal, S. and Meyarivan, T. (2002). A fast and elitist multiobjective
#' genetic algorithm: Nsga-ii, IEEE Transactions on Evolutionary Computation
#' 6(2): 182-197.}{}
#' \item{Avila, S. L.,  Kraehenbuehl, L. and Sareni, B. (2006). A multi-niching
#' multi-objective genetic algorithm for solving complex multimodal problems,
#' OIPE, Sorrento, Italy.}{}
#' }
#'
#' @seealso
#' A different way to explain predictions: \link{LocalModel}, \link{Shapley}
#'
#' @examples
#' if (require("randomForest")) {
#' # First we fit a machine learning model on the Boston housing data
#' data("Boston", package  = "MASS")
#' rf =  randomForest(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#'
#' # Then we explain the prediction of the first instance with the
#' # Counterfactuals method
#' x.interest = X[1,]
#' target = 30
#' counterfactual = Counterfactuals$new(mod, x.interest = x.interest,
#' target = target, generations = 10)
#' counterfactual
#'
#' # Look at the results in a table
#' counterfactual$results
#' # Or as a plot
#' plot(counterfactual)
#' plot(counterfactual, labels = TRUE, nr.solutions = 10)
#'
#' # Explain another instance
#' counterfactual$explain(X[2,], target = target)
#' plot(counterfactual)
#'
#' # Counterfactuals() can only focus on one class, not multiple
#' # classes at a time
#' rf = randomForest(Species ~ ., data = iris)
#' X = iris[-which(names(iris) == "Species")]
#' mod = Predictor$new(rf, data = X, type = "prob", class = "setosa")
#'
#' # Then we explain the prediciton of the first instance
#' counterfactuals = Counterfactuals$new(mod, x.interest = X[1,], target = 0,
#' generations = 10)
#' counterfactuals$results
#' plot(counterfactuals)
#' }
NULL


#'@export
Counterfactuals = R6::R6Class("Counterfactuals",
  inherit = InterpretationMethod,
  public = list(
    x.interest = NULL,
    y.hat.interest = NULL,
    target = NULL,
    epsilon = NULL,
    fixed.features   = NULL,
    max.changed  = NULL,
    mu  = NULL,
    generations  = NULL,
    p.mut  = NULL,
    p.rec  = NULL,
    p.mut.gen  = NULL,
    p.mut.use.orig = NULL,
    p.rec.gen  = NULL,
    p.rec.use.orig = NULL,
    k = NULL,
    weights = NULL,
    track.infeas = NULL,
    initialization = NULL,
    lower = NULL,
    upper = NULL,
    log = NULL,
    initialize = function(predictor, x.interest = NULL, target = NULL,
      epsilon = NULL, fixed.features = NULL, max.changed = NULL,
      mu = 50, generations = 50, p.rec = 0.9, p.rec.gen = 0.7, p.rec.use.orig = 0.7,
      p.mut = 0.2, p.mut.gen = 0.5, p.mut.use.orig = 0.2, k = 1L, weights = NULL,
      lower = NULL, upper = NULL, initialization = "random",
      track.infeas = TRUE) {

      super$initialize(predictor = predictor)
      fixed.features = private$sanitize_feature(fixed.features, predictor$data$feature.names)

      # can be missing
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1,
        max.len = 2, any.missing = FALSE)
      if (!is.null(target) && all(sapply(target, is.infinite))) {
        stop("One element of target must be finite")
      }
      checkmate::assert_number(epsilon, null.ok = TRUE)
      checkmate::assert_integerish(max.changed, null.ok = TRUE, len = 1)
      checkmate::assert_numeric(lower, null.ok = TRUE, any.missing = FALSE)
      checkmate::assert_numeric(upper, null.ok = TRUE, any.missing = FALSE)

      # should exist
      checkmate::assert_integerish(mu, lower = 1)
      assert(
        checkInt(generations, lower = 0),
        checkList(generations, types = "function")
      )
      checkmate::assert_number(p.mut, lower = 0, upper = 1)
      checkmate::assert_number(p.rec, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.use.orig, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.use.orig, lower = 0, upper = 1)
      checkmate::assert_integerish(k, lower = 1)
      checkmate::assert_numeric(weights, null.ok = TRUE, lower = 0, upper = 1, len = k)
      checkmate::assert_logical(track.infeas)
      checkmate::assert_character(initialization)
      checkmate::assert_true(initialization %in% c("random", "sd", "icecurve", "traindata"))

      # assign
      self$target = target
      self$epsilon = epsilon
      self$max.changed = max.changed
      self$fixed.features = fixed.features
      self$mu = mu
      self$generations = generations
      self$p.mut = p.mut
      self$p.rec = p.rec
      self$p.mut.gen = p.mut.gen
      self$p.mut.use.orig = p.mut.use.orig
      self$p.rec.gen = p.rec.gen
      self$p.rec.use.orig = p.rec.use.orig
      self$k = k
      self$lower = lower
      self$upper = upper
      self$track.infeas = track.infeas
      self$initialization = initialization
      
      # Check if column names of x.interest and observed data are identical
      if(any(!(self$predictor$data$feature.names %in% colnames(x.interest)))) {
        stop("colnames of x.interest must be identical to observed data")
      }
      
      # Define parameterset
      private$param.set= ParamHelpers::makeParamSet(
        params = make_paramlist(rbind(predictor$data$get.x(), x.interest[predictor$data$feature.names]),
          lower = lower, upper = upper))

      # Extract info from input.data
      private$range = ParamHelpers::getUpper(private$param.set) -
        ParamHelpers::getLower(private$param.set)
      private$range[ParamHelpers::getParamIds(private$param.set)
        [ParamHelpers::getParamTypes(private$param.set) == "discrete"]]  = NA
      private$range = private$range[predictor$data$feature.names]
      private$sdev = apply(Filter(is.numeric, predictor$data$get.x()), 2, sd)

      # Set x.interest
      if (!is.null(x.interest) & !is.null(target)) {
        private$set_x_interest(x.interest)
        private$run()
      }
    },
    explain = function(x.interest, target) {
      checkmate::assert_numeric(target, min.len = 1,
        max.len = 2, any.missing = FALSE, null.ok = FALSE)
      if (all(sapply(target, is.infinite))) {
        stop("One element of target must be finite")
      }
      checkmate::assert_true
      self$target = target
      private$set_x_interest(x.interest)
      private$flush()
      private$run()
      return(self)
    },
    subset_results = function(nr.solutions = 10L, epsilon = Inf) {
      cfexps = self$results$counterfactuals
      if (nr.solutions >= nrow(cfexps)) {
        warning("nr.solutions out of range, it was set to the number of solutions in self$results")
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
          idx = private$hv_contribution(t(cfexps[, obj.nams]), nr.solutions = nr.solutions, 
            best = best)
          results.subset = self$results
          results.subset$counterfactuals = results.subset$counterfactuals[idx, ]
          rownames(results.subset$counterfactuals) = NULL
          results.subset$counterfactuals.diff = results.subset$counterfactuals.diff[idx,]
          rownames(results.subset$counterfactuals.diff) = NULL
          return(results.subset)
      },
    plot_statistics = function() {
      min.obj = c("generation", paste(private$obj.names, "min", sep = "."))
      mean.obj = c("generation", paste(private$obj.names, "mean", sep = "."))
      eval = c("generation", "fitness.domHV")
      nameList = list(min.obj, mean.obj, eval)
      log = self$log
      p = lapply(nameList, function(nam) {
        df = reshape2::melt(log[,nam] , id.vars = "generation", variable.name = "legend")
        singlep = ggplot(df, aes(generation, value)) +
          geom_line(aes(colour = legend)) +
          theme_bw() +
          ylab("value")
        return(singlep)
      })
      p
    },
    plot_hv = function(ylim = NULL) {
      log = self$log
      if (is.null(ylim)) {
        ylim = c(min(log$fitness.domHV), max(log$fitness.domHV))
      }
      singlep = ggplot(log, aes(generation, fitness.domHV)) +
        geom_line() +
        theme_bw() +
        ylim(ylim[1], ylim[2]) +
        ylab("dominated hypervolume")
      return(singlep)
    },
    plot_search = function() {
      pf.over.gen = lapply(seq_len(nrow(self$log)),
        FUN = function(i) {
          pf.gen = as.data.frame(t(private$ecrresults$log$env$pop[[i]]$fitness))
          names(pf.gen) = paste("y", 1:3, sep = "")
          pf.gen$generation = i-1
          pf.gen
        })

      pf.over.gen.df = do.call(rbind, pf.over.gen)

      pfPlot = ggplot(data = pf.over.gen.df, aes(x=y1, y=y2, alpha = generation)) +
        geom_point(col = "black")+
        theme_bw() +
        xlab(private$obj.names[1]) +
        ylab(private$obj.names[2])

      pfPlot
    },
    plot_parallel = function(features = NULL, row.ids = NULL, nr.solutions = NULL, type = "parallel", epsilon = NULL, plot.x.interest = TRUE) {
      assert_true(type %in% c("parallel", "spider"))
      assert_character(features, null.ok = TRUE, min.len = 2L)
      assert_numeric(row.ids, null.ok = TRUE)
      assert_numeric(nr.solutions, null.ok = TRUE)

      if (is.null(features)) {
        features = self$predictor$data$feature.names
      }
      if (type == "spider" & length(features) < 3L) {
        stop("The number of features must be 3 or more for a spider plot.")
      }

      if (is.null(nr.solutions)) {
        cf = self$results$counterfactuals
      } else {
        cf = self$subset_results(nr.solutions)$counterfactuals
      }

      if (!is.null(row.ids)) {
        cf = cf[row.ids,]
      } else {
        row.ids = 1:nrow(cf)
      }
      cf = cf[, features]
      nrrows = nrow(cf)
      if (plot.x.interest) {
        cf = rbind(cf, self$x.interest[, features]) ## add x.original
      }
      char.id = sapply(cf, is.character)
      cf[, char.id] = sapply(cf[, char.id], as.numeric)

      factor.id = sapply(cf, is.factor)
      cf[, factor.id] = sapply(cf[, factor.id], unclass)
      
      
      mycolors = gray.colors(nrrows, start = 0.2, end = 0.8, gamma = 2.2)
        
      if (plot.x.interest) {
        mycolors = c(mycolors, "blue")
      }
      names(mycolors) <- rownames(cf)

      if (!is.null(epsilon)) {
        cf = cf[cf$dist.target<=epsilon, ]
      }
      if (type == "parallel") {
        cf$row.names = rownames(cf)
        param.set = self$.__enclos_env__$private$param.set
        val = getValues(param.set)
        val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
        colscale = scale_colour_manual(name = "rows",values = mycolors)
        p = GGally::ggparcoord(cf, columns = 1:length(features), groupColumn = "row.names",
          scale = "uniminmax", showPoints = TRUE) +
          theme_bw() +
          ylim(c(-0.1, 1.1)) +
          theme(legend.position="none") +
          ylab("Scaled feature values") +
          geom_point(aes()) +
          colscale
        for (i in 1:length(features)) {
          if (features[i] %in% names(val.l)) {
            max = tail(val.l[[features[i]]], 1)
            min = head(val.l[[features[i]]], 1)
          } else {
            max = round(max(cf[features[i]]), 3)
            min = round(min(cf[features[i]]), 3)
          }
          p = p + geom_text(aes_string(label = max, x = i-0.07, y = 1.05), colour = "black")
          p = p + geom_text(aes_string(label = min, x = i-0.07, y = -0.05), colour = "black")
        }
      }
      if (type == "spider") {
        p = radarchart(cf, maxmin = FALSE, plwd = c(rep(1, nrow(cf)-1), 2), axistype = 2,
          plty = 1,
          pcol = mycolors,
          axislabcol = "black")
      }
      return(p)
    },
    plot_surface = function (features = NULL, grid.size = 50L, epsilon = NULL) {
      assert_character(features, null.ok = TRUE, min.len = 2L)
      assert_integerish(grid.size, len = 1L)
      assert_numeric(epsilon, len = 1L, null.ok = TRUE)
      if (is.null(features)) {
        features = self$predictor$data$feature.names
      }
      if (length(features) != 2L) {
        stop("The number of features must be 2.")
      }

      cf = self$results$counterfactuals

      change.id = which(rowSums(self$results$counterfactuals.diff[, features] != 0) == cf$nr.changed)
      instances = cf[change.id, ]

      if (!is.null(epsilon)) {
        instances = instances[instances$dist.target<=epsilon, ]
      }
      res = get_ice_curve_area(instance = self$x.interest, features = features,
        predictor = self$predictor, param.set = private$param.set,
        grid.size = grid.size)
      x.interest = cbind(self$x.interest, pred = self$y.hat.interest)
      plot_ice_curve_area(res, predictor = self$predictor, instances,
        x.interest = x.interest)
    },
    continue_search = function(generations) {
      private$ecrresults = continueEcr(ecr.object = private$ecrresults, generations = generations)
      results = private$evaluate(private$ecrresults)
      private$dataDesign = results
      private$qResults = private$run.prediction(results)
      # self$generations = self$generations + generations
      self$results = private$aggregate()
    },
    get_hv = function() {
      return(tail(self$log$fitness.domHV, 1))
    },
    get_frequency = function(plot = FALSE, subset.zero = FALSE) {
      diff = self$results$counterfactuals.diff
      diff = diff[!(names(diff) %in% c(private$obj.names, "pred"))]
      freq = colSums(diff != 0)/nrow(diff)
      freq = freq[order(freq, decreasing = TRUE)]
      if (subset.zero) {
        freq = freq[which(freq != 0)]
      }
      if (plot) {
        barplot(freq, ylim = c(0, 1), ylab = "Relative frequency")
      }
      return(freq)
    }
  ),
  private = list(
    #featurenames = NULL,
    range = NULL,
    ref.point = NULL,
    sdev = NULL,
    param.set= NULL,
    param.set.init = NULL,
    ecrresults = NULL,
    obj.names = NULL,
    set_x_interest = function(x.interest) {
      assert_data_frame(x.interest, any.missing = FALSE, all.missing = FALSE,
        nrows = 1, null.ok = FALSE)
      x.interest = x.interest[setdiff(colnames(x.interest), self$predictor$data$y.names)]
      if (any(colnames(x.interest) != self$predictor$data$feature.names)) {
        warning("columns of x.interest were reordered according to predictor$data$feature.names")
        x.interest = x.interest[, self$predictor$data$feature.names]
      }
      x.interest.list = as.list(x.interest)
      x.interest.list$use.orig = rep(TRUE, ncol(x.interest))
      if (!ParamHelpers::isFeasible(private$param.set, x.interest.list)) {
        stop(paste("Feature values of x.interest outside range of observed data",
          "of predictor or given arguments lower or upper. Please modify arguments",
          "lower or upper accordingly."))
      }
      self$y.hat.interest = self$predictor$predict(x.interest)[1,]
      self$x.interest = x.interest
    },
    flush = function() {
      private$ecrresults = NULL
      self$results = NULL
      private$finished = FALSE
    },
    intervene = function() {

      # Define reference point for hypervolumn computation
      private$ref.point = c(min(abs(self$y.hat.interest - self$target)),
        1, ncol(self$x.interest))
      private$obj.names = c("dist.target", "dist.x.interest", "nr.changed")
      n.objectives = 3L
      if (self$track.infeas) {
        private$ref.point = c(private$ref.point, 1)
        private$obj.names = c(private$obj.names, "dist.train")
        n.objectives = n.objectives + 1L
      }
      if (is.infinite(private$ref.point[1])) {
        pred = self$predictor$predict(self$predictor$data$get.x())
        private$ref.point[1] = diff(c(min(pred), max(pred)))
      }

      # Create fitness function with package smoof
      fn = smoof::makeMultiObjectiveFunction(
        has.simple.signature = FALSE, par.set = private$param.set,
        n.objectives = n.objectives,
        noisy = TRUE, ref.point = private$ref.point,
        fn = function(x, fidelity = NULL) {
          fitness_fun(x, x.interest = self$x.interest, target = self$target,
            predictor = self$predictor, train.data = self$predictor$data$get.x(),
            range = private$range, track.infeas = self$track.infeas,
            k = self$k, weights = self$weights)
        })

      fn = mosmafs::setMosmafsVectorized(fn)

      # Initialize population based on x.interest, param.set

      # Strategy 1: Use sd to initialize numeric features (default FALSE)
      if (self$initialization == "sd" && length(private$sdev) > 0) {
        lower = self$x.interest[names(private$sdev)] - private$sdev
        upper = self$x.interest[names(private$sdev)] + private$sdev
        if (nrow(lower)>0 && nrow(upper)>0) {
          lower.ps = pmax(ParamHelpers::getLower(private$param.set), lower)
          upper.ps = pmin(ParamHelpers::getUpper(private$param.set), upper)
          lower.ps[names(self$lower)] = self$lower
          upper.ps[names(self$upper)] = self$upper
          private$param.set.init = ParamHelpers::makeParamSet(params = make_paramlist(
            self$predictor$data$get.x(),
            lower = lower.ps,
            upper = upper.ps))
        }
      } else {
        # Strategy 2: Randomly sample
        private$param.set.init = private$param.set
      }
      # Strategy 3: Use training data as first population
      if (self$initialization == "traindata") {
        train.data = as.data.frame(private$dataSample, stringsAsFactors = FALSE)

        train.data = train.data[head(sample.int(nrow(train.data)), 200), ]
        for (rx in seq_len(nrow(train.data))) {
          use.orig.feats = sample.int(length(self$x.interest), 1) - 1
          use.orig = seq_along(self$x.interest) <= use.orig.feats
          use.orig = sample(use.orig)
          train.data[rx, use.orig] = self$x.interest[use.orig]
        }
        fitness.train = fn(as.data.frame(train.data))
        if (nrow(train.data) > self$mu) {
          idx = select_nondom(fitness.train, self$mu, train.data , epsilon = self$epsilon,
            extract.duplicates = TRUE)
          train.sub = train.data[idx,]
        } else {
          train.sub = train.data
        }
        # Initialize also a use.orig vector
        init.df = cbind(train.sub, use.orig = train.sub == self$x.interest[rep(row.names(self$x.interest),
          nrow(train.sub)),])
        init.df = BBmisc::convertDataFrameCols(init.df, factors.as.char = TRUE)
        initial.pop = ParamHelpers::dfRowsToList(init.df, par.set = private$param.set.init)
        # necessity to manually set factors to characters
        initial.pop = lapply(initial.pop, function(ipop) {
          new.ipop = lapply(ipop, function(i) {
            if(is.factor(i)) i = as.character(i)
            return(i)
          })
          return(new.ipop)
        })
        # Add random samples if number of training data obs < mu
        n.missing = self$mu - length(initial.pop)
        if (n.missing > 0) {
          additional.pop = ParamHelpers::sampleValues(private$param.set.init, n.missing,
            discrete.names = TRUE)
          initial.pop = c(initial.pop, additional.pop)
        }
      } else {
        initial.pop = ParamHelpers::sampleValues(private$param.set.init, self$mu,
          discrete.names = TRUE)
      }

      # Strategy 4: Use ice curve variance to initialize use.original vector
      # while features are initialized randomly
      if (self$initialization == "icecurve") {
        ice.var = get_ICE_var(self$x.interest, self$predictor, private$param.set)
        prob.use.orig = 1 - mlr::normalizeFeatures(as.data.frame(ice.var),
          method = "range", range = c(0.01, 0.99))
        ilen = length(initial.pop[[1]]$use.orig)
        distribution = function() rbinom(n = ilen, size = ilen,
          prob = t(prob.use.orig))
        initial.pop = initSelector(initial.pop, vector.name = "use.orig",
          distribution = distribution)
      }
      i = sapply(self$x.interest, is.factor)
      x.interest = self$x.interest
      x.interest[i] = lapply(self$x.interest[i], as.character)

      initial.pop = lapply(initial.pop, function(x) {
        x = transform_to_orig(x, x.interest, delete.use.orig = FALSE,
          fixed.features = self$fixed.features, max.changed = self$max.changed)
      })


      # Define operators based on parameterset private$param.set
      # Messages can be ignored (only hint that operator was initialized, although
      # no feature of the corresponding type exists)
      sdev.l = sdev_to_list(private$sdev, private$param.set)
      # Use mutator based on conditional distributions if functions are given
      # it is the case if self$predictor$conditionals is NOT logical
      if (is.logical(self$predictor$conditionals)) {
        single.mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
          numeric = ecr::setup(mosmafs::mutGaussScaled, p = self$p.mut.gen, sdev = sdev.l$numeric),
          integer = ecr::setup(mosmafs::mutGaussIntScaled, p = self$p.mut.gen, sdev = sdev.l$integer),
          discrete = ecr::setup(mosmafs::mutRandomChoice, p = self$p.mut.gen),
          logical = ecr::setup(ecr::mutBitflip, p = self$p.mut.gen),
          use.orig = ecr::setup(mosmafs::mutBitflipCHW, p = self$p.mut.use.orig),
          .binary.discrete.as.logical = TRUE))
        mutator = ecr::makeMutator(function(ind) {
          transform_to_orig(single.mutator(ind), x.interest, delete.use.orig = FALSE,
            fixed.features = self$fixed.features, max.changed = self$max.changed)
        }, supported = "custom")
      } else {
        single.mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
          numeric = ecr::setup(mosmafs::mutGaussScaled, p = self$p.mut.gen, sdev = sdev.l$numeric),
          integer = ecr::setup(mosmafs::mutGaussIntScaled, p = self$p.mut.gen, sdev = sdev.l$integer),
          discrete = ecr::setup(mosmafs::mutRandomChoice, p = self$p.mut.gen),
          logical = ecr::setup(ecr::mutBitflip, p = self$p.mut.gen),
          use.orig = ecr::setup(mosmafs::mutBitflipCHW, p = self$p.mut.use.orig),
          .binary.discrete.as.logical = TRUE))

        mutator = ecr::makeMutator(function(ind) {
          # # Transform use.original
          ind$use.orig = as.logical(mosmafs::mutBitflipCHW(as.integer(ind$use.orig), p = self$p.mut.use.orig)) #SD
          # Transform as before
          ind = transform_to_orig(ind, x.interest, delete.use.orig = FALSE, #SD single.mutator(ind)
            fixed.features = self$fixed.features, max.changed = self$max.changed)
          ind.short = ind
          ind.short$use.orig = NULL
          # Select features to mutate:
          affect = NA
          affect = runif(length(ind.short)) < self$p.mut.gen
          cols.nams = names(ind.short)
          affected.cols = cols.nams[affect & !ind$use.orig]
          # Shuffle mutation order
          affected.cols = sample(affected.cols)
          if (length(affected.cols != 0)) {
            for (a in affected.cols) {
              X = data.table::as.data.table(data.frame(ind.short, stringsAsFactors = FALSE))
              single.mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
                .params.group = c(a),
                group = ecr::setup(mutConDens, X = X,
                  pred = self$predictor, param.set = private$param.set),
                use.orig = mutInd, numeric = mutInd, integer = mutInd, discrete = mutInd,
                logical = mutInd, .binary.discrete.as.logical = FALSE))
              ind = single.mutator(ind)
            }
          }
          return(ind)
        }, supported = "custom")
      }

      recombinator = suppressMessages(mosmafs::combine.operators(private$param.set,
        numeric = ecr::setup(ecr::recSBX, p = self$p.rec.gen),
        integer = ecr::setup(mosmafs::recIntSBX, p = self$p.rec.gen),
        discrete = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
        logical = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
        use.orig = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.use.orig),
        .binary.discrete.as.logical = TRUE))

      overall.recombinator <- ecr::makeRecombinator(function(inds, ...) {
        inds <- recombinator(inds)
        do.call(ecr::wrapChildren, lapply(inds, function(x) {
          transform_to_orig(x, x.interest, delete.use.orig = FALSE,
            fixed.features = self$fixed.features, max.changed = self$max.changed)
        }))
      }, n.parents = 2, n.children = 2)

      parent.selector = mosmafs::selTournamentMO

      survival.selector = ecr::setup(select_nondom,
        epsilon = self$epsilon,
        extract.duplicates = TRUE)

      # Extract algorithm information with a log object
      log.stats = list(fitness = lapply(
        seq_len(n.objectives),
        function(idx) {
          list(min = function(x) min(x[idx, ]), mean = function(x) mean(x[idx, ]))
        }))
      names(log.stats$fitness) = sprintf("obj.%s", seq_len(n.objectives))
      log.stats$fitness = unlist(log.stats$fitness, recursive = FALSE)
      log.stats$fitness = c(log.stats$fitness,
        list(
          n.row = function(x) sum(ecr::nondominated(x))
        ))

      # Compute counterfactuals
      ecrresults = mosmafs::slickEcr(fn, lambda = self$mu, population = initial.pop,
        mutator = mutator,
        recombinator = overall.recombinator, generations = self$generations,
        parent.selector = parent.selector,
        survival.strategy = select_diverse,
        survival.selector = survival.selector,
        p.recomb = self$p.rec,
        p.mut = self$p.mut, log.stats = log.stats)
      private$ecrresults = ecrresults
      private$evaluate(ecrresults)
    },
    evaluate = function(ecrresults) {

      # Settings for either 3 or 4 objectives
      if (self$track.infeas) {
        id.cols = 9L
        n.objectives = 4L
      } else {
        id.cols = 7L
        n.objectives = 3L
      }

      # Extract results
      # Only consider new candidates
      pop = getPopulations(ecrresults$log.newinds)

      getPopFitness  =  function(pop, dim) {
        vapply(pop, attr, numeric(dim), "fitness")
      }

      combineResults  =  function(pop1, pop2, dim) {
        fullpop  =  c(pop1, pop2)
        fullpopfitness  =  getPopFitness(fullpop, dim)
        fullpop  =  fullpop[ecr::nondominated(fullpopfitness)]
      }

      accpops  =  Reduce(function(pop1, pop2) combineResults(pop1, pop2, n.objectives),
        lapply(pop, get, x = "population"),
        accumulate = TRUE)

      # calculate hv for each generation
      hvdev  =  vapply(accpops, function(pop) {
        ecr::computeHV(getPopFitness(pop, n.objectives), private$ref.point)
      }, 0)
      # add hv to a log object
      log = mosmafs::getStatistics(ecrresults$log)
      log$fitness.domHV = hvdev
      sum.nam = paste(rep(private$obj.names, each = 2),
        c("min", "mean"), sep = ".")
      nam = c("generation", sum.nam)
      names(log)[1:id.cols] = nam
      evals = mosmafs::collectResult(ecrresults)$evals
      log$evals = evals

      log = log[c("generation", "state", "evals", nam[2:id.cols], "fitness.domHV",
        "fitness.n.row")]
      self$log = log

      # only return last population
      finalpop  =  tail(accpops, 1)[[1]]
      fit = data.frame(t(getPopFitness(finalpop, n.objectives)))
      names(fit) = private$obj.names
      finalpop = mosmafs::listToDf(finalpop, private$param.set)
      finalpop[, grepl("use.orig", names(finalpop))] = NULL

      # Delete duplicates
      select.id = which(!duplicated(finalpop))
      fit = fit[select.id,]
      finalpop = finalpop[select.id, ]

      result = cbind(finalpop, fit)

      # # Remove counterfactuals that do not satisfy the soft constraint epsilon
      # if (!is.null(self$epsilon)) {
      #   feas.id = result$dist.target <= self$epsilon
      #   if (any(feas.id)) {
      #     result = result[feas.id,]
      #   } else {
      #     message("no counterfactual found that satisfies the constraint epsilon.")
      #   }
      # }
      return(result)
    },
    aggregate = function() {
      # Generate a results object as a list
      pareto.set = private$dataDesign[, !(names(private$dataDesign) %in% private$obj.names)]
      pareto.front = private$dataDesign[, private$obj.names]

      pareto.set.diff = get_diff(pareto.set, self$x.interest)
      pred = data.frame(private$qResults)
      names(pred) = "pred"

      pareto.set.pf = cbind(private$dataDesign, pred = pred)
      pareto.set.pf = pareto.set.pf[order(pareto.set.pf$dist.target, pareto.set.pf$nr.changed,
        pareto.set.pf$dist.x.interest),]
      rownames(pareto.set.pf) = NULL
      pareto.set.diff.pf = cbind(pareto.set.diff, pareto.front, pred = pred)
      pareto.set.diff.pf = pareto.set.diff.pf[order(pareto.set.diff.pf$dist.target, pareto.set.diff.pf$nr.changed,
        pareto.set.diff.pf$dist.x.interest),]
      rownames(pareto.set.diff.pf) = NULL
      
      # Extract counterfactuals equal to x.interest
      xinterest.id = which(pareto.set.diff.pf$nr.changed == 0)
      
      if (nrow(pareto.set.pf) > 1 && length(xinterest.id) > 0) {
        pareto.set.pf = pareto.set.pf[-xinterest.id,]
        pareto.set.diff.pf = pareto.set.diff.pf[-xinterest.id,]
      }
      
      results = list()
      results$counterfactuals = pareto.set.pf
      results$counterfactuals.diff = pareto.set.diff.pf
      
      return(results)
    },
    generatePlot = function(labels = FALSE, decimal.points = 3, nr.solutions = NULL,
      nr.changed = NULL) {
      assert_logical(labels)
      assert_integerish(decimal.points, null.ok = !labels)
      assert_int(nr.solutions, null.ok = TRUE)
      assert_integerish(nr.changed, null.ok = TRUE)
      results_diff = self$results$counterfactuals.diff
      if (!is.null(nr.solutions)) {
        results_diff = self$subset_results(nr.solutions)$counterfactuals.diff
      }
      if (!is.null(nr.changed)) {
        results_diff = results_diff[results_diff$nr.changed %in% nr.changed, ]
      }
      pf = results_diff[, private$obj.names]

      p = ggplot(data = pf, aes(x=dist.target, y=dist.x.interest,
        color = as.factor(nr.changed))) +
        geom_point() +
        xlab("dist target") +
        ylab("dist x.interest") +
        theme_bw() +
        guides(color=guide_legend(title="nr changed"))

      if (labels) {
        diffs = results_diff[, !(names(results_diff) %in% c(private$obj.names, "pred", "X1"))]
        labels = c()
        for(i in 1:nrow(diffs)) {
          names = names(diffs[i,])[diffs[i,] != 0]
          lab = as.data.frame(diffs[i, names])
          lab = paste(paste(names, round_df(lab, decimal.points)), collapse = " & ")
          labels = c(labels, lab)
        }
        p = p + ggrepel::geom_label_repel(aes(label = labels),
          box.padding   = 0.35,
          point.padding = 0.4,
          show.legend = FALSE)
      }
      p
    },
    hv_contribution = function(fitness, nr.solutions, best) {
      ref.point = c(0.5, 1, max(fitness[3,])+1, 1)
      for (i in seq_len(nr.solutions)) {
        best = c(best, which.max(apply(fitness, 2, 
          function(obs) computeHV(cbind(fitness[,best], obs), ref.point = ref.point)
        )))
      }
      return(best)
    }, 
    sanitize_feature = function(fixed.features, feature.names) {
      if (is.numeric(fixed.features)) {
        assert_integerish(fixed.features, lower = 1, upper = length(feature.names),
          null.ok = TRUE)
        fixed.features = feature.names[fixed.features]
      }
      assert_character(fixed.features, null.ok = TRUE, unique = TRUE)
      stopifnot(all(fixed.features %in% feature.names))
      fixed.features
    }
  ))


#' Plot Counterfactuals
#'
#' \code{plot.Counterfactuals()} plots the Pareto front in 2-D of the found Counterfactuals.
#' The x-axis displays the distance of the prediction to the desired prediction.
#' The y-axis displays the distance of the counterfactual to the original datapoint x.interest.
#' The colouring displays how many features were changed.
#'
#' @format \code{\link{R6Class}} object.
#' @section Arguments:
#' \describe{
#' \item{labels:}{logical(1)\cr Whether labels with difference to feature values of
#' x.interest should be plotted. Default is FALSE.}
#' \item{decimal.points:}{integer(1)\cr Number of decimal places used. Default is 3.}
#' \item{nr.solutions:}{integer(1)\cr Number of solutions showed. Default NULL means,
#' all solutions are showed.}
#' \item{nr.changed:}{integer(1)\cr Plot only counterfactuals with certain number of
#' changed features. Default is NULL.}
#' }
#' @return ggplot2 plot object
#' @seealso
#' \link{Counterfactuals}
#' @examples
#' \dontrun{
#' if (require("randomForest")) {
#' data("Boston", package  = "MASS")
#' Boston = Boston
#' set.seed(1000)
#' rf =  randomForest(medv ~ ., data = Boston)
#' X = Boston[-which(names(Boston) == "medv")]
#' mod = Predictor$new(rf, data = X)
#'
#' # Then we calculate Counterfactuals for the first instance
#' x.interest = X[1,]
#' mod$predict(x.interest)
#' target = 30
#' cf = Counterfactuals$new(mod, x.interest = x.interest, target = target,
#'   mu = 50, generations = 60)
#'
#' # The results can be plotted
#' plot(cf)
#' plot(cf, labels = TRUE, nr.solutions = 10)
#' }
#' }
#' @export
plot.Counterfactuals = function(object, labels = FALSE, decimal.points = 3, nr.solutions = NULL, nr.changed = NULL) {
  object$plot(labels = labels, decimal.points = decimal.points, nr.solutions = nr.solutions,
    nr.changed = nr.changed)
}

#' @title Calculate frequency of altered features in final solution set
#'
#' @description Identify leverages that alter prediction to desired target
#'  over multiple datapoints. Leverages are identified by calculating
#'  the frequency a feature was altered within the set of the
#'  final calculated counterfactuals.
#'
#' @section Arguments:
#' \describe{
#' \item{counterfactual: }{(Counterfactuals)\cr Instance of class
#' Counterfactual to extract
#' dataset and target, if not needed, as well as all the parameters}
#' \item{target: }{(numeric(1)|numeric(2))\cr Desired outcome either a
#' single numeric or
#' a vector of two numerics, to define a desired interval of outcome.}
#' \item{obs: }{(data.frame) Data frame to use to identify leverages
#' by calculating counterfactuals for them}
#' \item{row.ids }{(integer) Rows with the specific row.ids are extracted
#' from the input data defined in the predictor field of the
#' Counterfactuals class. The subset is used to identify leverages.}
#' \item{plot: }{(logical(1)) Whether to plot the frequency over all
#' observations}
#' }
#'
#' @export
calculate_frequency_wrapper = function(counterfactual, target = NULL, obs = NULL,
  row.ids = NULL, plot = FALSE) {
  assert_data_frame(obs, null.ok = TRUE)
  assert_integerish(row.ids, null.ok = TRUE)
  assert_numeric(target, min.len = 1, max.len = 2, null.ok = TRUE)
  if (is.null(counterfactual$target) & is.null(target)) {
    stop("target not specified")
  }

  df = counterfactual$predictor$data$get.x()
  if (!is.null(obs)) {
    df = obs
  }
  if (!is.null(row.ids)) {
    df = df[row.ids,]
  }
  if(is.null(target)) {
    target = counterfactual$target
  }

  df = as.data.frame(df)
  freq = by(df, 1:nrow(df), function(row) {
    counterfactual = counterfactual$explain(row, target)
    counterfactual$get_frequency()
  })
  freq = do.call(rbind, freq)

  average_freq = colMeans(freq)

  if(plot) {
    barplot(average_freq, ylim = c(0, 1), ylab = "Relative frequency")
  }
  return(average_freq)

}


get_ice_curve_area = function (instance, features, predictor, param.set, grid.size) {
  instance =  instance[, 1:predictor$data$n.features]
  min.max = as.data.frame(rbind(getLower(param.set),
    getUpper(param.set)))

  val = getValues(param.set)
  val$use.orig = NULL
  val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
  values = c(as.list(min.max), val.l)


  grids = sapply(features, function(feat) {
    as.data.frame(equidistant.grid(values[[feat]], grid.size))
  })
  expgrid = expand.grid(a = grids[[1]], b = grids[[2]])
  colnames(expgrid) = features

  instance = instance[, !names(instance) %in% features]
  instance.df = instance[rep(row.names(instance), nrow(expgrid)), ]

  grid.df = cbind.data.frame(instance.df, expgrid)

  # predict outcomes
  pred = predictor$predict(newdata = grid.df)[[1]]
  cbind(expgrid, pred)
}

plot_ice_curve_area = function(grid, predictor, instance = NULL, x.interest) {
  nams = names(grid)[1:2]
  train.data = data.frame(predictor$data$get.x())

  if (all(predictor$data$feature.types[nams] %in% "numerical") ||
      all(predictor$data$feature.types[nams] %in% "categorical")) {
    p = ggplot(train.data, mapping = aes_string(x = nams[1],
      y = nams[2]))  +
      geom_point(color = "white") +
      theme_bw() +
      geom_tile(mapping = aes_string(x = nams[1], y = nams[2], fill = "pred"), data = grid) +
      stat_contour(mapping = aes_string(x = nams[1], y = nams[2], z = "pred"), data = grid, colour = "white") +
      metR::geom_text_contour(mapping = aes_string(x = nams[1], y = nams[2], z = "pred"),
        data = grid, colour = "white")
    if (nrow(instance) > 0) {
      p = p + geom_point(data = instance, aes_string(x=nams[1], y=nams[2]), colour = "black")
    }
    p = p + geom_point(data = x.interest, aes_string(x = nams[1], y = nams[2]), colour = "white")
    p = ggExtra::ggMarginal(p, type = "histogram")
  } else {
    train.data$pred = predictor$predict(train.data)[,1]
    categorical.feature = nams[predictor$data$feature.types[nams] == "categorical"]
    numerical.feature = setdiff(nams[1:2], categorical.feature)
    p = ggplot(train.data, mapping = aes_string(x = numerical.feature, y = "pred")) +
      geom_point(color = "white") +
      geom_line(data = grid, mapping = aes_string(x = numerical.feature,
        y = "pred", group = categorical.feature, color = categorical.feature)) +
      # geom_text(label = "counterfactual", aes(x = x.interest[1,numerical.feature]+8, y = x.interest$pred),
      #   colour = "gray") +
      theme_bw()
    if (nrow(instance) > 0) {
      p = p +  geom_point(data = instance, aes_string(x = numerical.feature, y = "pred"), colour = "black")
    }
    p = p + geom_point(aes(x = x.interest[1,numerical.feature], y = x.interest$pred), colour = "gray")
    p = ggExtra::ggMarginal(p, type = "histogram", margins = "x")
  }
  return(p)
}


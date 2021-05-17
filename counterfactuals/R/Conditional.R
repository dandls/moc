# For a grid (grid.dat) of features (param features) creates a blown up
# dataset with the marginals of features not in 'features'.
# The samples (n.sample.dist number of samples) for the marginals are drawn from dist.dat.
#            If n.sample.dist is not set, the whole cartesian product between grid.dat and dist.dat is built
# grid.dat only needs to contain the columns which are fixed. Decide here which grid points should be used.
# dist.dat needs to contain all columns
Conditional = R6Class(
  public = list(
    feature = NULL,
    data = NULL,
    model = NULL,
    ctrl = NULL,
    initialize = function(data, feature, ctrl = ctree_control()) {
      self$data = data
      self$feature = feature
      self$ctrl = ctrl
      private$fit_conditional()
    },
    csample_data = function(X, size){
      cmodel = self$model #SD
      X_nodes = self$cnode(X)
      if (is.null(private$data_nodes)) {
        private$data_nodes = self$cnode(self$data)
      }
      xj_samples = lapply(1:nrow(X), function(i) {
        node = X_nodes[i, "node"]
        data_ids = which(private$data_nodes$node == node)
        data_ids = setdiff(data_ids, i)
        data_ids_sample = data_ids[sample.int(length(data_ids), size = size, replace = TRUE)]
        xj = self$data[data_ids_sample, self$feature, with = FALSE]
        data.frame(t(xj))
      })
      data.table::rbindlist(xj_samples)

    },
    csample_parametric = function(X, size){
      cmodel = self$model
      x = self$data[[self$feature]]
      if (class(self$data[[self$feature]]) %in% c("character", "factor")) {
        x = unique(x)
      } else if (class(self$data[[self$feature]]) == "integer") {
        len = min(max(x)- min(x)+1, 100)
        xgrid = seq.int(min(x), max(x), length.out = len)
       } else {
        xgrid = seq(from = min(x), to = max(x), length.out = 100)
      }
      dens = self$cdens(X, xgrid)
      xj_samples = lapply(1:nrow(X), function(irow) {
        dens_i = dens[dens$.id.dist == irow, , drop = FALSE]
        xj = dens_i[[self$feature]][sample.int(nrow(dens_i), size = size, prob = dens_i[[".dens"]], replace = TRUE)]
        data.frame(t(xj))
      })
      data.table::rbindlist(xj_samples)
    },
    csample = function(X, size, type = "parametric"){
      assert_number(size, lower = 1)
      assert_character(self$feature)
      assert_data_table(X)
      assert_choice(type, c("data", "parametric"))
      if (type == 'parametric') {
        self$csample_parametric(X, size)
      } else {
        self$csample_data(X, size)
      }
    },
    cdens = function(X, xgrid = NULL){
      cmodel = self$model
      if (inherits(cmodel, "trafotree")) {
        if (class(self$data[[self$feature]]) != "integer") {
          probs.m = predict(cmodel, newdata = X, type = "logdensity", q = xgrid)
          probs.m = apply(probs.m, 2, function(col) {
            col = exp(col - max(col))
            col / sum(col)
          })
          densities = reshape2::melt(probs.m)$value
          densities = data.table::data.table(.dens = densities, .id.dist = rep(1:nrow(X), each = length(xgrid)),
            feature = rep(xgrid, times = nrow(X)))
        } else {
          if (is.null(private$data_nodes)) {
            private$data_nodes = self$cnode(self$data)
          }
          X_nodes = self$cnode(X)
          len = min(max(xgrid)- min(xgrid)+1, 100)
          probs.m = lapply(1:nrow(X), function(i) {
            node = X_nodes[i, "node"]
            data_ids = which(private$data_nodes$node == node)
            vec = data.frame(self$data)[data_ids, self$feature]
            dens = density(vec, n = len, from = min(xgrid), to = max(xgrid))
            prob.df = data.frame(cbind(.dens = dens$y, .id.dist = i, feature = dens$x))
          })
          densities = do.call("rbind", probs.m)
          ## might not always work 
          # probs.m = diff(predict(cmodel, newdata = X, type = "distribution", q = xgrid))
          # densities = reshape2::melt(probs.m)$value
          # densities = data.table::data.table(.dens = densities, .id.dist = rep(1:nrow(X), each = length(xgrid)),
          #   feature = rep(xgrid, times = nrow(X)))
        }
      } else if (class(self$data[[self$feature]]) %in% c("character", "factor")) {
        probs = predict(cmodel, newdata = X, type = "prob")
        probs.m = reshape2::melt(probs)$value
        densities = data.table::data.table(.dens = probs.m, .id.dist = rep(1:nrow(X), each = ncol(probs)),
          feature = factor(rep(colnames(probs), times = nrow(X)), levels = levels(self$data[[self$feature]])))
      } else {
        pr = predict(cmodel, newdata = X, type = "density")
        at = unique(self$data[[self$feature]])
        res = sapply(pr, function(pr) pr(at) / sum(pr(at)))
        res = data.table::data.table(t(res))
        colnames(res) = as.character(at)
        res.m = reshape2::melt(res, measure.vars = as.character(at))
        densities = data.table::data.table(.dens = res.m$value, .id.dist = rep(1:nrow(X), times = length(at)), feature = rep(at, each = nrow(X)))
      }
      colnames(densities) = c(".dens", ".id.dist", self$feature)
      densities
    },
    cnode = function(X,  prob = c(0.05, 0.95)) {
      cmodel = self$model
      node = predict(cmodel, newdata = X, type = "node")
      node_df = data.frame(node = (node), .id = names(node), .path = pathpred(cmodel, X))
      if(inherits(cmodel, "trafotree")) {
        # case of numerical feature
        quants = predict(cmodel, newdata = X, type = "quantile", prob = prob)
        quants = data.frame(t(quants))
        colnames(quants) = paste0("q", prob)
      } else if (class(self$data[[self$feature]]) %in% c("numeric", "integer")) {
        # case of numerical features with few unique values
        quants = predict(cmodel, newdata = X, type = "quantile", at = prob)
        colnames(quants) = paste0("q", prob)
      } else {
        # case of categorical feature
        quants = predict(cmodel, newdata = X, type = "prob")
        names(quants) = levels(X[[self$feature]])
      }
      cbind(node_df, quants)
    }
  ),
  private = list(
  data_nodes = NULL,
  fit_conditional = function() {
    require("trtf")
    y = self$data[[self$feature]]
    if (class(y) %in% c("numeric", "integer") & (length(unique(y)) > 2)) {
      yvar = numeric_var(self$feature, support = c(min(y), max(y)))
      By  =  Bernstein_basis(yvar, order = 5, ui = "incr")
      m = ctm(response = By,  todistr = "Normal", data = self$data )
      form = as.formula(sprintf("%s ~ 1 | .", self$feature))
      part_cmod = trafotree(m, formula = form,  data = self$data, control = self$ctrl)
    } else {
      form = as.formula(sprintf("%s ~ .", self$feature))
      part_cmod = ctree(form, data = self$data, control = self$ctrl)
    }
      self$model = part_cmod
    }
  )
)



#' Fit conditional models
#' 
#' Needed for conditional PDP and Feature Importance.
#' 
#' @param data data.frame with data for which to fit the conditional models
#' @return list of Conditional R6 objects
#' @importFrom partykit ctree_control
#' @export
fit_conditionals = function(data, ctrl = ctree_control()){
  assert_data_frame(data)
  features = colnames(data)
  cmods = lapply(features, function(fname){
    Conditional$new(data, fname, ctrl = ctrl)
  })
  names(cmods) = features
  cmods
}


# Return the paths of a ctree for each training data point
pathpred = function(object, ...) {
  ## coerce to "party" object if necessary
  if (!inherits(object, "party")) object = partykit::as.party(object)
  
  ## get rules for each node
  rls = list.rules.party(object)
  
  ## get predicted node and select corresponding rule
  rules = rls[as.character(predict(object, type = "node", ...))]
  rules = gsub("&", "&\n", rules)
  
  return(rules)
}


# Copied from internal partykit function
list.rules.party = function (x, i = NULL, ...){
  if (is.null(i)) 
    i <- partykit::nodeids(x, terminal = TRUE)
  if (length(i) > 1) {
    ret <- sapply(i, list.rules.party, x = x)
    names(ret) <- if (is.character(i)) 
      i
    else names(x)[i]
    return(ret)
  }
  if (is.character(i) && !is.null(names(x))) 
    i <- which(names(x) %in% i)
  stopifnot(length(i) == 1 & is.numeric(i))
  stopifnot(i <= length(x) & i >= 1)
  i <- as.integer(i)
  dat <- partykit::data_party(x, i)
  if (!is.null(x$fitted)) {
    findx <- which("(fitted)" == names(dat))[1]
    fit <- dat[, findx:ncol(dat), drop = FALSE]
    dat <- dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0) 
      dat <- x$data
  }
  else {
    fit <- NULL
    dat <- x$data
  }
  rule <- c()
  recFun <- function(node) {
    if (partykit::id_node(node) == i) 
      return(NULL)
    kid <- sapply(partykit::kids_node(node), partykit::id_node)
    whichkid <- max(which(kid <= i))
    split <- partykit::split_node(node)
    ivar <- partykit::varid_split(split)
    svar <- names(dat)[ivar]
    index <- partykit::index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) 
        index <- ((1:nlevels(dat[, svar])) > partykit::breaks_split(split)) + 
          1
      slevels <- levels(dat[, svar])[index == whichkid]
      srule <- paste(svar, " %in% c(\"", paste(slevels, 
        collapse = "\", \"", sep = ""), "\")", sep = "")
    }
    else {
      if (is.null(index)) 
        index <- 1:length(kid)
      breaks <- cbind(c(-Inf, partykit::breaks_split(split)), c(partykit::breaks_split(split), 
        Inf))
      sbreak <- breaks[index == whichkid, ]
      right <- partykit::right_split(split)
      srule <- c()
      if (is.finite(sbreak[1])) 
        srule <- c(srule, paste(svar, ifelse(right, ">", 
          ">="), sbreak[1]))
      if (is.finite(sbreak[2])) 
        srule <- c(srule, paste(svar, ifelse(right, "<=", 
          "<"), sbreak[2]))
      srule <- paste(srule, collapse = " & ")
    }
    rule <<- c(rule, srule)
    return(recFun(node[[whichkid]]))
  }
  node <- recFun(partykit::node_party(x))
  paste(rule, collapse = " & ")
}

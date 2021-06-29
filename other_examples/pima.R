####### Example MOC ##########
library("counterfactuals")
# devtools::install_github("susanne-207/moc", subdir = "counterfactuals", ref = "moc_without_iml")
library(iml)
library(mlr3)
library(mlr3learners)

#--- get pima task ---
task = tsk("pima")
# omit rows with NAs
ids = complete.cases(task$data())
task$filter(which(ids))
# explain prediction of fifth row
x.interest = data.frame(task$data()[5,])
x.interest$diabetes = NULL

#--- train SVM ---
set.seed(1000)
mod = lrn("classif.rpart", predict_type = "prob")
mod$train(task)

#--- create Predictor ---
pred = Predictor$new(model = mod, data = task$data(), y = "diabetes", class = "neg")
pred$predict(x.interest)

#--- fit conditionals for special mutator (see paper Sec. 4.3) ---
ctr = partykit::ctree_control(maxdepth = 5L)
set.seed(1234)
cond = fit_conditionals(pred$data$get.x(), ctrl = ctr)

#--- calculate cfexp ---
# best params obtained via irace (see paper Sec. 4.5)
# run MOC 
set.seed(1000)
cf = Counterfactuals$new(predictor = pred, 
  x.interest = x.interest, 
  target = c(0.5, 1), epsilon = 0, conditionals = cond, 
  generations = list(mosmafs::mosmafsTermStagnationHV(20), mosmafs::mosmafsTermGenerations(200)))

# Number of counterfactuals
nrow(cf$results$counterfactuals)
id = cf$results$counterfactuals$dist.target == 0
sum(id)

# Focus counterfactuals that met target
cf$results$counterfactuals = cf$results$counterfactuals[which(id), ]
cf$results$counterfactuals.diff = cf$results$counterfactuals.diff[which(id), ]

# Get relative frequency of feature changes
cf$get_frequency(plot = TRUE)


###---- Plots ----
cf$plot_parallel() # see all cfexp on one glance
cf$plot_surface(features = c("glucose", "age")) # most frequently changed
cf$plot_hv() # should increase

############################################
### German Credit Application
############################################

#--- Setup ----
# To run MOC 
library(devtools)
library(ParamHelpers)
library(GGally)
load_all("../iml/")
library("mosmafs")

# For plotting
library(ggplot2)
library(GGally)
library(metR)
library(ggExtra)

source("plot_helpers.R")
best.params = readRDS("../saved_objects/best_configs.rds")
USE_TRAINED_MODEL = TRUE
PARALLEL = TRUE

###---- Get data ----
credit = read.csv("german_credit_data.csv", row.names = 1)
names(credit)
# omit rows with NA entries
credit = credit %>% na.omit() 
# join groups with small frequencies 
levels(credit$Purpose) = c("others", "car", "others", "others", 
    "furniture", "radio/TV", "others", "others")
levels(credit$Saving.accounts) = c("little", "moderate", "rich", "rich")
# Colnames to lower 
names(credit) = tolower(names(credit))
# Drop levels
credit = droplevels.data.frame(credit)

###---- Train model ----
if (USE_TRAINED_MODEL) {
    pred = readRDS("predictor_svm.rds")
    credit.model = pred$model
} else {
    credit.task = mlr::makeClassifTask(id = "credit", 
        data = credit, target = "risk")
    n = getTaskSize(credit.task)
    set.seed(1234)
    train.set = sample(n, size = n*0.8)
    test.set = seq(1, n)[-train.set]
    train.task = subsetTask(credit.task, subset = train.set)
    test.task = subsetTask(credit.task, subset = test.set)
    lrn = mlr::makeLearner("classif.svm", predict.type = "prob")
    credit.lrn = cpoScale() %>>% cpoDummyEncode() %>>% lrn
    param.set = pSS(
        cost: numeric[0.01, 1]
    )
    ctrl = makeTuneControlRandom(maxit = 100L)
    if (PARALLEL) {
        parallelStartMulticore(cpus = 5L, level = c("mlr.tuneParams"))
    }
    res = tuneParams(credit.lrn, train.task, cv5, par.set = param.set, control = ctrl,
        show.info = FALSE)
    if (PARALLEL) {
        parallelStop()    
    }
    credit.lrn = setHyperPars2(credit.lrn, res$x) # evtl exp
    credit.model = mlr::train(credit.lrn, train.task)
    perf.test = performance(predict(credit.model, test.task), acc)
}

pred = Predictor$new(model = credit.model, data = credit, class = "good", 
    conditional = FALSE)
ctr = ctree_control(maxdepth = 2L)
pred$conditionals = fit_conditionals(pred$data$get.x(), ctrl = ctr)

###---- Compute counterfactuals ----
x.interest = credit[1,]
pred$predict(x.interest)

set.seed(1000)
system.time({credit.cf = Counterfactuals$new(predictor = pred, 
    x.interest = x.interest, 
    target = c(0.5, 1), epsilon = 0, generations = best.params$generations, 
    mu = best.params$mu, 
    p.mut = best.params$p.mut, p.rec = best.params$p.rec, 
    p.mut.gen = best.params$p.mut.gen, 
    p.mut.use.orig = best.params$p.mut.use.orig, 
    p.rec.gen = best.params$p.rec.gen, initialization = "icecurve",
    p.rec.use.orig = best.params$p.rec.use.orig, track.infeas = TRUE)})

# Get relative frequency of feature changes
credit.cf$get_frequency()

###---- Plots ----
credit.cf$plot_parallel(features = c("duration", "credit.amount", "age"))
credit.cf$plot_surface(features = c("duration", "credit.amount"))
plot_hv(credit.cf, ylim = c(0.65, 0.75))

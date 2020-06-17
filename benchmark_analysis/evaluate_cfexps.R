######################################
### EVALUATE RESULTS 
### Read .csv files with counterfactuals for MOC, Tweaking, Recourse and Dice
### Evaluate them with respect to coverage and objective values
### For MOC different versions are compared with respect to the ranks of the 
### hypervolume. 
######################################

#--- Setup ----
source("../helpers/libs_mlr.R")
source("../helpers/helpers_evaluate.R")
library(BBmisc)
library(stringr)
library(checkmate)
library(ParamHelpers)
library(mosmafs)
library(ggplot2)
library(ggpubr)
data.path = "../saved_objects_rerun/benchmark"
instances = readRDS(file.path(data.path, "models_benchmark.rds"))

obj.nams = c("dist.target", "dist.x.interest", "nr.changed", "dist.train")
task_ids = readRDS("../helpers/benchmark_task_ids.rds")
task.names = list.dirs(path = data.path, full.names = FALSE, recursive = FALSE)
set.seed = 1234
# --- Data set description ----
data.desc = mapply(function(inst, id) {
  rows = inst$predictor$data$n.rows + 10 # 10 observations were removed as x.interests
  cont.f = sum(inst$predictor$data$feature.types == "numerical")
  cat.f = sum(inst$predictor$data$feature.types == "categorical")
  data.frame(Task = as.integer(id), Name = inst$task.id, 
    Obs = rows, Cont = cont.f, Cat = cat.f, row.names = NULL)
}, instances[1:10], task_ids, SIMPLIFY = FALSE)

data.desc = do.call("rbind", data.desc)
data.desc = data.desc[order(as.character(data.desc$Name)),]

print(data.desc)


# --- Performance values of algorithms ----
perf = lapply(instances, function(inst) {
  data.frame(task = inst$task.id, learner = inst$learner.id, accuracy = inst$performance,
    row.names = NULL)
})
perf = do.call("rbind", perf)
tab = tidyr::spread(perf, "learner", "accuracy")
tab = tab[order(as.character(tab$task)),]
names(tab)[names(tab) == "randomforest"] = "rf"
names(tab)[1] = capitalizeStrings(names(tab)[1])
print(tab)


# --- Create a results data frame for each task  
# Read in datasets NSGA-II --> MOC 
moc.cf = lapply(task.names, function(task.nam) {
  ### NSGA-II + Random Search
  csv.path = file.path(data.path, task.nam, "moc")
  # Get csv files with 'cf' in name 
  csv.nams = list.files(path = csv.path, pattern = "cf-", recursive = TRUE, full.names = TRUE)
  res.list = lapply (csv.nams, function(csv.nam) {
    cf = read.csv(file = csv.nam)
    lrn.nam =  str_remove(str_extract(string = csv.nam, 
      pattern = "[:alpha:]*.csv"), ".csv")
    cf$learner = lrn.nam
    return(cf)
  })
  res.df = do.call(rbind, res.list)
  res.df$task = task.nam
  return(res.df)
})
names(moc.cf) = task.names

moc.cf.subset = lapply(moc.cf, function(cf) {
  combis = unique(cf[, c("method", "row_ids", "learner")])
  itlist = split(combis, seq(nrow(combis)))
  
  subset = lapply(itlist, function(it) {
    return(subset_results(cf[cf$method == it$method & cf$row_ids == it$row_ids & cf$learner == it$learner, ], 
      10, strategy = "hvcontr"))
  })
  cf = do.call(rbind, subset)
  rownames(cf) = NULL
  return(cf)
})
names(moc.cf.subset) = task.names

moc.cf.cov = lapply(moc.cf.subset, function(cf) {
  cf = cf[cf$dist.target == 0, ]
  return(cf)
})
names(moc.cf.cov) = task.names

# Read in results others
# Calculate coverage by matching results of moc
others.cf = lapply(task.names, function(task.nam) {
  ### NSGA-II + Random Search
  csv.path = file.path(data.path, task.nam)
  # Get csv files with 'cf' in name 
  csv.nams = list.files(path = csv.path, pattern = "cf-", recursive = FALSE, full.names = TRUE)
  # csv.nams = csv.nams[stringr::str_detect(csv.nams, "whatif")] #SD!!!!
  # Match results of moc 
  mocres = moc.cf.cov[[task.nam]]
  mocres = mocres[(mocres$method == "mocmod"),]
  res.list = lapply (csv.nams, function(csv.nam) {
    cf = read.csv(file = csv.nam)
    lrn.nam =  str_remove(str_extract(string = csv.nam, 
      pattern = "[:alpha:]*.csv"), ".csv")
    csv.nam.s = str_remove(string = csv.nam, pattern = csv.path)
    method.nam = str_remove(str_extract(string = csv.nam.s, pattern = "\\-[:alpha:]*"), "\\-")
    # Evaluate counterfactuals / calculate objective values
    if (method.nam %in% c("recourse", "dice", "whatif")) {
      instance = subset_instances(instances, task = task.nam, learner = lrn.nam)[[1]]
      cf = evaluate_cfexp(cf = cf, instance = instance, id = method.nam, remove.dom = TRUE, 
        data.dir = data.path)
      cf$method = method.nam
    }
    if (method.nam %in% c("tweaking")) { 
      cf$method = method.nam
    }
    cf$learner = lrn.nam
    # Get indicator if dominated
    cf$dominated = NA
    mocmod = mocres[(mocres$learner == lrn.nam),]
    for (row.id in unique(cf$row_ids)) {
      cf.moc = mocmod[mocmod$row_ids == row.id, obj.nams]
      if (nrow(cf.moc) > 0) {
        dom.ind = relative_coverage(pf1 = cf[cf$row_ids == row.id, obj.nams], 
          pf2 = cf.moc)
        cf[cf$row_ids == row.id, "dominated"] = dom.ind
      }
    }
    return(cf)
  })
  res.df = do.call(rbind, res.list)
  if (nrow(res.df) > 0) {
    res.df$task = task.nam
  } 
  return(res.df)
})
names(others.cf) = task.names

# --- Calculate coverage rate ----
cov = lapply(others.cf, function(res) {
  res = res[res$dist.target == 0, ]
  res = res[res$method != "whatif", ]
  methods = unique(res$method)
  coverage = data.frame(matrix(rep(NA, 3), ncol = 3L))
  names(coverage) = c("dice", "recourse", "tweaking")
  for (meth in unique(res$method)) {
    nr = sum(!is.na(res$dominated[res$method == meth]))
    nr.cov = sum(res$dominated[res$method == meth], na.rm = TRUE)
   sign = biotest(nr.cov/nr, nr)
    coverage[meth] = paste(round(nr.cov/nr, 2),  sign," (", 
      nr, ")", sep = "")
  }
  return(coverage)
})
cov.df = do.call("rbind", cov)
cov.df = cov.df[order(rownames(cov.df)),]
print(cov.df)

print(xtable::xtable(cov.df, label = "tab:cov", 
  caption = "MOC's coverage rate of methods to be compared per data set averaged over all models."),  floating = TRUE, 
  floating.environment = "table",
  caption.placement = "top", 
  size = getOption("xtable.size", "small"), booktabs = TRUE)

#--- Plot objective values per data set, model and method 
usedcolor <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
boxplot.list =mapply(function(other, moc, task.name) {
  browser()
  moc = moc[moc$method %in% c("moc", "mocmod"),]
  other$dominated = NULL
  df = rbind(other, moc)
  df = df[, c(obj.nams, "method", "learner", "row_ids")]
  df$learner[df$learner == "randomforest"] = "rf"
  names(df)[names(df) == "dist.x.interest"] = "dist.x.orig"
  names(df)[names(df) == "nr.changed"] = "no.changed"
  no.nondom = aggregate(df[, 1], 
    by = list(df$method, df$learner, df$row_ids), 
    FUN = length)
  df = df[, !names(df) %in% c("row_ids")]
  df.melt = reshape2::melt(df, id.vars = c("method", "learner"))
  no.nondom = no.nondom[, -which(names(no.nondom) == "Group.3")]
  no.nondom$variable = "no.nondom"
  names(no.nondom)[1:3] = c("method", "learner", "value")
  all = rbind(df.melt, no.nondom)
  all$task = task.name
  all$variable = factor(all$variable, levels = unique(all$variable), 
    labels = c(bquote("o"[1]), bquote("o"[2]), bquote("o"[3]), bquote("o"[4]), "count"))
  # var.labs = c(as.expression('o'[1]), as.expression('o'[2]), expression('o'[3]), expression('o'[4]), "no.nondom")
  # names(var.labs) = unique(all$variable)
  # lrn.labs = unique(all$learner)
  # names(lrn.labs) = unique(all$learner)
  ggplot(data= all , aes(x = method, y = value)) + 
    # ggtitle(task.name) +
    theme_bw() +
    # coord_flip() + # SD
    geom_boxplot(aes(fill = method)) + 
    facet_grid(variable ~ learner, scales = "free", labeller = label_parsed) + # added label_parsed
  # theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) + #SD
    xlab("") + ylab("") + theme(legend.position = "none") +
    scale_fill_manual(values = usedcolor) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}, others.cf, moc.cf.subset, task.names, SIMPLIFY = FALSE)

boxplot.list = lapply(boxplot.list, function(l) {
  l + theme(axis.text = element_text(size = 14), 
    strip.text =  element_text(size = 14))
})

others = combine_plots(boxplot.list[-which(names(boxplot.list) %in% c("diabetes", "no2"))])
for (i in seq_along(others)) {
  ggsave(paste0("results/boxplots_other", names(others)[i], ".pdf"), plot = others[[i]], 
    width = 6.1, height = 5.7)
}

showed = combine_plots(boxplot.list[c("diabetes", "no2")], shared.y = FALSE)
ggsave("results/boxplots_showeddiabetes.pdf", plot = showed[[1]], width = 6.1, height = 5.7) # height: 6
ggsave("results/boxplots_showedno2.pdf", plot = showed[[2]], width = 6.1, height = 5.7)

# --- Compare different versions of MOC ----
# Define dictonary of task and number of features
task.desc = list("boston" = 13, "cmc" = 9, "diabetes" = 8, "ilpd" = 10, 
  "kc2" = 21, "no2" = 7, "pc1" = 21, "plasma_retinol" = 13, 
  "kr-vs-kp" = 36, "tic-tac-toe" = 9)

# Read in hypervolume of MOC 
res.log = lapply(task.names, function(task.nam) {
  ### NSGA-II + Random Search
  csv.path = file.path(data.path, task.nam, "moc")
  # Get csv files with 'cf' in name 
  csv.nams = list.files(path = csv.path, pattern = "log-", recursive = TRUE, full.names = TRUE)
  csv.nams = csv.nams[!grepl("(tweaking)", csv.nams)] ### TO DO! 
  res.list = lapply (csv.nams, function(csv.nam) {
    lrn.nam =  str_remove(str_extract(string = csv.nam, 
      pattern = "[:alpha:]*.csv"), ".csv")
    # Read csv file 
    log = read.csv(file = csv.nam)
    
    # Calculate ranks 
    a = as.data.frame(t(apply(log[, grep("hv", names(log))], 1, rank)))
    names(a) = str_replace(names(a), "hv_", "rank_")
    log = cbind(log, a)
    
    # Mean over row_ids 
    res = aggregate(log[, grepl("(hv)|(rank)", names(log))], 
      by = list(log$generation, log$evals), 
      FUN = mean)
    names(res)[names(res) == "Group.1"] = "generation"
    names(res)[names(res) == "Group.2"] = "evals"
    res$learner = lrn.nam
    return(res)
  })
  # combine into one large df 
  res.df = do.call(rbind, res.list)
  res.df$task = paste(task.nam, " (", task.desc[[task.nam]], ")", sep = "")
  # rename ngsa2 to moc
  return(res.df)
})
names(res.log) = task.names
df.log = do.call(rbind, res.log)

# # Plot results
# plot_results(df.log, type = "hv", methods = NULL, xlim = c(0, 175), subset.col = "task", 
#   # pdf.file = "results/benchmarkres_hv.pdf",
#   width = 9, height = 8, line.width = 0.6, ncol = 2)

rankplot = plot_results(df.log, type = "rank", methods = NULL, line.width = 0.7,
  # pdf.file = "results/benchmarkres_ranks.pdf",
  ylab = "ranks w.r.t domhv", width = 4.5, height = 2, xlim = c(0, 175), subset.col = NULL)
ggsave("results/benchmark_ranks.pdf",  plot = rankplot, width = 4.5, height = 2)

rankplottask = plot_results(df.log, type = "rank", methods = NULL, 
  xlim = c(0, 175), subset.col = "task", ylab = "ranks w.r.t domhv",  
  # pdf.file = "results/benchmarkres_ranks_tasks.pdf",
  width = 9, height = 8, line.width = 0.6, ncol = 2)
ggsave("results/benchmark_ranks_tasks.pdf",  plot = rankplottask, width = 8, height = 10)

# plot_results(df.log, type = "rank", methods = NULL, xlim = c(0, 175), subset.col = c("task", "learner"), 
#   pdf.file = "results/benchmarkres_ranks_tasks_learner.pdf",
#   width = 9, height = 30, line.width = 0.6, ncol = 2)


#### Check
# 10 found? 
nr.rows = mapply(function(other, moc) {
  moc = moc[moc$method == "mocmod",]
  other$dominated = NULL
  df = rbind(other, moc)
  
  nr.sol = aggregate(df[, 1],
    by = list(df$method, df$learner, df$row_ids),
    FUN = length)
  agm = aggregate(df$row_ids, by = list(df$method, df$learner), 
    FUN = function(x) length(unique(x)))
  agm$task = moc$task[1]
  return(agm)
}, others.cf[-1], moc.cf[-1], SIMPLIFY = FALSE)
nr.rows.df = do.call("rbind", nr.rows)
assert_true(all(nr.rows.df$x == 10))

# Missing HVs
id.res = lapply(task.names, function(task.nam) {
  ### NSGA-II + Random Search
  csv.path = file.path(data.path, task.nam, "moc")
  # Get csv files with 'cf' in name 
  csv.nams = list.files(path = csv.path, pattern = "log-", recursive = TRUE, full.names = TRUE)
  csv.nams = csv.nams[!grepl("(tweaking)", csv.nams)] ### TO DO! 
  res.list = lapply (csv.nams, function(csv.nam) {
    lrn.nam =  str_remove(str_extract(string = csv.nam, 
      pattern = "[:alpha:]*.csv"), ".csv")
    
    # Read csv file 
    log = read.csv(file = csv.nam)
    det = which(apply(log[, names(log)[str_detect(names(log), "hv")]] == 0, MARGIN = 1, FUN = any))
    if(length(det) > 0) {
      return(paste("csv: ", csv.nam, "row.ids:", unique(log$row_ids[det])))
    } else NULL
  })
  return(res.list)
})


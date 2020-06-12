# Receive multiplication factor 
# Note: Saved hypervolumes of MOC are divided by hypervolume of ideal point with 
# objective values c(0, 0, 0, 0)
# To receive original dominated hypervolume, we have to mulitply the 
# hypervolume with the one of the ideal point 
library(plyr)

task.desc = list("boston" = 13, "cmc" = 9, "diabetes" = 8, "ilpd" = 10, 
  "kc2" = 21, "no2" = 7, "pc1" = 21, "plasma_retinol" = 13, 
  "kr-vs-kp" = 36, "tic-tac-toe" = 9)

all.refs = lapply(instances, function(inst) {
  train.data = data.frame(inst$predictor$data$get.x())
  if (inst$learner.id %in% c("logreg", "neuralnet")) {
    inst = load_keras_model(inst, data.dir = data.path) 
  }
  path = file.path(data.path, inst$task.id)
  x.interests = inst$sampled.rows
  y.hat = inst$predictor$predict(x.interests)
  n.feat = inst$predictor$data$n.features
  hv.ideal = sapply(1:10, FUN = function(j) {
    ref.point = c(min(abs(y.hat[j,] - 0.5)), 1, n.feat, 1)
    ecr::computeHV(matrix(rep(0, length(ref.point))), ref.point)
  })
  data.frame(task = inst$task.id, model = inst$learner.id, row_ids = as.numeric(row.names(x.interests))+1, hv_ideal = hv.ideal)
})
all.refs = do.call("rbind", all.refs)


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
    # Multiply with ideal point HV
    ref.points = all.refs[all.refs$task == task.nam & all.refs$model == lrn.nam, ]
    log = join(log, ref.points[, c("row_ids", "hv_ideal")], by = "row_ids")
    log[,"hv_random"] = log[, "hv_random"] * log$hv_ideal
    log$hv_ideal = NULL
    
    write.csv(log, file = csv.nam)
  
    return(NULL)
  })
  return(NULL)
})


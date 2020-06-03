renew_conditionals = function(instance, datapath, seed = 1234, save = TRUE) {
  assert_list(instance)
  assert_character(datapath)
  assert_logical(save)
  dir.name = file.path(datapath, instance$task.id)
  ctr = partykit::ctree_control(maxdepth = 5L)
  set.seed(seed)
  con = fit_conditionals(instance$predictor$data$get.x(), ctrl = ctr)
  if (save) {
    saveRDS(object = con, file = file.path(dir.name, paste0("conditional.rds")))
  }
  return(con)
}
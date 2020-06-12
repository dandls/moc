moc.cf.subset.random = lapply(moc.cf, function(cf) {
  combis = unique(cf[, c("method", "row_ids", "learner")])
  itlist = split(combis, seq(nrow(combis)))
  
  subset = lapply(itlist, function(it) {
    return(subset_results(cf[cf$method == it$method & cf$row_ids == it$row_ids & cf$learner == it$learner, ], 
      10, strategy = "random"))
  })
  cf = do.call(rbind, subset)
  rownames(cf) = NULL
  return(cf)
})
names(moc.cf.subset) = task.names

moc.cf.subset.my = lapply(moc.cf, function(cf) {
  combis = unique(cf[, c("method", "row_ids", "learner")])
  itlist = split(combis, seq(nrow(combis)))
  
  subset = lapply(itlist, function(it) {
    return(subset_results(cf[cf$method == it$method & cf$row_ids == it$row_ids & cf$learner == it$learner, ], 
      10, strategy = "crowdingdistx"))
  })
  cf = do.call(rbind, subset)
  rownames(cf) = NULL
  return(cf)
})

moc.cf.subset.ecr = lapply(moc.cf, function(cf) {
  combis = unique(cf[, c("method", "row_ids", "learner")])
  itlist = split(combis, seq(nrow(combis)))
  
  subset = lapply(itlist, function(it) {
    return(subset_results(cf[cf$method == it$method & cf$row_ids == it$row_ids & cf$learner == it$learner, ], 
      10, strategy = "crowdingdistecr"))
  })
  cf = do.call(rbind, subset)
  rownames(cf) = NULL
  return(cf)
})

dist.target.random = unlist(lapply(moc.cf.subset.random, function(m) mean(m$dist.target)))
dist.orig.random = unlist(lapply(moc.cf.subset.random, function(m) mean(m$dist.x.interest)))
changed.random = unlist(lapply(moc.cf.subset.random, function(m) mean(m$nr.changed)))

dist.target.my = unlist(lapply(moc.cf.subset.my, function(m) mean(m$dist.target)))
dist.orig.my = unlist(lapply(moc.cf.subset.my, function(m) mean(m$dist.x.interest)))
changed.my = unlist(lapply(moc.cf.subset.my, function(m) mean(m$nr.changed)))

dist.target.ecr = unlist(lapply(moc.cf.subset.ecr, function(m) mean(m$dist.target)))
dist.orig.ecr = unlist(lapply(moc.cf.subset.ecr, function(m) mean(m$dist.x.interest)))
changed.ecr = unlist(lapply(moc.cf.subset.ecr, function(m) mean(m$nr.changed)))

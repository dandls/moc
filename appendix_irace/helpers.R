### Return number of generations hv stagnated for 10 iterations
getTermGenerations = function(hv, nr = 10) {
  diffhv = diff(hv)
  nr = 10 - 1
  n_gen = length(hv)
  idx <- which(diffhv == 0)
  # at least 9 elements of the vector should have a diff == 0
  if (length(idx) >= 9) {
    # look if 9 consecutive 0s 
    idxstag = idx[sapply(idx, function(i) all(diffhv[i:(i+(nr-1))] == rep(0, nr)))]
    if (length(idxstag) >= 1 & any(!is.na(idxstag))) {
      return(min(idxstag, na.rm = TRUE))
    }
  }
  # if not return max generations
  return(n_gen)
}
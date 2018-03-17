library('condMVNorm')

getDevCnorm <- function(x, mu, sigma) {
  # x is a matrix
  # return fraction of conditional sd
  
  p = ncol(x)
  n = nrow(x)
  index = c(1:p)
  res = x
  for (j in index) {
    res[,j] = sapply(c(1:n), function(i) {
      if (is.na(x[i,j])) return(NA)
      idx = !is.na(x[i,])
      idx[j] = FALSE
      if (sum(idx) == 0) return(NA)
      condMeanVar = condMVN(mean = mu, sigma = sigma, dependent = j, given = index[idx], X.given = x[i,idx])
      abs(x[i,j] - condMeanVar$condMean) / sqrt(condMeanVar$condVar)
    })
  }
  return(res)
}

getqcnorm <- function(x, idx, mu, sigma, cap=3) {
  p = ncol(x)
  n = nrow(x)
  index = c(1:p)
  res = apply(idx, 1, function(ii) {
    i = ii[1]
    j = ii[2]
    if (is.na(x[i,j])) return(NA)
    idx = !is.na(x[i,])
    idx[j] = FALSE
    condMeanVar = condMVN(mean = mu, sigma = sigma, dependent = j, given = index[idx], X.given = x[i,idx])
    if (x[i,j] > condMeanVar$condMean) return(condMeanVar$condMean + cap*sqrt(condMeanVar$condVar))
    if (x[i,j] < condMeanVar$condMean) return(condMeanVar$condMean - cap*sqrt(condMeanVar$condVar))
  })
  return(res)
}
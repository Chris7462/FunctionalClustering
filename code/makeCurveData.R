makeCurveData <- function(X) {
  # X is a n-by-p data matrix with missing values
  missingIdx = which(apply(X, 1, function(y) {sum(!is.nan(y)) == 0}))
  if (length(missingIdx) > 1) X = X[-missingIdx, ]
  
  n = nrow(X)
  p = ncol(X)
  x = as.vector(t(X))
  curve = rep(c(1:n), each = p)
  timeindex = rep(c(1:p), times = n)
  
  idx = which(is.nan(x) | is.na(x))
  x = x[-idx]
  curve = curve[-idx]
  timeindex = timeindex[-idx]
    
  data = list(x=x, curve=curve, timeindex=timeindex)
  return(list(data = data, missingIdx = missingIdx))
}

mapIdx <- function(OrigIdx, missingIdx) {
  # map from the original data index to the processed data index
  map = OrigIdx
  if (length(missingIdx) == 0) return(map)
  map[missingIdx] = NA
  map[-missingIdx] = 1:(length(OrigIdx) - length(missingIdx))
  return(map)
}
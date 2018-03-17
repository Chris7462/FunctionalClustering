foldIndex <- function(N, fold) {
  x =sample(c(1:N),N,replace=FALSE)
  q = floor(N/fold)
  r = N - q*fold
  ind = rep(list(c()), fold)
  for (i in 1:fold) {
    if (i <= r) {
      ind[[i]] = x[c(1:(q+1)) + (i-1) * (q+1)]  
    } else {
      ind[[i]] = x[c(1:q) + (q+1) * r + q*(i-r-1)]
    }
    
  }
  return(ind)
}
getDistance <- function(data) {
  n = nrow(data)
  dis = matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dis[i,j] = sqrt(mean((data[i,] - data[j,])^2, na.rm=T))
      dis[j,i] = dis[i,j]
    }
  }
  return(dis)
}
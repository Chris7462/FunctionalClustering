library(Matrix)
library(mvtnorm)
getFclustll <- function(data, fit) {
  par = fit$parameters
  lambda.zero = par$lambda.zero
  Lambda = par$Lambda
  alpha = par$alpha
  pi = par$pi
  Gamma = par$Gamma
  sigma = par$sigma
  
  J = length(data)
  N = length(table(data[[1]]$curve))
  S = fit$FullS
  TT = nrow(S)
  FFullS = lapply(1:J, function(j) {S})
  FFullS = as.matrix(bdiag(FFullS))
  Sigma = FFullS %*% Gamma %*% t(FFullS) + diag(rep(sigma, rep(TT, J)))
  ll = sapply(1:N, function(i) {
    Y = lapply(data, function(xx) {xx$x[xx$curve==i]})
    index = lapply(1:J, function(j) {data[[j]]$timeindex[data[[j]]$curve==i] + (j-1)*TT})
    Y = unlist(Y)
    index = unlist(index)
    cov = Sigma[index, index]
    center = FFullS[index,] %*% (as.vector(lambda.zero) + Lambda %*% t(alpha))
    d = apply(center, 2, function(xx) {dmvnorm(Y, xx, cov)})
    log(sum(d * pi))
  })
  sum(ll)
}

getDistortion <- function(etapred, k.range) {
  p = ncol(etapred)
  d = sapply(k.range, function(k) {
    res = kmeans(etapred, k, nstart = 30, iter.max=50)
    1/p * res$tot.withinss
  })
  d
}
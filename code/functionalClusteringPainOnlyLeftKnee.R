#setwd(" ")

source('code/curvecnorm.R')
source("code/makeCurveData.R")
source("code/fclustMulticurve.R")
source("code/getFclustll.R")
source('code/foldIndex.R')
source("code/getDistance.R")
library(cluster)

savePath = 'fclustResult/PainLeftKnee/'
Data = readRDS('fclustResult/Data.rds')
Data = Data[3] # only pain data, left knee

### functional clustering
data = cbind(Data[[1]]) 

TT = 8
maxH = 6
G = c(1:4)
maxit=100

### functional clustering on full data
curveData = list(makeCurveData(Data[[1]])$data)

fclustFitFull = rep(list(), length(G))
for (i in 2:length(G)) {
  print(G[i])
  fclustFitFull[[i]] = fitfclust.multicurve(data = curveData, K = G[i], h=min(G[i]-1,maxH), grid = c(1:8),
                                            hard=F, plot=T, trace=T, maxit = maxit) 
  fclustPred = fclust.pred.multicurve(fclustFitFull[[i]])
  print(table(fclustPred$class.pred))
  print(summary(fclustPred$m))
  fclust.discrim.multicurve(fclustFitFull[[i]], abs=T, scale=T)
}
saveRDS(fclustFitFull, 
        paste(savePath,"fclustFitFull.rds",sep=''))
# fclustFitFull = readRDS(paste(savePath,"fclustFitFull.rds",sep=''))

data.std = data
dataDist = getDistance(data.std)
sum(is.na(dataDist))
saveRDS(dataDist, paste(savePath,"dataDist.rds",sep=''))
# dataDist = readRDS(paste(savePath,"dataDist.rds",sep=''))

## pick #clusters
fclustPredFull = lapply(fclustFitFull[-1], function(x) {
  fclust.pred.multicurve(x)
})
fclustPredFull = c(list(NULL), fclustPredFull)
fclustSil = sapply(fclustPredFull[-1], function(x) X = {summary(silhouette(x$class.pred, dmatrix=dataDist))$avg.width})

pdf(paste(savePath,"silhouetteFull.pdf",sep=''))
par(mfrow=c(1,1))
plot(G[-1], fclustSil, xlab = "# clusters", ylab = "silhouette", main="Silhouette of Sparse Functional Clustering")
dev.off()

### Cluster results when #cluster=k
k=3

fit = fclustFitFull[[k]]
fclustPred = fclustPredFull[[k]]
# reorder the class in decreasing order in #subjects
orderIdx = order(table(fclustPred$class.pred), decreasing=T) 
cat("Cluster assignment with K=", G[k])
temp = rep(NA, length(fclustPred$class.pred))
for (ii in 1:k) temp[fclustPred$class.pred==orderIdx[ii]]=ii
print(table(temp))
cat("Summary of posterior probability with K=", G[k])
print(summary(fclustPred$m))
par(mfrow=c(1,k))
for (i in 1:k) hist(fclustPred$probs[,orderIdx[i]])

pdf(paste(savePath, 'ProgressionFull_k=',k,'.pdf',sep=""),
    width=8, height=6)
res = fclust.curvepred.multicurve(fclustFitFull[[k]])
word = c("Left")
par(mfrow=c(1,1))
for (i in 1:1) {
  idx = (i-1)*8+c(1:8)
  matplot(c(1:8), res$meancurves[idx, orderIdx], xlab = "Years", ylab = "Pain score", 
          main = paste("Pain Score of the", word[i], "Affected Knee"), lty = 1, col=1:k, type="b")
  legend("topright", c("type 1", "type 2"), col=1:k, lty=1)
}
dev.off()

### divide the data into 10 folds, leave each fold as the test set
### and cluster using the rest 9 folds
# CV 10 folds
n = nrow(Data[[1]])
nFold = 10
# fold = foldIndex(n, nFold)
# saveRDS(fold, 'fclustResult/fold.rds')
fold = readRDS('fclustResult/fold.rds')

fclustFit = matrix(list(), nFold, length(G))
for (j in 1:10) {
  idx = unlist(fold[-j])
  
  curveData = list(makeCurveData(Data[[1]][idx,])$data)
  
  for (i in 2:length(G)) {
    print(G[i])
    fclustFit[[j, i]] = fitfclust.multicurve(data = curveData, K = G[i], h=min(G[i]-1,maxH), grid = c(1:8),
                                             hard=F, plot=T, trace=T, maxit = maxit) 
    fclustPred = fclust.pred.multicurve(fclustFit[[j,i]])
    print(table(fclustPred$class.pred))
    print(summary(fclustPred$m))
    fclust.discrim.multicurve(fclustFit[[j,i]], abs=T, scale=T)
    
    saveRDS(fclustFit, paste(savePath,"fclustFit.rds",sep=''))
  }
}

#fclustFit = readRDS(paste(savePath,"fclustFit.rds",sep=''))

### Assign cluster probability
## for each subject in the test fold, assign cluster posterior probability
fclustPredAll = matrix(list(), nFold, length(G))
for (j in 1:nFold) {
  print(j)
  for (i in 2:length(G)) {
    cat(i)
    fclustPredAll[[j, i]] = fclust.pred.multicurve(fclustFit[[j,i]])
  }
}

saveRDS(fclustPredAll, paste(savePath,"fclustPred.rds",sep=''))  
#fclustPredAll = readRDS(paste(savePath,"fclustPred.rds",sep=''))

### Pick #clusters
## pick the optimal number of clusters using silhouette measure
pdf(paste(savePath,"silhouette.pdf",sep=''))
fclustSil = matrix(NA, nFold, length(G)-1)
for (j in 1:nFold)  {
  idx = unlist(fold[-j])
  fclustSil[j,] = sapply(fclustPredAll[j, -1], function(x) X = {summary(silhouette(x$class.pred, dmatrix=dataDist[idx,idx]))$avg.width})
}
matplot(G[-1], t(fclustSil), xlab = "# clusters", ylab = "silhouette", main="Silhouette of Sparse Functional Clustering")
dev.off()

### Cluster results when #cluster=k
k=3
for (j in 1:nFold) {
  fit = fclustFit[[j,k]]
  fclustPred = fclustPredAll[[j,k]]
  # reorder the class in decreasing order in #subjects
  orderIdx = order(table(fclustPred$class.pred), decreasing=T) 
  cat("Cluster assignment with K=", G[k])
  temp = rep(NA, length(fclustPred$class.pred))
  for (ii in 1:k) temp[fclustPred$class.pred==orderIdx[ii]]=ii
  print(table(temp))
  cat("Summary of posterior probability with K=", G[k])
  print(summary(fclustPred$m))
  par(mfrow=c(1,k))
  for (ii in 1:k) hist(fclustPred$probs[,orderIdx[ii]])
  
  res = fclust.curvepred.multicurve(fclustFit[[j,k]])
  word = c("Left")
  par(mfrow=c(1,1))
  for (i in 1:1) {
    idx = (i-1)*8+c(1:8)
    matplot(c(1:8), res$meancurves[idx,orderIdx], xlab = "Years", ylab = "Pain score", 
            main = paste("Pain Score of the", word[i], "Affected Knee"), lty = 1, col=1:k, type="b")
    legend("topright", c("type 1", "type 2", "type 3"), col=1:k, lty=1)
  }
}

### assign cluster probability to test data
k = 3

testProb = matrix(NA, n, 3) # decreasing order in #subjects
for (j in 1:nFold) {
  idx = unlist(fold[[j]])
  curveData = list(makeCurveData(Data[[1]][idx,])$data)
  
  fclustPred = fclustPredAll[[j,k]]
  orderIdx = order(table(fclustPred$class.pred), decreasing=T)
  testProb[idx, ]=fclust.pred.multicurve(fclustFit[[j, k]], curveData)$probs[,orderIdx]
}
for (i in 1:k) hist(testProb[,i])
saveRDS(testProb, paste(savePath,"testProb.rds",sep=''))

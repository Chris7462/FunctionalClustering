#setwd(" ")

source('code/curvecnorm.R')
source("code/makeCurveData.R")
source("code/fclustMulticurve.R")
source("code/getFclustll.R")
source('code/foldIndex.R')
source("code/getDistance.R")
library(cluster)

savePath = 'fclustResult/Pain/'
Data = readRDS('fclustResult/Data.rds')

fclustPath = savePath
predPath = 'predResult/Pain/'

RDSFigsPath = 'figure_rds_files/'


pain = read.table("originalData/pain.txt")
availIdx = readRDS('fclustResult/subsetMap.rds')
length(availIdx)
pain = pain[availIdx, ]
dim(pain)


charac = readRDS('predResult/charac.rds')
p = ncol(charac)

### functional clustering
Data = Data[3:4] # only pain data
data = cbind(Data[[1]], Data[[2]]) 

TT = 8
maxH = 6
G = c(1:4)
maxit=100

### functional clustering on full data
curveData = list(makeCurveData(Data[[1]])$data, makeCurveData(Data[[2]])$data)

fclustFitFull = readRDS(paste(savePath,"fclustFitFull.rds",sep=''))

dataDist = readRDS(paste(savePath,"dataDist.rds",sep=''))

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
word = c("Left", "Right")
par(mfrow=c(1,2))
for (i in 1:2) {
  idx = (i-1)*8+c(1:8)
  matplot(c(1:8), res$meancurves[idx, orderIdx], xlab = "Years", ylab = "Pain score", 
          main = paste("Pain Score of the", word[i], "Affected Knee"), lty = 1, col=1:k, type="b")
  legend("topright", c("type 1", "type 2", "type 3"), col=1:k, lty=1)
}
dev.off()



### divide the data into 10 folds, leave each fold as the test set
### and cluster using the rest 9 folds
# CV 10 folds
n = nrow(Data[[1]])
nFold = 10
fold = readRDS('fclustResult/fold.rds')

fclustFit = readRDS(paste(savePath,"fclustFit.rds",sep=''))

### Assign cluster probability
## for each subject in the test fold, assign cluster posterior probability
fclustPredAll = readRDS(paste(savePath,"fclustPred.rds",sep=''))

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
  word = c("Left", "Right")
  par(mfrow=c(1,2))
  for (i in 1:2) {
    idx = (i-1)*8+c(1:8)
    matplot(c(1:8), res$meancurves[idx,orderIdx], xlab = "Years", ylab = "Pain score", 
            main = paste("Pain Score of the", word[i], "Affected Knee"), lty = 1, col=1:k, type="b")
    legend("topright", c("type 1", "type 2", "type 3"), col=1:k, lty=1)
  }
}


### Train the prediction model 
## treat each fold as the test set, use the rest 9 folds to train 
## a prediction model of cluster posterior probability
## we use lasso as the prediction model
library(glmnet)

fclustPredAll = readRDS(paste(fclustPath, 'fclustPred.rds', sep=''))
fold = readRDS('fclustResult/fold.rds')

nFold = 10
k = 3
nlambda=25

lassoFit = readRDS(paste(predPath, 'lassoFit.rds', sep=''))

### Predict on the test data
## for each subject in the test fold, predict its cluster probability
## using the prediction model trained on the rest 9 folds
n = length(availIdx)
testProbPredAll = readRDS(paste(predPath, 'testProbPredAll.rds', sep=''))

### Prediction results
## compare the predicted test probability with the assigned test probability
library('pROC')
testProb = readRDS(paste(fclustPath, 'testProb.rds', sep=''))

#for (year in 1:4) {
year = 1
  testProbPred = testProbPredAll[[year]]
  
  yearIdx = c(0:8)
  if (year == 1) {
    title = paste('Charac + No Curve Features')  
    filename = paste(predPath, 'noCurveFeatures.pdf', sep='')
  } else {
    title = paste('Charac + ', 'Year 0-', yearIdx[year], ' Features',
                  sep = '')
    filename = paste(predPath, 'Year0-', yearIdx[year], '.pdf', sep='')
  }
  
  if (year == 1) {
    characYear = charac # no curve data
  } else {
    # add curve 1-year data to characteristics
    characYear = cbind(pain[,c(1:year)], charac)  
  }
  
  characImp = characYear
  for (i in 1:ncol(characImp)) {
    x = characImp[,i]
    x[is.na(x)] = mean(x, na.rm=T)
    characImp[,i] = x
  }
  
  characImpScale = apply(characImp, 2, sd)
  
  pdf(filename, width=8, height=6)
  par(mfrow=c(2,3))
  
  fit = lassoFit[[1,year]]
  betaAll = lapply(lassoFit[, year], function(x) {
    res = sapply(x$glmnet.fit$beta, function(xx) {
      xx[,which(fit$lambda == fit$lambda.min)]
    })
    return(res)
  })
  beta = betaAll[[1]] # take the average over 10 folds
  for (ii in 2:nFold) {beta = beta + betaAll[[ii]]}
  beta = betaAll[[ii]]/nFold
  
  # rescale beta as if the variances of all variables are scaled to 1 
  beta = diag(characImpScale) %*% beta 
  rownames(beta) = rownames(fit$glmnet.fit$beta[[1]])
  selectedFeatures = apply(beta!=0, 1, sum) > 0
  beta[selectedFeatures]
  barplot(t(beta[selectedFeatures,]), main=title,
          legend = c("type 1", "type 2", "type 3"), beside=T)
  
  testProbR2 = apply(testProbPred, 3, function(x) {
    1 - apply((x-testProb)^2, 2, function(aa) {mean(aa, na.rm=T)})/
      apply(testProb, 2, var)
  })
  testProbR2 = t(testProbR2)
  
  matplot(fit$glmnet.fit$lambda, testProbR2, 
          log='x', xlab='lambda',
          ylab='Test Prob Prediction R2', main=title)
  for (j in 1:nFold) abline(v=lassoFit[[j, year]]$lambda.min, lty=2,col=2)
  
  lambda.min = median(sapply(lassoFit[,year], function(x) {
    x$lambda.min
  }))
  lambdaIdx = which.min(abs(fit$glmnet.fit$lambda - lambda.min))
  testProbR2[lambdaIdx,]
  matplot(testProb, testProbPred[,,lambdaIdx], 
          xlab = 'Test Prob', ylab = 'Test Prob Prediction', type='p',
          main = title, sub = paste('R2 =', round(mean(testProbR2[lambdaIdx,]), 3)))
  abline(a=0,b=1,lty=2,col=2)
  
  
  saveRDS(testProb, paste(predPath, 'testProb', year,'.rds', sep=''))
  saveRDS(testProbPred[,,lambdaIdx], paste(predPath, 'testProbPred', year,'.rds', sep=''))
  
  write.table(testProb, file = "predResult/Pain/testProb.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE)
  
  write.table(testProbPred[,,lambdaIdx], file = "predResult/Pain/testProbPred.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE)
  
  # differentiate type 2 from type 1 
  caseID = 2
  controlID =1 
  caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
  rocCurve = roc(cases = testProbPred[,caseID,lambdaIdx][caseIdx], 
                 controls = testProbPred[,caseID,lambdaIdx][!caseIdx], plot=F)
  df <- data.frame(1-rocCurve$specificities, rocCurve$sensitivities)
  names(df)[1]<-paste("Specificity")
  names(df)[2]<-paste("Sensitivity")
  saveRDS(df, paste(RDSFigsPath, 'PainBoth_AUC21_',year,'.rds', sep=''))
  
  AUC = auc(rocCurve)
  plot(rocCurve, print.auc=T,
       main=paste('case=type ', caseID, ', control=type ', controlID, sep=''))
  
  # differentiate type 3 from type 1 
  caseID = 3
  controlID =1 
  caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
  rocCurve = roc(cases = testProbPred[,caseID,lambdaIdx][caseIdx], 
                 controls = testProbPred[,caseID,lambdaIdx][!caseIdx], plot=F)
  df <- data.frame(1-rocCurve$specificities, rocCurve$sensitivities)
  names(df)[1]<-paste("Specificity")
  names(df)[2]<-paste("Sensitivity")
  saveRDS(df, paste(RDSFigsPath, 'PainBoth_AUC31_',year,'.rds', sep=''))
  AUC = auc(rocCurve)
  plot(rocCurve, print.auc=T, 
       main=paste('case=type ', caseID, ', control=type ', controlID, sep=''))
  
  # differentiate type 3 from type 2
  caseID = 3
  controlID =2
  caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
  rocCurve = roc(cases = testProbPred[,caseID,lambdaIdx][caseIdx], 
                 controls = testProbPred[,caseID,lambdaIdx][!caseIdx], plot=F)
  df <- data.frame(1-rocCurve$specificities, rocCurve$sensitivities)
  names(df)[1]<-paste("Specificity")
  names(df)[2]<-paste("Sensitivity")
  saveRDS(df, paste(RDSFigsPath, 'PainBoth_AUC32_',year,'.rds', sep=''))
  

  AUC = auc(rocCurve)
  plot(rocCurve, print.auc=T,
       main=paste('case=type ', caseID, ', control=type ', controlID, sep=''))
  
  dev.off()
#}
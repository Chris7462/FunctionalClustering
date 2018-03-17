source("code/foldIndex.R")
fclustPath = 'fclustResult/PainRightKnee/'
predPath = 'predResult/PainRightKnee/'

pain = read.table("originalData/pain.txt")
availIdx = readRDS('fclustResult/subsetMap.rds')
length(availIdx)
pain= pain[availIdx, 8+1:8]
dim(pain)

charac = readRDS('predResult/charac.rds')
p = ncol(charac)
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

lassoFit = matrix(list(), nFold, 4) 
# doesn't do 8 years -> data separable, logistic regression diverge

for (year in c(1:4)) {
  if (year == 1) {
    characYear = charac # no curve data
    penalty.factor = c(rep(1, ncol(charac)))
  } else {
    # add curve 1-year data to characteristics
    characYear = cbind(pain[,c(1:year)], charac)  
    penalty.factor = c(rep(0, year), rep(1, ncol(charac)))
  }
  
  # impute the missing charac with mean
  characImp = characYear
  for (i in 1:ncol(characImp)) {
    x = characImp[,i]
    x[is.na(x)] = mean(x, na.rm=T)
    characImp[,i] = x
  }
  
  for (j in 1:nFold) {
    cat('fold =',j,', year =', year, '\n')
    
    if (j != 1) {
      lambda = lassoFit[[1, year]]$glmnet.fit$lambda
    } else {
      lambda = NULL
    }
    
    idx = unlist(fold[-j])
    fclustPred = fclustPredAll[[j,k]]
    orderIdx = order(table(fclustPred$class.pred),decreasing = T)
    
    # make the class order as decreasing order of #subjects
    y = fclustPred$probs[, orderIdx]
    
    stime = proc.time()
    lassoFit[[j,year]] = cv.glmnet(x=as.matrix(characImp[idx,]), y=y, 
                                   nlambda = nlambda, lambda = lambda,
                                   family = 'multinomial',
                                   penalty.factor = penalty.factor,
                                   nfolds = 5, keep=T)
    time = proc.time()-stime
    print(time)
    
  }
  saveRDS(lassoFit, paste(predPath, 'lassoFit.rds', sep=''))
}

# lassoFit = readRDS(paste(predPath, 'lassoFit.rds', sep=''))

### Predict on the test data
## for each subject in the test fold, predict its cluster probability
## using the prediction model trained on the rest 9 folds
k = 3
n = length(availIdx)
testProbPredAll = rep(list(), 4) # probability of fast progressor for nlambda values

for (year in 1:4) {
  nlambda = length(lassoFit[[1,year]]$glmnet.fit$lambda)
  testProbPred = array(NA, dim=c(n, k, nlambda))
  
  if (year == 1) {
    characYear = charac # no curve data
  } else {
    # add curve 1-year data to characteristics
    characYear = cbind(pain[,c(1:year)], charac)  
  }
  
  # impute the missing charac with mean
  characImp = characYear
  for (i in 1:ncol(characImp)) {
    x = characImp[,i]
    x[is.na(x)] = mean(x, na.rm=T)
    characImp[,i] = x
  }
  
  for (j in 1:nFold) {
    idx = fold[[j]]
    
    testProbPred[idx, ,1:length(lassoFit[[j,year]]$glmnet.fit$lambda)] = 
      predict(lassoFit[[j,year]]$glmnet.fit, as.matrix(characImp[idx,]),
              type='response')
  }
  
  testProbPredAll[[year]] = testProbPred
}

saveRDS(testProbPredAll, paste(predPath, 'testProbPredAll.rds', sep=''))

### Prediction results
## compare the predicted test probability with the assigned test probability
library('pROC')
testProb = readRDS(paste(fclustPath, 'testProb.rds', sep=''))

for (year in 1:4) {
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
  
  # differentiate type 2 from type 1 
  caseID = 2
  controlID =1 
  caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
  rocCurve = roc(cases = testProbPred[,caseID,lambdaIdx][caseIdx], 
                 controls = testProbPred[,caseID,lambdaIdx][!caseIdx], plot=F)
  AUC = auc(rocCurve)
  plot(rocCurve, print.auc=T,
       main=paste('case=type ', caseID, ', control=type ', controlID, sep=''))
  
  # differentiate type 3 from type 1 
  caseID = 3
  controlID =1 
  caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
  rocCurve = roc(cases = testProbPred[,caseID,lambdaIdx][caseIdx], 
                 controls = testProbPred[,caseID,lambdaIdx][!caseIdx], plot=F)
  AUC = auc(rocCurve)
  plot(rocCurve, print.auc=T, 
       main=paste('case=type ', caseID, ', control=type ', controlID, sep=''))
  
  # differentiate type 3 from type 2
  caseID = 3
  controlID =2
  caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
  rocCurve = roc(cases = testProbPred[,caseID,lambdaIdx][caseIdx], 
                 controls = testProbPred[,caseID,lambdaIdx][!caseIdx], plot=F)
  AUC = auc(rocCurve)
  plot(rocCurve, print.auc=T,
       main=paste('case=type ', caseID, ', control=type ', controlID, sep=''))
  
  dev.off()
}


## features separating different types
## without curve features
year=1
testProbPred = testProbPredAll[[year]]

characYear = charac # no curve data
characImp = characYear
for (i in 1:ncol(characImp)) {
  x = characImp[,i]
  x[is.na(x)] = mean(x, na.rm=T)
  characImp[,i] = x
}

characImpScale = apply(characImp, 2, sd)

fit = lassoFit[[1,year]]
betaAll = lapply(lassoFit[, year], function(x) {
  res = sapply(x$glmnet.fit$beta, function(xx) {
    xx[,which(fit$lambda == fit$lambda.min)]
  })
  # rescale beta as if the variances of all variables are scaled to 1 
  res = diag(characImpScale) %*% res
  rownames(res) = rownames(fit$glmnet.fit$beta[[1]])
  selectedFeatures = apply(res!=0, 1, sum) > 0
  return(res[selectedFeatures,])
})

print(betaAll)
# save the coefficient to a csv file
# each matrix corresponds to the coefficients in the prediction with each fold left-out
# column x corresponds to the coefficients for type x
lapply(betaAll, write.table, file=paste(predPath, 'coef.csv', sep=''), append=TRUE, sep=',')  






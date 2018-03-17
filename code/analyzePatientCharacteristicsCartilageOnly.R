source("code/foldIndex.R")
fclustPath = 'fclustResult/Cart/'
predPath = 'predResult/Cart/'

cartilage = read.table("originalData/cartilage.txt")
availIdx = readRDS('fclustResult/subsetMap.rds')
length(availIdx)
cartilage = cartilage[availIdx, ]
dim(cartilage)

charac = readRDS('predResult/charac.rds')
p = ncol(charac)

### Train the prediction model 
## treat each fold as the test set, use the rest 9 folds to train 
## a prediction model of cluster posterior probability
## we use lasso as the prediction model
library(glmnet)

fclustPredAll = readRDS(paste(fclustPath, 'fclustPredCartOnly.rds', sep=''))
fold = readRDS('fclustResult/fold.rds')

nFold = 10
k = 2
nlambda=25

# remove years with no observations
cartilage14 = cartilage[, -c(6,8,14,16)]

lassoFit = matrix(list(), nFold, 6)

for (year in c(1:6)) {
  if (year == 1) {
    characYear = charac # no curve data
    penalty.factor = c(rep(1, ncol(charac)))
  } else {
    # add curve 1-year data to characteristics
    characYear = cbind(cartilage14[,c(1:year,6+1:year)], charac)  
    penalty.factor = c(rep(0, 2*year), rep(1, ncol(charac)))
  }
  
  # impute the missing charac with mean
  characImp = characYear
  for (i in 1:ncol(characImp)) {
    x = characImp[,i]
    x[is.na(x)] = mean(x, na.rm=T)
    characImp[,i] = x
  }
  
  for (j in 2:nFold) {
    cat('fold =',j,', year =', year, '\n')
    
    if (j != 1) {
      lambda = lassoFit[[1, year]]$glmnet.fit$lambda
    } else {
      lambda = NULL
    }
    
    idx = unlist(fold[-j])
    fclustPred = fclustPredAll[[j,k]]
    fastProgressor = which.min(table(fclustPred$class.pred))
    
    # make the second class as the fast progressor
    if (fastProgressor == 1) {
      y = fclustPred$probs[, c(2,1)]
    } else {
      y = fclustPred$probs
    }
    
    stime = proc.time()
    lassoFit[[j,year]] = cv.glmnet(x=as.matrix(characImp[idx,]), y=y, 
                                   nlambda = nlambda, lambda = lambda,
                                   family = 'binomial',
                                   penalty.factor = penalty.factor,
                                   nfolds = 5, keep=T)
    time = proc.time()-stime
    print(time)
  
  }
  saveRDS(lassoFit, paste(predPath, 'lassoFit.rds', sep=''))
}

### Predict on the test data
## for each subject in the test fold, predict its cluster probability
## using the prediction model trained on the rest 9 folds
n = length(availIdx)
testProbPredAll = rep(list(), 6) # probability of fast progressor for nlambda values

for (year in 1:6) {
  nlambda = length(lassoFit[[1,year]]$glmnet.fit$lambda)
  testProbPred = matrix(NA, n, nlambda)
  
  if (year == 1) {
    characYear = charac # no curve data
  } else {
    # add curve 1-year data to characteristics
    characYear = cbind(cartilage14[,c(1:year,6+1:year)], charac)  
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
    
    testProbPred[idx,] = predict(lassoFit[[j,year]]$glmnet.fit, as.matrix(characImp[idx,]),
                  type='response')
  }
  
  testProbPredAll[[year]] = testProbPred
}

saveRDS(testProbPredAll, paste(predPath, 'testProbPredAll.rds', sep=''))

### Prediction results
## compare the predicted test probability with the assigned test probability
library('pROC')
testProb = readRDS(paste(fclustPath, 'testProb.rds', sep=''))

for (year in 1:6) {
  testProbPred = testProbPredAll[[year]]
  
  yearIdx = c(0,1,2,3,4,6)
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
    characYear = cbind(cartilage14[,c(1:year,6+1:year)], charac)  
  }
  
  characImp = characYear
  for (i in 1:ncol(characImp)) {
    x = characImp[,i]
    x[is.na(x)] = mean(x, na.rm=T)
    characImp[,i] = x
  }
  
  characImpScale = apply(characImp, 2, sd)
  
  pdf(filename, width=8, height=6)
  par(mfrow=c(2,2))
  
  fit = lassoFit[[1,year]]
  beta = sapply(lassoFit[, year], function(x) {
    x$glmnet.fit$beta[,which(fit$lambda == fit$lambda.min)]
  })
  beta = apply(beta, 1, mean) # take the average over 10 folds
  # rescale beta as if the variances of all variables are scaled to 1 
  beta = characImpScale * beta 
  names(beta) = rownames(fit$glmnet.fit$beta)
  beta[beta!=0]
  barplot(beta[beta!=0], main=title)
  
  testProbR2 = apply(testProbPred, 2, function(x) {
    1 - mean((x-testProb)^2)/var(testProb)
  })
  
  plot(fit$glmnet.fit$lambda, testProbR2, 
       log='x', xlab='lambda',
       ylab='Test Prob Prediction R2', main=title)
  for (j in 1:nFold) abline(v=lassoFit[[j, year]]$lambda.min, lty=2,col=2)
  
  lambda.min = median(sapply(lassoFit[,year], function(x) {
    x$lambda.min
  }))
  lambdaIdx = which(fit$glmnet.fit$lambda == lambda.min)
  testProbR2[lambdaIdx]
  plot(testProb, testProbPred[,lambdaIdx], 
       xlab = 'Test Prob', ylab = 'Test Prob Prediction', 
       main = title, sub = paste('R2 =', round(testProbR2[lambdaIdx], 3)))
  abline(a=0,b=1,lty=2,col=2)
  
  rocCurve = roc(cases = testProbPred[,lambdaIdx][testProb>0.5], 
                 controls = testProbPred[,lambdaIdx][testProb<=0.5], plot=F)
  AUC = auc(rocCurve)
  plot(rocCurve, main=title, print.auc=T)
  
  dev.off()
}



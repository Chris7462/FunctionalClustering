## Load data
source("code/curvecnorm.R")
source("code/makeCurveData.R")
source("code/fclustMulticurve.R")
source("code/getFclustll.R")
library("mvtnorm")

cartilage = read.table("originalData/cartilage.txt")
pain = read.table("originalData/pain.txt")
patientID = read.table("originalData/patientIDs.txt")


### Visualize the data
n = 300
data = cartilage
data = as.matrix(data)

# progression
index = sample(1:nrow(data), n)
matplot(t(data[index,]), pch=20, type = "b", lty=1, col=1, ylab = "cartilage thickness", main="Cartilage (300 Curves)")
matlines(x = c(5,7), y=t(data[index,c(5,7)]), pch=20, type="b", lty=1, col=1)
matlines(x = c(7,9), y=t(data[index,c(7,9)]), pch=20, type="b", lty=1, col=1)
matlines(x = c(13,15), y=t(data[index,c(13,15)]), pch=20, type="b", lty=1, col=1)

# pairwise relationship
idx = apply(data, 1, function(x) {sum(!is.nan(x)) == 0})
pairs(data[!idx, 1:8][, -c(6,8)], main="Cartilage Knee 1")
pairs(data[!idx, 9:16][,-c(6,8)], main="Cartilage Knee 2")

# histogram
par(mfrow = c(2,3))
for (i in c(1:5,7)) {
hist(data[,i], breaks="FD", main=paste("Cartilage Knee 1 Year", i))
}
for (i in c(1:5,7)) {
hist(data[,i+8], breaks="FD", main=paste("Cartilage Knee 2 Year", i))
}
par(mfrow = c(2,3))
for (i in c(2:5,7)) {
hist(data[,i]-data[,1], breaks="FD", main=paste("Cartilage Knee 1 Year", i, "- Year 1"))
}
par(mfrow = c(2,3))
for (i in c(2:5,7)) {
hist(data[,i+8]-data[,1+8], breaks="FD", main=paste("Cartilage Knee 2 Year", i, "- Year 1"))
}

n = 300
data = pain
data = as.matrix(data)
par(mfrow=c(1,1))
index = sample(1:nrow(data), n)

# progression
matplot(t(data[index,]), pch=20, type = "b", lty=1, col=1, ylab = "pain score", main = "Pain (300 Curves)")

# pairwise relationship
pairs(data[,1:8], main="Pain Knee 1")
pairs(data[,9:16], main="Pain Knee 2")

# histogram
par(mfrow = c(2,4))
for (i in c(1:8)) {
hist(data[,i], breaks="FD", main=paste("Pain Knee 1 Year", i))
}
for (i in c(1:8)) {
hist(data[,i+8], breaks="FD", main=paste("Pain Knee 2 Year", i))
}
par(mfrow = c(2,4))
for (i in c(2:8)) {
hist(data[,i]-data[,1], breaks="FD", main=paste("Pain Knee 1 Year", i, "- Year 1"))
}
par(mfrow = c(2,4))
for (i in c(2:8)) {
hist(data[,i+8]-data[,1+8], breaks="FD", main=paste("Pain Knee 2 Year", i, "- Year 1"))
}


### Select patients with at least three visits
cartilage = as.matrix(cartilage)
pain = as.matrix(pain)
getRegNumber <- function(x) {apply(x, 1, function(y) {sum(!is.nan(y))})}
minObs = 3
index = (getRegNumber(cartilage[,1:8]) >= minObs) & (getRegNumber(cartilage[,9:16]) >= minObs)& (getRegNumber(pain[,1:8]) >= minObs) & (getRegNumber(pain[,9:16]) >= minObs)
subsetMap = which(index)
saveRDS(subsetMap, 'fclustResult/subsetMap.rds')

cat("Number of patients with at least three cartilage and pain observations\n")
sum(index)

Data = list(cartilage1 = cartilage[index,1:8], cartilage2 = cartilage[index, 9:16], 
pain1 = pain[index,1:8], pain2 = pain[index, 9:16])


### Outliers
## For both cartilage and pain data sets, the empirical distribution of $(y_2-y_1, \ldots, y_T-y_1)$ is very close to a multivariate normal distribution. I estimated the distribution with sample mean and covariance. For each $y_t$, I estimated its conditional distribution $y_t|y_{-t}$, and cap observations at three conditional standard deviations.

# cartilage
# knee 1 
par(mfrow=c(1,1))
data = Data[[1]][,-c(1,6,8)] - Data[[1]][,1]
data = as.matrix(data)
data[is.nan(data)] = NA

mu = colMeans(data, na.rm=T)
plot(c(2:5,7), mu, xlab = "Year", main="Sample Mean of Cartilage Knee 1 Data (Year 1 Subtracted)")
sigma = cov(data, use="pairwise.complete.obs")
cat("Sample Covariance Matrix of Cartilage Knee 1 Data (Year 1 Subtracted)\n")
sigma

normalSamples = rmvnorm(300, mu, sigma)
pairs(rbind(data, normalSamples), col = c(rep(1, nrow(data)), rep(2, 300)), 
main="Cartilage Knee 1 (Year 1 Subtracted) v.s. Gaussian (Red)")

cap = 3
fracCondsd = getDevCnorm(data, mu, sigma)
idx = which(fracCondsd>cap, arr.ind=T)
cat("Number of outliers in cartilage knee 1 data\n")
nrow(idx)


data1 = data
data1[idx] = getqcnorm(data, idx, mu, sigma, cap)
ss = sample(1:nrow(idx), 8)
par(mfrow=c(1,2))
for (i in idx[ss,1]) {
plot(c(2:5,7), data[i,], ylim=c(-5.2, 3.8), xlab = "Year", ylab = "", col=(fracCondsd[i,]>cap)+1, main=paste("Curve",i))
points(c(2:5,7)[fracCondsd[i,]>cap], data1[i, fracCondsd[i,]>cap], col=3)
legend("topright", c("regular","outlier","corrected"), col=c(1:3), pch=20)
}

newdata = Data[[1]]
newidx = !is.na(Data[[1]][,1])
newdata[newidx,-c(1,6,8)] = Data[[1]][newidx,1] + data1[newidx,]

Data[[1]] = newdata

# knee 2 
par(mfrow=c(1,1))
data = Data[[2]][,-c(1,6,8)] - Data[[2]][,1]
data = as.matrix(data)
data[is.nan(data)] = NA

mu = colMeans(data, na.rm=T)
plot(c(2:5,7), mu, xlab = "Year", main="Sample Mean of Cartilage Knee 2 Data (Year 1 Subtracted)")
sigma = cov(data, use="pairwise.complete.obs")
cat("Sample Covariance Matrix of Cartilage Knee 2 Data (Year 1 Subtracted)\n")
sigma

library("mvtnorm")
normalSamples = rmvnorm(300, mu, sigma)
pairs(rbind(data, normalSamples), col = c(rep(1, nrow(data)), rep(2, 300)), 
main="Cartilage Knee 2 (Year 1 Subtracted) v.s. Gaussian (Red)")

cap = 3
fracCondsd = getDevCnorm(data, mu, sigma)
idx = which(fracCondsd>cap, arr.ind=T)
cat("Number of outliers in cartilage knee 2 data\n")
nrow(idx)

data1 = data
data1[idx] = getqcnorm(data, idx, mu, sigma, cap)
ss = sample(1:nrow(idx), 8)
par(mfrow=c(1,2))
for (i in idx[ss,1]) {
plot(c(2:5,7), data[i,], ylim=c(-5.2, 3.8), xlab = "Year", ylab = "", col=(fracCondsd[i,]>cap)+1, main=paste("Curve",i))
points(c(2:5,7)[fracCondsd[i,]>cap], data1[i, fracCondsd[i,]>cap], col=3)
legend("topright", c("regular","outlier","corrected"), col=c(1:3), pch=20)
}

newdata = Data[[2]]
newidx = !is.na(Data[[2]][,1])
newdata[newidx,-c(1,6,8)] = Data[[2]][newidx,1] + data1[newidx,]

Data[[2]] = newdata

# pain
# knee 1
data = Data[[3]][,-1] - Data[[3]][,1]
data = as.matrix(data)
data[is.nan(data)] = NA

par(mfrow=c(1,1))
mu = colMeans(data, na.rm=T)
plot(2:8, mu, xlab="Year", main="Sample Mean of Pain Knee 1 Data (Year 1 Subtracted)")
sigma = cov(data, use="pairwise.complete.obs")
cat("Sample Covariance Matrix of Pain Knee 1 Data (Year 1 Subtracted)\n")
sigma

library("mvtnorm")
normalSamples = rmvnorm(300, mu, sigma)
pairs(rbind(data, normalSamples), col = c(rep(1, nrow(data)), rep(2, 300)),
main="Pain Knee 1 (Year 1 Subtracted) v.s. Gaussian (Red)")

cap = 3
fracCondsd = getDevCnorm(data, mu, sigma)
idx = which(fracCondsd>cap, arr.ind=T)
cat("Number of outliers in pain knee 1 data\n")
nrow(idx)

data1 = data
data1[idx] = getqcnorm(data, idx, mu, sigma, cap)
ss = sample(1:nrow(idx), 8)
par(mfrow=c(1,2))
for (i in idx[ss,1]) {
plot(2:8,data[i,], xlab = "Year", ylab="",ylim=c(-17,16),col=(fracCondsd[i,]>cap)+1, main=paste("Curve",i))
points(c(2:8)[fracCondsd[i,]>cap], data1[i, fracCondsd[i,]>cap], col=3)
legend("bottomright", c("regular","outlier","corrected"), col=c(1:3), pch=20)
}

newdata = Data[[3]]
newidx = !is.na(Data[[3]][,1])
newdata[newidx,-1] = Data[[3]][newidx,1] + data1[newidx,]
Data[[3]] = newdata

# knee 2
data = Data[[4]][,-1] - Data[[4]][,1]
data = as.matrix(data)
data[is.nan(data)] = NA

par(mfrow=c(1,1))
mu = colMeans(data, na.rm=T)
plot(2:8, mu, xlab="Year", main="Sample Mean of Pain Knee 2 Data (Year 1 Subtracted)")
sigma = cov(data, use="pairwise.complete.obs")
cat("Sample Covariance Matrix of Pain Knee 2 Data (Year 1 Subtracted)\n")
sigma

library("mvtnorm")
normalSamples = rmvnorm(300, mu, sigma)
pairs(rbind(data, normalSamples), col = c(rep(1, nrow(data)), rep(2, 300)),
main="Pain Knee 2 (Year 1 Subtracted) v.s. Gaussian (Red)")

cap = 3
fracCondsd = getDevCnorm(data, mu, sigma)
idx = which(fracCondsd>cap, arr.ind=T)
cat("Number of outliers in pain knee 2 data\n")
nrow(idx)

data1 = data
data1[idx] = getqcnorm(data, idx, mu, sigma, cap)
ss = sample(1:nrow(idx), 8)
par(mfrow=c(1,2))
for (i in idx[ss,1]) {
plot(2:8,data[i,], xlab = "Year", ylab="",ylim=c(-17,16),col=(fracCondsd[i,]>cap)+1, main=paste("Curve",i))
points(c(2:8)[fracCondsd[i,]>cap], data1[i, fracCondsd[i,]>cap], col=3)
legend("bottomright", c("regular","outlier","corrected"), col=c(1:3), pch=20)
}

newdata = Data[[4]]
newidx = !is.na(Data[[4]][,1])
newdata[newidx,-1] = Data[[4]][newidx,1] + data1[newidx,]

Data[[4]] = newdata

### Subtract intercept
## Since we are interested in the curve progression regardless of patients' starting status, for both cartilage and pain data sets, I fit each curve to a linear regression model and got the new data by subtracting the estimated intercept.
##The black lines are the means, and the red dashed lines are means +/- one standard deviations.

t = c(0:7)
data = Data[[1]]
intercept = apply(data, 1, function(x) {lm(x~t)$coefficients[1]})
newdata = data - intercept %*% t(rep(1, ncol(data)))
colMean = apply(newdata, 2, function(x) {mean(x,na.rm=T)})
colSd = apply(newdata, 2, function(x) {sd(x,na.rm=T)})
par(mfrow=c(1,1))
matplot(c(1:8), cbind(colMean, colMean+colSd, colMean-colSd), lty=c(1,2,2), col = c(1,2,2), type='b', pch=20, xlab='Year', ylab='cartilage',
        main = "Cartilage knee 1 data subtracted by intercept estimates")
Data[[1]] = newdata

data = Data[[2]]
intercept = apply(data, 1, function(x) {lm(x~t)$coefficients[1]})
newdata = data - intercept %*% t(rep(1, ncol(data)))
colMean = apply(newdata, 2, function(x) {mean(x,na.rm=T)})
colSd = apply(newdata, 2, function(x) {sd(x,na.rm=T)})
par(mfrow=c(1,1))
matplot(c(1:8), cbind(colMean, colMean+colSd, colMean-colSd), lty=c(1,2,2), col = c(1,2,2), type='b', pch=20, xlab='Year', ylab='cartilage',
        main = "Cartilage knee 2 data subtracted by intercept estimates")
Data[[2]] = newdata

data = Data[[3]]
intercept = apply(data, 1, function(x) {lm(x~t)$coefficients[1]})
newdata = data - intercept %*% t(rep(1, ncol(data)))
colMean = apply(newdata, 2, function(x) {mean(x,na.rm=T)})
colSd = apply(newdata, 2, function(x) {sd(x,na.rm=T)})
par(mfrow=c(1,1))
matplot(c(1:8), cbind(colMean, colMean+colSd, colMean-colSd), lty=c(1,2,2), col = c(1,2,2), type='b', pch=20, xlab='Year', ylab='cartilage',
        main = "Pain knee 1 data subtracted by intercept estimates")
Data[[3]] = newdata

data = Data[[4]]
intercept = apply(data, 1, function(x) {lm(x~t)$coefficients[1]})
newdata = data - intercept %*% t(rep(1, ncol(data)))
colMean = apply(newdata, 2, function(x) {mean(x,na.rm=T)})
colSd = apply(newdata, 2, function(x) {sd(x,na.rm=T)})
par(mfrow=c(1,1))
matplot(c(1:8), cbind(colMean, colMean+colSd, colMean-colSd), lty=c(1,2,2), col = c(1,2,2), type='b', pch=20, xlab='Year', ylab='cartilage',
        main = "Pain knee 2 data subtracted by intercept estimates")
Data[[4]] = newdata

pairs(Data[[1]][,-c(6,8)])
pairs(Data[[2]])
saveRDS(Data, "fclustResult/Data.rds")

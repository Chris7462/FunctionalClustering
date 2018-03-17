charac = read.table("originalData/AllPatientCharsCleaned.txt", header=T)
# cleaned characteristics, remove imbalanced variables

charac = charac[,-1] # remove IDs

# get the index of selected subjects
availIdx = readRDS('fclustResult/subsetMap.rds')
length(availIdx)

charac = charac[availIdx,]

nlevels = apply(charac, 2, function(x) {length(table(x))})
hist(nlevels)
nMissingPerCol = apply(charac, 2, function(x) {sum(is.na(x) | is.nan(x))})
hist(nMissingPerCol)
nMissingPerRow = apply(charac,1,function(x) {sum(is.nan(x)|is.na(x))}) # number of missing data for each row
hist(nMissingPerRow)

# take characteristrics with <= 500 missing values
idx = nMissingPerCol <= 500
sum(idx)
charac = charac[, idx]
dim(charac)
nMissingPerRow = apply(charac,1,function(x) {sum(is.nan(x)|is.na(x))}) # number of missing data for each row
hist(nMissingPerRow)
nMissingPerCol = nMissingPerCol[idx]
hist(nMissingPerCol)
sum(nMissingPerRow == 0)

p = ncol(charac)
n = nrow(charac)
for (i in 1:p) {
  charac[is.nan(charac[,i]),i] = NA
}

charac = as.matrix(sapply(charac, as.numeric))

characMean = apply(charac,2, function(x) {mean(x,na.rm=T)})
hist(characMean)
characStd = apply(charac,2, function(x) {sd(x, na.rm=T)})
hist(characStd)
idx = which(characStd == 0)
idx
# remove constant cols
charac = charac[,-idx]
dim(charac)

saveRDS(charac, 'predResult/charac.rds')


#setwd(" ")

source('code/curvecnorm.R')
source("code/makeCurveData.R")
source("code/fclustMulticurve.R")
source("code/getFclustll.R")
source('code/foldIndex.R')
source("code/getDistance.R")
library(cluster)
library(ggplot2)

savePath = 'fclustResult/Pain/'
Data = readRDS('fclustResult/Data.rds')

fclustPath = savePath
predPath = 'predResult/Pain/'

RDSFigsPath = 'figure_rds_files/'

testProb = readRDS(paste(fclustPath, 'testProb.rds', sep=''))
testProbPred = readRDS(paste(predPath, 'testProbPred1.rds', sep=''))
#Clss = read.table(paste(predPath, 'Class.txt', sep=''))
# caseIdx1 = Clss$V1==1
# caseIdx3 = Clss$V1==3


# caseID = 3
# controlID =1 
# caseIdx = testProb[,caseID]/(testProb[,controlID]+testProb[,caseID])>0.5
# testProb1=testProb[caseIdx,caseID]
# testProbPred1=testProbPred[caseIdx,caseID]
# 
# testProb3=testProb[!caseIdx,caseID]
# testProbPred3=testProbPred[!caseIdx,caseID]
# 
# df <- data.frame(Type = rep(c("1",  "3"), each=1243),
# P1=c(testProb3, testProb1),
# P2=c(testProbPred3, testProbPred1)
# )

df <- data.frame(Type = rep(c("1",  "3"), each=1243),
                 P1=c(testProb[,3], testProb[,3]),
                 P2=c(testProbPred[,3], testProbPred[,3])
)


a=((df$P1+df$P2)/2)-0.5

library(scales)
p <- ggplot(df, aes(P1, P2)) +
  geom_point( size=3) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(-0.01, 1.01), xlim = c(-0.01,1.01)) +
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  geom_point(aes(colour = -a), size=3) +
  scale_colour_gradient2()
p + labs(colour = "Class", x = "True Posterior Probablity", y = "Predicted Posterior Probability") 

# library(scales)
# p <- ggplot(df, aes(P1, P2)) +
#   geom_point( size=3) +
#   theme_minimal(base_size = 20) +
#   coord_cartesian(ylim = c(-0.01, 1.01), xlim = c(-0.01,1.01)) +
#   scale_x_continuous(breaks=seq(0, 1, 0.5)) +
#   scale_y_continuous(breaks=seq(0, 1, 0.5)) +
#   geom_point(aes(colour = -a), size=3) +
#   scale_colour_gradient2(low = "white", high = "gray")
#   #scale_colour_gradient(low = "gray")
# p + labs(colour = "Class", x = "True Posterior Probablity", y = "Predicted Posterior Probability") 

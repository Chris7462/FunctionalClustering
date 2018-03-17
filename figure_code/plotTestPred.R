#setwd(" ")

source('code/curvecnorm.R')
source("code/makeCurveData.R")
source("code/fclustMulticurve.R")
source("code/getFclustll.R")
source('code/foldIndex.R')
source("code/getDistance.R")
library(cluster)

savePath = 'fclustResult/Cart/'
Data = readRDS('fclustResult/Data.rds')

fclustPath = savePath
predPath = 'predResult/Cart/'

RDSFigsPath = 'figure_rds_files/'

testProb = readRDS(paste(fclustPath, 'testProb.rds', sep=''))
testProbPred = readRDS(paste(predPath, 'testProbPred3.rds', sep=''))


df <- data.frame(
  as.matrix(testProb),
  as.matrix(testProbPred)
)


a=((testProb+testProbPred)/2)-0.5

p <- ggplot(df, aes(testProb, testProbPred)) +
  geom_point( size=3) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(-0.01, 1.01), xlim = c(-0.01,1.01)) +
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  geom_point(aes(colour = -a), size=3) +
  scale_colour_gradient2() +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "Class", x = "True Posterior Probablity", y = "Predicted Posterior Probability") 

# testProb_P= testProb[testProb>0.5]
# testProbPred_P= testProbPred[testProb>0.5]
# testProb_N= testProb[testProb<0.5]
# testProbPred_N= testProbPred[testProb<0.5]
# df2 <- data.frame(
#   as.matrix(testProb_P),
#   as.matrix(testProbPred_P)
# )
# df3 <- data.frame(
#   as.matrix(testProb_N),
#   as.matrix(testProbPred_N)
# )
# # a2=((testProb_P+2*testProbPred_P)/3)-0.25
# p <- ggplot(df2, aes(testProb_P, testProbPred_P)) +
#   geom_point( size=3) +
#   theme_minimal(base_size = 20) +
#   coord_cartesian(ylim = c(-0.01, 1.01), xlim = c(-0.01,1.01)) +
#   scale_x_continuous(breaks=seq(0, 1, 0.5)) +
#   scale_y_continuous(breaks=seq(0, 1, 0.5)) +
#   geom_point(aes(colour = testProbPred_P), size=3) +
#   scale_colour_gradient2() +
#   theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
# p + labs(colour = "Class", x = "True Posterior Probablity", y = "Predicted Posterior Probability") 
# 
# a3=((testProb_N+2*testProbPred_N)/3)+0.25
# p <- ggplot(df3, aes(testProb_N, testProbPred_N)) +
#   geom_point( size=3) +
#   theme_minimal(base_size = 20) +
#   coord_cartesian(ylim = c(-0.01, 1.01), xlim = c(-0.01,1.01)) +
#   scale_x_continuous(breaks=seq(0, 1, 0.5)) +
#   scale_y_continuous(breaks=seq(0, 1, 0.5)) +
#   geom_point(aes(colour = testProbPred_N-1), size=3) +
#   scale_colour_gradient2() +
#   theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
# p + labs(colour = "Class", x = "True Posterior Probablity", y = "Predicted Posterior Probability") 


library(ggplot2)
source('fclustMulticurve.R')
setwd('../draft_figure_code')


fclustFit = readRDS("fclustFit.rds")
k = 2 # number of clusters
res = fclust.curvepred.multicurve(fclustFit[[k]])
FullS = fclustFit[[k]]$FullS
S = bdiag(FullS, FullS, FullS, FullS)
Gamma = fclustFit[[k]]$parameters$Gamma
sigma = fclustFit[[k]]$parameters$sigma
meancurve_cov = S %*% Gamma %*% t(S)
std = sqrt(diag(meancurve_cov)) # standard deviation without noise
#std = sqrt(diag(meancurve_cov)+rep(sigma, c(8,8,8,8))) # standard deviation with noise (prediction)

i=1
idx = (i-1)*8+c(1:8)
years <- c(0:7)
Mean <- res$meancurves[idx,2]
High <- res$meancurves[idx,2] + std[1:8]
Low <- res$meancurves[idx,2] - std[1:8]
limits <- aes(ymax = High, ymin=Low)
Prep <- res$meancurves[idx,1]
minptemp <- data.frame(years, High, Low, Mean, Prep)

plt <- ggplot(minptemp, aes(x= years, y=Mean, ymin= Low, ymax= High))
plt + geom_smooth(span = 0.5,fill="darkblue", colour="darkblue", size=2.5,alpha = 0.2)+
  geom_linerange(size=0.5, colour="darkblue")+theme_classic()

i=2
idx2 = (i-1)*8+c(1:8)
Mean2 <- res$meancurves[idx,1]
High2 <- res$meancurves[idx,1] + std[9:16]
Low2 <- res$meancurves[idx,1] - std[9:16]
limits2 <- aes(ymax = High2, ymin=Low2)
Prep2 <- res$meancurves[idx2,1]
minptemp2 <- data.frame(years, High2, Low2, Mean2, Prep2)


df2 <- data.frame(phenotype = rep(c("Slow", "Fast"), each=8),
                  year=years,
                  y=c(Mean,Mean2),
                  ymin=c(Low,Low2),
                  ymax=c(High,High2) )


# ggplot(df2, aes(x=year, y=y, group=phenotype, ymin=ymin, ymax=ymax)) +
#   geom_smooth(aes(linetype = phen, color = phen),size=2.5, se=FALSE)+
#   theme(legend.position="top") + theme_classic()+ geom_linerange(size=0.5) +
#   scale_linetype_manual(values=c("solid", "solid"))+
#   scale_color_manual(values=c('darkred','darkblue'))

ggplot(df2, aes(x=year, y=y, group=phenotype, ymin=ymin, ymax=ymax), color = phenotype) +
  geom_smooth(span = 0.5,aes(linetype = phenotype, color = phenotype),size=2.5, se=FALSE)+
  theme(legend.position="top") +  geom_errorbar(aes(color = phenotype),size=0.2,width=0.1) +
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c('darkred','darkblue'))+
  xlab("years since baseline visit") +  ylab( "cartilage wear (mm)") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12))

theme_classic()+
# 
# ggplot(df2, aes(x=year, y=y, group=phen)) + 
#   geom_smooth() + geom_point()
# 
# ggplot(df2, aes(x=year, y=y, group=phen))+ 
#   geom_smooth(se = FALSE) + theme_classic() + 
#   scale_x_continuous("the x label") + scale_x_continuous("the y label") +
#   scale_colour_discrete("")
# 
# ggplot(df2, aes(x=year, y=y, group=phen)) +
#   geom_smooth(span = 0.5,fill="darkblue", color=c("red","darkblue"), size=2.5,alpha = 0.2)+
#   theme(legend.position="top")+theme_classic()
# 
# 
# plt <- ggplot(minptemp, aes(x= years, y=Mean, ymin= Low, ymax= High))
# plt + geom_smooth(span = 0.5,fill="darkblue", colour="darkblue", size=2.5,alpha = 0.2)+
#   geom_linerange(size=0.5, colour="darkblue")+theme_classic()+
# ggplot(minptemp, aes(x= years, y=Mean2, ymin= Low2, ymax= High2))
# 
# plt <-ggplot(minptemp, aes(x= years, y=Mean2, ymin= Low2, ymax= High2))+
#   geom_smooth(span = 0.5,fill="darkblue", colour="darkblue", size=2.5,alpha = 0.2)+
#   geom_linerange(size=0.5, colour="darkblue")+theme_classic()

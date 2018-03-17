
library(ggplot2)

#setwd(" ")

RDSFigsPath = 'figure_rds_files/'

#_______________LEFT CARTILAGE___________________

#__________________Plot Sil_______________________
CL_S = readRDS(paste(RDSFigsPath,"CartL_Sil.rds",sep=''))
p <- ggplot(CL_S, aes(Nclust, fclustSil)) +
  geom_point(color = 'grey45', size=6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(0,1), xlim = c(1.75, 4.25)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "CCI", x = "Number of Clusters", y = "Silhouette")

#__________________Plot Traj_______________________
CL_T = readRDS(paste(RDSFigsPath,"CartL_Traj.rds",sep=''))
years <- c(0:7)
dff <- data.frame(Type = rep(c("Nonprogressor", "Progressor"), each=8),
                  Year=years,
                  Progression=c(CL_T$Mean1, CL_T$Mean2))
ggplot(dff, aes(x=Year, y=Progression), color = Type) +
  geom_smooth(span = 0.5,aes(linetype = Type, color = Type),size=2.5, se=FALSE)+
  theme_classic(base_size = 10) +
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c('darkred','darkblue'))+
  xlab("years since baseline visit") +  ylab( "cartilage wear (mm)") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
  theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12)) +
  theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

#__________________Plot AUC_______________________
df= readRDS(paste(RDSFigsPath,"CartL_AUC1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"CartL_AUC2.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"CartL_AUC3.rds",sep=''))
dff <- data.frame(Predictors = rep(c("YR 0", "YRS 0 to 1", "YRS 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=1, se=FALSE)+
  theme_minimal(base_size = 20) +
  theme(legend.position = c(0.75, 0.25)) +  
  scale_linetype_manual(values=c("solid", "twodash", "dotted"))+
  scale_color_manual(values=c('grey45','grey30', 'grey20'))+
  coord_cartesian(ylim = c(0,1), xlim = c(0, 1)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")


#_______________RIGHT CARTILAGE___________________

#__________________Plot Sil_______________________
CL_S = readRDS(paste(RDSFigsPath,"CartR_Sil.rds",sep=''))
p <- ggplot(CL_S, aes(Nclust, fclustSil)) +
  geom_point(color = 'grey45', size=6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(0,1), xlim = c(1.75, 4.25)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "CCI", x = "Number of Clusters", y = "Silhouette")

#__________________Plot Traj_______________________
CL_T = readRDS(paste(RDSFigsPath,"CartR_Traj.rds",sep=''))
years <- c(0:7)
dff <- data.frame(Type = rep(c("Nonprogressor", "Progressor"), each=8),
                  Year=years,
                  Progression=c(CL_T$Mean1, CL_T$Mean2))
ggplot(dff, aes(x=Year, y=Progression), color = Type) +
  geom_smooth(span = 0.5,aes(linetype = Type, color = Type),size=2.5, se=FALSE)+
  theme_classic(base_size = 10) +
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c('darkred','darkblue'))+
  xlab("years since baseline visit") +  ylab( "cartilage wear (mm)") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
  theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12)) +
  theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

#__________________Plot AUC_______________________
df= readRDS(paste(RDSFigsPath,"CartR_AUC1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"CartR_AUC2.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"CartR_AUC3.rds",sep=''))
dff <- data.frame(Predictors = rep(c("YR 0", "YRS 0 to 1", "YRS 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=1, se=FALSE)+
  theme_minimal(base_size = 20) +
  theme(legend.position = c(0.75, 0.25)) +  
  scale_linetype_manual(values=c("solid", "twodash", "dotted"))+
  scale_color_manual(values=c('grey45','grey30', 'grey20'))+
  coord_cartesian(ylim = c(0,1), xlim = c(0, 1)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")


library(ggplot2)
library(RColorBrewer)
coul = brewer.pal(5, "BuPu") 


#setwd(" ")
RDSFigsPath = 'figure_rds_files/'

#__________________Right Cart_______________________
df= readRDS(paste(RDSFigsPath,"CartR_AUC1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"CartR_AUC2.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"CartR_AUC3.rds",sep=''))
dff <- data.frame(Predictors = rep(c("yr 0", "yrs 0 to 1", "yrs 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  #theme(legend.position = c(0.75, 0.25)) + 
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid","solid"))+
  scale_color_manual(values=c('seashell2','lavenderblush3', 'thistle4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")

#__________________Left Cart_______________________
df= readRDS(paste(RDSFigsPath,"CartL_AUC1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"CartL_AUC2.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"CartL_AUC3.rds",sep=''))
dff <- data.frame(Predictors = rep(c("yr 0", "yrs 0 to 1", "yrs 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
 # theme(legend.position = c(0.75, 0.25)) +  
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid","solid"))+
  scale_color_manual(values=c('seashell2','lavenderblush3', 'thistle4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")


#__________________Both Cart_______________________
df= readRDS(paste(RDSFigsPath,"CartBoth_AUC1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"CartBoth_AUC2.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"CartBoth_AUC3.rds",sep=''))
dff <- data.frame(Predictors = rep(c("yr 0", "yrs 0 to 1", "yrs 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid","solid"))+
  scale_color_manual(values=c('seashell2','lavenderblush3', 'thistle4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")

#__________________to make legend_______________________
df= readRDS(paste(RDSFigsPath,"CartBoth_AUC1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"CartBoth_AUC2.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"CartBoth_AUC3.rds",sep=''))
dff <- data.frame(Predictors = rep(c("baseline clinical variables  ", "baseline variables & one-year progression  ", "baseline variables & two-year progression  "), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid","solid"))+
  scale_color_manual(values=c('seashell2','lavenderblush3', 'thistle4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
 theme(legend.position = "bottom") +  
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")


#__________________________________________________________

#__________________Right Pain_______________________
df= readRDS(paste(RDSFigsPath,"PainR_AUC21_1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"PainR_AUC32_1.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"PainR_AUC31_1.rds",sep=''))
dff <- data.frame(Predictors = rep(c("yr 0", "yrs 0 to 1", "yrs 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.1, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  #theme(legend.position = c(0.75, 0.25)) +  
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))+
  scale_color_manual(values=c('azure3','cornsilk3', 'azure4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")

#__________________Left Pain_______________________
df= readRDS(paste(RDSFigsPath,"PainL_AUC21_1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"PainL_AUC32_1.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"PainL_AUC31_1.rds",sep=''))
dff <- data.frame(Predictors = rep(c("yr 0", "yrs 0 to 1", "yrs 0 to 2"), each=1223),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  # theme(legend.position = c(0.75, 0.25)) +  
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))+
  scale_color_manual(values=c('azure3','cornsilk3', 'azure4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")

#__________________Both Pain_______________________
df= readRDS(paste(RDSFigsPath,"PainBoth_AUC21_1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"PainBoth_AUC32_1.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"PainBoth_AUC31_1.rds",sep=''))
# df3$Specificity[1242]=0.000656898
# df3$Specificity[1243]=0.000356898
# df3$Specificity[1244]=0.000
dff <- data.frame(Predictors = rep(c("yr 0", "yrs 0 to 1", "yrs 0 to 2"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  # theme(legend.position = c(0.75, 0.25)) +  
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  scale_y_continuous(breaks=seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))+
  scale_color_manual(values=c('azure3','cornsilk3', 'azure4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")

#__________________to make legend_______________________
df= readRDS(paste(RDSFigsPath,"PainR_AUC21_1.rds",sep=''))
df2= readRDS(paste(RDSFigsPath,"PainR_AUC32_1.rds",sep=''))
df3= readRDS(paste(RDSFigsPath,"PainR_AUC31_1.rds",sep=''))
dff <- data.frame(Predictors = rep(c("Stable vs. Worsening", "Stable vs. Improving", "Worsening vs Improving"), each=1244),
                  Specificity=df$Specificity,
                  Sensitivity=c(df$Sensitivity,df2$Sensitivity, df3$Sensitivity))
p <- ggplot(dff, aes(Specificity, Sensitivity), color=Predictors) +
  geom_smooth(span = 0.5, aes(linetype = Predictors, color = Predictors),size=3, se=FALSE)+
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom") +  
  scale_linetype_manual(values=c("solid", "solid", "solid"))+
  scale_color_manual(values=c('azure3','cornsilk3', 'azure4'))+
  coord_cartesian(ylim = c(-0.05,1.05), xlim = c(-0.05,1.05)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(x = "1 - Specificity", y = "Sensitivity")


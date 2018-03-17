

library(ggplot2)
#setwd(" ")
RDSFigsPath = 'figure_rds_files/'

#__________________Right Cart Traj_______________________
CR_T = readRDS(paste(RDSFigsPath,"CartR_Traj.rds",sep=''))
years <- c(0:7)
dff <- data.frame(Type = rep(c("Nonprogressor", "Progressor"), each=8),
                  Year=years,
                  Progression=c(CR_T$Mean1, CR_T$Mean2))
ggplot(dff, aes(x=Year, y=Progression), color = Type) +
  geom_smooth(span = 0.5,aes(linetype = Type, color = Type),size=2.5, se=FALSE)+
  theme_classic(base_size = 10) +
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c('darkblue', 'darkred' ))+
  xlab("Years since baseline visit") +  ylab( "Cartilage wear (mm)") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
  coord_cartesian(ylim = c(-3.1,0.1)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1.2)) +
  theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12)) +
  theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

#__________________Left Cart Traj_______________________
CL_T = readRDS(paste(RDSFigsPath,"CartL_Traj.rds",sep=''))
years <- c(0:7)
dff <- data.frame(Type = rep(c("Nonprogressor", "Progressor"), each=8),
                  Year=years,
                  Progression=c(CL_T$Mean1, CL_T$Mean2))
ggplot(dff, aes(x=Year, y=Progression), color = Type) +
  geom_smooth(span = 0.5,aes(linetype = Type, color = Type),size=2.5, se=FALSE)+
  theme_classic(base_size = 10) +
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c('darkblue', 'darkred'))+
  xlab("Years since baseline visit") +  ylab( "Cartilage wear (mm)") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
  coord_cartesian(ylim = c(-3.1,0.1)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1.2)) +
  theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12)) +
  theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )


#__________________Right Pain Traj_______________________
CR_P = readRDS(paste(RDSFigsPath,"PainR_Traj.rds",sep=''))
years <- c(0:7)
dff <- data.frame(Type = rep(c("Worsening", "Improving", "Stable"), each=8),
                  Year=years,
                  Progression=c(CR_P$Mean1, CR_P$Mean2, CR_P$Mean3))
ggplot(dff, aes(x=Year, y=Progression), color = Type) +
  geom_smooth(span = 0.5,aes(linetype = Type, color = Type),size=2.5, se=FALSE)+
  theme_classic(base_size = 10) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))+
  scale_color_manual(values=c('darkblue', 'gray','darkred'))+
  xlab("Years since baseline visit") +  ylab( "Change in WOMAC score") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
  coord_cartesian(ylim = c(-5.3,7.7)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1.2)) +
  theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12)) +
  theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )
#__________________Left Pain Traj_______________________
CL_P = readRDS(paste(RDSFigsPath,"PainL_Traj.rds",sep=''))
years <- c(0:7)
dff <- data.frame(Type = rep(c("Worsening", "Stable", "Improving"), each=8),
                  Year=years,
                  Progression=c(CL_P$Mean1, CL_P$Mean2, CL_P$Mean3))
ggplot(dff, aes(x=Year, y=Progression), color = Type) +
  geom_smooth(span = 0.5,aes(linetype = Type, color = Type),size=2.5, se=FALSE)+
  theme_classic(base_size = 10) +
  scale_linetype_manual(values=c("solid", "solid","solid"))+
  scale_color_manual(values=c('darkblue', 'gray','darkred'))+
  xlab("Years since baseline visit") +  ylab( "Change in WOMAC score") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +
  coord_cartesian(ylim = c(-5.3,7.7)) +
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1.2)) +
  theme(text = element_text(size = 15) , legend.title=element_text(size=12) , legend.text=element_text(size=12)) +
  theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

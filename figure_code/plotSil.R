
library(ggplot2)

#setwd(" ")

RDSFigsPath = 'figure_rds_files/'

#__________________Right Cart_______________________
CL_S = readRDS(paste(RDSFigsPath,"CartR_Sil.rds",sep=''))
p <- ggplot(CL_S, aes(Nclust, fclustSil)) +
  geom_point(color = 'grey45', size=6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(-0.05,0.65), xlim = c(1.75, 4.25)) +
  scale_x_continuous(breaks=c(2,3,4))+
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "CCI", x = "Number of Clusters", y = "Silhouette")

#__________________Left Cart_______________________
CL_S = readRDS(paste(RDSFigsPath,"CartL_Sil.rds",sep=''))
p <- ggplot(CL_S, aes(Nclust, fclustSil)) +
  geom_point(color = 'grey45', size=6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(-0.05,0.65), xlim = c(1.75, 4.25)) +
  scale_x_continuous(breaks=c(2,3,4))+
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "CCI", x = "Number of Clusters", y = "Silhouette")

#__________________Right Pain_______________________
CL_S = readRDS(paste(RDSFigsPath,"PainR_Sil.rds",sep=''))
p <- ggplot(CL_S, aes(Nclust, fclustSil)) +
  geom_point(color = 'grey45', size=6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(-0.05,0.65), xlim = c(1.75, 4.25)) +
  scale_x_continuous(breaks=c(2,3,4))+
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "CCI", x = "Number of Clusters", y = "Silhouette")

#__________________Left Pain_______________________
CL_S = readRDS(paste(RDSFigsPath,"PainL_Sil.rds",sep=''))
p <- ggplot(CL_S, aes(Nclust, fclustSil)) +
  geom_point(color = 'grey45', size=6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(ylim = c(-0.05,0.65), xlim = c(1.75, 4.25)) +
  scale_x_continuous(breaks=c(2,3,4))+
  theme(axis.title.x = element_text(vjust = -0.2), axis.title.y = element_text(vjust = 1))
p + labs(colour = "CCI", x = "Number of Clusters", y = "Silhouette")

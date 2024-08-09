library(openxlsx)
library(ggplot2)
library(ggrepel)
medsRA<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\Snapshot_Image_Creator.xlsx', sheet=3)
medsRA$Composite.NNH1<-ifelse(medsRA$Composite.NNH<=500,medsRA$Composite.NNH,ifelse(medsRA$Composite.NNH<11000,NA,medsRA$Composite.NNH-10500))
medsRA <- medsRA[!is.na(medsRA$Composite.NNH1) ,]
data("medsRA", package = "ggplot2")
ggplot(medsRA, aes(x=Composite.NNT, y=Composite.NNH))
names(medsRA)[names(medsRA) == "Clinical.effectiveness.ranking"] <- "Ranking"
g<-ggplot(medsRA[which(medsRA$Ranking=="A"),], aes(y=Composite.NNT, x=Composite.NNH))+
  geom_point(aes(col=Name,shape=Ranking), size=6)+ 
  # geom_text(aes(label = paste(Drugs, Adjusted.Value.Score)))+
  geom_text_repel(aes(label=paste(Name, Adjusted.Value.Score),size=1),show.legend = FALSE)+
  # coord_cartesian(xlim=c(0,12), ylim=c(-1000, 12000))+ 
  # scale_y_continuous(breaks = 1:6, labels = c(1:3,"break",7:8))+
  ggtitle("Clinical Effectiveness of RA Medications", 
          subtitle="Composite NNT Vs Composite NNH") + xlab("Composite NNH") + ylab("Composite NNT")+
  theme(
    legend.position="none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(g)

g<-ggplot(medsRA[which(medsRA$Ranking=="A"|medsRA$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  xlim(-50, 50) + ylim(-50, 50)+
  geom_point(aes(col=Name,shape=Ranking), size=6)+ 
  geom_text_repel(aes(label=paste(Name, Adjusted.Value.Score),size=1),show.legend = FALSE, hjust = 0.5, vjust = 1)+
  ggtitle("Clinical Effectiveness of RA Medications", 
          subtitle="Distance from median composite NNT Vs Distance from median composite NNH") + xlab("Distance from median composite NNH") + ylab("Distance from median composite NNT")+
  theme(
    legend.position="none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(g)
g<-ggplot(medsRA[which(medsRA$Ranking=="A"|medsRA$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  xlim(-50, 50) + ylim(-50, 50)+
  geom_point(aes(col=Name2,shape=Ranking), size=6)+ 
  geom_text_repel(aes(label=Name2, size=1),show.legend = FALSE, hjust = 0.5, vjust = 1)+
  ggtitle("Clinical Effectiveness of RA Medications", 
          subtitle="Distance from median composite NNT Vs Distance from median composite NNH") + xlab("Distance from median composite NNH") + ylab("Distance from median composite NNT")+
  theme(
    legend.position="none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(g)

g<-ggplot(medsRA[which(medsRA$Ranking=="A"|medsRA$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  # xlim(-50, 50) + ylim(-50, 50)+
  geom_point(aes(col='black',shape=Ranking), size=6)+ 
  # geom_text_repel(aes(label=paste0(Name, ' (',Adjusted.Value.Score,')'),size=1),show.legend = FALSE, hjust = 0.5, vjust = 1)+
  geom_label_repel(aes(label=paste0(Drugs, ' (',Adjusted.Value.Score,')')),show.legend = FALSE, size=6, hjust = 0.5, vjust = 1, fill = alpha(c("white"),0.5))+
  # ggtitle("Clinical Effectiveness of RA Medications"
  # , 
  # subtitle="Distance from median composite NNT Vs Distance from median composite NNH"
  # ) + 
  # xlab("Distance from median composite NNH") + ylab("Distance from median composite NNT")+
  theme(
    legend.position="bottom",
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 2),
    axis.title=element_text(size=15)
  )+
  scale_color_discrete(guide=FALSE)+
  scale_x_continuous("% point from median composite NNH", breaks=c(-50,-20,0,20,50), limits = c(-50,50))+
  scale_y_continuous("% point from median composite NNT", breaks=c(-50,-20,0,20,50))
plot(g)
g<-ggplot(medsRA[which(medsRA$Ranking=="A"|medsRA$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  geom_point(aes(col='black',shape=Ranking), size=6)+ 
  geom_label_repel(aes(label=Name2),show.legend = FALSE, size=6, hjust = 0.5, vjust = 1, fill = alpha(c("white"),0.5))+
  theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 2),
    axis.title=element_text(size=15)
  )+
  scale_color_discrete(guide=FALSE)+
  scale_x_continuous("% point from median composite NNH", breaks=c(-50,-20,0,20,50), limits = c(-50,50))+
  scale_y_continuous("% point from median composite NNT", breaks=c(-50,-20,0,20,50))
plot(g)
rects<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\rects.xlsx', sheet=1)

##MS####
medsMS<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\Snapshot_Image_Creator.xlsx', sheet='MS')
names(medsMS)[names(medsMS) == "Clinical.effectiveness.ranking"] <- "Ranking"
g<-ggplot(medsMS[which(medsMS$Ranking=="A"|medsMS$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  geom_point(aes(col='black',shape=Ranking), size=6)+ 
  geom_label_repel(aes(label=paste0(Drugs, ' (',Adjusted.Value.Score,')')),show.legend = FALSE, size=5.5, hjust = 0.5, vjust = 1, fill = alpha(c("white"),0.5))+
  theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 2),
    axis.title=element_text(size=15)
  )+
  scale_color_discrete(guide=FALSE)+
  scale_x_continuous("% point from median composite NNH", breaks=c(-50,-20,0,20,50), limits = c(-50,50))+
  scale_y_continuous("% point from median composite NNT", breaks=c(-50,-20,0,20,50))
plot(g)
##ASTHMA####
medsAsthma<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\Snapshot_Image_Creator.xlsx', sheet='Asthma')
names(medsAsthma)[names(medsAsthma) == "Clinical.effectiveness.ranking"] <- "Ranking"
g<-ggplot(medsAsthma[which(medsAsthma$Ranking=="A"|medsAsthma$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  geom_point(aes(col='black',shape=Ranking), size=6)+ 
  geom_label_repel(aes(label=paste0(Drugs, ' (',Adjusted.Value.Score,')')),show.legend = FALSE, size=5.5, hjust = 0.5, vjust = 1, fill = alpha(c("white"),0.5))+
  theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 2),
    axis.title=element_text(size=15)
  )+
  scale_color_discrete(guide=FALSE)+
  scale_x_continuous("% point from median composite NNH", breaks=c(-50,-20,0,20,50), limits = c(-50,50))+
  scale_y_continuous("% point from median composite NNT", breaks=c(-50,-20,0,20,50))
plot(g)
##Hemophilia Proph####
medsHemoProph<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\Snapshot_Image_Creator.xlsx', sheet='Hemo_Proph')
names(medsHemoProph)[names(medsHemoProph) == "Clinical.effectiveness.ranking"] <- "Ranking"

g<-ggplot(medsHemoProph, aes(x = reorder(Drugs, -fromNNT), y = fromNNT)) + 
  geom_bar(stat = "identity")+
  xlab('Medication name')+
  ylab("% point from median composite NNT")
plot(g)
##Hemophilia OD####
medsHemoOD<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\Snapshot_Image_Creator.xlsx', sheet='Hemo_OD')
names(medsHemoOD)[names(medsHemoOD) == "Clinical.effectiveness.ranking"] <- "Ranking"

g<-ggplot(medsHemoOD, aes(x = reorder(Drugs, -fromNNT), y = fromNNT)) + 
  geom_bar(stat = "identity")+
  xlab('Medication name')+
  ylab("% point from median composite NNT")
plot(g)
##Psoriasis####
medsPso<-openxlsx::read.xlsx('C:\\Users\\CA08449\\Documents\\VBSP\\Excel tool\\Snapshot_Image_Creator.xlsx', sheet='Psoriasis')
names(medsPso)[names(medsPso) == "Clinical.effectiveness.ranking"] <- "Ranking"
g<-ggplot(medsPso[which(medsPso$Ranking=="A"|medsPso$Ranking=="B"),], aes(y=fromNNT, x=fromNNH))+
  geom_point(aes(col='black',shape=Ranking), size=6)+ 
  geom_label_repel(aes(label=paste0(Drugs, ' (',Adjusted.Value.Score,')')),show.legend = FALSE, size=5.5, hjust = 0.5, vjust = 1, fill = alpha(c("white"),0.5))+
  theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 2),
    axis.title=element_text(size=15)
  )+
  scale_color_discrete(guide=FALSE)+
  scale_x_continuous("% point from median composite NNH", breaks=c(-50,-20,0,20,50), limits = c(-50,50))+
  scale_y_continuous("% point from median composite NNT", breaks=c(-50,-20,0,20,50))
plot(g)

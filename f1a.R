setwd('*/ASD_families/analysis/f1a')
rm(list=ls())

library(vegan)
library(ggpubr)
library(reshape2)
library(ggsci)
library (lubridate)
library("dplyr")
library("ggExtra")
options(stringsAsFactors=F)

df1<-read.delim('species.csv',sep = ',',row.names = 1)
sd <- read.delim("sample list.csv",head=T,sep=",",check.names = FALSE)
sd <- subset(sd, Group == "ASD-M"|Group == "TD")
#sd <- subset(sd, Group == "ASD-S"|Group == "TD")
#sd <- subset(sd, Group == "ASD-O"|Group == "TD")

df1 <- df1[,colnames(df1) %in% sd$SubjectID]
df1 <- as.matrix(df1)
sd <- sd[sd$SubjectID %in% colnames(df1),]

df1 <- df1[,sd$SubjectID]


df<-df1
dataT<-as.data.frame(t(df))
dataT[is.na(dataT)] <- 0
dist <- vegdist(dataT, method="bray")
dist <- as.matrix(dist)
adist<-as.dist(dist)

rownames(sd) <- as.character(sd[,1])
pc_num <-c(1,2)
pc_x <- pc_num[1]
pc_y <- pc_num[2]
pcoa <- cmdscale(dist, k=3, eig=TRUE)
pc12 <- pcoa$points[,pc_num]
pc <- round(pcoa$eig/sum(pcoa$eig)*100,digits = 2)
pc12 <- as.data.frame(pc12)
colnames(pc12) <- c("pc_x","pc_y")
pc12['sample'] <- rownames(pc12)
colnames(sd)[1:2] <- c("sample","group")
sd$group<-factor(sd$group,levels=sd$group[!duplicated(sd$group)])
pc12 <- merge(pc12,sd,by="sample")
pc12$group<-factor(pc12$group,levels=levels(sd$group))

pc_aver<-as.data.frame(pc12 %>% dplyr::select(-sample) %>% group_by(group)%>%summarise_all(funs(mean)))

mycols<-c("#CC6666","#5E4FA2")
#mycols<-c("#53868B","#5E4FA2")
#mycols<-c("#DD8E08","#5E4FA2")

sd<-sd[rownames(dist),]
ADONIS<-adonis(dist~sd$group)

TEST<-ADONIS$aov.tab$`Pr(>F)`[1]
R2adonis<-round(ADONIS$aov.tab$R2[1],digits = 3)
F.Model<-round(ADONIS$aov.tab$F.Model[1],digits = 3)

p1<-ggscatter(pc12, x = "pc_x", y = "pc_y",color = 'group',
                fill = "group", shape = "group", palette = mycols, size=3,
                ellipse = F,# conf.int.level = 0.95,
                alpha=0.5,
                mean.point = F,
                star.plot = TRUE,star.plot.lty = 1,star.plot.lwd = 0.2)+
  ylab(paste0("PCoA",pc_y,"(",round(pc[pc_y],2),"%",")"))+
  xlab(paste0("PCoA",pc_x,"(",round(pc[pc_x],2),"%",")"))+
  scale_shape_manual(values = rep(23,length(levels(pc12$group))))+
  geom_point(data=pc_aver,aes(x=pc_x,y=pc_y,fill=group),size=6,shape=21)+
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.border = element_rect(color = "black",size = 1.0,fill = NA),
        text = element_text(size=12))+ 
  annotate('text',x=0.24,y=-0.35,label=paste0('p=',TEST))+ 
  annotate('text',x=0.24,y=-0.31,label=paste0('F=',F.Model))
p1

ggsave('pcoa_plot_Multiplex_TD.pdf',width=6,height=5)
ggsave('pcoa_plot_Simplex_TD.pdf',width=6,height=5)
ggsave('pcoa_plot_ASD-O_TD.pdf',width=6,height=5)



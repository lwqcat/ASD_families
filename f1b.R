setwd('*/ASD_families/analysis/f1b')

divdata=read.table("divdata.csv", sep=",", header=TRUE, row.names = 1)

divdata$Group<-factor(divdata$Group,order=TRUE, levels = c("ASD-M","ASD-S","ASD-O","TD"))

divdata1 <- subset(divdata, Group == "ASD-M"|Group == "TD")
divdata2 <- subset(divdata, Group == "ASD-S"|Group == "TD")
divdata3 <- subset(divdata, Group == "ASD-O"|Group == "TD")

my_comparisons<-list(c("ASD-M","TD"))
shannon<-ggviolin(divdata1, x="Group", y="shannon_diversity", fill = "Group",
                  palette = c("#CC6666","#DD8E08"),
                  add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75, label = "p.signif",label.y=c())+
  scale_y_continuous()+labs(x="", y = "α-diversity (Shannon)")+
  theme(legend.title=element_text(size = 17, face = "bold"),legend.text=element_text(size=15),axis.text.x=element_text(size=14), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))

richness<-ggviolin(divdata1, x="Group", y="richness", fill = "Group",
                   palette = c("#CC6666","#DD8E08"),
                   add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75, label = "p.signif",label.y=c())+
  scale_y_continuous()+labs(x="", y = "Richness")+
  theme(legend.title=element_text(size = 17, face = "bold"),legend.text=element_text(size=15),axis.text.x=element_text(size=14), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))

my_comparisons<-list(c("ASD-S","TD"))
shannon<-ggviolin(divdata2, x="Group", y="shannon_diversity", fill = "Group",
palette = c("#CC6666","#DD8E08"),
add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75, label = "p.signif",label.y=c())+
  scale_y_continuous()+labs(x="", y = "α-diversity (Shannon)")+
  theme(legend.title=element_text(size = 17, face = "bold"),legend.text=element_text(size=15),axis.text.x=element_text(size=14), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))

richness<-ggviolin(divdata2, x="Group", y="richness", fill = "Group",
                   palette = c("#CC6666","#DD8E08"),
                   add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75, label = "p.signif",label.y=c())+
  scale_y_continuous()+labs(x="", y = "Richness")+
  theme(legend.title=element_text(size = 17, face = "bold"),legend.text=element_text(size=15),axis.text.x=element_text(size=14), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))

my_comparisons<-list(c("ASD-O","TD"))
shannon<-ggviolin(divdata3, x="Group", y="shannon_diversity", fill = "Group",
palette = c("#CC6666","#DD8E08"),
add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75, label = "p.signif",label.y=c())+
  scale_y_continuous()+labs(x="", y = "α-diversity (Shannon)")+
  theme(legend.title=element_text(size = 17, face = "bold"),legend.text=element_text(size=15),axis.text.x=element_text(size=14), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))

richness<-ggviolin(divdata3, x="Group", y="richness", fill = "Group",
palette = c("#CC6666","#DD8E08"),
add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75, label = "p.signif",label.y=c())+
  scale_y_continuous()+labs(x="", y = "Richness")+
  theme(legend.title=element_text(size = 17, face = "bold"),legend.text=element_text(size=15),axis.text.x=element_text(size=14), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))

ggarrange(shannon,richness,common.legend=TRUE,ncol = 2)

ggsave("F1b_ASD-M.pdf", width = 10, height = 15, units = "cm")
ggsave("F1b_ASD-S.pdf", width = 10, height = 15, units = "cm")
ggsave("F1b_ASD-O.pdf", width = 10, height = 15, units = "cm")

setwd('*/ASD_families/analysis/f3')
rm(list=ls())

p3<-ggboxplot(data, x="Family_type", y="transmission", color = "Family_type",palette = c("#CC6666", "#53868B", "#5E4FA2","#FEE08B","#5E4FA2"),size = 1.5,width = 0.6)+#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  stat_compare_means(comparisons = my_comparisons,size=5,bracket.size = 0.75,label.y = c(),
                     label = "p.signif",
                     method="wilcox.test")+ 
  stat_compare_means(label.y = 105,method = "kruskal.test",size=4)+ 
  ylim(0,105)+

  labs(x="", y = "Strain-Sharing in sibing pairs (%)")+
  theme(legend.title=element_text(size = 14, face = "bold"),legend.text=element_text(size=12),legend.key.size = unit(1, "cm"),legend.position = "right",axis.text.x=element_text(size=13,angle = 50, hjust = 1, vjust = 1), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 16),axis.line = element_line(size = 1.4, linetype = "solid"))
p3

ggsave("Strain Sharing rate_boxplot.pdf", width = 15, height = 16, units = "cm")

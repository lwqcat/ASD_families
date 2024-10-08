setwd('*/ASD_families/analysis/f2')
rm(list=ls())

p1<-ggboxplot(BC_data, x="Relationship", y="Bray.Curtis.dissimilarity", color = "Relationship",palette = c("#3288BD",'#DD8E08',"#CC6666","#53868B","#5E4FA2", "#F46D43","gold2","#FEE08B","palegreen4", "steelblue3","#5E4FA2"),size = 1.5,width = 0.6)+
  stat_compare_means(comparisons = my_comparisons,size=4,bracket.size = 0.75,label.y = c(),
                     label = "p.format",
                     method="wilcox.test")+ 
  stat_compare_means(label.y = 1.6,method = "kruskal.test",size=4)+ 
  scale_y_continuous()+
  labs(x="", y = "Bray–Curtis distance of species")+geom_jitter(aes(colour = Relationship),size = 4,width = 0.15,alpha=0.6)+
  theme(legend.title=element_text(size = 12, face = "bold"),legend.text=element_text(size=10),legend.key.size = unit(1, "cm"),legend.position = "right",axis.text.x=element_text(size=12,angle = 45, hjust = 1, vjust = 1), axis.title = element_text(size=18, face = "bold"),axis.text.y = element_text(size = 12),axis.line = element_line(size = 1.4, linetype = "solid"))
p1

ggsave("Bray-Curtis dissimilarity.jpg", width = 16, height = 15, units = "cm")

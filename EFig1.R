setwd('*/ASD_families/analysis/ef1')
rm(list=ls())

ggbiplot(pca_result, 
         var.axes=F,            
         groups = dfGroup[,1],  
         ellipse = T, 
         ellipse.prob = 0.95,
         ellipse.alpha = 0,
         circle = F)+theme_bw()+
  labs(fill = "Groups", color = "Groups") +
  scale_color_manual(values=c("#CC6666","#53868B","darkseagreen3","#5E4FA2","#DD8E08"))

ggsave("PCA_diet.pdf", width = 12, height = 9, units = "cm")

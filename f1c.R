setwd('*/ASD_families/analysis/f1c')
rm(list=ls())

library("ggplot2")

p <- ggplot(data = mydata, aes(x = coef, 
                                   y = -log10(qval), 
                                   color = sig)) +
  geom_point(size = 1.5) +  
  scale_color_manual(values = c("#5E4FA2","#CC6666",'gray'), limits = c('TD', 'ASD-MPX', ' ')) +  
  labs(x = 'Coef (by MaAsLin2)', 
       y = expression("-log "["10"]~FDR),
       title = 'ASD-M vs TD', color = '') +  
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), panel.grid = element_blank(), 
        panel.background = element_rect(color = 'transparent', fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent')) +
  theme(legend.title=element_text(size = 12, face = "bold"),
        legend.text=element_text(size=12),
        axis.title = element_text(size=13, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.line = element_line(size = 1.4, linetype = "solid")
  )+
  geom_vline(xintercept = c(0), lty = 3, color = 'grey') +  
  xlim(-2.5,2.5)  +  
  ylim(0,1.7)+
  geom_text_repel(label=Bac_mydata$label1,size = 2.7, color = '#CC6666')+
  geom_text_repel(label=Bac_mydata$label2,size = 2.7, color = '#5E4FA2')#+
p

ggsave("plot Log2Fold change.pdf", width = 15.5, height = 15.5, units = "cm")
ggsave("plot Log2Fold change.jpg", width = 15.5, height = 15.5, units = "cm")

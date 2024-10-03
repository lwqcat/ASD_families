setwd('*/ASD_families/analysis/f4')
rm(list=ls())

P_Chisq_SM_adj<-ggplot(table1, aes(x= Species, y=transmission, fill=Strain_Sharing))+
  scale_fill_manual(values=c('white','#018571','#80cdc1'))+
  geom_bar(stat="identity",position = "stack",width=0.7, size = 0.5,color='#018571')+
  theme(panel.grid=element_blank())+
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(strip.background = element_rect(colour = "black",fill="white")) +
  theme(strip.text.x = element_text(size=14.5, color="black",face="bold"))+ 
  theme(axis.title.x = element_text(size=14.5, color="black",face="bold"))+ 
  theme(axis.text.y = element_text(size=11, color="black"))+
  theme(legend.title = element_text(size=14.5, face="bold"))+
  theme(legend.text = element_text(size=11))+
  coord_flip()  +
  labs(x="" , y="Percentage of Sharing (%)")+
  facet_wrap(~Family_type)#+

P_Chisq_SM_adj

ggsave("strain_sharing_Chi_sig_SM_padj.pdf", width = 28, height = 10, units = "cm",limitsize = FALSE) 

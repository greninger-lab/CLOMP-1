if (!require("xlsx")) { 
  install.packages("xlsx")
  library('xlsx')
}
if (!require("cowplot")) { 
  install.packages("cowplot")
  library('cowplot')
}
if(!require("vridis")){ 
  install.packages("viridis")
  library("viridis")
  
  }


rpm_ct<-read.xlsx('/Users/gerbix/Documents/vikas/scratch/draft_6/RPM_CT_values.xlsx', sheetIndex = 1)

rpm_ct_zero_removed<-rpm_ct[which(rpm_ct$SARS.CoV.2_RPM > 0),]


colors<-c('#e03210',
  '#e87252',
  '#e5a592',
  '#d4d4d4',
  '#a8b3d9',
  '#7594dc',
  '#1277de') 

rpm_ct_zero_removed$RdRp.gene.CT<-as.numeric(as.character(rpm_ct_zero_removed$RdRp.gene.CT))
rpm_ct_zero_removed$Sample<-as.character(rpm_ct_zero_removed$Sample)

rl <- lm(SARS.CoV.2_RPM ~ RdRp.gene.CT , data = rpm_ct_zero_removed)
x<-summary(rl)
x$r.squared



RPM_CT_plot<-ggplot(rpm_ct_zero_removed, aes(x = rpm_ct_zero_removed$RdRp.gene.CT, y = rpm_ct_zero_removed$SARS.CoV.2_RPM)) + 
  geom_point(aes(color = rpm_ct_zero_removed$Sample, shape = rpm_ct_zero_removed$Gene), size = 4) + 
  theme_classic() + 
  ggtitle('RPM of assigned SARS-CoV-2 reads') + 
  xlab('RdRp gene CT') + 
  ylab('RPM') + 
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position="bottom", legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 8)) + 
  geom_smooth(method = "lm", se = FALSE, color = 'black') +
  scale_y_log10(limits = c(1,8000)) + 
  xlim(c(10,40)) 
RPM_CT_plot

ggsave(plot = RPM_CT_plot, file = 'Figure_2.pdf', height = 5, width = 5)





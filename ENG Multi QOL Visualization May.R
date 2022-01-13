#This code visualizes changes in QOL

#Created by Bradley Fritz on 03/01/2021
#Last updated by Bradley Fritz on 05/11/2021
###############################################
setwd("\\\\files.wustl.edu/bafritz/ENGAGES Multi")
library(ggplot2)
library(egg)
##############################################
#Load data
rawenroll <- read.csv('ENG Multi Matched Enrolled Cohort.csv')
rawcontrol <- read.csv('ENG Multi Matched Control Cohort.csv')

#Join cohort together
cohort <- rbind(rawenroll[,c('SurgeryID','enroll','PCS12_preop','MCS12_preop',
                             'PCS12_30d','MCS12_30d','PCS12_1yr','MCS12_1yr')],
                rawcontrol[,c('SurgeryID','enroll','PCS12_preop','MCS12_preop',
                              'PCS12_30d','MCS12_30d','PCS12_1yr','MCS12_1yr')])

cohort$Group[cohort$enroll==1]<-'ENGAGES'
cohort$Group[cohort$enroll==0]<-'Control'


cohorttall <- rbind(data.frame(Time = as.factor('Baseline'),
                               Group = cohort$Group,
                               SurgeryID = cohort$SurgeryID,
                               PCS12 = cohort$PCS12_preop,
                               MCS12 = cohort$MCS12_preop),
                    data.frame(Time = as.factor('30 Days'),
                               Group = cohort$Group,
                               SurgeryID = cohort$SurgeryID,
                               PCS12 = cohort$PCS12_30d,
                               MCS12 = cohort$MCS12_30d),
                    data.frame(Time = as.factor('1 Year'),
                               Group = cohort$Group,
                               SurgeryID = cohort$SurgeryID,
                               PCS12 = cohort$PCS12_1yr,
                               MCS12 = cohort$MCS12_1yr))
cohorttall <- cohorttall[!is.na(cohorttall$PCS12),]


#####################################################
#Violin plot of QOL at three time points, stratified by enroll status
viol_pcs <- ggplot(data=cohorttall,aes(x=Time,
                                       y=PCS12,
                                       group=interaction(Group,Time), 
                                       fill=Group))+
  geom_violin()+
  geom_boxplot(width=0.2, outlier.shape=NA, fill="white", size=0.1,
               position=position_dodge(0.9))+
  scale_fill_manual(values=c('#33CCFF','#CC0000'))+
  theme_minimal()+
  labs(x='',y='Physical Composite Summary')+
  ylim(0,70)+
  annotate('text',x=0.5,y=4,size=4,fontface=2,label='(A)')

viol_mcs <- ggplot(data=cohorttall,aes(x=Time,
                           y=MCS12,
                           group=interaction(Group,Time), 
                           fill=Group))+
  geom_violin()+
  geom_boxplot(width=0.2, outlier.shape=NA, fill="white", size=0.1,
               position=position_dodge(0.9))+
  scale_fill_manual(values=c('#33CCFF','#CC0000'))+
  theme_minimal()+
  labs(x='',y='Mental Composite Summary')+
  ylim(0,80)+
  annotate('text',x=0.5,y=4,size=4,fontface=2,label='(B)')
######################################################
#Arrange the panels
g1 <- ggplotGrob(viol_pcs)
g2 <- ggplotGrob(viol_mcs)

grobbed_panels <- list(g1, g2)

figure <- grid.arrange(
  grobs = list(g1,g2),
  heights = c(1,1),
  layout_matrix = rbind(1,2)
)

ggsave("Figure 2.eps", width=6, height=6, figure)
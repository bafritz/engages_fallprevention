#This code creates figure showing distribution of propensity scores for all pts
#(matched and unmatched) in ENGAGES multicomponent study

#Created by Bradley Fritz on 01/11/2022 (save as from ENG Multi Propensity Figure.R)
#Last updated by Bradley Fritz on 01/11/2022
###############################################
setwd("\\\\files.wustl.edu/bafritz/ENGAGES Multi")
library(xlsx)
library(ggplot2)
library(egg)
##############################################
#Load data
##############################################
rawdata <- read.csv('ENG Multi PScores.csv')

##############################################
#Density plot of propensity scores for matched groups
##############################################
ggplot(rawdata[rawdata$matched==1,],aes(x=pscore, fill=as.factor(enroll)))+
  geom_density(alpha=0.6)+
  theme_minimal()+
  theme(legend.position = c(0.9,0.85))+
  scale_fill_manual(values=c('#33CCFF','#CC0000'),
                    labels=c('Control','ENGAGES'))+
  labs(x='Propensity Score for Enrollment',y='Density',fill='')

##############################################
#Density plot of propensity scores for all (matched and unmatched)
##############################################
ggplot(rawdata,aes(x=pscore, fill=as.factor(enroll)))+
  geom_density(alpha=0.6)+
  theme_minimal()+
  theme(legend.position = c(0.9,0.85))+
  scale_fill_manual(values=c('#33CCFF','#CC0000'),
                    labels=c('Control','ENGAGES'))+
  labs(x='Propensity Score for Enrollment',y='Density',fill='')

ggsave("ENGAGES Multi Propensity Figure.pdf", width=6, height=4, compress=F, pane)

##############################################
#Determine pairwise difference in propensity scores for matched pairs
##############################################
#Create temporary data frames for each group
matchedenroll <- rawdata[rawdata$enroll==1 & rawdata$matched==T,]
colnames(matchedenroll)[colnames(matchedenroll)=='pscore'] <- 'pscore_enroll'

matchedcontrol <- rawdata[rawdata$enroll==0 & rawdata$matched==T,]
colnames(matchedcontrol)[colnames(matchedcontrol)=='pscore'] <- 'pscore_control'
colnames(matchedcontrol)[colnames(matchedcontrol)=='SurgeryID'] <- 'ControlID'

#Merge
pairdata <- merge(matchedcontrol[,c('ControlID','pscore_control')],
                  matchedenroll[,c('ControlID','pscore_enroll')])
rm(matchedenroll,matchedcontrol)

#Calculate pairwise diff
pairdata$pscore_diff <- pairdata$pscore_enroll - pairdata$pscore_control

fivenum(pairdata$pscore_diff)

###############################################
#Propensity score summaries across the various groups
###############################################
#All ENGAGES patients
fivenum(rawdata$pscore[rawdata$enroll==1])

#Matched ENGAGES patients
fivenum(rawdata$pscore[rawdata$enroll==1 & rawdata$matched==T])

#Unmatched ENGAGES patients
fivenum(rawdata$pscore[rawdata$enroll==1 & rawdata$matched==F])

#All control patients
fivenum(rawdata$pscore[rawdata$enroll==0])

#Matched control patients
fivenum(rawdata$pscore[rawdata$enroll==0 & rawdata$matched==T])

#Unmatched control patients
fivenum(rawdata$pscore[rawdata$enroll==0 & rawdata$matched==F])

################################################
#Wilcoxon rank sum test for difference in propensity scores
################################################
#All patients
wilcox.test(rawdata$pscore[rawdata$enroll==0],
            rawdata$pscore[rawdata$enroll==1])

#Matched patients
a <- wilcox.test(rawdata$pscore[rawdata$enroll==0 & rawdata$matched==T],
            rawdata$pscore[rawdata$enroll==1 & rawdata$matched==T])
#This code compares outcomes for subgroups of propensity-matched cohort
#of patients enrolled in ENGAGES and control SATISFY-SOS patients (Table 2)

#05/11/2021 - Change from marginal effects to standardized risk difference

#Created by Bradley Fritz on 02/24/2021
#Last updated by Bradley Fritz on 05/11/2021
###############################################
setwd("\\\\files.wustl.edu/bafritz/ENGAGES Multi")
library(xlsx)
##############################################
#Load data
rawenroll <- read.xlsx('ENG Multi Matched Enrolled Cohort (1).xlsx',1)
rawcontrol <- read.csv('ENG Multi Matched Control Cohort.csv')

#Variable for HSSAT with at least some changes
rawenroll$HSSAT.changes.any <- rawenroll$HSSAT.changes%in%1:3

#####################################################
#Build table for sensitivity analyses

sens <- data.frame(Group = character(),
                   N_Pairs = character(),
                   Med_Review = character(),
                   Med_Changes_Rec = character(),
                   Med_Changed_Followed = character(),
                   HSSAT_Done = character(),
                   Home_Changes = character(),
                   Home_Visit = character(),
                   ENGAGES_Falls1yr = character(),
                   Control_Falls1yr = character(),
                   Difference <- character(),
                   p = character())


#####################################################
#Compare falls and falls with injury between groups
groups <- data.frame(label=c('Whole Cohort','Med Changes Recommended',
                             'Med Changes Followed',
                            'HSSAT Done','Changed Home Envmnt','Home Visit'),
                     var = c('enroll','Recs.Any','Rec.foll','HSSAT.yes','HSSAT.changes.any','HomeVisit'))
                     
for (i in 1:dim(groups)[1]){
  #Find ENGAGES patients who meet this group's criterion
  subset1 <- rawenroll[rawenroll[,which(colnames(rawenroll)==groups$var[i])]==1&
                         !is.na(rawenroll[,which(colnames(rawenroll)==groups$var[i])]),]
  
  #Add the matching control patients
  subset2 <- rbind(subset1[,c('enroll','Falls_1yr')],
                   rawcontrol[rawcontrol$SurgeryID%in%subset1$ControlID,c('enroll','Falls_1yr')])
  
  temp <- data.frame(table(subset2$enroll,subset2$Falls_1yr))
  
  x1 = temp$Freq[temp$Var1==1&temp$Var2==1]
  n1 = sum(temp$Freq[temp$Var1==1])
  x0 = temp$Freq[temp$Var1==0&temp$Var2==1]
  n0 = sum(temp$Freq[temp$Var1==0])
  p1 = x1/n1
  p0 = x0/n0
  
  temp2 <- data.frame(summary(glm(Falls_1yr~enroll,family=binomial,data=subset2))$coefficients)

  sens <-rbind(sens,data.frame(
    Group = groups$label[i],
    N_Pairs = dim(subset1)[1],
    
    Med_Review = paste(sum(subset1$MedRev==1),' (',
                        round(100*sum(subset1$MedRev==1)/dim(subset1)[1]),'%)',sep=''),
    Med_Changes_Rec = paste(sum(subset1$Recs.Any==1),' (',
                             round(100*sum(subset1$Recs.Any==1)/dim(subset1)[1]),'%)',sep=''),
    Med_Changed_Followed = paste(sum(subset1$Rec.foll==1),' (',
                                 round(100*sum(subset1$Rec.foll==1)/dim(subset1)[1]),'%)',sep=''),
    HSSAT_Done = paste(sum(subset1$HSSAT.yes==1,na.rm=T),' (',
                        round(100*sum(subset1$HSSAT.yes==1,na.rm=T)/dim(subset1)[1]),'%)',sep=''),
    Home_Changes = paste(sum(subset1$HSSAT.changes.any==1,na.rm=T),' (',
                           round(100*sum(subset1$HSSAT.changes.any==1,na.rm=T)/dim(subset1)[1]),'%)',sep=''),
    Home_Visit = paste(sum(subset1$HomeVisit==1,na.rm=T),' (',
                        round(100*sum(subset1$HomeVisit==1,na.rm=T)/dim(subset1)[1]),'%)',sep=''),
    
    ENGAGES_Falls1yr = paste(x1,'/',n1,' (',round(100*p1),'%)',sep=''),
    Control_Falls1yr = paste(x0,'/',n0,' (',round(100*p0),'%)',sep=''),
    Difference = paste(round(100*(p1-p0),1),'% (',
                       round(100*(p1-p0-1.96*sqrt(p1*(1-p1)/n1+p0*(1-p0)/n0)),1),'% to ',
                       round(100*(p1-p0+1.96*sqrt(p1*(1-p1)/n1+p0*(1-p0)/n0)),1),'%)',sep=''),
    p = round(temp2[2,4],4)
    ))
  rm(temp,temp2,subset1,subset2,x1,n1,p1,x0,n0,p0)
}
rm(i,groups)

#########################################################
#Save results
#write.xlsx(sens,'ENGAGES Multi Results.xlsx',sheetName='Sensitivity',append=T,row.names = F)
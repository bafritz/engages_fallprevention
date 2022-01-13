#This code compares outcomes for the propensity-matched cohort
#of patients enrolled in ENGAGES and control SATISFY-SOS patients (Table 2)

#05/11/2021 - Change from marginal effects to standardized risk difference
#01/11/2022 - Retrieve quantile regression coefficients for online supplement

#Created by Bradley Fritz on 02/03/2021
#Last updated by Bradley Fritz on 01/11/2022
###############################################
setwd("\\\\files.wustl.edu/bafritz/ENGAGES Multi")
library(quantreg)
library(xlsx)
##############################################
#Load data
rawenroll <- read.csv('ENG Multi Matched Enrolled Cohort.csv')
rawcontrol <- read.csv('ENG Multi Matched Control Cohort.csv')

#Join cohort together
cohort <- rbind(rawenroll[,c('SurgeryID','enroll','Age','Sex','ASA','Num.Comorbidities',
                             'FunctionalCapacity','SBT_preop','SurgicalService','CTSurg',
                             'anest_length','HxFalls','PCS12_preop','MCS12_preop',
                             'Falls_30d','Falls.inj_30d','PCS12_30d','MCS12_30d',
                             'Falls_1yr','Falls.inj_1yr','PCS12_1yr','MCS12_1yr')],
                rawcontrol[,c('SurgeryID','enroll','Age','Sex','ASA','Num.Comorbidities',
                              'FunctionalCapacity','SBT_preop','SurgicalService','CTSurg',
                              'anest_length','HxFalls','PCS12_preop','MCS12_preop',
                              'Falls_30d','Falls.inj_30d','PCS12_30d','MCS12_30d',
                              'Falls_1yr','Falls.inj_1yr','PCS12_1yr','MCS12_1yr')])

cohort$Age <- floor(cohort$Age) #Convert age to years

#####################################################
#Build table of Comparison statistics

comp <- data.frame(Variable = character(),
                   Enroll = character(),
                   Control = character(),
                   Difference = character(),
                   p = character())

#N
comp <- rbind(comp,data.frame(
  Variable = 'N',
  Enroll = paste(dim(cohort[cohort$enroll==1,])[1]),
  Control= paste(dim(cohort[cohort$enroll==0,])[1]),
  Difference='',
  p = ''
))


#####################################################
#Compare falls and falls with injury between groups
vars <- data.frame(name=c('Falls_30d','Falls.inj_30d','Falls_1yr','Falls.inj_1yr'),
                   label=c('Falls at 30 days','Falls with injury at 30 days',
                           'Falls at 1 year','Falls with injury at 1 year'))

for (i in 1:dim(vars)[1]){
  subset <- cohort[,c(which(colnames(cohort)=='enroll'),
                      which(colnames(cohort)==vars$name[i]))]
  colnames(subset)[2] <- 'TempName'
  
  temp <- data.frame(table(subset$enroll,subset$TempName))
  x1 = temp$Freq[temp$Var1==1&temp$Var2==1]
  n1 = sum(temp$Freq[temp$Var1==1])
  x0 = temp$Freq[temp$Var1==0&temp$Var2==1]
  n0 = sum(temp$Freq[temp$Var1==0])
  p1 = x1/n1
  p0 = x0/n0
  
  temp2 <- data.frame(summary(glm(TempName~enroll,family=binomial,data=subset))$coefficients)
  
  comp <-rbind(comp,data.frame(
    Variable = vars$label[i],
    Enroll = paste(x1,'/',n1,' (',round(100*p1),'%)',sep=''),
    Control= paste(x0,'/',n0,' (',round(100*p0),'%)',sep=''),
    Difference = paste(round(100*(p1-p0),1),'% (',
                       round(100*(p1-p0-1.96*sqrt(p1*(1-p1)/n1+p0*(1-p0)/n0)),1),'% to ',
                       round(100*(p1-p0+1.96*sqrt(p1*(1-p1)/n1+p0*(1-p0)/n0)),1),'%)',sep=''),
    p = round(temp2[2,4],4)
  ))
  
  rm(temp,temp2,x1,n1,x0,n0,p1,p0,subset)
}
rm(i,vars)

########################################################
#Compare quality of life between groups

vars <- data.frame(timepoint=c(rep('30 days',2),rep('1 year',2)),
                   name=c('PCS12_30d','MCS12_30d','PCS12_1yr','MCS12_1yr'),
                   label=rep(c('PCS-12','MCS-12')),
                   preop=rep(c('PCS12_preop','MCS12_preop'),2))

coefs <- data.frame(Outcome = character(),
                    Predictor = character(),
                    Coefficient = numeric(),
                    StdError = numeric(),
                    CoefCI = character(),
                    p = character())

for (i in 1:dim(vars)[1]){

  subset <- cohort[,c(which(colnames(cohort)=='enroll'),
                      which(colnames(cohort)==vars$name[i]),
                      which(colnames(cohort)==vars$preop[i]))]
  colnames(subset)[2] <- 'TempName'
  colnames(subset)[3] <- 'Preop'
  
  #Calculate difference from baseline
  subset$Delta <- subset$TempName - subset$Preop
  
  if (i%%2==1){
    comp <- rbind(comp,data.frame(
      Variable = paste('Quality of Life at',vars$timepoint[i]),
      Enroll = paste('N =',sum(!is.na(subset$TempName)&!is.na(subset$Preop)&subset$enroll==1)),
      Control = paste('N = ',sum(!is.na(subset$TempName)&!is.na(subset$Preop)&subset$enroll==0)),
      Difference = '',
      p = ''
    ))
  }
  
  temp1 <- round(fivenum(subset$TempName[subset$enroll==1]),1)
  temp2 <- round(fivenum(subset$TempName[subset$enroll==0]),1)
  
  #Median regression for postop QOL
  a <- summary(rq(TempName~enroll+Preop,data=subset))$coefficients
  
  #Save coefficients
  for (j in 1:dim(a)[1]){
    coefs <- rbind(coefs,data.frame(
      Outcome = vars$name[i],
      Predictor = rownames(a)[j],
      Coefficient = a[j,1],
      StdError = a[j,2],
      CoefCI = paste(round(a[j,1]-1.96*a[j,2],2),'to',
                     round(a[j,1]+1.96*a[j,2],2)),
      p = round(a[j,4],4)
    ))
  }
  
  comp <- rbind(comp,data.frame(
    Variable = paste('    ',vars$label[i]),
    Enroll = paste(temp1[3], ' [',temp1[2],' to ',temp1[4],']',sep=''),
    Control= paste(temp2[3], ' [',temp2[2],' to ',temp2[4],']',sep=''),
    Difference = paste(round(a[2,1],2),' (',
                       round(a[2,1]-1.96*a[2,2],2),' to ',
                       round(a[2,1]+1.96*a[2,2],2),')',sep=''),
    p = round(a[2,4],4)
  ))
  
  #Linear regression for delta QOL. (In histograms, delta had normal distribution)
  b <- summary(glm(Delta~enroll,data=subset))$coefficients
  
  comp <- rbind(comp,data.frame(
    Variable = paste('     Delta',vars$label[i]),
    Enroll = paste(round(mean(subset$Delta[subset$enroll==1],na.rm=T),2),' (',
                   round(sd(subset$Delta[subset$enroll==1],na.rm=T),2),')',sep=''),
    Control = paste(round(mean(subset$Delta[subset$enroll==0],na.rm=T),2),' (',
                    round(sd(subset$Delta[subset$enroll==0],na.rm=T),2),')',sep=''),
    Difference = paste(round(b[2,1],2),' (',
                       round(b[2,1]-1.96*b[2,2],2),' to ',
                       round(b[2,1]+1.96*b[2,2],2),')',sep=''),
    p = round(b[2,4],4)
  ))
  
  
  rm(subset,temp1,temp2,a,b)
}
rm(i,vars)

#########################################################
#Save description table
#write.xlsx(comp,'ENGAGES Multi Results.xlsx',sheetName='Comparisons',append=T,row.names = F)
#write.xlsx(coefs,'ENGAGES Multi Quantile Regressions.xlsx',row.names=F)
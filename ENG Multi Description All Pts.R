#This code performs descriptive statistics for the full (unmatched) cohort
#of patients enrolled in ENGAGES and control SATISFY-SOS patients (eTable)

#Created by Bradley Fritz on 01/11/2022 via save-as from 'ENG Multi Description.R'
#Last updated by Bradley Fritz on 01/11/2022
###############################################
setwd("\\\\files.wustl.edu/bafritz/ENGAGES Multi")
library(quantreg)
library(xlsx)
##############################################
#Load data
rawdata <- read.csv('ENG Multi PScores.csv')

cohort <- rawdata[!is.na(rawdata$pscore),]

cohort$Age <- floor(cohort$Age) #Convert age to years
#####################################################
#Create simplified sugery type variable
cohort$SurgeryType <- 'Other'
cohort$SurgeryType[cohort$SurgicalService==6] <- 'Cardiothoracic'
cohort$SurgeryType[cohort$SurgicalService%in%c(8,1,2,14)] <- 'Gastrointestinal'
cohort$SurgeryType[cohort$SurgicalService%in%c(36,37)] <- 'Gynecologic'
cohort$SurgeryType[cohort$SurgicalService==12] <- 'Hepatobiliary'
cohort$SurgeryType[cohort$SurgicalService==27] <- 'Urology'
cohort$SurgeryType[cohort$SurgicalService==28] <- 'Vascular'

#####################################################
#Build table of descriptive statistics

desc <- data.frame(Variable <- character(),
                   Enroll <- character(),
                   Control <- character(),
                   p <- character())

#N
desc <- rbind(desc,data.frame(
  Variable = 'N',
  Enroll = paste(dim(cohort[cohort$enroll==1,])[1]),
  Control= paste(dim(cohort[cohort$enroll==0,])[1]),
  p = ''
))

#Age
temp1 <- fivenum(cohort$Age[cohort$enroll==1])
temp2 <- fivenum(cohort$Age[cohort$enroll==0])

desc <- rbind(desc,data.frame(
  Variable = 'Age (yrs)',
  Enroll = paste(temp1[3], ' [',temp1[2],' to ',temp1[4],']',sep=''),
  Control= paste(temp2[3], ' [',temp2[2],' to ',temp2[4],']',sep=''),
  p = round(wilcox.test(cohort$Age[cohort$enroll==1],cohort$Age[cohort$enroll==0])$p.value,4)
))
rm(temp1,temp2)

#Sex (1 = male, 2 = female)
temp <- data.frame(table(cohort$enroll,cohort$Sex))
desc <- rbind(desc,data.frame(
  Variable = 'Sex',Enroll='',Control='',
  p = round(chisq.test(cohort$enroll,cohort$Sex)$p.value,4)
))
lvls <- data.frame(label=c('Female','Male'),code=c(2,1))
for (j in 1:dim(lvls)[1]){
  desc <- rbind(desc,data.frame(
    Variable = paste('     ',lvls$label[j],sep=''),
    Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
    Control= paste(temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
    p = ''
  ))
}
rm(lvls,j,temp)

#ASA (range 1 to 5)
temp <- data.frame(table(cohort$enroll,cohort$ASA))
desc <- rbind(desc,data.frame(
  Variable = 'ASA',Enroll='',Control='',
  p = round(fisher.test(cohort$enroll,cohort$ASA)$p.value,4)
))
lvls <- data.frame(label=1:5,code=1:5)
for (j in 1:dim(lvls)[1]){
  desc <- rbind(desc,data.frame(
    Variable = paste('     ',lvls$label[j],sep=''),
    Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
    Control= paste(temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
    p = ''
  ))
}
rm(lvls,j,temp)

#Number of comorbidities
temp1 <- fivenum(cohort$Num.Comorbidities[cohort$enroll==1])
temp2 <- fivenum(cohort$Num.Comorbidities[cohort$enroll==0])

desc <- rbind(desc,data.frame(
  Variable = 'Number of Comorbidities',
  Enroll = paste(temp1[3], ' [',temp1[2],' to ',temp1[4],']',sep=''),
  Control= paste(temp2[3], ' [',temp2[2],' to ',temp2[4],']',sep=''),
  p = round(wilcox.test(cohort$Num.Comorbidities[cohort$enroll==1],
                        cohort$Num.Comorbidities[cohort$enroll==0])$p.value,4)
))
rm(temp1,temp2)

#Short Blessed Test
temp1 <- fivenum(cohort$SBT_preop[cohort$enroll==1])
temp2 <- fivenum(cohort$SBT_preop[cohort$enroll==0])
desc <- rbind(desc,data.frame(
  Variable = 'Short Blessed Test preop',
  Enroll = paste(temp1[3], ' [',temp1[2],' to ',temp1[4],']',sep=''),
  Control= paste(temp2[3], ' [',temp2[2],' to ',temp2[4],']',sep=''),
  p = round(wilcox.test(cohort$SBT_preop[cohort$enroll==1],
                        cohort$SBT_preop[cohort$enroll==0])$p.value,4)
))
rm(temp1,temp2)

#Functional Capacity
temp <- data.frame(table(cohort$enroll,cohort$FunctionalCapacity))
desc <- rbind(desc,data.frame(
  Variable = 'Functional Capacity',Enroll='',Control='',
  p = round(fisher.test(cohort$enroll,cohort$FunctionalCapacity)$p.value,4)
))
lvls <- data.frame(label=c('6-10 METs','4-6 METs','<4 METs','Ambulates with assistance only',
                           'Cannot assess'),code=7:11)
for (j in 1:dim(lvls)[1]){
  desc <- rbind(desc,data.frame(
    Variable = paste('     ',lvls$label[j],sep=''),
    Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
    Control= paste(temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
    p = ''
  ))
}
rm(lvls,j,temp)




#Simplified Surgical Service
temp <- data.frame(table(cohort$enroll,cohort$SurgeryType))
desc <- rbind(desc,data.frame(
  Variable = 'Simplified Surgical Service',Enroll='',Control='',
  p = round(fisher.test(cohort$enroll,cohort$SurgeryType,simulate.p.value = T)$p.value,4)
))
lvls <- data.frame(label=c('Cardiothoracic','Gastrointestinal','Gynecologic',
                           'Hepatobiliary','Urology','Vascular','Other'))
for (j in 1:dim(lvls)[1]){
  desc <- rbind(desc,data.frame(
    Variable = paste('     ',lvls$label[j],sep=''),
    Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==lvls$label[j]],' (',
                   round(100*temp$Freq[temp$Var1==1&temp$Var2==lvls$label[j]]/
                           sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
    Control= paste(temp$Freq[temp$Var1==0&temp$Var2==lvls$label[j]],' (',
                   round(100*temp$Freq[temp$Var1==0&temp$Var2==lvls$label[j]]/
                           sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
    p = ''
  ))
}
rm(lvls,j,temp)

#Cardiac Surgery
temp <- data.frame(table(cohort$enroll,cohort$CTSurg))
desc <- rbind(desc,data.frame(
  Variable = 'Cardiac Surgery',
  Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==1],' (',
                 round(100*temp$Freq[temp$Var1==1&temp$Var2==1]/
                         sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
  Control= paste(temp$Freq[temp$Var1==0&temp$Var2==1],' (',
                 round(100*temp$Freq[temp$Var1==0&temp$Var2==1]/
                         sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
  p = round(chisq.test(cohort$enroll,cohort$CTSurg)$p.value,4)
))
rm(temp)

#History of Falls
temp <- data.frame(table(cohort$enroll,cohort$HxFalls))
desc <- rbind(desc,data.frame(
  Variable = 'History of Falls',
  Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==1],' (',
                 round(100*temp$Freq[temp$Var1==1&temp$Var2==1]/
                         sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
  Control= paste(temp$Freq[temp$Var1==0&temp$Var2==1],' (',
                 round(100*temp$Freq[temp$Var1==0&temp$Var2==1]/
                         sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
  p = round(chisq.test(cohort$enroll,cohort$HxFalls)$p.value,4)
))
rm(temp)

#Preop PCS12, MCS12, and anesthesia length
vars <- data.frame(name=c('PCS12_preop','MCS12_preop','anest_length'),
                   label=c('Preop PCS-12','Preop MCS-12',
                           'Anesthesia Duration (minutes)'))
for (i in 1:dim(vars)[1]){
  ind <- which(colnames(cohort)==vars$name[i])
  
  temp1 <- round(fivenum(cohort[cohort$enroll==1,ind]),1)
  temp2 <- round(fivenum(cohort[cohort$enroll==0,ind]),1)
  
  desc <- rbind(desc,data.frame(
    Variable = vars$label[i],
    Enroll = paste(temp1[3], ' [',temp1[2],' to ',temp1[4],']',sep=''),
    Control= paste(temp2[3], ' [',temp2[2],' to ',temp2[4],']',sep=''),
    p = round(wilcox.test(cohort[cohort$enroll==1,ind],cohort[cohort$enroll==0,ind])$p.value,4)
  ))
  
  rm(ind,temp1,temp2)
}
rm(i,vars)

#Surgical Service
cohort$SurgicalService[cohort$SurgicalService==2] <- 1 #Recode ACCS -> General surgery

desc <- rbind(desc,data.frame(Variable='',Enroll='',Control='',p = '')) #Empty row

temp <- data.frame(table(cohort$enroll,cohort$SurgicalService))
desc <- rbind(desc,data.frame(
  Variable = 'Detailed Surgical Service',Enroll='',Control='',p = ''))
lvls <- data.frame(label=c('Anesthesiology','Cardiology','Cardiothoracic','Colorectal',
                           'Endocrine Oncology','Gastroenterology','General Oncology',
                           'General Surgery','Gynecology','Gynecologic Oncology',
                           'Hepatobiliary','Minimally Invasive','Neurosurgery',
                           'Orthopedic','Otolaryngology','Plastic',
                           'Transplant','Urology','Vascular'),
                   code=c(3,7,6,8,11,9,10,1,36,37,12,14,15,17,18,19,26,27,28))
for (j in 1:dim(lvls)[1]){
  desc <- rbind(desc,data.frame(
    Variable = paste('     ',lvls$label[j],sep=''),
    Enroll = paste(temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==1&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==1]),2),'%)',sep=''),
    Control= paste(temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]],' (',
                   round(100*temp$Freq[temp$Var1==0&temp$Var2==lvls$code[j]]/
                           sum(temp$Freq[temp$Var1==0]),2),'%)',sep=''),
    p = ''
  ))
}
rm(lvls,j,temp)
#########################################################
#Save description table
#write.xlsx(desc,'ENGAGES Multi Description All Pts.xlsx',row.names = F)
#The purpose of this code is to perform propensity matching between patients 
#enrolled in ENGAGES and controls selected from SATISFY-SOS.

#01/29/2021 - Add falls leading to injury to output data frames
#02/08/2021 - Remove orthopedic surgery from control cohort, add Num.Comorbidities to match
#02/19/2021 - Remove neurosurgery from control cohort. Add simplified surgery type to match
#02/22/2021 - Exact match for ASA
#01/11/2022 - Export propensity scores for whole cohort (including unmatched)

#Created by Bradley Fritz on 01/26/2021
#Last updated by Bradley Fritz on 01/11/2022
##################################################
#setwd("\\\\files.wustl.edu/bafritz/ENGAGES Multi")
library(MatchIt)
####################################################
#Import data
rawcontrol <- read.csv('ENG Multi Control Cohort.csv')[-1]
rawenroll <- read.csv('ENG Multi Enrolled Cohort.csv')[-1]

#Reduce datasets to the variables that will be used
# usecontrol <- rawcontrol[,c('SurgeryID','Age','Sex','ASA','Num.Comorbidities',
#                             'FunctionalCapacity','SBT_preop','CTSurg','HxFalls',
#                             'anest_length','PCS12_preop','MCS12_preop','Falls_30d',
#                             'PCS12_30d','MCS12_30d','Falls_1yr','PCS12_1yr','MCS12_1yr')]
# useenroll <- rawenroll[,c('Engages_ID','Age','Sex','ASA','Num.Comorbidities',
#                           'FunctionalCapacity','SBT_preop','CTSurg','HxFalls',
#                           'anest_length','PCS12_preop','MCS12_preop','Falls_30d',
#                           'PCS12_30d','MCS12_30d','Falls_1yr','PCS12_1yr','MCS12_1yr',
#                           'Recs.Any','HSSAT.yes','HSSAT.changes','HomeVisit')]

usecontrol <- rawcontrol
useenroll <- rawenroll

#Label fall as present at 1 year if it was reported at 30 days
usecontrol$Falls_1yr[usecontrol$Falls_30d==1] <- 1
useenroll$Falls_1yr[useenroll$Falls_30d==1] <- 1

usecontrol$Falls.inj_1yr[usecontrol$Falls.inj_30d==1] <- 1
useenroll$Falls.inj_1yr[useenroll$Falls.inj_30d==1] <- 1

#Reduce dataset to those who returned the one year survey
usecontrol <- usecontrol[!is.na(usecontrol$Falls_1yr),]
useenroll <- useenroll[!is.na(useenroll$Falls_1yr),]

#Remove orthopedic surgery patients from the potential control group
usecontrol <- usecontrol[!usecontrol$SurgicalService%in%c(15,17),]
######################################################
#####################################################
#Combine Datasets
usecontrol$enroll <- 0 #Indicator variable for enrollment in ENGAGES
useenroll$enroll <- 1

colnames(useenroll)[colnames(useenroll)=='Engages_ID']<-'SurgeryID' #Give matching variable name

tomatch <- rbind(usecontrol[,c('SurgeryID','enroll','Age','Sex','ASA','CTSurg','HxFalls','Num.Comorbidities','SurgicalService','anest_length')],
                useenroll[,c('SurgeryID','enroll','Age','Sex','ASA','CTSurg','HxFalls','Num.Comorbidities','SurgicalService','anest_length')])

#Remove rows with missing data
tomatch <- tomatch[!is.na(tomatch$Sex)&!is.na(tomatch$CTSurg)&!is.na(tomatch$HxFalls)&
                   !is.na(tomatch$anest_length),]

######################################################
#Calculate simplified surgery type variable
tomatch$SurgeryType <- 'Other'
tomatch$SurgeryType[tomatch$SurgicalService==6] <- 'Cardiothoracic'
tomatch$SurgeryType[tomatch$SurgicalService%in%c(8,1,2,14)] <- 'Gastrointestinal'
tomatch$SurgeryType[tomatch$SurgicalService%in%c(36,37)] <- 'Gynecologic'
tomatch$SurgeryType[tomatch$SurgicalService==12] <- 'Hepatobiliary'
tomatch$SurgeryType[tomatch$SurgicalService==27] <- 'Urology'
tomatch$SurgeryType[tomatch$SurgicalService==28] <- 'Vascular'

tomatch$length_gt300 <- tomatch$anest_length > 300

##############################################
#Perform matching
temp <- matchit(enroll~Age+Sex+ASA+HxFalls+Num.Comorbidities+anest_length+SurgeryType,
                data=tomatch,exact=c('ASA','length_gt300'),caliper=0.2)

#Retrieve propensity scores
tomatch$pscore <- temp$distance
usecontrol <- merge(usecontrol,tomatch[,c('SurgeryID','pscore')],all.x=T)
useenroll  <- merge(useenroll ,tomatch[,c('SurgeryID','pscore')],all.x=T)

#Retrieve indices
group1 <- match(row.names(temp$match.matrix),row.names(tomatch))
group2 <- match(temp$match.matrix,row.names(tomatch))
#Remove rows that did not match
matched.enrolled <- tomatch[group1[!is.na(group2)],] #Enrolled
matched.notenrolled <- tomatch[group2[!is.na(group2)],] #Not enrolled

################################################
#Retrieve patient IDs from the matches
idmatrix <- data.frame(Enrolled = tomatch$SurgeryID[group1],
                       Control = tomatch$SurgeryID[group2])

#Export match IDs
#write.csv(idmatrix,'ENG Multi Matches.csv',row.names = F)
#########################################################
#Output datasets
outenroll <- useenroll[!is.na(idmatrix$Control),]
outcontrol <- usecontrol[usecontrol$SurgeryID%in%idmatrix$Control,]
outcontrol <- outcontrol[!is.na(outcontrol$SurgeryID),]

#Add the SurgeryID of the control match to the OUTENROLL data frame
outenroll$ControlID <- idmatrix$Control[!is.na(idmatrix$Control)]

#Propensity scores for all (including unmatched)
allpscores <- rbind(data.frame(enroll=1,
                               matched=useenroll$SurgeryID%in%outenroll$SurgeryID,
                               SurgeryID=useenroll$SurgeryID,
                               pscore=useenroll$pscore),
                    data.frame(enroll=0,
                               matched=usecontrol$SurgeryID%in%outcontrol$SurgeryID,
                               SurgeryID=usecontrol$SurgeryID,
                               pscore=usecontrol$pscore))

allpscores <- merge(allpscores,outenroll[,c('SurgeryID','ControlID')],all.x=T)

#Add columns needed for running descriptive statistics
temp <- rbind(useenroll[,c('SurgeryID','Age','Sex','ASA','Num.Comorbidities',
                           'FunctionalCapacity','SBT_preop','SurgicalService','CTSurg',
                           'anest_length','HxFalls','PCS12_preop','MCS12_preop',
                           'Falls_30d','Falls.inj_30d','PCS12_30d','MCS12_30d',
                           'Falls_1yr','Falls.inj_1yr','PCS12_1yr','MCS12_1yr')],
              usecontrol[,c('SurgeryID','Age','Sex','ASA','Num.Comorbidities',
                           'FunctionalCapacity','SBT_preop','SurgicalService','CTSurg',
                           'anest_length','HxFalls','PCS12_preop','MCS12_preop',
                           'Falls_30d','Falls.inj_30d','PCS12_30d','MCS12_30d',
                           'Falls_1yr','Falls.inj_1yr','PCS12_1yr','MCS12_1yr')])

allpscores <- merge(allpscores,temp,all.x=T)

######################################################
#Export the matched datasets
#write.csv(outenroll,'ENG Multi Matched Enrolled Cohort.csv',row.names=F)
#write.csv(outcontrol,'ENG Multi Matched Control Cohort.csv',row.names=F)
#write.csv(allpscores,'ENG Multi PScores.csv',row.names = F)
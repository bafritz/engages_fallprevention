#This code performs a power calculation for the ENGAGES falls study.
#Assume a baseline fall incidence of 25%, will determine sample size
#needed to detect various effect sizes with 80% power at alpha = 0.05

#01/11/2022 - Check sample size across various baseline incidence of falls

#Created by Bradley Fritz on 02/19/2021
#Last updated by Bradley Fritz on 01/11/2022
#############################################
library(ggplot2)
#############################################
#Create matrix of effect sizes
out <- data.frame(p1 = seq(0.15,0.2,0.0005))
out$p2 <- 0.25
out$diff <- abs(out$p1-out$p2)

for (i in 1:dim(out)[1]){
  out$n[i] <- power.prop.test(p1=out$p1[i],p2=out$p2[i],sig.level=0.05,power=0.8)$n
}
rm(i)

ggplot(data=out,aes(x=diff,y=n))+
  theme_minimal()+
  geom_line()+
  #geom_hline(yintercept=0.8,linetype='dashed',color='red')+
  xlab(paste('Difference in Incidence of Falls\n(Assuming Baseline Incidence ',
             100*out$p2[1],'%)',sep=''))+
  ylab('Sample Size Required\nat beta = 0.2 and alpha = 0.05')

##############################################
#Keep the effect size constant (25% relative reduction) and vary baseline incidence
out <- data.frame(p1 = seq(0.25,0.72,0.01)) #Baseline incidence
out$p2 <- 0.75*out$p1 #25% relative reduction

for (i in 1:dim(out)[1]){
  out$n[i] <- power.prop.test(p1=out$p1[i],p2=out$p2[i],sig.level=0.05,power=0.8)$n
}
rm(i)

ggplot(data=out,aes(x=p1,y=n))+
  theme_minimal()+
  geom_line()+
  xlab('Baseline Incidence of Falls')+
  ylab('Sample Size Required\nto detect 25% relative reduction\nat beta = 0.2 and alpha = 0.05')
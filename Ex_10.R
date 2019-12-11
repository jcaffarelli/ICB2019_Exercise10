### Exercise 10 ###

#set working directory
setwd("C:/Users/jcaff/Documents/Notre Dame/Senior/fall_19/Biocomputing/R/Tutorial/Ex_10/ICB2019_Exercise10")

#clear global environment
rm(list=ls())

#load libraries
library(ggplot2)

#Question 1: Generate a script that simulates the growth of he two-subpopulations in the tumor
#to equilibrium followed by a drug treatment. Plot using a line graph

#setting initial values:
rN=0.1
rM=0.1
rD=-0.1
K=1000000
N0=99
M0=1
timesteps=300

#setting vectors for 
Ns=numeric(length = timesteps)
Ns[1]=N0

Ms=numeric(length=timesteps)
Ms[1]=M0

#equation for normal growth
Ns[t+1]=Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))

#equation for mutant growth
Ms[t+1]=Ms[t]+rM*Ms[t]*(1-((Ns[t]+Ms[t])/K))

#for loop simulation w treatment
for (t in 1:(timesteps-1)){
  if(t<200){
  Ns[t+1]=Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
  Ms[t+1]=Ms[t]+rM*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }else{
    Ns[t+1]=Ns[t]+rD*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1]=Ms[t]+rM*Ms[t]*(1-((Ns[t]+Ms[t])/K))
    }
}


#plotting
growthN<-data.frame(time=1:length(Ns),N=Ns,mN=Ms)
ggplot(data=growthN)+
  geom_line(aes(x=time,y=N),col='black')+
  geom_line(aes(x=time,y=mN),col='red')+
  theme_classic()

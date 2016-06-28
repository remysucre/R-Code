## SET UP CODE
## CLARA: 6/13/2016

rm(list = ls())
setwd("C:/Users/Clara/Desktop/Harvard Privacy Tools/Datasets/Nyhan 2012 Replication Data")

# install.packages("MatchIt")
# install.packages("Zelig")
# install.packages("Matching")
# install.packages("foreign")
# install.packages("WhatIf")
# install.packages("arm")



#Required packages
library(MatchIt)
library(Zelig)
library(Matching)
library(foreign)
library(WhatIf)
library(arm)

########
#HEALTH#
########

#Read in data with dpvote<60
house<-read.dta("formatch.dta")
house<-subset(house,pcdemvt <100 & pcdemvt >0 & party2=="dem" & incumb==1,select=c(fipsst,district,dpvote,unity,pcdemvt,pcdprevt,dwnom1,health2,gwarm,stimulus))

#Test balance
balance.original <- MatchBalance(health2~dpvote+unity+dwnom1,data=house,nboots=1000)

#Using health2 yes as treatment, not no!
genmatch<-matchit(health2~dpvote+unity+dwnom1,data=house,method="genetic",pop.size=1000,replace=TRUE,reestimate=TRUE,discard="control")

#Summarize results
summary(genmatch)

#Create matched dataset
matched.house.health<-match.data(genmatch)
write.dta(matched.house.health,file="healthmatch.dta")

#Test for balance
balance.final.health<- MatchBalance(health2~dpvote+unity+dwnom1,data=matched.house.health,nboots=1000,weights=matched.house.health$weights)

##########
#STIMULUS#
##########

#Read in data with dpvote<60
house<-read.dta("formatch.dta")
house<-subset(house,pcdemvt <100 & pcdemvt >0 & party2=="dem" & incumb==1,select=c(fipsst,district,dpvote,unity,pcdemvt,pcdprevt,dwnom1,health2,gwarm,stimulus))

#Test balance
balance.original <- MatchBalance(stimulus~dpvote+unity+dwnom1,data=house,nboots=1000)

#Using stimulus yes as treatment, not no!

genmatch<-matchit(stimulus~dpvote+unity+dwnom1,data=house,method="genetic",pop.size=1000,replace=TRUE,reestimate=TRUE,discard="control")

#Summarize results
summary(genmatch)

#Create matched dataset
matched.house.stimulus<-match.data(genmatch)
write.dta(matched.house.stimulus,file="stimulusmatch.dta")

#Test for balance
balance.final.stimulus<- MatchBalance(stimulus~dpvote+unity+dwnom1,data= matched.house.stimulus,nboots=1000,weights=matched.house.stimulus$weights)

###############
#CAP AND TRADE#
###############

#Read in data with dpvote<60
house<-read.dta("formatch.dta")
house<-subset(house,pcdemvt <100 & pcdemvt >0 & party2=="dem" & incumb==1,select=c(fipsst,district,dpvote,unity,pcdemvt,pcdprevt,dwnom1,health2,gwarm,stimulus))

#Test balance
balance.original <- MatchBalance(gwarm~dpvote+unity+dwnom1,data=house,nboots=1000)

#Using gwarm yes as treatment, not no!

genmatch<-matchit(gwarm~dpvote+unity+dwnom1,data=house,method="genetic",pop.size=1000,replace=TRUE,reestimate=TRUE,discard="control")

#Summarize results
summary(genmatch)

#Create matched dataset
matched.house.gwarm<-match.data(genmatch)
write.dta(matched.house.gwarm,file="gwarmmatch.dta")

#Test for balance
balance.final.gwarm<- MatchBalance(gwarm~dpvote+unity+dwnom1,data= matched.house.gwarm,nboots=1000,weights=matched.house.gwarm$weights)
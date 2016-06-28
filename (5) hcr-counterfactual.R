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

#Read in data
house<-read.dta("cd-for-replication.dta")
house<-subset(house,pcdemvt <100 & pcdemvt >0 & party2=="dem" & incumb==1,select=c(fipsst,district,dpvote,unity,pcdemvt,pcdprevt,dwnom1,health2,gwarm,stimulus))

#Model Dem vote
#Simulation requires using regular standard errors
model<-lm(pcdemvt~dpvote+unity+dwnom1+health2,data=house)
summary(model)

#simulate counterfactual for yes votes

#posterior distributions of model coefficients
sim<-sim(model,n.sims=10000)
coef<-coef(sim)

#add intercept, subset on covariates
data<-cbind(1,house)
data<-subset(data,select=c(1,dpvote,unity,dwnom1,health2))

#make fake data where everyone votes no with DPV<60%
dataalt<-data
i<-1
while (i<nrow(dataalt)) {
dataalt[i,5]<-ifelse(dataalt[i,2]<60,0,dataalt[i,5])
i<-i+1
}

i<-1
voterealvec<-rep(NA,10000)
allnovec<-rep(NA,10000)
windiff<-rep(NA,10000)

#block draw posterior for entire set of covariates
#calculate differences within same draw

while (i<10001) {
row<-sample(1:10000,1,replace=T)
coefvec<-as.matrix(as.numeric(coef[row,]))
data<-as.matrix(data)
dataalt<-as.matrix(dataalt)
votereal<-data %*% coefvec
allno<-dataalt %*% coefvec
voterealvec[i]<-sum(votereal>50)
allnovec[i]<-sum(allno>50)
windiff[i]<-sum(allno>50)-sum(votereal>50)
i<-i+1
}

median(windiff)
quantile(windiff,.05)
quantile(windiff,.95)

sum(windiff>24)/length(windiff)
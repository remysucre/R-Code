#Required packages
library(mediation)
library(arm)
library(foreign)

setwd("ENTER DIRECTORY PATHNAME HERE")

#TABLES A-1/A-2

#FULL SAMPLE

i<-1
simnum<-1000

while (i<4) {

medvec<-c("gwarm","stimulus","health2")

#Read in data

data <- read.dta("individual-for-mediation.dta")
data <- subset(data,is.na(ideoldiff2)==FALSE,select=c(ideoldiff2,pid7,
			pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,
			stdist,weight,dem,rep,ind))
data <- subset(data,is.na(housevote)==FALSE,select=c(ideoldiff2,pid7,
			pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,
			stdist,weight,dem,rep,ind))
data <- subset(data,is.na(pid7)==FALSE,select=c(ideoldiff2,pid7,
			pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,
			stdist,weight,dem,rep,ind))
data <- data[data$weight>0,]
data$health2 <- as.numeric(data$health2)-1
data$stimulus <- as.numeric(data$stimulus)-1
data$gwarm <- as.numeric(data$gwarm)-1
data$housevote <- as.numeric(data$housevote)-1
data <- data[order(data$stdist),]
means <- tapply(data$ideoldiff2, data$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data <- merge(data, means, by.x="stdist", by.y="dist")
data$ideoldiff2.wi <- data$ideoldiff2 - data$ideoldiff2.bt
attach.all(data)

##Imai, et al. mediation model
#create backup version of ideoldiff2 & ideoldiff2.wi
data$ideoldiff2.all <- data$ideoldiff2
data$ideoldiff2.wi.all <- data$ideoldiff2.wi
#create random variable for first-stage model & swap out ideoldiff2.wi
data$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data$ideoldiff2.wi <- data$random
#first-stage model
b <- lm(ideoldiff2 ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2.wi, 
		weights=weight,data=data)
#replace full ideoldiff2 with between-district version
data$ideoldiff2 <- data$ideoldiff2.bt
#return ideoldiff2.wi to original version
data$ideoldiff2.wi <- data$ideoldiff2.wi.all
#second-stage model, w/ between & within versions of ideoldiff2
c <- glm(housevote ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2+
		ideoldiff2.wi, 
		family = binomial(link = "probit"),weights=weight,data=data)
#analysis of results
continuous_boot1 <- mediate(b, c, boot=TRUE, sims=5, treat=medvec[i],
		mediator="ideoldiff2") 
print(medvec[i])
print(summary(continuous_boot1))
plot(continuous_boot1)
sens.cont <- medsens(continuous_boot1, rho.by = 0.05)
print(summary(sens.cont))
plot(sens.cont, sens.par = "rho")
#return ideoldiff2 to original version
data$ideoldiff2 <- data$ideoldiff2.all

i<-i+1
}

##Analysis by party
#dem
data.dem<-subset(data,dem==1)
data.dem <- data.dem[order(data.dem$stdist),]
means <- tapply(data.dem$ideoldiff2, data.dem$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data.dem <- merge(data.dem, means, by.x="stdist", by.y="dist")
data.dem$ideoldiff2.wi <- data.dem$ideoldiff2 - data.dem$ideoldiff2.bt
attach.all(data.dem)
data.dem$ideoldiff2.all <- data.dem$ideoldiff2
data.dem$ideoldiff2.wi.all <- data.dem$ideoldiff2.wi
data.dem$random <- rnorm(length(data.dem$ideoldiff2), mean=0, sd=1)
data.dem$ideoldiff2.wi <- data.dem$random
b <- lm(ideoldiff2 ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2.wi, 
	weights=weight,data=data.dem) 
data.dem$ideoldiff2 <- data.dem$ideoldiff2.bt
data.dem$ideoldiff2.wi <- data.dem$ideoldiff2.wi.all
c <- glm(housevote ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2+
	ideoldiff2.wi, 
	data=data.dem,weights=weight,family = binomial(link = "probit")) 
continuous_boot3 <- mediate(b, c, boot=TRUE, sims=1000, treat="health2",mediator="ideoldiff2") 
summary(continuous_boot3) 
plot(continuous_boot3)
sens.cont3 <- medsens(continuous_boot3, rho.by = 0.05)
summary(sens.cont3)
plot(sens.cont3, sens.par = "rho")
data.dem$ideoldiff2 <- data.dem$ideoldiff2.all

#ind
data.ind<-subset(data,ind==1)
data.ind <- data.ind[order(data.ind$stdist),]
means <- tapply(data.ind$ideoldiff2, data.ind$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data.ind <- merge(data.ind, means, by.x="stdist", by.y="dist")
data.ind$ideoldiff2.wi <- data.ind$ideoldiff2 - data.ind$ideoldiff2.bt
attach.all(data.ind)
data.ind$ideoldiff2.all <- data.ind$ideoldiff2
data.ind$ideoldiff2.wi.all <- data.ind$ideoldiff2.wi
data.ind$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data.ind$ideoldiff2.wi <- data.ind$random
b <- lm(ideoldiff2 ~ pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2.wi, 
	weights=weight,data=data.ind) 
data.ind$ideoldiff2 <- data.ind$ideoldiff2.bt
data.ind$ideoldiff2.wi <- data.ind$ideoldiff2.wi.all
c <- glm(housevote ~ pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2+
	ideoldiff2.wi, 
	data=data.ind,weights=weight,family = binomial(link = "probit")) 
continuous_boot4 <- mediate(b, c, boot=TRUE, sims=1000, treat="health2",mediator="ideoldiff2") 
summary(continuous_boot4) 
plot(continuous_boot4)
sens.cont4 <- medsens(continuous_boot4, rho.by = 0.05)
summary(sens.cont4)
plot(sens.cont4, sens.par = "rho")
data.ind$ideoldiff2 <- data.ind$ideoldiff2.all

#rep
data.rep<-subset(data,rep==1)
data.rep <- data.rep[order(data.rep$stdist),]
means <- tapply(data.rep$ideoldiff2, data.rep$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data.rep <- merge(data.rep, means, by.x="stdist", by.y="dist")
data.rep$ideoldiff2.wi <- data.rep$ideoldiff2 - data.rep$ideoldiff2.bt
attach.all(data.rep)
data.rep$ideoldiff2.all <- data.rep$ideoldiff2
data.rep$ideoldiff2.wi.all <- data.rep$ideoldiff2.wi
data.rep$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data.rep$ideoldiff2.wi <- data.rep$random
b <- lm(ideoldiff2 ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2.wi, 
	weights=weight,data=data.rep) 
data.rep$ideoldiff2 <- data.rep$ideoldiff2.bt
data.rep$ideoldiff2.wi <- data.rep$ideoldiff2.wi.all
c <- glm(housevote ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+ideoldiff2+
	ideoldiff2.wi, 
	data=data.rep,weights=weight,family = binomial(link = "probit")) 
continuous_boot5 <- mediate(b, c, boot=TRUE, sims=1000, treat="health2",mediator="ideoldiff2") 
summary(continuous_boot5) 
plot(continuous_boot5)
sens.cont5 <- medsens(continuous_boot5, rho.by = 0.05)
summary(sens.cont5)
plot(sens.cont5, sens.par = "rho")
data.rep$ideoldiff2 <- data.rep$ideoldiff2.all

#Matched data

i<-1
simnum<-1000

while (i<4) {

medvec<-c("gwarm","stimulus","health2")

#Read in and format data
filename<-paste("ind-for-med-",medvec[i],".dta",sep="")
data <- read.dta(filename)
data <- subset(data,is.na(ideoldiff2)==FALSE,select=c(ideoldiff2,pid7,pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,stdist,weight,dem,rep,ind,dpvote,unity))
data <- subset(data,is.na(housevote)==FALSE,select=c(ideoldiff2,pid7,pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,stdist,weight,dem,rep,ind,dpvote,unity))
data <- subset(data,is.na(pid7)==FALSE,select=c(ideoldiff2,pid7,
pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,stdist,weight,dem,rep,ind,dpvote,unity))
data <- data[data$weight>0,]
data$health2 <- as.numeric(data$health2)-1
data$stimulus <- as.numeric(data$stimulus)-1
data$gwarm <- as.numeric(data$gwarm)-1
data$housevote <- as.numeric(data$housevote)-1
data <- data[order(data$stdist),]
means <- tapply(data$ideoldiff2, data$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data <- merge(data, means, by.x="stdist", by.y="dist")
data$ideoldiff2.wi <- data$ideoldiff2 - data$ideoldiff2.bt
attach(data)

##Imai, et al. mediation model
#all respondents
#create backup version of ideoldiff2 & ideoldiff2.wi
data$ideoldiff2.all <- data$ideoldiff2
data$ideoldiff2.wi.all <- data$ideoldiff2.wi
#create random variable for first-stage model & swap out ideoldiff2.wi
data$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data$ideoldiff2.wi <- data$random
#first-stage model
b <- lm(ideoldiff2 ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+dpvote+unity+
		ideoldiff2.wi, 
		weights=weight, data=data)
summary(b)
#replace full ideoldiff2 with between-district version
data$ideoldiff2 <- data$ideoldiff2.bt
#return ideoldiff2.wi to original version
data$ideoldiff2.wi <- data$ideoldiff2.wi.all
#second-stage model, w/ between & within versions of ideoldiff2
c <- glm(housevote ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+dpvote+unity+
		ideoldiff2+ideoldiff2.wi, weights=weight,
		family = binomial(link = "probit"),data=data)
summary(c)
#analysis of results
continuous_boot1 <- mediate(b, c, boot=TRUE, sims=simnum, treat=medvec[i],
		mediator="ideoldiff2") 
print(medvec[i])
print(summary(continuous_boot1))
plot(continuous_boot1)
sens.cont <- medsens(continuous_boot1, rho.by = 0.05)
print(summary(sens.cont))
plot(sens.cont, sens.par = "rho")
#return ideoldiff2 to original version
data$ideoldiff2 <- data$ideoldiff2.all

i<-i+1
}

data <- read.dta("ind-for-med-health2.dta")
data <- subset(data,is.na(ideoldiff2)==FALSE,select=c(ideoldiff2,pid7,pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,stdist,weight,dem,rep,ind,dpvote,unity))
data <- subset(data,is.na(housevote)==FALSE,select=c(ideoldiff2,pid7,pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,stdist,weight,dem,rep,ind,dpvote,unity))
data <- subset(data,is.na(pid7)==FALSE,select=c(ideoldiff2,pid7,
pcdprevt,health2,stimulus,gwarm,dwnom1,housevote,mcapprove,stdist,weight,dem,rep,ind,dpvote,unity))
data <- data[data$weight>0,]
data$health2 <- as.numeric(data$health2)-1
data$stimulus <- as.numeric(data$stimulus)-1
data$gwarm <- as.numeric(data$gwarm)-1
data$housevote <- as.numeric(data$housevote)-1

##Analysis by party
#dem
data.dem<-subset(data,dem==1)
data.dem <- data.dem[order(data.dem$stdist),]
means <- tapply(data.dem$ideoldiff2, data.dem$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data.dem$ideoldiff2.bt <- NULL
data.dem <- merge(data.dem, means, by.x="stdist", by.y="dist")
data.dem$ideoldiff2.wi <- data.dem$ideoldiff2 - data.dem$ideoldiff2.bt
attach.all(data.dem)
data.dem$ideoldiff2.all <- data.dem$ideoldiff2
data.dem$ideoldiff2.wi.all <- data.dem$ideoldiff2.wi
data.dem$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data.dem$ideoldiff2.wi <- data.dem$random
b <- lm(ideoldiff2 ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+dpvote+unity+
	ideoldiff2.wi, 
	weights=data.dem$weight, data=data.dem)
summary(b)
data.dem$ideoldiff2 <- data.dem$ideoldiff2.bt
data.dem$ideoldiff2.wi <- data.dem$ideoldiff2.wi.all
c <- glm(housevote ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+dpvote+unity+
	ideoldiff2+ideoldiff2.wi,weights=data.dem$weight, 
	data=data.dem,family = binomial(link = "probit")) 
summary(c)
continuous_boot3 <- mediate(b, c, boot=TRUE, sims=simnum, treat="health2",mediator="ideoldiff2") 
print("health2")
summary(continuous_boot3) 
plot(continuous_boot3)
sens.cont3 <- medsens(continuous_boot3, rho.by = 0.05)
summary(sens.cont3)
plot(sens.cont3, sens.par = "rho")
data.dem$ideoldiff2 <- data.dem$ideoldiff2.all

#ind
data.ind<-subset(data,ind==1)
data.ind <- data.ind[order(data.ind$stdist),]
means <- tapply(data.ind$ideoldiff2, data.ind$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data.ind$ideoldiff2.bt <- NULL
data.ind <- merge(data.ind, means, by.x="stdist", by.y="dist")
data.ind$ideoldiff2.wi <- data.ind$ideoldiff2 - data.ind$ideoldiff2.bt
attach.all(data.ind)
data.ind$ideoldiff2.all <- data.ind$ideoldiff2
data.ind$ideoldiff2.wi.all <- data.ind$ideoldiff2.wi
data.ind$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data.ind$ideoldiff2.wi <- data.ind$random
b <- lm(ideoldiff2 ~ health2+pcdprevt+stimulus+gwarm+dwnom1+dpvote+unity+
	ideoldiff2.wi, 
	weights=data.ind$weight, data=data.ind)
summary(b)
data.ind$ideoldiff2 <- data.ind$ideoldiff2.bt
data.ind$ideoldiff2.wi <- data.ind$ideoldiff2.wi.all
c <- glm(housevote ~ health2+pcdprevt+stimulus+gwarm+dwnom1+dpvote+unity+
	ideoldiff2+ideoldiff2.wi, weights=data.ind$weight,
	data=data.ind,family = binomial(link = "probit")) 
summary(c)
continuous_boot4 <- mediate(b, c, boot=TRUE, sims=simnum, treat="health2",mediator="ideoldiff2") 
print("health2")
summary(continuous_boot4) 
plot(continuous_boot4)
sens.cont4 <- medsens(continuous_boot4, rho.by = 0.05)
summary(sens.cont4)
plot(sens.cont4, sens.par = "rho")
data.ind$ideoldiff2 <- data.ind$ideoldiff2.all

#rep
data.rep<-subset(data,rep==1)
data.rep <- data.rep[order(data.rep$stdist),]
means <- tapply(data.rep$ideoldiff2, data.rep$stdist, mean)
means <- data.frame(dist=as.numeric(names(means)), ideoldiff2.bt=means)
data.rep$ideoldiff2.bt <- NULL
data.rep <- merge(data.rep, means, by.x="stdist", by.y="dist")
data.rep$ideoldiff2.wi <- data.rep$ideoldiff2 - data.rep$ideoldiff2.bt
attach(data.rep)
data.rep$ideoldiff2.all <- data.rep$ideoldiff2
data.rep$ideoldiff2.wi.all <- data.rep$ideoldiff2.wi
data.rep$random <- rnorm(length(ideoldiff2), mean=0, sd=1)
data.rep$ideoldiff2.wi <- data.rep$random
b <- lm(ideoldiff2 ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+dpvote+unity+
	ideoldiff2.wi, 
	weights=data.rep$weight, data=data.rep) 
summary(b)
data.rep$ideoldiff2 <- data.rep$ideoldiff2.bt
data.rep$ideoldiff2.wi <- data.rep$ideoldiff2.wi.all
c <- glm(housevote ~ pid7+pcdprevt+health2+stimulus+gwarm+dwnom1+dpvote+unity+
	ideoldiff2+ideoldiff2.wi, weights=data.rep$weight,
	data=data.rep, family = binomial(link = "probit")) 
summary(c)
continuous_boot5 <- mediate(b, c, boot=TRUE, sims=simnum, treat="health2",mediator="ideoldiff2") 
print("health2")
summary(continuous_boot5) 
plot(continuous_boot5)
sens.cont5 <- medsens(continuous_boot5, rho.by = 0.05)
summary(sens.cont5)
plot(sens.cont5, sens.par = "rho")
data.rep$ideoldiff2 <- data.rep$ideoldiff2.all



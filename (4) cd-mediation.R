#Required packages
library(mediation)
library(foreign)

setwd("ENTER DIRECTORY PATHNAME HERE")

#TABLE B-2

#Read in data
data<-read.dta("formediation.dta")
datahealth<-subset(data,healthmatch==1)
datastim<-subset(data,stimulusmatch==1)
datagwarm<-subset(data,gwarmmatch==1)

#all
b <- lm(ideoldiff2 ~ dpvote+unity+dwnom1+health2, data=data) 
c <- lm(pcdemvt ~ dpvote+unity+dwnom1+health2+ideoldiff2, data=data) 
continuous_boot <- mediate(b, c, boot=TRUE, sims=1000, treat="health2",mediator="ideoldiff2") 
summary(continuous_boot) 

#matched
b <- lm(ideoldiff2 ~ dpvote+unity+dwnom1+health2, data=datahealth,weights=healthweights) 
c <- lm(pcdemvt ~ dpvote+unity+dwnom1+health2+ideoldiff2, data= datahealth,weights=healthweights) 
continuous_boot3 <- mediate(b, c, boot=TRUE, sims=1000, treat="health2",mediator="ideoldiff2") 
summary(continuous_boot3) 

#sensitivity code
sens.cont <- medsens(continuous_boot3, rho.by=.05, sims=1000) 
summary(sens.cont)

#all
b <- lm(ideoldiff2 ~ dpvote+unity+dwnom1+stimulus, data=data) 
c <- lm(pcdemvt ~ dpvote+unity+dwnom1+stimulus+ideoldiff2, data=data) 
continuous_boot7 <- mediate(b, c, boot=TRUE, sims=1000, treat="stimulus",mediator="ideoldiff2") 
summary(continuous_boot7) 

#matched
b <- lm(ideoldiff2 ~ dpvote+unity+dwnom1+stimulus, data=datastim,weights=stimweights) 
c <- lm(pcdemvt ~ dpvote+unity+dwnom1+stimulus+ideoldiff2, data= datastim,weights=stimweights) 
continuous_boot9 <- mediate(b, c, boot=TRUE, sims=1000, treat="stimulus",mediator="ideoldiff2") 
summary(continuous_boot9) 

sens.cont <- medsens(continuous_boot9, rho.by=.05, sims=1000) 
summary(sens.cont) 
 
#all
b <- lm(ideoldiff2 ~ dpvote+unity+dwnom1+gwarm, data=data) 
c <- lm(pcdemvt ~ dpvote+unity+dwnom1+gwarm+ideoldiff2, data=data) 
continuous_boot4 <- mediate(b, c, boot=TRUE, sims=1000, treat="gwarm",mediator="ideoldiff2") 
summary(continuous_boot4) 

#matched
b <- lm(ideoldiff2 ~ dpvote+unity+dwnom1+gwarm, data=datagwarm,weights=gwarmweights) 
c <- lm(pcdemvt ~ dpvote+unity+dwnom1+gwarm+ideoldiff2, data=datagwarm,weights=gwarmweights) 
continuous_boot6 <- mediate(b, c, boot=TRUE, sims=1000, treat="gwarm",mediator="ideoldiff2") 
summary(continuous_boot6) 

sens.cont <- medsens(continuous_boot6, rho.by=.05, sims=1000) 
summary(sens.cont) 
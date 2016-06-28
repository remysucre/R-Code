## CLARA REPLICATION ADJUSTMENTS
wd <- "C:/Users/Clara/Desktop/Harvard Privacy Tools/Datasets/Schmitt 2015 Replication Data"
setwd(wd)
data <- paste0(wd, "/EUP clean.sav")

## REPLICATION CODE PROVIDED BY AUTHORS
rm(list=ls())
require (foreign)
require(texreg)
require (lme4)
require (ggplot2)
require(stargazer)
require (boot)
require (MASS)
require(texreg)
require(multilevel)
require(ndl)




#mydata<-read.spss("~/EUP clean.sav", to.data.frame=TRUE)
mydata <- read.spss(data, to.data.frame=TRUE)
summary (mydata)

###group centering indiv

##centering macro variables
mydata$nat_turn_cent<-mydata$nat_turn-mean(mydata$nat_turn, na.rm=T)
mydata$over_rep_cent<-mydata$over_rep-mean(mydata$over_rep, na.rm=T)
mydata$MEP_cent<-mydata$MEP_scale-mean(mydata$MEP_scale, na.rm=T)

##population in milloon 
mydata$pop_mil<-mydata$pop_min2-mean(mydata$pop_min2, na.rm=T)


data<-subset(mydata, voted!= 'NA')
data<-subset(data, married!= 'NA')
data<-subset(data, know!= 'NA')
data<-subset(data, intpol!= 'NA')
data<-subset(data, discuss!= 'NA')
data<-subset(data, allefic!= 'NA')
data<-subset(data, partisan!= 'NA')
data<-subset(data, maxnews!= 'NA')
data<-subset(data, involv!= 'NA')
data<-subset(data, unemployed!= 'NA')
data<-subset(data, rural!= 'NA')
data<-subset(data, religios!= 'NA')
data<-subset(data, immigrant!= 'NA')
data<-subset(data, internet!= 'NA')
data<-subset(data, contact!= 'NA')
data<-subset(data, trustNAT_dum!= 'NA')
data<-subset(data, trustEU_dum!= 'NA')
data<-subset(data, eugood!= 'NA')
data<-subset(data, compvote!= 'NA')
data<-subset(data, other_el!= 'NA')
data<-subset(data, nat_turn_cent!= 'NA')
data<-subset(data, secondary != 'NA')
data<-subset(data, age!= 'NA')
data<-subset(data, female!= 'NA')
data<-subset(data, age!= 'NA')
data<-subset(data, rural!= 'NA')
data<-subset(data, union!= 'NA')
data<-subset(data, exposure!= 'NA')

##Table 2, Model 1

without_MEP<-glmer(voted~1  + married + secondary + tertiary +age+     
                     female + unemployed + rural + religios + union + immigrant +internet +  
                     know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                     contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                     +compvote+other_el + nat_turn_cent +   + control_cand  MEP_cent+ 
                     +(1|region),
                   mydata, family=binomial(link = "logit"), verbose=T)
summary(without_MEP)



p0 <- predict(without_MEP, type="response")

data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-p0


data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 

a<-crosstableStatistics(mytable)
lambdap_model1<-a$lambda.prediction
##lambdap_model1
lambdap_model1


rm(juncker)
### Table2 , Model 2
juncker_MEP<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                     female + unemployed + rural + religios + union + immigrant +internet +  
                     know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                     contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                     +compvote+other_el + nat_turn_cent + control_cand+ MEP_cent+  
                     Juncker_camp_dum+
                     +(1+juncker|region),
                   mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_MEP)


pjuncker <- predict(juncker_MEP, type="response")

data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-pjuncker


data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 


model2_fit<-crosstableStatistics(mytable)
lambdap_model2<-model2_fit$lambda.prediction
##lambdap_model2
lambdap_model2



# Table2 , Model 5

juncker_interact_MEP<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                              female + unemployed + rural + religios + union + immigrant +internet +  
                              know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                              contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                              +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+
                              Juncker_camp_dum*juncker+
                              +(1+juncker|region),
                            mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_MEP)



anova(juncker_interact_MEP)



pjuncker_interact <- predict(juncker_interact_MEP, type="response")

data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-pjuncker_interact


data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 


model5_fit<-crosstableStatistics(mytable)
lambdap_model5<-model5_fit$lambda.prediction

#lambdap Model5
lambdap_model5



###Table2, model 3
schulz_MEP<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                    female + unemployed + rural + religios + union + immigrant +internet +  
                    know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                    contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                    +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+   
                    Schulz_camp_dum+
                    +(1+schlulz|region),
                  mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_MEP)


pschulz<- predict(schulz_MEP, type="response")


data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-pschulz
summary(data_prediction$fitted)

data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 


model3_fit<-crosstableStatistics(mytable)
lambdap_model3<-model3_fit$lambda.prediction
#lambdap Model3
lambdap_model3


###Table2, model 6
schlulz_interact_MEP<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                              female + unemployed + rural + religios + union + immigrant +internet +  
                              know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                              contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                              +compvote+other_el + nat_turn_cent + control_cand+MEP_cent +  
                              Schulz_camp_dum*schlulz+
                              +(1+schlulz|region),
                            mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_MEP)


anova(schlulz_interact_MEP)




p_schlulz_interact<- predict(schlulz_interact, type="response")

data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-p_schlulz_interact
summary(data_prediction$fitted)

data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 


model6_fit<-crosstableStatistics(mytable)
#lambdap Model6
model6_fit$lambda.prediction



### Table2, model 4
verhofstadt_MEP<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                         female + unemployed + rural + religios + union + immigrant +internet +  
                         know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                         contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                         +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+   
                         Verhofstadt_camp_dum+
                         +(1+verhofstadt|region),
                       mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_MEP)



pverhofstadt<- predict(verhofstadt_MEP, type="response")

data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-pverhofstadt
summary(data_prediction$fitted)

data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 


model4_fit<-crosstableStatistics(mytable)
#lambdap Model4
model4_fit$lambda.prediction


###Table2, model  7
verhofstadt_interact_MEP<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                                  female + unemployed + rural + religios + union + immigrant +internet +  
                                  know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                  contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                  +compvote+other_el + nat_turn_cent + control_cand+MEP_cent +  
                                  Verhofstadt_camp_dum*verhofstadt+
                                  +(1+verhofstadt|region),
                                mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_MEP)



anova(verhofstadt_interact_MEP)

pverhofstadt_interact<- predict(verhofstadt_interact_MEP, type="response")

data_prediction<- data.frame(matrix(NA,ncol = 2, nrow = 24137))
names(data_prediction) <- c("observed","fitted")

data_prediction$observed<-as.numeric(data$voted)-1
data_prediction$fitted<-pverhofstadt_interact
summary(data_prediction$fitted)

data_prediction$fitted2<-NA
data_prediction$fitted2<-ifelse(data_prediction[,2]>0.49999, c("1"), c("0"))
mytable <- xtabs(~observed+fitted2, data=data_prediction)
ftable(mytable) # print table 


model7_fit<-crosstableStatistics(mytable)
#lambdap Model7
model7_fit$lambda.prediction

stargazer(without_MEP,juncker_MEP, schulz_MEP, verhofstadt_MEP,
          juncker_interact_MEP, schlulz_interact_MEP, verhofstadt_interact_MEP,
          type="html",
          column.labels=c("Model 1: Without  the candidates", "Model 3: Juncker",
                          "Model 4: Schulz", "Model 5: Verhofstadt",  "Mode  6: Juncker, visits interaction",
                          "Model 7: Schulz visits interaction", "Model 8: Verhofstadt visits interaction"),
          out = "Table2.html", style="default",  star.cutoffs=c(0.1, 0.05, 0.01, 0.001),
          intercept.top=T, intercept.bottom=F, 
          keep.stat=c("ll","AIC", "n"), notes.align="c",single.row=TRUE)



############CONTROLING FOR POPULATION SIZE, weighting number of campaing visits by numbers of MEP, Appendix 5, Table 4
mydata$Schulz_camp_MEP<-mydata$Schulz_camp/mydata$MEP
hist(mydata$Juncker_camp) 
mydata$Junker_camp_MEP<-mydata$Juncker_camp/mydata$MEP
mydata$Verhofstadt_camp_MEP<-mydata$Verhofstadt_camp/mydata$MEP


###divided by the MEP
mydata$Schulz_camp_MEP_cent<-mydata$Schulz_camp_MEP-mean(mydata$Schulz_camp_MEP, na.rm=T)
mydata$Junker_camp_MEP_cent<-mydata$Junker_camp_MEP-mean(mydata$Junker_camp_MEP, na.rm=T)
mydata$Verhofstadt_camp_MEP_cent<-mydata$Verhofstadt_camp_MEP-mean(mydata$Verhofstadt_camp_MEP, na.rm=T)
hist(mydata$Schulz_camp_MEP_cent) 


#Table 4, Model 8
without<-glmer(voted~1  + married + secondary + tertiary +age+     
                 female + unemployed + rural + religios + union + immigrant +internet +  
                 know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                 contact +  +trustNAT_dum + trustEU_dum +eugood+ control_cand+
                 +compvote+other_el + nat_turn_cent +  
                 +(1|region),
               mydata, family=binomial(link = "logit"), verbose=T)
summary(without)


#Table 4, Model 9
juncker_campMEP<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                         female + unemployed + rural + religios + union + immigrant +internet +  
                         know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                         contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                         +compvote+other_el + nat_turn_cent + control_cand+
                         Junker_camp_MEP_cent+
                         +(1+juncker|region),
                       mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_campMEP)



#Table 4, Model 12
juncker_interact_campMEP<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                                  female + unemployed + rural + religios + union + immigrant +internet +  
                                  know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                  contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                  +compvote+other_el + nat_turn_cent + control_cand+
                                  Junker_camp_MEP_cent*juncker+
                                  +(1+juncker|region),
                                mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_campMEP)


#Table 4, Model 10
schulz_campMEP<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                        female + unemployed + rural + religios + union + immigrant +internet +  
                        know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                        contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                        +compvote+other_el + nat_turn_cent + control_cand+   
                        Schulz_camp_MEP_cent+
                        +(1+schlulz|region),
                      mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_campMEP)



#Table 4, Model 13
schlulz_interact_campMEP<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                                  female + unemployed + rural + religios + union + immigrant +internet +  
                                  know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                  contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                  +compvote+other_el + nat_turn_cent + control_cand +  
                                  Schulz_camp_MEP_cent*schlulz+
                                  +(1+schlulz|region),
                                mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_campMEP)

#Table 4, Model 11



verhofstadt_campMEP<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                             female + unemployed + rural + religios + union + immigrant +internet +  
                             know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                             contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                             +compvote+other_el + nat_turn_cent + control_cand+   
                             Verhofstadt_camp_MEP_cent+
                             +(1+verhofstadt|region),
                           mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_campMEP)

#Table 4, Model 14
verhofstadt_interact_campMEP<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                                      female + unemployed + rural + religios + union + immigrant +internet +  
                                      know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                      contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                      +compvote+other_el + nat_turn_cent + control_cand +  
                                      Verhofstadt_camp_MEP_cent*verhofstadt+
                                      +(1+verhofstadt|region),
                                    mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_campMEP)


stargazer(without_MEP,juncker_campMEP, schulz_campMEP, verhofstadt_campMEP,
          juncker_interact_campMEP, schlulz_interact_campMEP, verhofstadt_interact_campMEP,
          type="html",
          column.labels=c("Model 1: Without  the candidates", "Model 3: Juncker",
                          "Model 4: Schulz", "Model 5: Verhofstadt",  "Mode  6: Juncker, visits interaction",
                          "Model 7: Schulz visits interaction", "Model 8: Verhofstadt visits interaction"),
          out = "Appendix5_Table4.html", style="default",  star.cutoffs=c(0.1, 0.05, 0.01, 0.001),
          intercept.top=T, intercept.bottom=F, 
          keep.stat=c("ll","AIC", "n"), notes.align="c",single.row=TRUE)



#########controling for vote in the previous national election, stong robustenness check, commment reviewer 4, Appendix 6, Table 5
####to show model1_vote_prev

##Table5, Model 15
without_vote_prev<-glmer(voted~1 +vote_prev + married + secondary + tertiary +age+     
                           female + unemployed + rural + religios + union + immigrant +internet +  
                           know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                           contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                           +compvote+other_el + nat_turn_cent + control_cand+     
                           +(1|region),
                         mydata, family=binomial(link = "logit"), verbose=T)
summary(without_vote_prev)

##Table5, Model 16
juncker_vote_prev<-glmer(voted~1+vote_prev +juncker + married + secondary + tertiary +age+     
                           female + unemployed + rural + religios + union + immigrant +internet +  
                           know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                           contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                           +compvote+other_el + nat_turn_cent + control_cand+    
                           Juncker_camp_dum+
                           +(1+juncker|region),
                         mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_vote_prev)



##Table5, Model 19
juncker_interact_vote_prev<-glmer(voted~1 +vote_prev+juncker + married + secondary + tertiary +age+     
                                    female + unemployed + rural + religios + union + immigrant +internet +  
                                    know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                    contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                    +compvote+other_el + nat_turn_cent + control_cand+    MEP_cent+
                                    Juncker_camp_dum*juncker+
                                    +(1+juncker|region),
                                  mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_vote_prev)


##Table5, Model 17
schulz_vote_prev<-glmer(voted~1 +vote_prev+schlulz + married + secondary + tertiary +age+     
                          female + unemployed + rural + religios + union + immigrant +internet +  
                          know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                          contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                          +compvote+other_el + nat_turn_cent + control_cand+  MEP_cent+  
                          Schulz_camp_dum+
                          +(1+schlulz|region),
                        mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_vote_prev)



##Table5, Model 20
schlulz_interact_vote_prev<-glmer(voted~1 +vote_prev+schlulz + married + secondary + tertiary +age+     
                                    female + unemployed + rural + religios + union + immigrant +internet +  
                                    know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                    contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                    +compvote+other_el + nat_turn_cent + control_cand+   MEP_cent+ 
                                    Schulz_camp_dum*schlulz+
                                    +(1+schlulz|region),
                                  mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_vote_prev)

##Table5, Model 18

verhofstadt_vote_prev<-glmer(voted~1 +vote_prev+verhofstadt + married + secondary + tertiary +age+     
                               female + unemployed + rural + religios + union + immigrant +internet +  
                               know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                               contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                               +compvote+other_el + nat_turn_cent + control_cand+    
                               Verhofstadt_camp_dum+
                               +(1+verhofstadt|region),
                             mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_vote_prev)



##Table5, Model 21
verhofstadt_interact_vote_prev<-glmer(voted~1 +vote_prev+verhofstadt + married + secondary + tertiary +age+     
                                        female + unemployed + rural + religios + union + immigrant +internet +  
                                        know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                        contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                        +compvote+other_el + nat_turn_cent + control_cand+   MEP_cent+ 
                                        Verhofstadt_camp_dum*verhofstadt+
                                        +(1+verhofstadt|region),
                                      mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_vote_prev)


stargazer(without_vote_prev,juncker_vote_prev, schulz_vote_prev, verhofstadt_vote_prev,
          juncker_interact_vote_prev, schlulz_interact_vote_prev, verhofstadt_interact_vote_prev,
          type="html",
          column.labels=c("Model 1: Without  the candidates", "Model 3: Juncker",
                          "Model 4: Schulz", "Model 5: Verhofstadt",  "Mode  6: Juncker, visits interaction",
                          "Model 7: Schulz visits interaction", "Model 8: Verhofstadt visits interaction"),
          out = "Appendix6_Table5.html", style="default",  star.cutoffs=c(0.1, 0.05, 0.01, 0.001),
          intercept.top=T, intercept.bottom=F, 
          keep.stat=c("ll","AIC",  "n"), notes.align="c",single.row=TRUE)



###nesting in countries (sentivity check asked by reviewer 4), Appendix 7, Table 6
#Table 6,  model22 
without_country<-glmer(voted~1  + married + secondary + tertiary +age+     
                 female + unemployed + rural + religios + union + immigrant +internet +  
                 know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                 contact +  +trustNAT_dum + trustEU_dum +eugood+  
                 +compvote+other_el + nat_turn_cent +  MEP_cent 
                 +(1|country),
               mydata, family=binomial(link = "logit"), verbose=T)
summary(without_country)


#Table 6,  model23
juncker_country<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                 female + unemployed + rural + religios + union + immigrant +internet +  
                 know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                 contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                 +compvote+other_el + nat_turn_cent + control_cand+ MEP_cent+
                 Juncker_camp_dum+
                 +(1+juncker|country),
               mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_country)



#Table 6, model26
juncker_interact_country<<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                          female + unemployed + rural + religios + union + immigrant +internet +  
                          know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                          contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                          +compvote+other_el + nat_turn_cent + control_cand+ MEP_cent+  
                          Juncker_camp_dum*juncker+
                          +(1+juncker|country),
                        mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_country)



#Table 6,  model24
schulz_country<<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                female + unemployed + rural + religios + union + immigrant +internet +  
                know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+   
                Schulz_camp_dum+
                +(1+schlulz|country),
              mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_country)



#Table 6,  model27
schlulz_interact_country<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                          female + unemployed + rural + religios + union + immigrant +internet +  
                          know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                          contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                          +compvote+other_el + nat_turn_cent + control_cand+  MEP_cent+ 
                          Schulz_camp_dum*schlulz+
                          +(1+schlulz|country),
                        mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_country)

#Table 6,  model25

verhofstadt_country<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                     female + unemployed + rural + religios + union + immigrant +internet +  
                     know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                     contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                     +compvote+other_el + nat_turn_cent + control_cand+   MEP_cent+
                     Verhofstadt_camp_dum+
                     +(1+verhofstadt|country),
                   mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_country)

##########Table 6, model28

verhofstadt_interact_country<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                              female + unemployed + rural + religios + union + immigrant +internet +  
                              know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                              contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                              +compvote+other_el + nat_turn_cent + control_cand+ MEP_cent+  
                              Verhofstadt_camp_dum*verhofstadt+
                              +(1+verhofstadt|country),
                            mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_country)


stargazer(without_country,juncker_country, schulz_country, verhofstadt_country,
          juncker_interact_country, schlulz_interact_country, verhofstadt_interact_country,
          type="html",     
          column.labels=c("Model 1: Without  the candidates", "Model 3: Juncker",
          "Model 4: Schulz", "Model 5: Verhofstadt",  "Mode  6: Juncker, visits interaction",
          "Model 7: Schulz visits interaction", "Model 8: Verhofstadt visits interaction"),
          out = "Appendix7_Table6.html", style="default",  star.cutoffs=c(0.1,0.05, 0.01, 0.001),
          intercept.top=T, intercept.bottom=F, 
          keep.stat=c("ll","AIC", "n"), notes.align="c",single.row=TRUE)





############CONTROLING FOR POPULATION SIZE using population in million, reviewer 2 comment, not shown (Tabale, alternative 1)
#Table 2, Model 1, allternative 1

hist(mydata$MEP_cent)
##population in milloon 
mydata$pop_mil<-mydata$pop_min2-mean(mydata$pop_min2, na.rm=T)

without_popul<-glmer(voted~1  + married + secondary + tertiary +age+     
                       female + unemployed + rural + religios + union + immigrant +internet +  
                       know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                       contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                       +compvote+other_el + nat_turn_cent + control_cand +    pop_mil+ 
                       +(1|region),
                     mydata, family=binomial(link = "logit"), verbose=T)
summary(without_popul)


#Table 2, Molde 2 allternative 1
juncker_popul<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                       female + unemployed + rural + religios + union + immigrant +internet +  
                       know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                       contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                       +compvote+other_el + nat_turn_cent + control_cand+ pop_mil+  
                       Juncker_camp_dum+
                       +(1+juncker|region),
                     mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_popul)



#Table 2, Model 5, allternative 1
juncker_interact_popul<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                                female + unemployed + rural + religios + union + immigrant +internet +  
                                know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                +compvote+other_el + nat_turn_cent + control_cand+pop_mil+
                                Juncker_camp_dum*juncker+
                                +(1+juncker|region),
                              mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_popul)


#Table 2,  6, Model 3, allternative 1
schulz_popul<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                      female + unemployed + rural + religios + union + immigrant +internet +  
                      know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                      contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                      +compvote+other_el + nat_turn_cent + control_cand+pop_mil+   
                      Schulz_camp_dum+
                      +(1+schlulz|region),
                    mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_popul)



#Table 2, Model 6, allternative 1
schlulz_interact_popul<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                                female + unemployed + rural + religios + union + immigrant +internet +  
                                know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                +compvote+other_el + nat_turn_cent + control_cand+pop_mil +  
                                Schulz_camp_dum*schlulz+
                                +(1+schlulz|region),
                              mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_popul)

#Table 2, Model 4, allternative 1
verhofstadt_popul<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                           female + unemployed + rural + religios + union + immigrant +internet +  
                           know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                           contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                           +compvote+other_el + nat_turn_cent + control_cand+pop_mil+   
                           Verhofstadt_camp_dum+
                           +(1+verhofstadt|region),
                         mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_popul)

#Table 2, Model 7, allternative 1


verhofstadt_interact_popul<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                                    female + unemployed + rural + religios + union + immigrant +internet +  
                                    know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                    contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                    +compvote+other_el + nat_turn_cent + control_cand+pop_mil +  
                                    Verhofstadt_camp_dum*verhofstadt+
                                    +(1+verhofstadt|region),
                                  mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_popul)


stargazer(without_popul,juncker_popul, schulz_popul, verhofstadt_popul,
          juncker_interact_popul, schlulz_interact_popul, verhofstadt_interact_popul,
          type="html",
          column.labels=c("Model 1: Without  the candidates", "Model 3: Juncker",
                          "Model 4: Schulz", "Model 5: Verhofstadt",  "Mode  6: Juncker, visits interaction",
                          "Model 7: Schulz visits interaction", "Model 8: Verhofstadt visits interaction"),
          out = "Table2_alternative 1.html", style="default",  star.cutoffs=c(0.1, 0.05, 0.01, 0.001),
          intercept.top=T, intercept.bottom=F, 
          keep.stat=c("ll","AIC", "n"), notes.align="c",single.row=TRUE)

############CONTROLING FOR POPULATION SIZE, reviewer 2 comment, usimg the log of population, (Table 2, alternative 2)
hist(mydata$lnpop)
hist(mydata$MEP_cent)
##population in milloon 
mydata$lnpop_cent<-mydata$lnpop-mean(mydata$lnpop, na.rm=T)
#Table 2, Model 1,  allternative 2
without_lnpop<-glmer(voted~1  + married + secondary + tertiary +age+     
                       female + unemployed + rural + religios + union + immigrant +internet +  
                       know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                       contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                       +compvote+other_el + nat_turn_cent +    lnpop+ 
                       +(1|region),
                     mydata, family=binomial(link = "logit"), verbose=T)
summary(without_lnpop)


#Table 2, Model 2,  allternative 2
juncker_lnpop<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                       female + unemployed + rural + religios + union + immigrant +internet +  
                       know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                       contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                       +compvote+other_el + nat_turn_cent + control_cand+ lnpop+  
                       Juncker_camp_dum+
                       +(1+juncker|region),
                     mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_lnpop)



#Table 2, Model 5,  allternative 2
juncker_interact_lnpop<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                                female + unemployed + rural + religios + union + immigrant +internet +  
                                know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                +compvote+other_el + nat_turn_cent + control_cand+lnpop+
                                Juncker_camp_dum*juncker+
                                +(1+juncker|region),
                              mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_lnpop)


#Table 2, Model 3,  allternative 2
schulz_lnpop<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                      female + unemployed + rural + religios + union + immigrant +internet +  
                      know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                      contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                      +compvote+other_el + nat_turn_cent + control_cand+lnpop+   
                      Schulz_camp_dum+
                      +(1+schlulz|region),
                    mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_lnpop)



#Table 2, Model 6,  allternative 2
schlulz_interact_lnpop<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                                female + unemployed + rural + religios + union + immigrant +internet +  
                                know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                +compvote+other_el + nat_turn_cent + control_cand+lnpop +  
                                Schulz_camp_dum*schlulz+
                                +(1+schlulz|region),
                              mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_lnpop)

#Table 2, Model 4,  allternative 2

verhofstadt_lnpop<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                           female + unemployed + rural + religios + union + immigrant +internet +  
                           know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                           contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                           +compvote+other_el + nat_turn_cent + control_cand+lnpop+   
                           Verhofstadt_camp_dum+
                           +(1+verhofstadt|region),
                         mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_lnpop)

#Table 2, Model 7,  allternative 2


verhofstadt_interact_lnpop<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                                    female + unemployed + rural + religios + union + immigrant +internet +  
                                    know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                    contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                    +compvote+other_el + nat_turn_cent + control_cand+lnpop +  
                                    Verhofstadt_camp_dum*verhofstadt+
                                    +(1+verhofstadt|region),
                                  mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_lnpop)


stargazer(without_lnpop,juncker_lnpop, schulz_lnpop, verhofstadt_lnpop,
          juncker_interact_lnpop, schlulz_interact_lnpop, verhofstadt_interact_lnpop,
          type="html",
          column.labels=c("Model 1: Without  the candidates", "Model 3: Juncker",
                          "Model 4: Schulz", "Model 5: Verhofstadt",  "Mode  6: Juncker, visits interaction",
                          "Model 7: Schulz visits interaction", "Model 8: Verhofstadt visits interaction"),
          out = "Table_alt2.html", style="default",  star.cutoffs=c(0.1, 0.05, 0.01, 0.001),
          intercept.top=T, intercept.bottom=F, 
          keep.stat=c("ll","AIC", "n"), notes.align="c",single.row=TRUE)



##################################Plotting Figures###########################

#############schulz Recogniton and campaing main effects##################
schulz_MEP<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                    female + unemployed + rural + religios + union + immigrant +internet +  
                    know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                    contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                    +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+   
                    Schulz_camp_dum+
                    +(1+schlulz|region),
                  mydata, family=binomial(link = "logit"), verbose=T)
summary(schulz_MEP)


meanbetas.int<-c(fixef(schulz_MEP))
covmatrix.int<-vcov(schulz_MEP)

# take 1000 draws from the estimated coefficient vector and variance-covariance matrix
?set.seed
set.seed(461982)

MCdata<-mvrnorm(n=1000, meanbetas.int, covmatrix.int) 



#schulz regoniton
x_betahat_schulz<-MCdata[,"(Intercept)"] + 1*MCdata[,"schlulz"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##schulz no regoniton
x_betahat_noschulz<-MCdata[,"(Intercept)"] + 0*MCdata[,"schlulz"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


#1mean(data$intefic,na.rm=T)*MCdata[,"facebook:intefic"] 
prob_schulz <- inv.logit(x_betahat_schulz)
schulz_eff <- mean(prob_schulz)   ##0.42 mean, media =.42
lo_schulz <- quantile(prob_schulz, probs=c(0.025)) #0.28 mean 0.30
hi_schulz<- quantile(prob_schulz, probs=c(0.975)) #0.57, media 0.54
prob_noschulz <- inv.logit(x_betahat_noschulz) 
noschulz_eff <- mean(prob_noschulz)   ##0.35 for high trust, mean .34
lo_noschulz <- quantile(prob_noschulz, probs=c(0.025)) #0.23, mean .024
hi_noschulz<- quantile(prob_noschulz, probs=c(0.975)) #0.50, mean 0.47


dif_schulzrec <-prob_schulz-prob_noschulz
schulz_recon<- mean(dif_schulzrec)   ##0.17
lo_schulz_recon<- quantile(dif_schulzrec, probs=c(0.025)) #0.07
hi_schulz_recon<- quantile(dif_schulzrec, probs=c(0.975)) #0.26

#schulz campaing 
x_betahat_schulz_nocamp<-MCdata[,"(Intercept)"] + 0*MCdata[,"schlulz"]+  0*MCdata[,"Schulz_camp_dum"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]




##schulz  no campaign
x_betahat_schulz_camp<-MCdata[,"(Intercept)"] + 0*MCdata[,"schlulz"]+  1*MCdata[,"Schulz_camp_dum"]+
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


#1mean(data$intefic,na.rm=T)*MCdata[,"facebook:intefic"] 
prob_schulz_camp <- inv.logit(x_betahat_schulz_camp)
schulz_eff_camp<- mean(prob_schulz_camp)   ##0.47
lo_schulz_camp <- quantile(prob_schulz_camp, probs=c(0.025)) #0.37
hi_schulz_camp<- quantile(prob_schulz_camp, probs=c(0.975)) #0.57
prob_noschulz_camp <- inv.logit(x_betahat_schulz_nocamp) 
schulz_eff_nocamp <- mean(prob_noschulz_camp)   ##0.34 for high trust
lo_schulz_nocamp <- quantile(prob_noschulz_camp, probs=c(0.025)) #0.23
hi_schulz_nocamp<- quantile(prob_noschulz_camp, probs=c(0.975)) #0.49




dif_schulzcamp<-prob_schulz_camp-prob_noschulz_camp
schulz_camp<- mean(dif_schulzcamp)   ##0.17
lo_schulz_camp<- quantile(dif_schulzcamp, probs=c(0.05)) #0.07
hi_schulz_camp<- quantile(dif_schulzcamp, probs=c(0.95)) #0.26


##############Juncker Recogniton and campaing main effects###################
juncker_MEP<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                     female + unemployed + rural + religios + union + immigrant +internet +  
                     know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                     contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                     +compvote+other_el + nat_turn_cent + control_cand+ MEP_cent+  
                     Juncker_camp_dum+
                     +(1+juncker|region),
                   mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_MEP)


meanbetas.int1<-c(fixef(juncker_MEP))
covmatrix.int1<-vcov(juncker_MEP)

# take 1000 draws from the estimated coefficient vector and variance-covariance matrix
?set.seed
set.seed(461982)

MCdata_junk<-mvrnorm(n=10000, meanbetas.int1, covmatrix.int1) 



#junker recogition
x_betahat_juncker<-MCdata_junk[,"(Intercept)"] + 1*MCdata_junk[,"juncker"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+ mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]

##no junker recogition
x_betahat_nojuncker<-MCdata_junk[,"(Intercept)"] + 0*MCdata_junk[,"juncker"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]



prob_juncker <- inv.logit(x_betahat_juncker)
juncker_eff <- mean(prob_juncker)   ##0.53 mean, media =.42
lo_juncker <- quantile(prob_juncker, probs=c(0.025)) #0.41, mean 0.30
hi_juncker<- quantile(prob_juncker, probs=c(0.975)) #0.65, media 0.54
prob_nojuncker <- inv.logit(x_betahat_nojuncker) 
nojuncker_eff <- mean(prob_nojuncker)   ##0.47 for high trust, mean .34
lo6 <- quantile(prob_nojuncker, probs=c(0.025)) #0.35, mean .024
hi6<- quantile(prob_nojuncker, probs=c(0.975)) #0.59, mean 0.47


dif_junckerrec <-prob_juncker-prob_nojuncker
juncker_recon<- mean(dif_junckerrec)   ##0.17
lo_juncker_recon<- quantile(dif_junckerrec, probs=c(0.025)) #0.07
hi_juncker_recon<- quantile(dif_junckerrec, probs=c(0.975)) #0.26


#junker campaining 
x_betahat_juncker_camp<-MCdata_junk[,"(Intercept)"] + 1*MCdata_junk[,"Juncker_camp_dum"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+ mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##junker not campaining 
x_betahat_nojuncker_camp<-MCdata_junk[,"(Intercept)"] + 0*MCdata_junk[,"Juncker_camp_dum"]+ 
  mean(mydata$know,na.rm=T)*MCdata_junk[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata_junk[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata_junk[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata_junk[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata_junk[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata_junk[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata_junk[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata_junk[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata_junk[,"maxnews"]+ mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]



#1mean(data$intefic,na.rm=T)*MCdata_junk[,"facebook:intefic"] 
prob_juncker_camp <- inv.logit(x_betahat_juncker_camp)
juncker_eff_camp <- mean(prob_juncker_camp )   ##0.53 mean, media =.42
lo_juncker_camp  <- quantile(prob_juncker_camp , probs=c(0.025)) #0.41, mean 0.30
hi_juncker_camp <- quantile(prob_juncker_camp , probs=c(0.975)) #0.65, media 0.54
prob_juncker_nocamp <- inv.logit(x_betahat_nojuncker_camp) 
nojuncker_eff_nocamp <- mean(prob_juncker_nocamp)   ##0.47 for high trust, mean .34
lo6_nocamp <- quantile(prob_juncker_nocamp, probs=c(0.025)) #0.35, mean .024
hi6_nocamp<- quantile(prob_juncker_nocamp, probs=c(0.975)) #0.59, mean 0.47




dif_junckercamp <-prob_juncker_camp-prob_juncker_nocamp
juncker_camp<- mean(dif_junckercamp)   ##0.17
lo_juncker_camp<- quantile(dif_junckercamp, probs=c(0.05)) #0.07
hi_juncker_camp<- quantile(dif_junckercamp, probs=c(0.95)) #0.26


##############vershfostad Recogniton and campaing main effects##############
verhofstadt_MEP<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                         female + unemployed + rural + religios + union + immigrant +internet +  
                         know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                         contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                         +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+   
                         Verhofstadt_camp_dum+
                         +(1+verhofstadt|region),
                       mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_MEP)


meanbetas.int_vrh<-c(fixef(verhofstadt_MEP))
covmatrix.int_vrh<-vcov(verhofstadt_MEP)

# take 1000 draws from the estimated coefficient vector and variance-covariance matrix
?set.seed
set.seed(461982)

MCdata<-mvrnorm(n=10000, meanbetas.int_vrh, covmatrix.int_vrh) 


###verhofstadt no recogition
x_betahat_no_verhofstadt<-MCdata[,"(Intercept)"] + 0*MCdata[,"verhofstadt"]+  0*MCdata[,"Verhofstadt_camp_dum"]+ 
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]



###verhofstadt  recogition
x_betahat_verhofstadt<-MCdata[,"(Intercept)"] + 1*MCdata[,"verhofstadt"]+  0*MCdata[,"Verhofstadt_camp_dum"]+
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]

prob_verhofstadtp <- inv.logit(x_betahat_verhofstadt)
prob_verhofstadt<- mean(prob_verhofstadtp)   ##0.48
lo_verhofstadt <- quantile(prob_verhofstadtp, probs=c(0.025)) #0.38
hi_verhofstadt<- quantile(prob_verhofstadtp, probs=c(0.975)) #0.59
prob_noverhofstadt <- inv.logit(x_betahat_no_verhofstadt) 
verhofstadt_eff_no <- mean(prob_noverhofstadt)   ##0.40 for high trust
lo_verhofstadt_no <- quantile(prob_noverhofstadt, probs=c(0.025)) #0.38
hi_verhofstadt_no<- quantile(prob_noverhofstadt, probs=c(0.975)) #0.59

dif_verhofrec <-prob_verhofstadtp-prob_noverhofstadt
verhof_recon<- mean(dif_verhofrec)   ##0.17
lo_verhof_recon<- quantile(dif_verhofrec, probs=c(0.025)) #0.07
hi_verhof_recon<- quantile(dif_verhofrec, probs=c(0.975)) #0.26


###verhofstadt  no campaign

x_betahat_verhofstadt_nocamp<-MCdata[,"(Intercept)"] + 0*MCdata[,"verhofstadt"]+  0*MCdata[,"Verhofstadt_camp_dum"]+ 
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]



###verhofstadt   campaign

x_betahat_verhofstadt_camp<-MCdata[,"(Intercept)"] + 0*MCdata[,"verhofstadt"]+  1*MCdata[,"Verhofstadt_camp_dum"]+
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]
                                       



prob_verhofstadt_camp <- inv.logit(x_betahat_verhofstadt_camp)
verhofstadt_eff_camp<- mean(prob_verhofstadt_camp)   ##0.48
lo_verhofstadt_camp <- quantile(prob_verhofstadt_camp, probs=c(0.025)) #0.38
hi_verhofstadt_camp<- quantile(prob_verhofstadt_camp, probs=c(0.975)) #0.59
prob_verhofstadt_nocamp <- inv.logit(x_betahat_verhofstadt_nocamp) 
verhofstadt_eff_nocamp <- mean(prob_verhofstadt_nocamp)   ##0.40 for high trust
lo_verhofstadt_nocamp <- quantile(prob_verhofstadt_nocamp, probs=c(0.025)) #0.38
hi_verhofstadt_nocamp<- quantile(prob_verhofstadt_nocamp, probs=c(0.975)) #0.59




dif_verhofcamp <-prob_verhofstadt_camp-prob_verhofstadt_nocamp
verhof_camp<- mean(dif_verhofcamp)   ##0.17
lo_verhof_camp<- quantile(dif_verhofcamp, probs=c(0.05)) #0.07
hi_verhof_camp<- quantile(dif_verhofcamp, probs=c(0.95)) #0.26



##################plotin Figure 1#######################



recogniton<- data.frame(condtion=c("Schulz", "Juncker", "Verhofstadt"))

recogniton$meanint<-c(schulz_recon,juncker_recon, verhof_recon)
recogniton$loint<-c(lo_schulz_recon,lo_juncker_recon, lo_verhof_recon)
recogniton$hiint<-c(hi_schulz_recon,hi_juncker_recon, hi_verhof_recon)

recogniton$candidate<-c("Schulz", "Juncker",  "Verhofstadt")

Figure_1 <- ggplot(recogniton, aes(as.factor(condtion), meanint, ymin =loint, ymax=hiint))
Figure_1<-Figure_1 + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+
  xlab("")+
  ylab("Change in predicted proability of voting, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  scale_x_discrete(limits=c("Schulz","Juncker", "Verhofstadt"),
                   labels= c("Schulz recognition \nvs. no recognition",
                             "Juncker recognition \n vs. no recognition",
                             "Verhofstadt recognition \n vs. no recognition"))+
  theme(axis.title.x = element_text(size = rel(1.8)), axis.title.y = element_text(size = rel(1.2)))+
  theme(axis.text.x = element_text(size = rel(1.6)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))
Figure_1

ggsave(Figure_1, file="Figure_1.jpg",  width=8, height=5)


##################plotin Figure 2#############################################


campaign<- data.frame(condtion=c("Schulz", "Juncker", "Verhofstadt"))

campaign$meanint<-c(schulz_camp,juncker_camp, verhof_camp)
campaign$loint<-c(lo_schulz_camp,lo_juncker_camp, lo_verhof_camp)
campaign$hiint<-c(hi_schulz_camp,hi_juncker_camp, hi_verhof_camp)

recogniton$candidate<-c("Schulz", "Juncker",  "Verhofstadt")

Figure_2 <- ggplot(campaign, aes(as.factor(condtion), meanint, ymin =loint, ymax=hiint))
Figure_2<-Figure_2 + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+
  xlab("")+
  ylab("Change in predicted proability of voting, 90% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  scale_x_discrete(limits=c("Schulz","Juncker", "Verhofstadt"),
                   labels= c("Schulz visited \nvs. not visited",
                             "Juncker visited \nvs. not visited",
                             "Verhofstadt rvisited \nvs. not visited"))+
  theme(axis.title.x = element_text(size = rel(1.7)), axis.title.y = element_text(size = rel(1.2)))+
  theme(axis.text.x = element_text(size = rel(1.8)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))
Figure_2


ggsave(Figure_2, file="Figure_2.jpg",  width=8, height=5)




############## Plotting moderating effect of recognition visits depending on campaign, Schulz###################


schlulz_interact_MEP<-glmer(voted~1 +schlulz + married + secondary + tertiary +age+     
                              female + unemployed + rural + religios + union + immigrant +internet +  
                              know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                              contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                              +compvote+other_el + nat_turn_cent + control_cand+MEP_cent +  
                              Schulz_camp_dum*schlulz+
                              +(1+schlulz|region),
                            mydata, family=binomial(link = "logit"), verbose=T)
summary(schlulz_interact_MEP)


meanbetas.int_sch<-c(fixef(schlulz_interact_MEP))
covmatrix.int_sch<-vcov(schlulz_interact_MEP)

# take 1000 draws from the estimated coefficient vector and variance-covariance matrix
?set.seed
set.seed(461982)

MCdata<-mvrnorm(n=1000, meanbetas.int_sch, covmatrix.int_sch) 

##predicted pobabilities: no recognition, no campaing
x_betahat_noschulz_nocamp<-MCdata[,"(Intercept)"] + 0*MCdata[,"schlulz"]+  0*MCdata[,"Schulz_camp_dum"]+
  +0*MCdata[,"schlulz:Schulz_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##predicted pobabilities:  recognition, no campaing
x_betahat_schulz_nocamp<-MCdata[,"(Intercept)"] + 1*MCdata[,"schlulz"]+  0*MCdata[,"Schulz_camp_dum"]+
  +1*0*MCdata[,"schlulz:Schulz_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##predicted pobabilities: no recognition,  campaing
x_betahat_noschulz_camp<-MCdata[,"(Intercept)"] + 0*MCdata[,"schlulz"]+  1*MCdata[,"Schulz_camp_dum"]+
  +1*0*MCdata[,"schlulz:Schulz_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##predicted pobabilities:  recognition,  campaing
x_betahat_schulz_camp<-MCdata[,"(Intercept)"] + 1*MCdata[,"schlulz"]+  1*MCdata[,"Schulz_camp_dum"]+
  +1*1*MCdata[,"schlulz:Schulz_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+ mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]




prob_noschulz_nocamp <- inv.logit(x_betahat_noschulz_nocamp)
noschulz_nocamp<- mean(prob_noschulz_nocamp)   ##0.45
lo_noschulz_nocamp <- quantile(prob_noschulz_nocamp, probs=c(0.025)) #0.37
hi_noschulz_nocamp<- quantile(prob_noschulz_nocamp, probs=c(0.975)) #0.54

prob_schulz_nocamp <- inv.logit(x_betahat_schulz_nocamp)
schulz_nocamp<- mean(prob_schulz_nocamp)   ##0.45
lo_schulz_nocamp <- quantile(prob_schulz_nocamp, probs=c(0.025)) #0.37
hi_schulz_nocamp<- quantile(prob_schulz_nocamp, probs=c(0.975)) #0.54


prob_noschulz_camp <- inv.logit(x_betahat_noschulz_camp)
noschulz_camp<- mean(prob_noschulz_camp)   ##0.45
lo_noschulz_camp <- quantile(prob_noschulz_camp, probs=c(0.025)) #0.37
hi_noschulz_camp<- quantile(prob_noschulz_camp, probs=c(0.975)) #0.54


prob_schulz_camp <- inv.logit(x_betahat_schulz_camp)
schulz_camp<- mean(prob_schulz_camp)   ##0.45
lo_schulz_camp <- quantile(prob_schulz_camp, probs=c(0.025)) #0.37
hi_schulz_camp<- quantile(prob_schulz_camp, probs=c(0.975)) #0.54


########## Plotting Appendix 4, Figure A3.A

supdata1 <- data.frame(condtion=c("no schulz no campaign","schulz no campaign","no schulz campaign","schulz campaign"))

supdata1$meanint<-c(noschulz_nocamp,schulz_nocamp, noschulz_camp, schulz_camp)
supdata1$loint<-c(lo_noschulz_nocamp,lo_schulz_nocamp, lo_noschulz_camp, lo_schulz_camp)
supdata1$hiint<-c(hi_noschulz_nocamp,hi_schulz_nocamp, hi_noschulz_camp, hi_schulz_camp)

supdata1$schulz<-c("no recognition","recognition","no recognition", "recognition")
supdata1$campaign<-c("no campaign", "no campaign","campaign","campaign")

?theme


Figure_A3A <- ggplot(supdata1, aes(as.factor(campaign), meanint, ymin =loint, ymax=hiint))
Figure_A3A<-Figure_A3A + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+facet_grid(. ~ schulz, margins = "vs")+
  xlab("")+
  ylab("Predicted proability to vote, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  theme(axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1.7)))+
  theme(axis.text.x = element_text(size = rel(1.8)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))+
  scale_x_discrete(limits=c("no campaign","campaign"), labels= c("not visited","visited"))
Figure_A3A


ggsave(Figure_A3A, file="Figure_A3A.jpg",  width=8, height=5)

########## Plotting Figure 3.A

dif_schulzviz_noschulz_novisit<-prob_schulz_camp-prob_noschulz_nocamp
schulz_nocamp_noschulz<- mean(difmaxmin)   ##0.17
lo_schulz_nocamp_noschulz<- quantile(difmaxmin, probs=c(0.025)) #0.07
hi_schulz_nocamp_noschulz<- quantile(difmaxmin, probs=c(0.975)) #0.26



dif_nosch_vis_nosch_novisit<-prob_noschulz_camp-prob_noschulz_nocamp
nosch_vis_nosch_novisit<- mean(dif_nosch_vis_nosch_novisit)   ##0.09
lo_nosch_vis_nosch_novisit<- quantile(dif_nosch_vis_nosch_novisit, probs=c(0.025)) #0.002
hi_nosch_vis_nosch_novisit<- quantile(dif_nosch_vis_nosch_novisit, probs=c(0.975)) #0.17


dif_sch_vis_sch_novisit<-prob_schulz_camp-prob_schulz_nocamp
sch_vis_sch_novisit<- mean(dif_sch_vis_sch_novisit)   ##0.09
lo_sch_vis_sch_novisit<- quantile(dif_sch_vis_sch_novisit, probs=c(0.025)) #0.002
hi_sch_vis_sch_novisit<- quantile(dif_sch_vis_sch_novisit, probs=c(0.975)) #0.17


schdif2 <- data.frame(condtion=c("recognition moderating campaign", "recognition moderating campaign"))

schdif2$meanint<-c( nosch_vis_nosch_novisit,sch_vis_sch_novisit )
schdif2$loint<-c( lo_nosch_vis_nosch_novisit,lo_sch_vis_sch_novisit)
schdif2$hiint<-c( hi_nosch_vis_nosch_novisit,hi_sch_vis_sch_novisit)

schdif2$moderator<-c("1","2")


Figure_3A<- ggplot(schdif2, aes(moderator, meanint, ymin =loint, ymax=hiint))
Figure_3A<-Figure_3A + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+
  xlab("")+
  ylab("Change in predicted proability of voting, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  theme(axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1.15)))+
  theme(axis.text.x = element_text(size = rel(1.7)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))+
  scale_x_discrete(limits=c("1","2"),
                   labels=c("visited vs. not visited \n (no recognition)",
                            "visited vs. not visited \n  (recognition)"))
Figure_3A


ggsave(Figure_3A, file="Figure_3A.jpg",  width=8, height=5)



############## Plotting moderating effect of recognition visits depending on campaign, Verhofstradt####################

verhofstadt_interact_MEP<-glmer(voted~1 +verhofstadt + married + secondary + tertiary +age+     
                                  female + unemployed + rural + religios + union + immigrant +internet +  
                                  know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                                  contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                                  +compvote+other_el + nat_turn_cent + control_cand+MEP_cent +  
                                  Verhofstadt_camp_dum*verhofstadt+
                                  +(1+verhofstadt|region),
                                mydata, family=binomial(link = "logit"), verbose=T)
summary(verhofstadt_interact_MEP)


meanbetas.int_vrh<-c(fixef(verhofstadt_interact_MEP))
covmatrix.int_vrh<-vcov(verhofstadt_interact_MEP)

# take 1000 draws from the estimated coefficient vector and variance-covariance matrix
?set.seed
set.seed(461982)

MCdata<-mvrnorm(n=1000, meanbetas.int_vrh, covmatrix.int_vrh) 

##predicted pobabilities: no recognition, no campaing
x_betahat_noVerhofstadt_nocamp<-MCdata[,"(Intercept)"] + 0*MCdata[,"verhofstadt"]+  0*MCdata[,"Verhofstadt_camp_dum"]+
  +0*MCdata[,"verhofstadt:Verhofstadt_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]

##predicted pobabilities:  recognition, no campaing
x_betahat_Verhofstadt_nocamp<-MCdata[,"(Intercept)"] + 1*MCdata[,"verhofstadt"]+  0*MCdata[,"Verhofstadt_camp_dum"]+
  +1*0*MCdata[,"verhofstadt:Verhofstadt_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]

##predicted pobabilities: no recognition,  campaing
x_betahat_noVerhofstadt_camp<-MCdata[,"(Intercept)"] + 0*MCdata[,"verhofstadt"]+  1*MCdata[,"Verhofstadt_camp_dum"]+
  +1*0*MCdata[,"verhofstadt:Verhofstadt_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]

##predicted pobabilities: no recognition, no campaing
x_betahat_Verhofstadt_camp<-MCdata[,"(Intercept)"] + 1*MCdata[,"verhofstadt"]+  1*MCdata[,"Verhofstadt_camp_dum"]+
  +1*1*MCdata[,"verhofstadt:Verhofstadt_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+
  mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]

prob_noverhofstadt_nocamp <- inv.logit(x_betahat_noVerhofstadt_nocamp)
noverhofstadt_nocamp<- mean(prob_noverhofstadt_nocamp)   ##0.45
lo_noverhofstadt_nocamp <- quantile(prob_noverhofstadt_nocamp, probs=c(0.025)) #0.37
hi_noverhofstadt_nocamp<- quantile(prob_noverhofstadt_nocamp, probs=c(0.975)) #0.54

prob_verhofstadt_nocamp <- inv.logit(x_betahat_Verhofstadt_nocamp)
verhofstadt_nocamp<- mean(prob_verhofstadt_nocamp)   ##0.45
lo_verhofstadt_nocamp <- quantile(prob_verhofstadt_nocamp, probs=c(0.025)) #0.37
hi_verhofstadt_nocamp<- quantile(prob_verhofstadt_nocamp, probs=c(0.975)) #0.54


prob_noverhofstadt_camp <- inv.logit(x_betahat_noVerhofstadt_camp)
noverhofstadt_camp<- mean(prob_noverhofstadt_camp)   ##0.45
lo_noverhofstadt_camp <- quantile(prob_noverhofstadt_camp, probs=c(0.025)) #0.37
hi_noverhofstadt_camp<- quantile(prob_noverhofstadt_camp, probs=c(0.975)) #0.54


prob_verhofstadt_camp <- inv.logit(x_betahat_Verhofstadt_camp)
verhofstadt_camp<- mean(prob_verhofstadt_camp)   ##0.45
lo_verhofstadt_camp <- quantile(prob_verhofstadt_camp, probs=c(0.025)) #0.37
hi_verhofstadt_camp<- quantile(prob_verhofstadt_camp, probs=c(0.975)) #0.54


########## Plotting Appendix 4, Figure A3.B

supdata1 <- data.frame(condtion=c("no verhofstadt no campaign","verhofstadt no campaign","no verhofstadt campaign","verhofstadt campaign"))

supdata1$meanint<-c(noverhofstadt_nocamp,verhofstadt_nocamp, noverhofstadt_camp, verhofstadt_camp)
supdata1$loint<-c(lo_noverhofstadt_nocamp,lo_verhofstadt_nocamp, lo_noverhofstadt_camp, lo_verhofstadt_camp)
supdata1$hiint<-c(hi_noverhofstadt_nocamp,hi_verhofstadt_nocamp, hi_noverhofstadt_camp, hi_verhofstadt_camp)

supdata1$verhofstadt<-c("no recognition","recognition","no recognition", "recognition")
supdata1$campaign<-c("no campaign", "no campaign","campaign","campaign")

?theme

Figure_A3B <- ggplot(supdata1, aes(as.factor(campaign), meanint, ymin =loint, ymax=hiint))
Figure_A3B<-Figure_A3B + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+facet_grid(. ~verhofstadt , margins = "vs")+
  xlab("")+
  ylab("Predicted proability to vote, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  theme(axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1.7)))+
  theme(axis.text.x = element_text(size = rel(1.8)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))+
  scale_x_discrete(limits=c("no campaign","campaign"), labels= c("not visited","visited"))
Figure_A3B


ggsave(Figure_A3B, file="Figure_A3B.jpg",  width=8, height=5)


##########Plotting Figure 3.B 

dif_nover_vis_nover_novisit<-prob_noverhofstadt_camp-prob_noverhofstadt_nocamp
nover_vis_nover_novisit<- mean(dif_nover_vis_nover_novisit)   ##0.09
lo_nover_vis_nover_novisit<- quantile(dif_nover_vis_nover_novisit, probs=c(0.025)) #0.002
hi_nover_vis_nover_novisit<- quantile(dif_nover_vis_nover_novisit, probs=c(0.975)) #0.17


dif_ver_vis_ver_novisit<-prob_verhofstadt_camp-prob_verhofstadt_nocamp
ver_vis_ver_novisit<- mean(dif_ver_vis_ver_novisit)   ##0.09
lo_ver_vis_ver_novisit<- quantile(dif_ver_vis_ver_novisit, probs=c(0.025)) #0.002
hi_ver_vis_ver_novisit<- quantile(dif_ver_vis_ver_novisit, probs=c(0.975)) #0.17


verdif2 <- data.frame(condtion=c("recognition moderating campaign", "recognition moderating campaign"))

verdif2$meanint<-c( nover_vis_nover_novisit,ver_vis_ver_novisit )
verdif2$loint<-c( lo_nover_vis_nover_novisit,lo_ver_vis_ver_novisit)
verdif2$hiint<-c( hi_nover_vis_nover_novisit,hi_ver_vis_ver_novisit)

verdif2$moderator<-c("no moderator","moderator")
c
?theme

Figure_3B <- ggplot(verdif2, aes(as.factor(moderator), meanint, ymin =loint, ymax=hiint))
Figure_3B<-Figure_3B + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+
  xlab("")+
  ylab("Change in predicted proability to vote, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  theme(axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1.15)))+
  theme(axis.text.x = element_text(size = rel(1.7)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))+
  scale_x_discrete(limits=c("no moderator","moderator"),
                   labels=c("visited vs. not visited \n (no recognition)",
                            "visited vs. not visited \n  (recognition)"))
Figure_3B


ggsave(Figure_3B, file="Figure_3B.jpg",  width=8, height=5)




############## Plotting moderating effect of recognition visits depending on campaign, Verhofstradt, JUNCKER####################


juncker_interact_MEP<-glmer(voted~1 +juncker + married + secondary + tertiary +age+     
                              female + unemployed + rural + religios + union + immigrant +internet +  
                              know + intpol +discuss + allefic + partisan++maxnews +exposure + involv+ 
                              contact +  +trustNAT_dum + trustEU_dum +eugood+ 
                              +compvote+other_el + nat_turn_cent + control_cand+MEP_cent+
                              Juncker_camp_dum*juncker+
                              +(1+juncker|region),
                            mydata, family=binomial(link = "logit"), verbose=T)
summary(juncker_interact_MEP)

meanbetas.int_jun<-c(fixef(juncker_interact_MEP))
covmatrix.int_jun<-vcov(juncker_interact_MEP)

# take 1000 draws from the estimated coefficient vector and variance-covariance matrix
?set.seed
set.seed(461982)

MCdata<-mvrnorm(n=1000, meanbetas.int_jun, covmatrix.int_jun) 

##predicted pobabilities: no recognition, no campaing
x_betahat_nojuncker_nocamp<-MCdata[,"(Intercept)"] + 0*MCdata[,"juncker"]+  0*MCdata[,"Juncker_camp_dum"]+
  +0*MCdata[,"juncker:Juncker_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##predicted pobabilities:  recognition, no campaing
x_betahat_juncker_nocamp<-MCdata[,"(Intercept)"] + 1*MCdata[,"juncker"]+  0*MCdata[,"Juncker_camp_dum"]+
  +1*0*MCdata[,"juncker:Juncker_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##predicted pobabilities: no recognition,  campaing
x_betahat_nojuncker_camp<-MCdata[,"(Intercept)"] + 0*MCdata[,"juncker"]+  1*MCdata[,"Juncker_camp_dum"]+
  +1*0*MCdata[,"juncker:Juncker_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


##predicted pobabilities:  recognition,  campaing
x_betahat_juncker_camp<-MCdata[,"(Intercept)"] + 1*MCdata[,"juncker"]+  1*MCdata[,"Juncker_camp_dum"]+
  +1*1*MCdata[,"juncker:Juncker_camp_dum"]+  
  mean(mydata$know,na.rm=T)*MCdata[,"know"]+ mean(mydata$involv,na.rm=T)*MCdata[,"involv"]+
  mean(mydata$discuss,na.rm=T)*MCdata[,"discuss"]+ mean(mydata$allefic,na.rm=T)*MCdata[,"allefic"]+
  mean(mydata$age,na.rm=T)*MCdata[,"age"]+ mean(mydata$intpol,na.rm=T)*MCdata[,"intpol"]+
  mean(mydata$religios,na.rm=T)*MCdata[,"religios"]+ mean(mydata$internet,na.rm=T)*MCdata[,"internet"]+
  mean(mydata$maxnews,na.rm=T)*MCdata[,"maxnews"]+mean(mydata$nat_turn_cent,na.rm=T)*MCdata[,"nat_turn_cent"]+
  mean(mydata$MEP_cent,na.rm=T)*MCdata[,"MEP_cent"]


prob_nojuncker_nocamp <- inv.logit(x_betahat_nojuncker_nocamp)
nojuncker_nocamp<- mean(prob_nojuncker_nocamp)   ##0.47
lo_nojuncker_nocamp <- quantile(prob_nojuncker_nocamp, probs=c(0.025)) #0.35
hi_nojuncker_nocamp<- quantile(prob_nojuncker_nocamp, probs=c(0.975)) #0.60

prob_juncker_nocamp <- inv.logit(x_betahat_juncker_nocamp)
juncker_nocamp<- mean(prob_juncker_nocamp)   ##0.53
lo_juncker_nocamp <- quantile(prob_juncker_nocamp, probs=c(0.025)) #0.40
hi_juncker_nocamp<- quantile(prob_juncker_nocamp, probs=c(0.975)) #0.65


prob_nojuncker_camp <- inv.logit(x_betahat_nojuncker_camp)
nojuncker_camp<- mean(prob_nojuncker_camp)   ##0.48
lo_nojuncker_camp <- quantile(prob_nojuncker_camp, probs=c(0.025)) #0.37
hi_nojuncker_camp<- quantile(prob_nojuncker_camp, probs=c(0.975)) #0.58


prob_juncker_camp <- inv.logit(x_betahat_juncker_camp)
juncker_camp<- mean(prob_juncker_camp)   ##0.53
lo_juncker_camp <- quantile(prob_juncker_camp, probs=c(0.025)) #0.43
hi_juncker_camp<- quantile(prob_juncker_camp, probs=c(0.975)) #0.64

########## Plotting Appendix 4, Figure A3.C
supdata1jun <- data.frame(condtion=c("no juncker no campaign","juncker no campaign","no juncker campaign","juncker campaign"))

supdata1jun$meanint<-c(nojuncker_nocamp,juncker_nocamp, nojuncker_camp, juncker_camp)
supdata1jun$loint<-c(lo_nojuncker_nocamp,lo_juncker_nocamp, lo_nojuncker_camp, lo_juncker_camp)
supdata1jun$hiint<-c(hi_nojuncker_nocamp,hi_juncker_nocamp, hi_nojuncker_camp, hi_juncker_camp)

supdata1jun$juncker<-c("no recognition","recognition","no recognition", "recognition")
supdata1jun$campaign<-c("no campaign", "no campaign","campaign","campaign")

?theme

Figure_A3C<- ggplot(supdata1jun, aes(as.factor(campaign), meanint, ymin =loint, ymax=hiint))
Figure_A3C<-Figure_A3C + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+facet_grid(. ~juncker , margins = "vs")+
  xlab("")+
  ylab("Predicted proability to vote, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  theme(axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1.7)))+
  theme(axis.text.x = element_text(size = rel(1.8)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))+
  scale_x_discrete(limits=c("no campaign","campaign"), labels= c("not visited","visited"))
Figure_A3C


ggsave(Figure_A3C, file="Figure_A3C.jpg",  width=8, height=5)



##########Plotting Figure 3.c
dif_nojun_vis_nojun_novisit<-prob_nojuncker_camp-prob_nojuncker_nocamp
nojun_vis_nojun_novisit<- mean(dif_nojun_vis_nojun_novisit)   ##0.09
lo_nojun_vis_nojun_novisit<- quantile(dif_nojun_vis_nojun_novisit, probs=c(0.025)) #0.002
hi_nojun_vis_nojun_novisit<- quantile(dif_nojun_vis_nojun_novisit, probs=c(0.975)) #0.17


dif_jun_vis_jun_novisit<-prob_juncker_camp-prob_juncker_nocamp
jun_vis_jun_novisit<- mean(dif_jun_vis_jun_novisit)   ##0.09
lo_jun_vis_jun_novisit<- quantile(dif_jun_vis_jun_novisit, probs=c(0.025)) #0.002
hi_jun_vis_jun_novisit<- quantile(dif_jun_vis_jun_novisit, probs=c(0.975)) #0.17


jundif2 <- data.frame(condtion=c("recognition moderating campaign", "recognition moderating campaign"))

jundif2$meanint<-c( nojun_vis_nojun_novisit,jun_vis_jun_novisit )
jundif2$loint<-c( lo_nojun_vis_nojun_novisit,lo_jun_vis_jun_novisit)
jundif2$hiint<-c( hi_nojun_vis_nojun_novisit,hi_jun_vis_jun_novisit)

jundif2$moderator<-c("no moderator","moderator")


Figure_3c<- ggplot(jundif2, aes(as.factor(moderator), meanint, ymin =loint, ymax=hiint))
Figure_3c<-Figure_3c + geom_linerange(lwd=2)+ geom_errorbar(width = 0.3,lwd=1)+geom_pointrange(size=1.3)+
  xlab("")+
  ylab("Change in predicted proability to vote, 95% CI")+ 
  theme_bw()+guides(colour=FALSE)+theme(panel.grid.major=element_line(size=1.2))+
  theme(axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1.15)))+
  theme(axis.text.x = element_text(size = rel(1.7)),axis.text.y = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 18))+
  scale_x_discrete(limits=c("no moderator","moderator"),
                   labels=c("visited vs. not visited \n (no recognition)",
                            "visited vs. not visited \n  (recognition)"))
Figure_3c


ggsave(Figure_3c, file="Figure_3c.jpg",  width=8, height=5)




save.image("EUP models and figures.RData")

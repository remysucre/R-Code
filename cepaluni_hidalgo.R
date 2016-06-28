## ---- packages ----

#Run on R version 3.2.2
#Load necessary packages; see bottom of this file for precise versions of packages
library(lubridate)
library(mgcv)
library(dplyr)
library(ggplot2)
library(survey)
library(stargazer)
library(ggthemes)
library(sandwich)
library(lmtest)
library(rdrobust)
library(multiwayvcov)
library(rdd)
library(foreign)

## ---- estimation_data ----


#Load datasets
load('cepaluni_hidalgo.RData')


cadastro42 <- tbl_df(cadastro42)
cadastro92 <- tbl_df(cadastro92)
cadastro94 <- tbl_df(cadastro94)
pnad0513_1994 <- tbl_df(pnad0513_1994)

##Drop residents of Distrito Federal
cadastro42 <- filter(cadastro42, uf != 'DF')
cadastro94 <- filter(cadastro94, uf != 'DF')
pnad0513_1994 <- filter(pnad0513_1994, uf!= 'DF')

##Add a weekday fixed effects
cadastro94$weekday <- wday(cadastro94$bdate, label = TRUE)
cadastro42$weekday <- wday(cadastro42$bdate, label = TRUE)
cadastro92$weekday <- wday(cadastro92$bdate, label = TRUE)

##Add Education Variable
cadastro94 <- mutate(cadastro94, primary_edu_complete =
                       ifelse(education %in% c('Analfabeto', 'Ensino Fundamental Incompleto', 'Le e Escreve'), 0, 1))
cadastro94 <- mutate(cadastro94, secondary_complete =
                       ifelse(education %in% c('Ensino Medio Completo', 'Superior Completo', 'Superior Incompleto'), 1, 0))
cadastro42 <- mutate(cadastro42, primary_edu_complete =
                       ifelse(education %in% c('Analfabeto', 'Ensino Fundamental Incompleto', 'Le e Escreve'), 0, 1))
cadastro42 <- mutate(cadastro42, secondary_complete =
                       ifelse(education %in% c('Ensino Medio Completo', 'Superior Completo', 'Superior Incompleto'), 1, 0))

##Add Gender, Marital Status, and State Variables
cadastro42 <- mutate(cadastro42, 
                     woman = ifelse(sexo %in% c('Feminino'), 1, 0),
                     married = ifelse(marital_status %in% c('Casado'), 1, 0))

cadastro94 <- mutate(cadastro94, 
                     woman = ifelse(sexo %in% c('Feminino'), 1, 0),
                     married = ifelse(marital_status %in% c('Casado'), 1, 0))

## ---- survey_state_services ----

#
#Figure 1
#

#Specify Survey Design
svy_des <- svydesign(id = ~psu, weights = ~weight, data = svy_data)


#Use of State Services
table_state_services <- svyby(~num_state_services, by = ~edu_cat, design = svy_des, FUN = svymean)

ggplot(table_state_services, aes(y= num_state_services, x = edu_cat)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymax = num_state_services + (1.96 * se), ymin = num_state_services - (1.96 * se)))  +
  coord_flip() +
  theme_bw() +
  xlab('Education') + 
  ylab('Mean Number of State Services Used')

## ---- functions ----

###Function produces a dataset that lists number of estimated people born per day, turnout, registration.
#Input is individual level cadastro data and pnad data
est_ppl <- function(cadastro, pnad){
  pnad_agg <- summarise(group_by(pnad, fv), weekday = unique(weekday),
                        ppl = sum(sample_weight) / length(unique(year))
  )
  gam_model <- gam(ppl ~ weekday + s(fv, bs = 'cr', k = 30), data = pnad_agg, method = "GCV.Cp")

  cadastro_agg <-  group_by(cadastro, fv) %>%
    summarise(treat = unique(treat),
              weekday = unique(weekday),
              turnout = sum(turnout),
              registered = n()
    )
  cadastro_agg$estppl <- round(predict(gam_model, cadastro_agg))
  cadastro_agg
}

gen_estim_data <- function(cadastro, pnad, bw = 30, pnad_year = 2011){
  cadastro_agg <- list()
  cadastro_agg$primary_incomplete_women <-  est_ppl(cadastro = filter(cadastro , education != 'Analfabeto' & primary_edu_complete == 0 & sexo == 'Feminino'),
                                                    pnad = filter(pnad, literate == 1 & year >= pnad_year & primary_edu_complete == 0 & sex == 'Feminino'))
  cadastro_agg$primary_incomplete_men <-  est_ppl(cadastro = filter(cadastro , education != 'Analfabeto' & primary_edu_complete == 0 & sexo == 'Masculino'),
                                                  pnad = filter(pnad, literate == 1 & year >= pnad_year & primary_edu_complete == 0 & sex == 'Masculino'))
  cadastro_agg$primary_complete_women <-  est_ppl(cadastro = filter(cadastro , education != 'Analfabeto' & primary_edu_complete == 1 & sexo == 'Feminino'),
                                                  pnad = filter(pnad, literate == 1 & year >= pnad_year & primary_edu_complete == 1 & sex == 'Feminino'))
  cadastro_agg$primary_complete_men <-  est_ppl(cadastro = filter(cadastro , education != 'Analfabeto' & primary_edu_complete == 1 & sexo == 'Masculino'),
                                                pnad = filter(pnad, literate == 1 & year >= pnad_year & primary_edu_complete == 1 & sex == 'Masculino'))

  cadastro_agg$primary_incomplete_women$primary_edu_complete <- 0
  cadastro_agg$primary_incomplete_men$primary_edu_complete <- 0
  cadastro_agg$primary_complete_women$primary_edu_complete <- 1
  cadastro_agg$primary_complete_men$primary_edu_complete <- 1

  cadastro_agg$primary_incomplete_women$female <- 1
  cadastro_agg$primary_incomplete_men$female <- 0
  cadastro_agg$primary_complete_women$female <- 1
  cadastro_agg$primary_complete_men$female <- 0

  estim_data <- rbind_all(cadastro_agg)

  estim_data <- group_by(estim_data, fv, primary_edu_complete) %>%
    summarise(treat = unique(treat), weekday = unique(weekday), turnout = sum(turnout), registered = sum(registered), estppl = sum(estppl))
  estim_data$turnout <- estim_data$turnout / estim_data$estppl

  estim_data$wts <- 1 - abs(estim_data$fv / bw)

  estim_data[abs(estim_data$fv) <= bw, ]
}


## ---- rdd_plot ----

#
#Figure 2
#

plot_data42 <- cadastro42 %>%
  filter(education != "Analfabeto") %>%
  group_by(fv, primary_edu_complete) %>%
  summarise(treat = unique(treat), turnout = mean(turnout), num_voters = n()) %>%
  filter(abs(fv) <= 100)
plot_data42$Education <- ifelse(plot_data42$primary_edu_complete == 0, "Less Than Primary Education", "Primary Education or More")

#Calculate bin midpoint
plot_data42$bin <- NA
plot_data42$bin[plot_data42$primary_edu_complete == 1] <-  as.character(cut(plot_data42$fv[plot_data42$primary_edu_complete == 1], seq(-100,100, 5), include.lowest = TRUE, right = FALSE))
plot_data42$bin[plot_data42$primary_edu_complete == 0] <-  as.character(cut(plot_data42$fv[plot_data42$primary_edu_complete == 0], seq(-100,100, 5), include.lowest = TRUE, right = FALSE))
plot_data42$bin_midpoint <- (as.numeric(gsub(pattern = "(\\(|\\[)(-*[0-9]{1,}),.*", x = plot_data42$bin, replacement = "\\2")) + as.numeric(gsub(pattern = ".*\\,(-*[0-9]{1,})(\\)|])", x = plot_data42$bin, replacement = "\\1")))/2



ggplot(plot_data42, aes(x = fv, y = turnout)) +
  stat_summary(aes(x = bin_midpoint), fun.y = mean, geom = "point", size = 3)  +
  geom_point(alpha = .2) +
  geom_smooth(aes(group = treat), method = 'loess', degree = 2, se = FALSE, span = 1, size = 1.5) +
  facet_wrap(~Education, nrow = 1) +
  ylab("Turnout") +
  xlab("Days Until 70th Birthday") +
  ylim(c(.76, .925)) + 
  theme_bw() + 
  geom_vline(xintercept = 0, size = .05, linetype = "dotted") +
  annotate("text", x=-50, y=.92, label="Voluntary\nVoting", size = 3) +
  annotate("text", x=50, y=.92, label="Compulsory\nVoting", size = 3)

## ---- estimation ----

#
#Table 1
#


pnad_year <- 2009

##Create estimation datasets for 69-70 Year Old Sample

estim_data42_agg <- cadastro42 %>%
  filter(education != "Analfabeto") %>%
  group_by(fv) %>%
  summarise(treat = unique(treat), turnout = mean(turnout), num_voters = n(),
            primary_edu_complete = mean(primary_edu_complete)) %>%
  filter(abs(fv) <= 365)

estim_data42 <- cadastro42 %>%
  filter(education != "Analfabeto") %>%
  group_by(fv, weekday, primary_edu_complete) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n()) %>%
  filter(abs(fv) <= 365)

#Models for Top Panel of Table 1
mod_all42 <- rdrobust(y = estim_data42_agg$turnout, x = estim_data42_agg$fv)
mod_eduhi42 <- rdrobust(y = estim_data42$turnout[estim_data42$primary_edu_complete == 1], x = estim_data42$fv[estim_data42$primary_edu_complete == 1] )
mod_edulo42 <- rdrobust(y = estim_data42$turnout[estim_data42$primary_edu_complete == 0], x = estim_data42$fv[estim_data42$primary_edu_complete == 0] )

#Difference in LATE between High Education and Lower Education Sample
diff_hilo42 <- list()
diff_hilo42$est <- mod_eduhi42$coef[1] - mod_edulo42$coef[1]
diff_hilo42$se <- sqrt((mod_eduhi42$se[3])^2 + (mod_edulo42$se[3])^2)

##Create estimation datasets for 17-18 Year Old Sample

estim_data94_agg <- group_by(gen_estim_data(cadastro = cadastro94,
                                   pnad = pnad0513_1994,
                                   pnad_year = pnad_year,
                                   bw = 365),
                             fv) %>%
  summarise(turnout = weighted.mean(turnout, estppl), estppl = sum(estppl))
estim_data94 <- gen_estim_data(cadastro = cadastro94,
                               pnad = pnad0513_1994,
                               pnad_year = pnad_year,
                               bw = 365)

#Models for Bottom Panel of Table 2

mod_all94 <- rdrobust(y = estim_data94_agg$turnout, x = estim_data94_agg$fv)
mod_eduhi94 <- rdrobust(y = estim_data94$turnout[estim_data94$primary_edu_complete == 1], x = estim_data94$fv[estim_data94$primary_edu_complete == 1] )
mod_edulo94 <- rdrobust(y = estim_data94$turnout[estim_data94$primary_edu_complete == 0], x = estim_data94$fv[estim_data94$primary_edu_complete == 0] )

#Difference in LATE between High Education and Lower Education Sample

diff_hilo94 <- list()
diff_hilo94$est <- mod_eduhi94$coef[1] - mod_edulo94$coef[1]
diff_hilo94$se <- sqrt((mod_eduhi94$se[3])^2 + (mod_edulo94$se[3])^2)

fullsample_coefs <- coef(lm(turnout ~ treat * fv, data = estim_data42_agg, 
                       weights = kweight(estim_data42_agg$fv, 0, mod_all42$h, kernel = "tri"))) 
eduhi_coefs <- coef(lm(turnout ~ treat * fv, data = estim_data42[estim_data42$primary_edu_complete == 1, ], 
                    weights = kweight(estim_data42$fv[estim_data42$primary_edu_complete == 1], 0, mod_eduhi42$h, kernel = "tri"))) 
edulo_coefs <- coef(lm(turnout ~ treat * fv, data = estim_data42[estim_data42$primary_edu_complete == 0, ], 
                       weights = kweight(estim_data42$fv[estim_data42$primary_edu_complete == 0], 0, mod_edulo42$h, kernel = "tri"))) 


## ---- bandwidth_robustness ----

#Estimate models using different bandwidths

#1942 Robustness

bw <- seq(from = 15, to = 300)
main_effect_robustness42 <- data.frame(bw = bw, est = NA, cilo = NA, cihi = NA)
less_educated_robustness42 <- data.frame(bw = bw, est = NA, cilo = NA, cihi = NA)
more_educated_robustness42 <- data.frame(bw = bw, est = NA, cilo = NA, cihi = NA)

for(i in bw){
  robust_all <- rdrobust(y = estim_data42_agg$turnout, x = estim_data42_agg$fv, h = i)
  main_effect_robustness42$est[main_effect_robustness42$bw == i] <- robust_all$coef[1]
  main_effect_robustness42$cilo[main_effect_robustness42$bw == i] <- robust_all$ci[1,1]
  main_effect_robustness42$cihi[main_effect_robustness42$bw == i] <- robust_all$ci[1,2]
  
  robust_less_edu <- with(estim_data42, rdrobust(y = turnout[primary_edu_complete == 0], 
                                                 x = fv[primary_edu_complete == 0], h = i))
  less_educated_robustness42$est[less_educated_robustness42$bw == i] <- robust_less_edu$coef[1]
  less_educated_robustness42$cilo[less_educated_robustness42$bw == i] <- robust_less_edu$ci[1,1]
  less_educated_robustness42$cihi[less_educated_robustness42$bw == i] <- robust_less_edu$ci[1,2]
  
  robust_more_edu <- with(estim_data42, rdrobust(y = turnout[primary_edu_complete == 1], 
                                                 x = fv[primary_edu_complete == 1], h = i))
  more_educated_robustness42$est[more_educated_robustness42$bw == i] <- robust_more_edu$coef[1]
  more_educated_robustness42$cilo[more_educated_robustness42$bw == i] <- robust_more_edu$ci[1,1]
  more_educated_robustness42$cihi[more_educated_robustness42$bw == i] <- robust_more_edu$ci[1,2]
  
}

main_effect_robustness42$Sample <- 'Full Sample'
less_educated_robustness42$Sample <- 'Less than Primary Education'
more_educated_robustness42$Sample <- 'Primary Education or More'

robustness42 <- rbind(main_effect_robustness42,
                      less_educated_robustness42,
                      more_educated_robustness42)

#1994 Robustness, these estimates are used in appendix

estim_data94 <- gen_estim_data(cadastro = cadastro94,
                               pnad = pnad0513_1994,
                               pnad_year = pnad_year,
                               bw = 365)
main_effect_robustness94 <- data.frame(bw = bw, est = NA, cilo = NA, cihi = NA)
less_educated_robustness94 <- data.frame(bw = bw, est = NA, cilo = NA, cihi = NA)
more_educated_robustness94 <- data.frame(bw = bw, est = NA, cilo = NA, cihi = NA)

for(i in bw){
  robust_all <- rdrobust(y = estim_data94_agg$turnout, x = estim_data94_agg$fv, h = i)
  main_effect_robustness94$est[main_effect_robustness94$bw == i] <- robust_all$coef[1]
  main_effect_robustness94$cilo[main_effect_robustness94$bw == i] <- robust_all$ci[1,1]
  main_effect_robustness94$cihi[main_effect_robustness94$bw == i] <- robust_all$ci[1,2]
  
  robust_less_edu <- with(estim_data94, rdrobust(y = turnout[primary_edu_complete == 0], 
                                                 x = fv[primary_edu_complete == 0], h = i))
  less_educated_robustness94$est[less_educated_robustness94$bw == i] <- robust_less_edu$coef[1]
  less_educated_robustness94$cilo[less_educated_robustness94$bw == i] <- robust_less_edu$ci[1,1]
  less_educated_robustness94$cihi[less_educated_robustness94$bw == i] <- robust_less_edu$ci[1,2]
  
  robust_more_edu <- with(estim_data94, rdrobust(y = turnout[primary_edu_complete == 1], 
                                                 x = fv[primary_edu_complete == 1], h = i))
  more_educated_robustness94$est[more_educated_robustness94$bw == i] <- robust_more_edu$coef[1]
  more_educated_robustness94$cilo[more_educated_robustness94$bw == i] <- robust_more_edu$ci[1,1]
  more_educated_robustness94$cihi[more_educated_robustness94$bw == i] <- robust_more_edu$ci[1,2]
  
}

main_effect_robustness94$Sample <- 'Full Sample'
less_educated_robustness94$Sample <- 'Less than Primary Education'
more_educated_robustness94$Sample <- 'Primary Education or More'

robustness94 <- rbind(main_effect_robustness94,
                      less_educated_robustness94,
                      more_educated_robustness94)


## ---- bandwidth_robustness_1942 ----

#
#Figure 3
#


robust_labels42 <- data.frame(x = c(mod_all42$h, mod_eduhi42$h, mod_edulo42$h), 
                              y = c(mod_all42$coef[1], mod_eduhi42$coef[1], mod_edulo42$coef[1]), 
                              label = c("X", "X", "X"),
                              Sample = c("Full Sample","Primary Education or More",  "Less than Primary Education")
)

ggplot(robustness42, aes(x = bw, y = est)) +
  geom_line(size = 2) +
  theme_bw() +
  xlab('Bandwidth') +
  ylab('Estimate') +
  geom_line(aes(x = bw, y = cilo), size = .7, linetype = 2) + 
  geom_line(aes(x = bw, y = cihi), size = .7, linetype = 2) + 
  facet_wrap(~Sample, ncol = 3) + 
  geom_text(aes(x, y, label=label, group=NULL),data=robust_labels42, size = 7, color = "red")


#############
####Appendix
#############



## ---- births_day_of_week ----

#
#Figure A.1
#

ppl_byday_92 <- group_by(cadastro92, fv, weekday) %>%
    summarise(ppl = n())

ggplot(ppl_byday_92, aes(x = fv, y = ppl)) +
    geom_point(size = 1.7) + theme_bw() +
    xlab('Day of Birth') +
    ylab('Number of Registered Voters') +
    facet_wrap(~weekday, nrow = 2) +
    scale_x_continuous(breaks = c(-200,0, 200))

## ---- ests_day_of_week ----

#
#Figure A.2
#


###Use PNAD to Calculate People per Day
ppl_byday94 <- group_by(filter(pnad0513_1994, literate == 1 & year >= 2012), fv) %>%
    summarise(weekday = unique(weekday),
              ppl = sum(sample_weight) / length(unique(year))
              )
gam94 <- gam(ppl ~ weekday + s(fv, bs = 'cr', k = 30), data = ppl_byday94, method = "GCV.Cp")

ppl_byday94$estppl <- predict(gam94)

ggplot(ppl_byday94, aes(x = fv, y = estppl)) +
    geom_line(size = 2) +
    geom_point(aes(x = fv, y = ppl), size = 1.5) +
    facet_wrap(~weekday, ncol = 4) +
    ylab('Estimate') + xlab("Day of Birth") +
    theme_bw() +
    scale_x_continuous(breaks = c(-200,0, 200))


## ---- smoothness_1942 ----

#
#Figure A.3
#


density_plot_data <- filter(estim_data42_agg, abs(fv) <= 365)

density_plot_data$bin <- NA
density_plot_data$bin <-  as.character(cut(density_plot_data$fv, seq(-365,365, 5), include.lowest = TRUE, right = FALSE))
density_plot_data$bin_midpoint <- (as.numeric(gsub(pattern = "(\\(|\\[)(-*[0-9]{1,}),.*", x = density_plot_data$bin, replacement = "\\2")) + as.numeric(gsub(pattern = ".*\\,(-*[0-9]{1,})(\\)|])", x = density_plot_data$bin, replacement = "\\1")))/2


ggplot(density_plot_data, aes(x = fv, y = num_voters)) +
  stat_summary(aes(x = bin_midpoint), fun.y = mean, geom = "point", size = 3)  +
  geom_point(alpha = .2) +
  geom_smooth(aes(group = treat), method = 'loess', degree = 2, se = TRUE, span = 1, size = 1.5) + 
  ylab("Number of Registered Voters") +
  xlab("Days Until 70th Birthday") +
  theme_bw()  + 
  ylim(0, max(density_plot_data$num_voters))

## ---- balance_1942 ----

#
#Figure A.4
#


bal_data42 <- cadastro42 %>%
  filter(education != "Analfabeto") %>%
  group_by(fv) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n(),
            primary_edu_complete = mean(primary_edu_complete),
            secondary_edu_complete = mean(secondary_complete),
            woman = mean(woman),
            married = mean(married)) %>%
  filter(abs(fv) <= 365)

check_balance <- function(covar, data){
  data$covar <- data.frame(data)[, covar]
  mod <- rdrobust(y = data$covar, x = data$fv)
  return(data_frame(covar = c(covar), 
                    coef = c(mod$coef[1]),
                    se = c(mod$se[3]),
                    ci_hi = c(mod$ci[3,2]),
                    ci_lo = c(mod$ci[3,1]),
                    bw = c("CCT Bandwidth")))
}


bal_stats <- rbind_all(lapply(c("married", "primary_edu_complete", "secondary_edu_complete", "woman"),
                              check_balance, 
                              data = bal_data42))
bal_stats$t_stat <- bal_stats$coef/bal_stats$se
bal_stats$pvalue <- pt(abs(bal_stats$t_stat), nrow(bal_data42), lower.tail  = FALSE) * 2
bal_stats$Variable <- rep(c("Married", "Primary Education", "Secondary Education", "Women"), each = 1)
bal_stats$Variable <- factor(bal_stats$Variable, levels = c("Women", "Secondary Education", "Primary Education", "Married"))

balance_plot <- ggplot(bal_stats[bal_stats$bw == "CCT Bandwidth",], aes( x = coef, y = Variable)) +
  theme_bw() + 
  geom_errorbarh(aes(xmax = ci_lo,
                     xmin = ci_hi),
                 height = 0, color = 'darkgray') +
  geom_vline(xintercept = 0, linetype = 3, size = .25) +
  geom_point(size = 4) +
  xlab("Coefficient") 

balance_plot


## ---- balance_1994 ----

#
#Figure A.5
#


bal_pnad <- pnad0513_1994[pnad0513_1994$year > pnad_year, ]
pnad0513_1994$treat <- ifelse(pnad0513_1994$fv >= 0, 1, 0)
pnad0513_1994$woman <- ifelse(pnad0513_1994$sex == "Feminino", 1, 0)


bal_data94 <- filter(pnad0513_1994, year >= pnad_year) %>%
  filter(literate == 1) %>%
  group_by(fv) %>%
  summarise(treat = unique(treat), 
            num_voters = sum(sample_weight) / length(unique(year)),
            primary_edu_complete = weighted.mean(primary_edu_complete, w = sample_weight / length(unique(year))),
            secondary_edu_complete = weighted.mean(secondary_edu_complete, w = sample_weight / length(unique(year))),
            woman = weighted.mean(woman, w = sample_weight / length(unique(year)))) %>%
  filter(abs(fv) <= 365)

bal_stats94 <- rbind_all(lapply(c("primary_edu_complete", "secondary_edu_complete", "woman"),
                              check_balance, 
                              data = bal_data94))
bal_stats94$t_stat <- bal_stats94$coef/bal_stats94$se
bal_stats94$pvalue <- pt(abs(bal_stats94$t_stat), nrow(bal_data94), lower.tail  = FALSE) * 2
bal_stats94$Variable <- rep(c( "Primary Education", "Secondary Education", "Women"), each = 1)
bal_stats94$Variable <- factor(bal_stats94$Variable, levels = c("Women", "Secondary Education", "Primary Education"))

##Check results after conditioniing on gender
estim_data94_gender <- left_join(estim_data94_agg, bal_data94)
estim_data94_gender$turnout_resids <- lm(turnout ~ woman, data = estim_data94_gender)$residuals
gender_robust_check <- rdrobust(estim_data94_gender$turnout_resids, estim_data94_gender$fv)

balance_plot94 <- ggplot(bal_stats94[bal_stats94$bw == "CCT Bandwidth",], aes( x = coef, y = Variable)) +
  theme_bw() + 
  geom_errorbarh(aes(xmax = ci_hi,
                     xmin = ci_lo),
                 height = 0, color = 'darkgray') +
  geom_vline(xintercept = 0, linetype = 3, size = .25) +
  geom_point(size = 4) +
  xlab("Coefficient")

balance_plot94


## ---- rdd_plot_1994 ----

#
#Figure A.6
#

plot_data94 <- gen_estim_data(cadastro = cadastro94,
                              pnad = pnad0513_1994,
                              pnad_year = pnad_year,
                              bw = 100) %>%
  group_by(fv, primary_edu_complete) %>%
  summarise(turnout = mean(turnout), treat = unique(treat))

plot_data94$Education <- ifelse(plot_data94$primary_edu_complete == 0, "Less Than Primary Education", "Primary Education or More")

#Calculate bin midpoint
plot_data94$bin <- NA
plot_data94$bin[plot_data94$primary_edu_complete == 1] <-  as.character(cut(plot_data94$fv[plot_data94$primary_edu_complete == 1], seq(-100,100, 5), include.lowest = TRUE, right = FALSE))
plot_data94$bin[plot_data94$primary_edu_complete == 0] <-  as.character(cut(plot_data94$fv[plot_data94$primary_edu_complete == 0], seq(-100,100, 5), include.lowest = TRUE, right = FALSE))
plot_data94$bin_midpoint <- (as.numeric(gsub(pattern = "(\\(|\\[)(-*[0-9]{1,}),.*", x = plot_data94$bin, replacement = "\\2")) + as.numeric(gsub(pattern = ".*\\,(-*[0-9]{1,})(\\)|])", x = plot_data94$bin, replacement = "\\1")))/2


ggplot(plot_data94, aes(x = fv, y = turnout)) +
  stat_summary(aes(x = bin_midpoint), fun.y = mean, geom = "point", size = 3)  +
  geom_point(alpha = .2) +
  geom_smooth(aes(group = treat), method = 'loess', degree = 2, se = FALSE, span = 1, size = 1.5) +
  facet_wrap(~Education, nrow = 1) +
  ylab("Turnout") +
  xlab("Days Until 18th Birthday") +
  theme_bw() + 
  geom_vline(xintercept = 0, size = .05, linetype = "dotted") +
  annotate("text", x=-50, y=.92, label="Voluntary\nVoting", size = 3) +
  annotate("text", x=50, y=.92, label="Compulsory\nVoting", size = 3)

## ---- local_randomization ----

#
#Table A.1
#


cadastro42_1week <- filter(cadastro42, fv <= 1 & fv >= -2 & education != "Analfabeto")

binom_results <- binom.test(c(sum(cadastro42_1week$fv>= 0), sum(cadastro42_1week$fv< 0)), p = 1/2)

#Estimates using for Table A.1
local_rand_mods <- list()
local_rand_mods$all <- lm(turnout ~ treat, data = cadastro42_1week)
local_rand_mods$all$se <- sqrt(diag(cluster.vcov(local_rand_mods$all, 1:nrow(cadastro42_1week))))
local_rand_mods$edulo <- lm(turnout ~ treat, data = cadastro42_1week, subset = primary_edu_complete == 0)
local_rand_mods$edulo$se <- sqrt(diag(cluster.vcov(local_rand_mods$edulo, 1:nrow(cadastro42_1week[cadastro42_1week$primary_edu_complete == 0, ]))))
local_rand_mods$eduhi <- lm(turnout ~ treat, data = cadastro42_1week, subset = primary_edu_complete == 1)
local_rand_mods$eduhi$se <- sqrt(diag(cluster.vcov(local_rand_mods$eduhi, 1:nrow(cadastro42_1week[cadastro42_1week$primary_edu_complete == 1, ]))))
local_rand_mods$eduinter <- lm(turnout ~ treat * primary_edu_complete, data = cadastro42_1week)
local_rand_mods$eduinter$se <- sqrt(diag(cluster.vcov(local_rand_mods$eduinter, 1:nrow(cadastro42_1week))))



## ---- alternative_se_1994 ----

#
#Table A.2
#

nboots <- 1000

###Function for Bootstrapping

bs_se <- function(pnad, cadastro, pnad_year = 2011, bw = 30, mod, nboots = 1000, subset = NULL){
  svy_data <- filter(pnad, year >= pnad_year)
  svy_data$sample_weight <- svy_data$sample_weight / length(unique(svy_data$year))
  
  svy_pnad <- svydesign(
    id = ~psu,
    strata = ~strata,
    data = svy_data,
    weights = ~ sample_weight,
    nest = TRUE
  )
  
  bs_svy_pnad <- as.svrepdesign(svy_pnad, type = "bootstrap", replicates = nboots, compress = FALSE)$repweights
  
  bs_coefs <- vector(mode = "list", length = nboots)
  
  for(i in 1:nboots){
    svy_data_bs <- svy_data[rep(seq.int(1, nrow(svy_data)), bs_svy_pnad[, i]), ]
    svy_data_bs$sample_weight <- svy_data_bs$sample_weight * length(unique(svy_data_bs$year))
    estim_data_bs <- gen_estim_data(cadastro = cadastro, pnad = svy_data_bs, pnad_year = pnad_year, bw = bw)
    estim_data_bs <- estim_data_bs[sample(1:nrow(estim_data_bs), size = nrow(estim_data_bs), replace = TRUE), ]
    bs_coefs[[i]] <- coef(lm(as.formula(mod),
                             data = estim_data_bs,
                             weights = estppl * wts,
                             subset = eval(expression(subset))))
  }
  bs_coefs <- do.call(rbind, bs_coefs)
  apply(bs_coefs, 2, sd)
}


bw_94 <- rdbwselect(y = estim_data94_agg$turnout, estim_data94_agg$fv)$bws[1]
mod_main_effect94 <- (lm(turnout ~ treat * fv,
                         data = gen_estim_data(cadastro = cadastro94,
                                               pnad = pnad0513_1994,
                                               pnad_year = pnad_year,
                                               bw = bw_94),
                         weights = wts * estppl))
mod_main_effect94$se <- bs_se(pnad = pnad0513_1994,
                                  cadastro = cadastro94,
                                  pnad_year = pnad_year,
                                  bw = bw_94,
                                  mod = "turnout ~ treat * fv",
                                  nboots = nboots)



mod_inter_edu94 <- (lm(turnout ~ treat * fv * primary_edu_complete,
                       data = gen_estim_data(cadastro = cadastro94,
                                             pnad = pnad0513_1994,
                                             pnad_year = pnad_year,
                                             bw = bw_94),
                       weights = wts))
mod_inter_edu94$se <- bs_se(pnad = pnad0513_1994,
                                cadastro = cadastro94,
                                pnad_year = pnad_year,
                                bw = bw_94,
                                mod = "turnout ~ treat * fv * primary_edu_complete",
                                nboots = nboots)



stargazer(mod_main_effect94, mod_inter_edu94,
          se = list(mod_main_effect94$se, mod_inter_edu94$se),
          dep.var.labels = c("DV: Turnout"),
          covariate.labels = c("Compulsory Voting", "Primary Education", "Compulsory Voting x Primary Education", "Intercept"),
          add.lines = list(
            c("Bandwidth (Days)", round(bw_94), round(bw_94)),
            c("Number of Individuals", 
              format(with(estim_data94_agg, sum(estppl[abs(fv) <= bw_94])), big.mark = ","),
              format(with(estim_data94_agg, sum(estppl[abs(fv) <= bw_94])), big.mark = ","))
          ),
          omit = "fv",
          keep.stat = "n",
          star.cutoffs = c(.05),
          notes = "Bootstrapped standard errors in parentheses.$^{*}$p$<$0.05",
          notes.append = FALSE,
          float = FALSE,
          font.size = "footnotesize",
          type = "latex")




## ---- bandwidth_robustness_1994 ----

#
#Figure A.7
#


robust_labels94 <- data.frame(x = c(mod_all94$h, mod_eduhi94$h, mod_edulo94$h), 
                              y = c(mod_all94$coef[1], mod_eduhi94$coef[1], mod_edulo94$coef[1]), 
                              label = c("X", "X", "X"),
                              Sample = c("Full Sample","Primary Education or More",  "Less than Primary Education")
)

ggplot(robustness94, aes(x = bw, y = est)) +
  geom_line(size = 2) +
  theme_bw() +
  xlab('Bandwidth') +
  ylab('Estimate') +
  geom_line(aes(x = bw, y = cilo), size = .7, linetype = 2) + 
  geom_line(aes(x = bw, y = cihi), size = .7, linetype = 2) + 
  facet_wrap(~Sample, ncol = 3) + 
  geom_text(aes(x, y, label=label, group=NULL),data=robust_labels94, size = 7, color = "red")

## ---- alternative_thresholds42 ----

#
#Figure A.8
#


placebo_ests_42 <- data_frame(threshold = seq(-300, 300, 60), 
                           estimate = NA,
                           se = NA) 
for(i in seq(-300, 300, 60)){
  estim_data42_agg$placebo_fv <- estim_data42_agg$fv - i
  estim_data42_agg$placebo_treat <- ifelse(estim_data42_agg$placebo_fv > 0, 1, 0)
  placebo_rd <- rdrobust(y = estim_data42_agg$turnout, 
           x = estim_data42_agg$placebo_fv)
  placebo_ests_42$estimate[placebo_ests_42$threshold == i] <-placebo_rd$coef[1]
  placebo_ests_42$se[placebo_ests_42$threshold == i] <- placebo_rd$se[3]
}

ggplot(placebo_ests_42, aes(x = threshold, y = estimate)) + 
  geom_point(size = 3) + 
  theme_bw() + 
  xlab("Threshold") + 
  ylab("Estimate") + 
  geom_hline(yintercept = 0, size = .5, linetype = "dotted") + 
  geom_errorbar(aes(ymax = estimate + 1.96 * se,
                     ymin = estimate - 1.96 * se),
                 height = 0, color = 'darkgray')

## ---- alternative_thresholds94 ----

#
#Figure A.9
#

thresh_94_data <- gen_estim_data(cadastro = cadastro94,
                                 pnad = pnad0513_1994,
                                 pnad_year = pnad_year,
                                 bw = 330)  %>%
  summarise(turnout = weighted.mean(turnout, estppl), estppl = sum(estppl))


placebo_ests_94 <- data_frame(threshold = seq(-300, 300, 60), 
                              estimate = NA,
                              se = NA) 
for(i in seq(-300, 300, 60)){
  thresh_94_data$placebo_fv <- thresh_94_data$fv - i
  thresh_94_data$placebo_treat <- ifelse(thresh_94_data$placebo_fv > 0, 1, 0)
  placebo_rd <- rdrobust(y = thresh_94_data$turnout, 
                         x = thresh_94_data$placebo_fv)
  placebo_ests_94$estimate[placebo_ests_94$threshold == i] <-placebo_rd$coef[1]
  placebo_ests_94$se[placebo_ests_94$threshold == i] <- placebo_rd$se[3]
}

ggplot(placebo_ests_94, aes(x = threshold, y = estimate)) + 
  geom_point(size = 3) + 
  theme_bw() +
  xlab("Threshold") + 
  ylab("Estimate") + 
  geom_hline(yintercept = 0, size = .5, linetype = "dotted") + 
  geom_errorbar(aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                height = 0, color = 'darkgray')

## ---- results_at_other_edu_levels ----

#
#Figure A.10
#


estim_data_byedu42 <- cadastro42 %>%
  group_by(fv, weekday, education) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n()) %>%
  filter(abs(fv) <= 365)

estim_data_medio_incomplete42 <- cadastro42 %>%
  filter(education == "Ensino Medio Incompleto") %>%
  group_by(fv, weekday) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n()) %>%
  filter(abs(fv) <= 365)


estim_data_medio_complete42 <- cadastro42 %>%
  filter(education == "Ensino Medio Completo") %>%
  group_by(fv, weekday) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n()) %>%
  filter(abs(fv) <= 365)


estim_data_superior_incomplete42 <- cadastro42 %>%
  filter(education == "Superior Incompleto") %>%
  group_by(fv, weekday) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n()) %>%
  filter(abs(fv) <= 365)

estim_data_superior_complete42 <- cadastro42 %>%
  filter(education == "Superior Completo") %>%
  group_by(fv, weekday) %>%
  summarise(treat = unique(treat), 
            turnout = mean(turnout), 
            num_voters = n()) %>%
  filter(abs(fv) <= 365)

mod_fund_le42 <- with(filter(estim_data_byedu42, education == "Le e Escreve"), rdrobust(y = turnout, x = fv))
mod_fund_incomplete42 <- with(filter(estim_data_byedu42, education == "Ensino Fundamental Incompleto"), rdrobust(y = turnout, x = fv))
mod_fund_complete42 <- with(filter(estim_data_byedu42, education == "Ensino Fundamental Completo"), rdrobust(y = turnout, x = fv))
mod_medio_incomplete42 <- with(filter(estim_data_byedu42, education == "Ensino Medio Incompleto"), rdrobust(y = turnout, x = fv))
mod_medio_complete42 <- with(filter(estim_data_byedu42, education == "Ensino Medio Completo"), rdrobust(y = turnout, x = fv))
mod_superior_incomplete42 <- with(filter(estim_data_byedu42, education == "Superior Incompleto"), rdrobust(y = turnout, x = fv))
mod_superior_complete42 <- with(filter(estim_data_byedu42, education == "Superior Completo"), rdrobust(y = turnout, x = fv))

results_by_edu <- data.frame(coef = sapply(list(mod_fund_le42, mod_fund_incomplete42, mod_fund_complete42, mod_medio_incomplete42, mod_medio_complete42, mod_superior_incomplete42, mod_superior_complete42), function(x) x$coef[1]),
                             cilo = sapply(list(mod_fund_le42, mod_fund_incomplete42, mod_fund_complete42, mod_medio_incomplete42, mod_medio_complete42, mod_superior_incomplete42, mod_superior_complete42), function(x) x$ci[3,1]),
                             cihi = sapply(list(mod_fund_le42, mod_fund_incomplete42, mod_fund_complete42, mod_medio_incomplete42, mod_medio_complete42, mod_superior_incomplete42, mod_superior_complete42), function(x) x$ci[3,2]))
results_by_edu$Education <- factor(c("Read and Write", "Primary Incomplete", "Primary Completed", "Secondary Education Incomplete", "Secondary Education Completed", "College Incomplete", "College Completed"),
                                   levels = c("Read and Write", "Primary Incomplete", "Primary Completed", "Secondary Education Incomplete", "Secondary Education Completed", "College Incomplete", "College Completed"))

ggplot(results_by_edu, aes(x = coef, y = Education)) + 
  theme_bw() + 
  geom_errorbarh(aes(xmax = cilo,
                     xmin = cihi),
                 height = 0, color = 'darkgray') +
  geom_vline(xintercept = 0, linetype = 3, size = .25) +
  geom_point(size = 4) +
  xlab("Coefficient")


## ---- survey_thresholds_knowledge ----

#
#Figure A.11
#

#Information about Thresholds
table_age18 <- svyby(~age18_correct, by = ~edu_cat, design = svy_des, FUN = svymean)
table_age18$Variable <- "Correctly Stated\nAge 18"
names(table_age18)[names(table_age18) == "age18_correct"] <- 'thresh_correct'
table_age70 <- svyby(~age70_correct, by = ~edu_cat, design = svy_des, FUN = svymean)
table_age70$Variable <- "Correctly Stated\nAge 70"
names(table_age70)[names(table_age70) == "age70_correct"] <- 'thresh_correct'

table_thresh <- rbind(table_age18, table_age70)
table_thresh$sample <- "Full Sample"

table_age18 <- svyby(~age18_correct, by = ~edu_cat, design = svydesign(id = ~psu, weights = ~weight, data = filter(svy_data, age < 30))
, FUN = svymean)
table_age18$Variable <- "Correctly Stated\nAge 18"
names(table_age18)[names(table_age18) == "age18_correct"] <- 'thresh_correct'
table_age70 <- svyby(~age70_correct, by = ~edu_cat, design = svydesign(id = ~psu, weights = ~weight, data = filter(svy_data, age < 30)), FUN = svymean)
table_age70$Variable <- "Correctly Stated\nAge 70"
names(table_age70)[names(table_age70) == "age70_correct"] <- 'thresh_correct'

table_thresh_young <- rbind(table_age18, table_age70)
table_thresh_young$sample <- "Under 30"

table_age18 <- svyby(~age18_correct, by = ~edu_cat, design = svydesign(id = ~psu, weights = ~weight, data = filter(svy_data, age > 60))
                     , FUN = svymean)
table_age18$Variable <- "Correctly Stated\nAge 18"
names(table_age18)[names(table_age18) == "age18_correct"] <- 'thresh_correct'
table_age70 <- svyby(~age70_correct, by = ~edu_cat, design = svydesign(id = ~psu, weights = ~weight, data = filter(svy_data, age > 60)), FUN = svymean)
table_age70$Variable <- "Correctly Stated\nAge 70"
names(table_age70)[names(table_age70) == "age70_correct"] <- 'thresh_correct'

table_thresh_old <- rbind(table_age18, table_age70)
table_thresh_old$sample <- "Over 60"

ggplot(rbind(table_thresh, table_thresh_young, table_thresh_old), aes(y= thresh_correct, x = edu_cat)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymax = thresh_correct + (1.96 * se), ymin = thresh_correct - (1.96 * se))) +
  facet_grid(sample~Variable) +
  coord_flip() +
  theme_bw() +
  xlab('') +
  ylab('Proportion Correct') 

##
# > sessionInfo()
# R version 3.2.2 (2015-08-14)
# Platform: x86_64-apple-darwin14.5.0 (64-bit)
# Running under: OS X 10.11.2 (El Capitan)
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] foreign_0.8-66     rdd_0.56           Formula_1.2-1      AER_1.2-4          survival_2.38-3    car_2.1-0          multiwayvcov_1.2.2
# [8] rdrobust_0.80      lmtest_0.9-34      zoo_1.7-12         sandwich_2.3-4     ggthemes_2.2.1     stargazer_5.2      survey_3.30-3     
# [15] ggplot2_1.0.1      dplyr_0.4.3        mgcv_1.8-9         nlme_3.1-122       lubridate_1.5.0   
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.2        nloptr_1.0.4       compiler_3.2.2     plyr_1.8.3         tools_3.2.2        boot_1.3-17        digest_0.6.8      
# [8] lme4_1.1-10        gtable_0.1.2       lattice_0.20-33    Matrix_1.2-3       DBI_0.3.1          parallel_3.2.2     SparseM_1.7       
# [15] proto_0.3-10       stringr_1.0.0      MatrixModels_0.4-1 nnet_7.3-11        R6_2.1.1           minqa_1.2.4        reshape2_1.4.1    
# [22] magrittr_1.5       splines_3.2.2      scales_0.3.0       MASS_7.3-45        assertthat_0.1     pbkrtest_0.4-2     colorspace_1.2-6  
# [29] quantreg_5.19      stringi_1.0-1      munsell_0.4.2     
# Data analysis condensed script for 'Premature mortality and timing of your life: An exploratory correlational study'
# Mona Joly and colleagues
# 11/03/21

rm(list=ls())

### Loading required packages ####

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(tidylog)){ 
  install.packages("tidylog")
  library(tidylog)
}
if(!require(rmarkdown)){
  install.packages("rmarkdown")
  library(rmarkdown)
}
if(!require(multilevel)){
  install.packages("multilevel")
  library(multilevel)
}                           # for sobel test
if(!require(bda)){
  install.packages("bda")
  library(bda)
}                           # another way to calculate sobel test
# if(!require(lme4)) install.packages("lme4") # for mixed-effects models
if(!require(sjlabelled)){
  install.packages("sjlabelled")
  library(sjlabelled)
}                           # all the sj**** : for graphical representation
if(!require(sjmisc)) {
  install.packages("sjmisc")
  library(sjmisc)
}
if(!require(sjstats)){
  install.packages("sjstats")
  library(sjstats)
}
if(!require(ggeffects)){
  install.packages("ggeffects")
  library(ggeffects)
}
if(!require(sjPlot)) {
  install.packages("sjPlot")
  library(sjPlot)
}
if(!require(effects)){
  install.packages("effects")
  library(effects)
}



### Loading the data ####

# setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona
# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

d <- read.table("data_famhist.txt",dec=",",sep="\t",header=TRUE)        # Read final data

lapply(d, class)
cols.num <- c("look_after_health_log","look_after_health_sqrt","personal_income","controllability","closeness")
d[cols.num] <- lapply(d[cols.num],as.numeric)
sapply(d[cols.num],class)
cols.factor <- c("checkup","breastfeed_length","YPLL_dummy","patience_score_bi")
d[cols.factor] <- lapply(d[cols.factor],as.factor)
sapply(d[cols.factor],class)

################################
######## MAIN ANALYSIS #########
################################

### Creation of additional dataframes for the YPLL_sum analyses ####

# all the YPLL_sum analyses are done on a sub-sample excluding participants who reported one of their relative died before the age of 20 (noted as 19) as that seems unreliable
d3 <- d[-c(which(d$parent1_age_2 == 19 | d$parent2_age_2 == 19 | d$gp1_age_2 == 19 | d$gp2_age_2 == 19 | d$gp3_age_2 == 19 | d$gp4_age_2 == 19)),]

cor.test(d3$age,d3$YPLL_sum)
plot(d3$age,d3$YPLL_sum) 
# visually, participants younger than 30 yo might have a lower YPLL_sum

d5 <- d3%>% filter(age >= 30)
cor.test(d5$age,d5$YPLL_sum) 
# the correlation if quite small for this subsample

d5b <- d3%>% filter(age >= 40)
d5c <- d3%>% filter(age >= 50)
cor.test(d5b$age,d5b$YPLL_sum)
cor.test(d5c$age,d5c$YPLL_sum)
# actually the correlation increase when we restrict for 40+ or 50+ so might as well restrict to 30+ to increase power

d6 <- d3 %>% filter (age >=40 & age <= 60)
cor.test(d6$age,d6$YPLL_sum)
plot(d6$age,d6$YPLL_sum)
# interesting case study to see what happens to the relationship between YPLL_sum and the outcome variables when YPLL_sum does not vary with age

#####################################
######## Looking after health #######
#####################################
### Choosing the right transformations of the variables based on hist of residuals ####

hist(d3$look_after_health) # right-skewed
hist(d3$YPLL_sum) # highly left-skewed

# basic model
lm1 <- glm(look_after_health ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3) 
hist(lm1$residuals) # without any transformation of the variables, the distribution of the residuals is right-skewed
plot(lm1)
plot(lm1$residuals ~ lm1$fitted.values) #shotgun pattern

# with look_after_health_sqrt = sqrt(max(x+1)-x)
lm2 <- glm(look_after_health_sqrt ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm2$residuals) # looks much better
plot(lm2)
plot(lm2$residuals ~ lm2$fitted.values) # much more evenly distributed
# 1/x, log(x), sqrt(x), log(K-x) do not bring any improvements, I'll skip those here do stay concise

# with log(YPLL_sum+1)
lm3 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d3)
hist(lm3$residuals) # looks better than lm4, but not super significantly. 
plot(lm3)
plot(lm3$residuals ~ lm3$fitted.values)

### MODELS ####
# So far, I used the sqrt transformation for effort in looking after health and the log transformation for YPLL_sum

# model on the whole (reliable) sample
lm_health1 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_health1)
plot_model(lm_health1, type="est",show.values = TRUE)
plot_model(lm_health1, type="pred",terms="YPLL_sum")
# It would seem the higher the number of years of life lost in the close family, the more participants look after their health (contrary to predictions), but the effect is not significant

# model on 30+
lm_health2 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d5)
summary(lm_health2) # SES subj loses its significance whereas personal income becomes significant, the estimate and p-values for YPLL_sum remain comparable
plot_model(lm_health2, type="est",show.values = TRUE)

# model on 40-60 yo
lm_health3 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d6)
summary(lm_health3) # nothing remains significant, the estimate for YPLL_sum remain comparable, less significant
plot_model(lm_health3, type="est",show.values = TRUE)

##############################
####### PATIENCE SCORE #######
##############################

# Falk's 2016 future discounting measure translates into a patience score : a higher patience score means a lower time discounting

# model on the whole (reliable) sample
lm_discounting1 <- glm(patience_score ~ scale(log(YPLL_sum+1)) + gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_discounting1) # nothing remains significant, the estimate for YPLL_sum remain comparable, less significant
plot_model(lm_discounting1, type="est",show.values = TRUE)
plot_model(lm_discounting1, type="pred",terms="YPLL_sum")
# It would seem the higher the number of years of life lost in the close family, the more participants discount the future, but the effect is not significant

# model on 30+
lm_discounting1b <- glm(patience_score ~ scale(log(YPLL_sum+1)) + gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d5)
summary(lm_discounting1b) # the effect of YPLL_sum is closer to a null effect, SES_subj becomes slightly stronger
plot_model(lm_discounting1b, type="est",show.values = TRUE)

# model on 40-60 yo
lm_discounting1c <- glm(patience_score ~ scale(log(YPLL_sum+1)) + gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d6)
summary(lm_discounting1c) # only the effect of subj SES remains. The estimate for YPLL_sum is closer to what it is on the 1st model, slightly stronger, but still not significant
plot_model(lm_discounting1c, type="est",show.values = TRUE)

### As I saw graphically there might be a gender interaction, I added an interaction with gender
# model on the whole (reliable) sample
lm_discounting2a <- glm(patience_score ~ scale(log(YPLL_sum+1))*gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_discounting2a) # for women, YPLL_sum has a significant effect on time discounting
plot_model(lm_discounting2a, type="est",show.values = TRUE)

# model on 30+
lm_discounting2b <- glm(patience_score ~ scale(log(YPLL_sum+1))*gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d5)
summary(lm_discounting2b) # the effect vanishes on the 30+ subsample
plot_model(lm_discounting2b, type="est",show.values = TRUE)

# model on 40-60 yo
lm_discounting2c <- glm(patience_score ~ scale(log(YPLL_sum+1))*gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d6)
summary(lm_discounting2c) # same
plot_model(lm_discounting2c, type="est",show.values = TRUE)


#################################
###### Secondary analyses #######
#################################

####### Effect of controllability and closeness of the age of death ####
### On effort in looking after health ####
### CONTROL ###
lm_health_control <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability), data=d3)
summary(lm_health_control)
# a higher controllability means "they could have done more to look after their health"
# The more people feel their dead relatives were responsible for their death, the more they look after their health
# The more people feel their relatives' death was uncontrollable, the less they look after their health

lm_health_control2 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1))*scale(controllability) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_health_control2)
# There is no interaction between controllability and YPLL_sum

plot_model(lm_health_control, type="est",show.values=TRUE)
plot_model(lm_health_control, type="pred",terms="controllability") 
plot(d3$controllability,d3$look_after_health)

### CLOSENESS ###
lm_health_control_close <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability) + scale(closeness), data=d3)
summary(lm_health_control_close) # no impact of closeness to the relative

lm_health_control_close2 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1))*scale(closeness) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability), data=d3)
summary(lm_health_control_close2) # no interaction between closeness and YPLL_sum

### On temporal discounting ####
### CONTROL ###
lm_patience_control <- glm(patience_score ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability), data=d3)
summary(lm_patience_control) # no impact of controllability on future discounting

lm_patience_control2 <- glm(patience_score ~ scale(log(YPLL_sum+1))*scale(controllability) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_patience_control2) # no interaction between controllability and YPLL_sum

### CLOSENESS ###
lm_patience_control_close <- glm(patience_score ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability) + scale(closeness), data=d3)
summary(lm_patience_control_close) # significant impact of closeness!
# The more the participants felt close to their relatives who died, the less patient they are --> the more they discount the future

lm_patience_control_close2 <- glm(patience_score ~ scale(log(YPLL_sum+1))*scale(closeness) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability), data=d3)
summary(lm_patience_control_close2) # but no interaction between closeness and YPLL_sum




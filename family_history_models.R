# Data analysis script for 'Premature mortality and timing of your life: An exploratory correlational study'
# Mona Joly and colleagues
# 25/05/21

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

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(robustbase)){
  install.packages("robustbase")
  library(robustbase)
}

if(!require(sandwich)){
  install.packages("sandwich")
  library(sandwich)
}

if(!require(lmtest)){
  install.packages("lmtest")
  library(lmtest)
}

if(!require(modelr)){
  install.packages("modelr")
  library(modelr)
}

if(!require(broom)){
  install.packages("broom")
  library(broom)
}

# library(e1071)      # to calculate skewness
# library(dlookr)     # to transform data
# library(ggpubr)     # for density plots
# library(car)        # for qqPlots
# library(lme4)

### Loading the data ####

# setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona

# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

# setwd("C:/Users/monaj/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

d <- read.table("data_famhist.txt",dec=",",sep="\t",header=TRUE)        # Read final data

lapply(d, class)
cols.num <- c("time_discounting","look_after_health_log","look_after_health_sqrt","personal_income","controllability","closeness")
d[cols.num] <- lapply(d[cols.num],as.numeric)
sapply(d[cols.num],class)
cols.factor <- c("gender","checkup","breastfeed_length","YPLL_dummy","patience_score_bi")
d[cols.factor] <- lapply(d[cols.factor],as.factor)
sapply(d[cols.factor],class)

d3b <- d %>% filter(YPLL_sum < mean(YPLL_sum, na.rm = TRUE) + 3*sd(YPLL_sum,na.rm=TRUE)) 
d3 <- d[-c(which(d$parent1_age_2 == 19 | d$parent2_age_2 == 19 | d$gp1_age_2 == 19 | d$gp2_age_2 == 19 | d$gp3_age_2 == 19 | d$gp4_age_2 == 19)),]
d4 <- d3%>% filter(attention_4 ==4) 
d5 <- d3%>% filter(age >= 30)
d6 <- d3 %>% filter (age >=40 & age <= 60)

cor.test(d3$age,d3$YPLL_sum)
plot(d3$age,d3$YPLL_sum)
cor.test(d5$age,d5$YPLL_sum)
cor.test(d6$age,d6$YPLL_sum)
plot(d6$age,d6$YPLL_sum)

fig1 = ggplot(d5, aes(x=age, y=YPLL_sum)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("age") + 
  ylab("YPLL sum")
fig1

################################
######## MAIN ANALYSIS #########
################################
######## Looking after health #######
#####################################
### Choosing the right transformations of the variables based on hist of residuals ####

hist(d3$look_after_health)
hist(d3$YPLL_sum)
plot(lm1)

lm1 <- glm(look_after_health ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm1$residuals) # right-skewed

lm2 <- glm(log(look_after_health+1) ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm2$residuals) # even more right-skewed

lm3 <- glm(sqrt(look_after_health) ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm3$residuals) # still worse than lm1

lm4 <- glm(look_after_health_sqrt ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm4$residuals) # looks really great (with sqrt(max(x+1)-x))

lm5 <- glm(look_after_health_log ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm5$residuals) # looks maybe worse than lm1, becomes left-skewed

# --> the best look_after_health transformation if look_after_health_sqrt (lm4)

lm6 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d3)
hist(lm6$residuals) # looks better than lm4, but not super significantly. 
summary(lm6)
summary(lm4)

lm7 <- glm(look_after_health_sqrt ~ scale(sqrt(YPLL_sum)) + age + gender + ethnicity,data=d3)
hist(lm7$residuals) # looks comparable to lm6, so might as well opt for log
plot(lm7)

plot(lm4)
plot(lm6)
plot(lm4$residuals ~ lm4$fitted.values) #shotgun pattern
plot(lm6$residuals ~ lm6$fitted.values) # slightly better, but not very obvious

lm8 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d4) # without the ones who didn't reply 4 at the attention task
plot(lm8)
hist(lm8$residuals)
summary(lm8)

### Models ####

lm_health1 <- glm(look_after_health ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
summary(lm_health1)

# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              -5.243788   0.382191 -13.720  < 2e-16 ***
# scale(log(YPLL_sum + 1))  0.070090   0.092878   0.755 0.450931    
# age                       0.022587   0.005797   3.897 0.000115 ***
# genderMale               -0.134530   0.174952  -0.769 0.442405    
# ethnicityBlack            0.251017   0.620747   0.404 0.686164    
# ethnicityMixed           -0.186106   0.598144  -0.311 0.755867    
# ethnicityOther           -0.396589   0.620975  -0.639 0.523437    
# ethnicityWhite           -0.799417   0.324802  -2.461 0.014295 *
# AIC: 1505

# using d or non-logged variables doesn't change much.

bptest(lm_health1) # Well apparently we cannot reject the null that the variance of
# the residuals is constant. So that's good.

coeftest(lm_health1, vcov = vcovHC(lm_health1))

lm_health1b <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d5)
summary(lm_health1b) # exactly the same

lm_health1c <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d6)
summary(lm_health1c) # the effect of age loses its significance but the effect of YPLL_sum remains comparable

plot_model(lm_health1, type="est", show.values = TRUE)
plot_model(lm_health1, type="pred",terms=c("YPLL_sum","age")) # interesting to see the effect is the same at all ages

lm_health_dummy1 <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + YPLL_dummy + age + gender,data=d3)
summary(lm_health_dummy1) # pretty similar

lm_health2 <- glm(look_after_health ~ scale(YPLL_sum) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj), data=d3)
summary(lm_health2)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -5.137900   0.378931 -13.559  < 2e-16 ***
# scale(log(YPLL_sum + 1))     0.090512   0.093462   0.968 0.333454    
# age                          0.019512   0.005783   3.374 0.000818 ***
# genderMale                  -0.198155   0.173558  -1.142 0.254304    
# scale(log(personal_income))  0.100580   0.095084   1.058 0.290827    
# scale(SES_subj)              0.262150   0.093806   2.795 0.005465 ** 
# AIC = 1497

bptest(lm_health2) # Well apparently we cannot reject the null that the variance of
                   # the residuals is constant. So that's good.

coeftest(lm_health2, vcov = vcovHC(lm_health2))

lm_health2b <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj), data=d5)
summary(lm_health2b) # diminishes the effect of SES_subj and makes personal income significant, doesn't change YPLL_sum at all

lm_health2c <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj), data=d6)
summary(lm_health2c) # removes the effect of subj SES, doesn't change YPLL sum

plot_model(lm_health2, type="est",show.values=TRUE)
plot_model(lm_health2, type="pred",terms="YPLL_sum") 

# The older ppl are, the more they look after their health: for one additional year, they take 0.01sd more care of their health
# More affluent people take better care of their health (0.1 sd more)

lm_health3 <- glm(look_after_health ~ scale(YPLL_sum) + age + gender + scale(personal_income) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_health3)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -5.034968   0.380635 -13.228  < 2e-16 ***
# scale(log(YPLL_sum + 1))     0.096841   0.093114   1.040  0.29900    
# age                          0.016940   0.005893   2.875  0.00428 ** 
# genderMale                  -0.284763   0.177902  -1.601  0.11030    
# ethnicityBlack               0.206960   0.610173   0.339  0.73466    
# ethnicityMixed              -0.116422   0.589781  -0.197  0.84362    
# ethnicityOther              -0.328595   0.613700  -0.535  0.59267    
# ethnicityWhite              -0.668593   0.322017  -2.076  0.03856 *  
# scale(log(personal_income))  0.084969   0.094983   0.895  0.37159    
# scale(SES_subj)              0.231541   0.094590   2.448  0.01483 *  
# scale(stress)               -0.191629   0.093433  -2.051  0.04097 * 
# AIC = 1494

bptest(lm_health3) # p=.11

coeftest(lm_health3, vcov = vcovHC(lm_health3))

lm_health3b <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d5)
summary(lm_health3b) # SES subj loses its significance whereas personal income becomes significant, no other change

lm_health3c <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d6)
summary(lm_health3c) # nothing is significant, YPLL sum very comparable

plot_model(lm_health3, type="est",show.values = TRUE)
plot_model(lm_health3, type="pred",terms="YPLL_sum")

fig1 = ggplot(d3, aes(x=YPLL_sum, y=look_after_health)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("YPLL sum") + 
  ylab("Effort in looking after health")
fig1

fig2 = ggplot(d3, aes(x=SES_subj, y=look_after_health)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("Subjective SES") + 
  ylab("Effort in looking after health")
fig2

mediation.test(d$SES_subj,d$stress,d$look_after_health)
#              Sobel     Aroian    Goodman
# z.value -2.603378382 -2.57404833 -2.633734447
# p.value  0.009231001  0.01005162  0.008445151
# Stress mediates the relationship between looking after health and subjective SES  

lm_health4 <- glm(look_after_health ~ scale(YPLL_sum) + age + gender + scale(personal_income) + scale(SES_subj) + scale(stress)+ scale(extrinsic_risk), data=d3)
summary(lm_health4)

bptest(lm_health4) # p<.001

coeftest(lm_health4, vcov = vcovHC(lm_health4))

lm_health4b <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income)) + scale(SES_subj) + scale(stress)+ scale(log(extrinsic_risk+1)), data=d5)
summary(lm_health4b) # subj SES loses its significance, no other change

lm_health4c <- glm(look_after_health_sqrt ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income)) + scale(SES_subj) + scale(stress)+ scale(log(extrinsic_risk+1)), data=d6)
summary(lm_health4c) # everything loses its significance

plot_model(lm_health4, type="est",show.values = TRUE)

mediation.test(d$extrinsic_risk,d$YPLL_sum,d$look_after_health)

##############################
####### PATIENCE SCORE #######
##############################

fig1 = ggplot(d3, aes(x=YPLL_sum, y=time_discounting)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap(~gender) +
  xlab("YPLL sum") + 
  ylab("Time discounting")
fig1


lm1 <- glm(patience_score ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
hist(lm1$residuals)
lm2 <- glm(patience_score ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d3)
hist(lm2$residuals)
lm3 <- glm(log(patience_score) ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d3)
hist(lm3$residuals)
lm4 <- glm(sqrt(patience_score) ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d3)
hist(lm4$residuals)
lm5 <- glm(1/(patience_score) ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d3)
hist(lm5$residuals)
# Nothing looks really good... Might as well keep lm2



lm_discounting1 <- glm(time_discounting ~ scale(YPLL_sum) + age + gender + ethnicity,data=d3)
summary(lm_discounting1)
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              -0.285069   0.221640  -1.286  0.19917    
# scale(log(YPLL_sum + 1)) -0.070396   0.053861  -1.307  0.19202    
# age                       0.011517   0.003362   3.426  0.00068 ***
# genderMale                0.066023   0.101458   0.651  0.51561    

bptest(lm_discounting1) # p=0.2
coeftest(lm_discounting1, vcov = vcovHC(lm_discounting1))

lm_discounting1b <- glm(time_discounting ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d5)
summary(lm_discounting1b) # no change

lm_discounting1c <- glm(time_discounting ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity,data=d6)
summary(lm_discounting1c) # no change (except for the effect of age which vanishes)

lm_discounting2 <- glm(time_discounting ~ scale(YPLL_sum)*gender + age + ethnicity,data=d3)
summary(lm_discounting2)
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         -0.284530   0.220835  -1.288 0.198392    
# scale(log(YPLL_sum + 1))            -0.160678   0.071132  -2.259 0.024466 *  
# genderMale                           0.064831   0.101091   0.641 0.521713    
# age                                  0.011581   0.003349   3.457 0.000608 ***
# scale(log(YPLL_sum + 1)):genderMale  0.196327   0.101526   1.934 0.053896 .  

# When we add it the interaction with gender, YPLL_sum becomes significant.
# (It's stronger with d)

bptest(lm_discounting2) # p<0.05
coeftest(lm_discounting2, vcov = vcovHC(lm_discounting2))

lm_discounting2b <- glm(time_discounting ~ scale(log(YPLL_sum+1))*gender + age + ethnicity,data=d5)
summary(lm_discounting2b) # we lose the effect for women and the interaction with gender

lm_discounting2c <- glm(time_discounting ~ scale(log(YPLL_sum+1))*gender + age + ethnicity,data=d6)
summary(lm_discounting2c) # all the effects disappear

plot_model(lm_discounting2, type="est")
plot_model(lm_discounting2, type="pred",terms=c("YPLL_sum","gender")) 

lm_discounting4 <- glm(time_discounting ~ scale(YPLL_sum) + age + gender + ethnicity + scale(personal_income) + scale(SES_subj),data=d3)
summary(lm_discounting4)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                 -0.251868   0.221484  -1.137  0.25619   
# scale(log(YPLL_sum + 1))    -0.055108   0.054628  -1.009  0.31373   
# age                          0.010293   0.003380   3.046  0.00249 **
# genderMale                   0.042301   0.101444   0.417  0.67693   
# scale(log(personal_income))  0.005063   0.055576   0.091  0.92746   
# scale(SES_subj)              0.129449   0.054829   2.361  0.01874 * 

# Positive effect of SES on patience (conversely on delay discounting). Yay

bptest(lm_discounting4) # p=0.35
coeftest(lm_discounting4, vcov = vcovHC(lm_discounting4))

lm_discounting4b <- glm(time_discounting ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj),data=d5)
summary(lm_discounting4b) #stronger effect of subj SES

lm_discounting4c <- glm(time_discounting ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj),data=d6)
summary(lm_discounting4c) # only the effect of subj SES remains

lm_discounting5 <- glm(time_discounting ~ scale(YPLL_sum)*gender + age + ethnicity + scale(personal_income) + scale(SES_subj),data=d3)
summary(lm_discounting5)
#                                     Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                         -0.248058   0.220680  -1.124  0.26171   
# scale(log(YPLL_sum + 1))            -0.147141   0.072253  -2.036  0.04241 * 
# genderMale                           0.040153   0.101078   0.397  0.69141   
# age                                  0.010332   0.003368   3.068  0.00231 **
# ethnicityBlack                      -0.558665   0.357385  -1.563  0.11885   
# ethnicityMixed                      -0.150946   0.344578  -0.438  0.66160   
# ethnicityOther                      -0.352947   0.358030  -0.986  0.32487   
# ethnicityWhite                      -0.236478   0.187662  -1.260  0.20841   
# scale(log(personal_income))          0.015260   0.055622   0.274  0.78397   
# scale(SES_subj)                      0.124896   0.054679   2.284  0.02292 * 
# scale(log(YPLL_sum + 1)):genderMale  0.196332   0.101373   1.937  0.05354 . 

# Same, but with the Holm-Bonferroni correction the effect will no longer be significant

bptest(lm_discounting5) # p=0.09
coeftest(lm_discounting5, vcov = vcovHC(lm_discounting5))

lm_discounting5b <- glm(time_discounting ~ scale(log(YPLL_sum+1))*gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj),data=d5)
summary(lm_discounting5b) # again, stronger effect of subj SES but no effect for women or interaction with gender

#plot_model(lm_discounting5, type="est",show.values = TRUE)
#plot_model(lm_discounting5, type="pred",terms=c("YPLL_sum","gender")) 

lm_discounting6 <- glm(time_discounting ~ scale(YPLL_sum) + gender + age + ethnicity + scale(personal_income) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_discounting6)
# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 12.95210    2.33241   5.553 5.23e-08 ***
# scale(log(YPLL_sum + 1))    -0.80815    0.56729  -1.425 0.155090    
# genderMale                   0.97165    1.07831   0.901 0.368106    
# age                          0.12295    0.03595   3.420 0.000694 ***
# ethnicityBlack              -6.03660    3.75091  -1.609 0.108354    
# ethnicityMixed              -1.61387    3.62207  -0.446 0.656161    
# ethnicityOther              -4.05553    3.76776  -1.076 0.282430    
# ethnicityWhite              -2.65269    1.92544  -1.378 0.169092    
# scale(log(personal_income))  0.15372    0.57803   0.266 0.790434    
# scale(SES_subj)              1.49236    0.57017   2.617 0.009208 ** 
# scale(stress)                0.94698    0.57375   1.651 0.099652 . 

bptest(lm_discounting6) # p=0.09
coeftest(lm_discounting6, vcov = vcovHC(lm_discounting6))

lm_discounting6b <- glm(time_discounting ~ scale(log(YPLL_sum+1)) + gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d5)
summary(lm_discounting6b) # removes the effect of stress, SES_subj becomes slightly stronger

lm_discounting6c <- glm(time_discounting ~ scale(log(YPLL_sum+1)) + gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d6)
summary(lm_discounting6c) # only the effect of subj SES remains

plot_model(lm_discounting6, type="est",show.values = TRUE)
plot_model(lm_discounting6, type="pred",terms="YPLL_sum") 

lm_discounting7 <- glm(time_discounting ~ scale(YPLL_sum)*gender + age + ethnicity + scale(personal_income) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_discounting7)
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         12.90262    2.32451   5.551 5.31e-08 ***
# scale(log(YPLL_sum + 1))            -1.77785    0.75867  -2.343 0.019618 *  
# genderMale                           0.92056    1.07493   0.856 0.392312    
# age                                  0.12292    0.03583   3.431 0.000667 ***
# ethnicityBlack                      -5.46407    3.74991  -1.457 0.145898    
# ethnicityMixed                      -1.54480    3.60977  -0.428 0.668928    
# ethnicityOther                      -3.79583    3.75722  -1.010 0.312998    
# ethnicityWhite                      -2.64578    1.91881  -1.379 0.168738    
# scale(log(personal_income))          0.26933    0.57919   0.465 0.642191    
# scale(SES_subj)                      1.40821    0.56990   2.471 0.013906 *  
# scale(stress)                        0.86678    0.57330   1.512 0.131376    
# scale(log(YPLL_sum + 1)):genderMale  2.01989    1.05387   1.917 0.056022 .  

bptest(lm_discounting7) # p=0.3
coeftest(lm_discounting7, vcov = vcovHC(lm_discounting7))

plot_model(lm_discounting7, type="est",show.values = TRUE)
plot_model(lm_discounting7, type="pred",terms=c("YPLL_sum","gender")) 

#d$gender <- relevel(d$gender,ref="Female")

#################################
###### Secondary analyses #######
#################################

######### Alternative ways to measure the effect of (premature) death exposure within the family on FOBs
#### The number of deaths within the family ####
  #### Its effect on looking after health ####
lm_health_n <- glm(look_after_health_sqrt ~ n_deaths + gender + age + ethnicity,data=d)
summary(lm_health_n)
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -5.435277   0.342591 -15.865  < 2e-16 ***
# n_deaths4 deaths       -0.376062   0.213710  -1.760  0.07901 .  
# n_deaths5-6 deaths     -0.678538   0.254469  -2.666  0.00789 ** 
# age                     0.036640   0.006862   5.339 1.36e-07 ***
# genderMale             -0.430736   0.146339  -2.943  0.00338 ** 
# ethnicityBlack          0.211939   0.442302   0.479  0.63200    
# ethnicityMixed         -0.384575   0.521071  -0.738  0.46080    
# ethnicityOther         -0.270621   0.566920  -0.477  0.63330    
# ethnicityWhite         -0.744234   0.279848  -2.659  0.00805 **
AIC(lm_health_n) #2232, not as good

plot_model(lm_health_n, type="est")
plot_model(lm_health_n, type="pred",terms="n_deaths")

lm_health_n2 <- glm(look_after_health_sqrt ~ n_deaths*age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj),data=d)
summary(lm_health_n2)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -5.337145   0.345466 -15.449  < 2e-16 ***
# n_deaths4 deaths            -0.377232   0.214277  -1.760  0.07888 .  
# n_deaths5-6 deaths          -0.662287   0.253616  -2.611  0.00926 ** 
# age                          0.034793   0.006866   5.068  5.5e-07 ***
# genderMale                  -0.459914   0.146224  -3.145  0.00175 ** 
# ethnicityBlack               0.170530   0.440831   0.387  0.69902    
# ethnicityMixed              -0.418895   0.520096  -0.805  0.42092    
# ethnicityOther              -0.338666   0.565508  -0.599  0.54950    
# ethnicityWhite              -0.747358   0.279616  -2.673  0.00774 ** 
# scale(log(personal_income))  0.062399   0.079813   0.782  0.43465    
# scale(SES_subj)              0.170473   0.078469   2.172  0.03024 *
AIC(lm_health_a) #2229

plot_model(lm_health_n2, type="est")
plot_model(lm_health_n2, type="pred",terms="n_deaths") 

lm_health_n3 <- glm(look_after_health_sqrt ~ n_deaths + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d)
summary(lm_health_n3)
plot_model(lm_health_n3, type="est", show.values=TRUE)
plot_model(lm_health_n3, type="pred",terms="n_deaths") 

  #### Its effect on patience score ####
lm_discounting_n <- glm(patience_score ~ n_deaths + age + gender + ethnicity,data=d)
summary(lm_discounting_n) # nothing

plot_model(lm_discounting_n, type="pred",terms="n_deaths")

lm_discounting_n2 <- glm(patience_score ~ n_deaths + age + gender + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress),data=d)
summary(lm_discounting_n2) # only subjSES remains

plot_model(lm_discounting_n2, type="est",show.values = T)
plot_model(lm_discounting_n2, type="pred",terms="n_deaths")

d$gender <- relevel(d$gender,ref="Female")

lm_discounting_n3 <- glm(time_discounting ~ n_deaths*gender + age + scale(log(personal_income+1)) + scale(SES_subj)+ scale(stress),data=d)
summary(lm_discounting_n3) # marginal effect of having more than 4 deaths in the family, bc of women with 5-6 deaths with a strong future discounting
# count(d %>% filter(gender=="Male" & n_deaths=="5-6 deaths")) # 118 participants, not that few
count(d %>% filter(gender=="Female" & n_deaths=="5-6 deaths")) # 137 participants

plot_model(lm_discounting_n3, type="est",show.values=T)
plot_model(lm_discounting_n3, type="pred",terms=c("n_deaths","gender")) # super fun: men with lots of deaths discount the future less, women discount it more

#### The number of premature deaths within the family ####
  #### Its effect on looking after health ####

lm_health_np1 <- glm(look_after_health_sqrt ~ n_prem + age + gender + ethnicity,data=d3)
summary(lm_health_np1) #nothing

plot_model(lm_health_np1, type="est")
plot_model(lm_health_np1, type="pred",terms="n_prem")

lm_health_np2 <- glm(look_after_health_sqrt ~ n_prem + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj),data=d3)
summary(lm_health_np2) # SES toujours vaillant

plot_model(lm_health_np2, type="est")

lm_health_np3 <- glm(look_after_health_sqrt ~ n_prem *age + gender + scale(log(personal_income)) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_health_np3) # stress tambien

plot_model(lm_health_np3, type="est", show.values = TRUE)
plot_model(lm_health_np3, type="pred",terms="n_prem")

plot(d$n_deaths,d$age)

  #### Its effect on patience score ####

lm_discounting_np <- glm(patience_score ~ n_prem + age + gender + ethnicity,data=d3)
summary(lm_discounting_np) # nothing

lm_discounting_np2 <- lm(patience_score ~ n_prem*gender + age + ethnicity,data=d3)
summary(lm_discounting_np2) # marginal effect of having more than 4 prem_deaths for women. Significant with d.

plot_model(lm_discounting_np2, type="est")
plot_model(lm_discounting_np2, type="pred",terms=c("n_prem","gender")) 
# super fun: no effect for men, but strong effect for women of n of prem_deaths

lm_discounting_np3 <- lm(patience_score ~ n_prem*gender + age + ethnicity + scale(log(personal_income)) + scale(SES_subj),data=d3)
summary(lm_discounting_np3) # idem
plot_model(lm_discounting_np3, type="pred",terms=c("n_prem","gender")) 

lm_discounting_np4 <- lm(patience_score ~ n_prem*gender + age + scale(log(personal_income)) + scale(SES_subj) + stress,data=d3)
summary(lm_discounting_np4) # idem
plot_model(lm_discounting_np4, type="est",show.values = T)
plot_model(lm_discounting_np4, type="pred",terms=c("n_prem","gender"))

lm_discounting_np5 <- lm(patience_score ~ n_prem + gender + age + scale(log(personal_income)) + scale(SES_subj) + stress,data=d3)
summary(lm_discounting_np5) # idem
plot_model(lm_discounting_np5, type="est",show.values = T)
plot_model(lm_discounting_np5, type="pred",terms="n_prem")


#### The youngest death experienced ####
  #### Its effect on looking after health ####
lm_health_youngest <- lmrob(look_after_health ~ youngest_death + gender + age + SES_subj + personal_income + stress + extrinsic_risk,data=d)
summary(lm_health_youngest)

plot_model(lm_health_youngest, type="est", show.values = TRUE)
plot_model(lm_health_youngest, type="pred",terms="youngest_death") # interesting to see the effect is the same at all ages

fig1 = ggplot(d, aes(x=youngest_death, y=look_after_health)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("Youngest death experienced") + 
  ylab("Looking after health")
fig1

mediation.test(d$extrinsic_risk,d$youngest_death,d$look_after_health) #nope

  #### Its effect on time discounting ####
lm_discounting_youngest <- lmrob(time_discounting ~ youngest_death + gender + age + SES_subj + personal_income + stress + extrinsic_risk,data=d)
summary(lm_discounting_youngest)

plot_model(lm_discounting_youngest, type="est", show.values = TRUE)
plot_model(lm_discounting_youngest, type="pred",terms="youngest_death") # interesting to see the effect is the same at all ages

mediation.test(d$extrinsic_risk,d$youngest_death,d$time_discounting) #nope

d$gender <- relevel(d$gender,ref="Male")

lm_discounting_youngest_IA <- lmrob(time_discounting ~ youngest_death*gender + age + ethnicity + SES_subj + personal_income + stress + extrinsic_risk,data=d)
summary(lm_discounting_youngest_IA)

plot_model(lm_discounting_youngest_IA, type="est", show.values = TRUE)
plot_model(lm_discounting_youngest_IA, type="pred",terms="youngest_death") # interesting to see the effect is the same at all ages


######### POTENTIAL MEDIATORS ############
  ##### Does PEMR mediate the relationship between SES and looking after health? ####

lm_SES_hb <-glm(look_after_health_sqrt ~ SES_subj + age + gender + ethnicity, data=d)
summary(lm_SES_hb)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.493676   0.227282  -2.172  0.03027 *  
# SES_subj        0.066955   0.025642   2.611  0.00927 ** 
# age             0.012088   0.002622   4.610    5e-06 ***
# genderMale     -0.251758   0.081753  -3.079  0.00218 ** 
# ethnicityBlack  0.128965   0.247189   0.522  0.60207    
# ethnicityMixed -0.230787   0.290806  -0.794  0.42776    
# ethnicityOther -0.348307   0.306907  -1.135  0.25691    
# ethnicityWhite -0.358928   0.157269  -2.282  0.02285 * 
# The subjective SES has a significant effect on looking after health, but the effect is very small:
# For any increase in the 10-point social ladder, they take 0.07 sd better care of their health
# Funny, there is a gender effect, but it disappears once YPLL_sum is added to the model

lm_SES_extrinsic <- glm(extrinsic_risk_sqrt ~ SES_subj + age + gender + ethnicity, data=d)
summary(lm_SES_extrinsic)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.445850   0.231734   1.924 0.054870 .  
# SES_subj       -0.096448   0.026145  -3.689 0.000247 ***
# age            -0.005304   0.002673  -1.984 0.047744 *  
# genderMale      0.056608   0.083355   0.679 0.497346    
# ethnicityBlack  0.078794   0.252031   0.313 0.754673    
# ethnicityMixed  0.539461   0.296502   1.819 0.069388 .  
# ethnicityOther  0.584956   0.312919   1.869 0.062101 .  
# ethnicityWhite  0.326012   0.160349   2.033 0.042515 * 

lm_extrinsic_hb <- glm(look_after_health_sqrt~scale(extrinsic_risk_sqrt) + age + gender + ethnicity,data=d)
summary(lm_extrinsic_hb)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.157582   0.182296  -0.864  0.38773    
# scale(extrinsic_risk_sqrt)  -0.188483   0.040638  -4.638 4.39e-06 ***
# age                          0.011555   0.002589   4.463 9.80e-06 ***
# genderMale                  -0.232688   0.080602  -2.887  0.00404 ** 
# ethnicityBlack               0.148734   0.244016   0.610  0.54242    
# ethnicityMixed              -0.109815     0.287617  -0.382  0.70275    
# ethnicityOther              -0.225754     0.303753  -0.743  0.45767    
# ethnicityWhite              -0.308754     0.155782  -1.982  0.04798 *  

lm_SES_hb <-glm(look_after_health_sqrt ~ scale(SES_subj),data=d)
summary(lm_SES_hb)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -5.23049    0.32397 -16.145  < 2e-16 ***
# scale(SES_subj)  0.22006    0.07233   3.042  0.00245 ** 
# age              0.02152    0.00462   4.658 3.96e-06 ***
# genderMale      -0.46158    0.14425  -3.200  0.00145 ** 
# ethnicityBlack   0.22261    0.43832   0.508  0.61174    
# ethnicityMixed  -0.35167    0.49361  -0.712  0.47649    
# ethnicityOther  -0.58642    0.54644  -1.073  0.28365    
# ethnicityWhite  -0.64125    0.27279  -2.351  0.01907 *

lm_SES_extrinsic_hb <-glm(look_after_health_sqrt ~ scale(SES_subj) + scale(log(extrinsic_risk+1)),data=d)
summary(lm_SES_extrinsic_hb) 
# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    -5.236487   0.319448 -16.392  < 2e-16 ***
# scale(SES_subj)                 0.184519   0.071827   2.569  0.01045 *  
# scale(log(extrinsic_risk + 1)) -0.300996   0.072291  -4.164 3.61e-05 ***
# age                             0.019523   0.004581   4.262 2.37e-05 ***
# genderMale                     -0.448823   0.142266  -3.155  0.00169 ** 

## The SES effect remains once extrinsic mortality risk is added to the model, but it is smaller

plot_model(lm_SES_extrinsic_hb, type="est", show.values = TRUE)
plot_model(lm_SES_extrinsic_hb, type="pred",terms=c("extrinsic_risk","SES_subj")) 
# plot_model(lm_SES_extrinsic_hb, type="slope")

sobel(d$SES_subj,d$extrinsic_risk_sqrt,d$look_after_health_x3)
mediation.test(d$extrinsic_risk,d$SES_subj,d$look_after_health)
#               Sobel      Aroian     Goodman
# z.value 3.6561556399 3.6276544437 3.6853393390
# p.value 0.0002560259 0.0002860077 0.0002283981

# PEMR is a mediator between subjective SES and looking after health (p<.001, z=3,66). Yay!

  #### Does PEMR mediate the relationship between YPLL_sum and time discounting? ####

lm_discounting9 <- glm(patience_score ~ scale(log(extrinsic_risk+1)) + scale(SES_subj),data=d)
summary(lm_discounting9)
# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     16.1237     0.4380  36.814  < 2e-16 ***
# scale(log(extrinsic_risk + 1))  -0.5563     0.4417  -1.259    0.208    
# scale(SES_subj)                  1.7997     0.4417   4.074 5.26e-05 ***

plot_model(lm_discounting9, type="est", show.values = TRUE)

  #### Does financial time discounting mediate the relationship between YPLL_sum and health effort? ####
mediation.test(d$patience_score,d$YPLL_sum,d$look_after_health) # pas du tout.
mediation.test(d$patience_score,d$n_prem,d$look_after_health) # niet
mediation.test(d$patience_score,d$n_deaths,d$look_after_health) # nicht

  #### And with YPLL_sum as another covariate

lm_extrinsic_hb_YPLL <- glm(look_after_health_sqrt ~ scale(log(extrinsic_risk+1)) + scale(SES_subj) + scale(log(YPLL_sum+1)),data=d)
summary(lm_extrinsic_hb_YPLL)
# very significant

lm_discounting10 <- glm(patience_score ~ scale(log(extrinsic_risk+1)) + scale(SES_subj) + scale(log(YPLL_sum+1)),data=d)
summary(lm_discounting10)
#nothing


##### Effects of familial death exposure on exploratory DV
  ######## Age at first child ##########

lm_1st_child1 <- glm(age_first_child ~ scale(log(YPLL_sum+1)) + gender + age,data=d3)
summary(lm_1st_child1)
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              26.673204   1.826972  14.600   <2e-16 ***
# scale(log(YPLL_sum + 1)) -0.308266   0.491999  -0.627   0.5316    
# genderMale                1.943620   0.869834   2.234   0.0265 *  
# age                      -0.002029   0.033282  -0.061   0.9515 

fig5 = ggplot(d3, aes(x=YPLL_sum, y=age_first_child)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("YPLL sum") + 
  ylab("Age first child")
fig5 # Again, interacts with gender

lm_1st_child2 <- glm(age_first_child ~ scale(log(YPLL_sum+1))* gender + age,data=d3)
summary(lm_1st_child2)
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         26.890192   1.868311  14.393   <2e-16 ***
# scale(log(YPLL_sum + 1))            -0.429829   0.653524  -0.658   0.5115    
# genderMale                           1.603196   0.921859   1.739   0.0836 .  
# age                                 -0.001693   0.034289  -0.049   0.9607    
# scale(log(YPLL_sum + 1)):genderMale  1.304761   1.034734   1.261   0.2088 
# Devient pas significatif pour autant.

# plot_model(lm_1st_child2, type="pred",terms=c("YPLL_sum","gender")) # super fun: men with lots of deaths discount the future less, women discount it more


lm_1st_child3 <- glm(age_first_child ~ scale(log(YPLL_sum+1)) + gender + age + scale(log(personal_income+1)) + scale(SES_subj),data=d3)
summary(lm_1st_child3)
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     27.040530   1.874642  14.424   <2e-16 ***
# scale(log(YPLL_sum + 1))         0.165429   0.514527   0.322   0.7482    
# genderMale                       1.620554   0.903851   1.793   0.0745 .  
# age                             -0.005992   0.034257  -0.175   0.8613    
# scale(log(personal_income + 1)) -0.138960   0.563691  -0.247   0.8055    
# scale(SES_subj)                  0.960222   0.502527   1.911   0.0575 . 

lm_1st_child4 <- glm(age_first_child ~ scale(log(YPLL_sum+1))*gender + age + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_1st_child4)

# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     26.59446    1.92147  13.841   <2e-16 ***
# scale(log(YPLL_sum + 1))         0.14366    0.51480   0.279   0.7805    
# genderMale                       1.83927    0.92720   1.984   0.0487 *  
# age                              0.00160    0.03500   0.046   0.9636    
# scale(log(personal_income + 1)) -0.07775    0.56653  -0.137   0.8910    
# scale(SES_subj)                  1.01464    0.50504   2.009   0.0459 *  
# scale(stress)                    0.50435    0.47926   1.052   0.2939 

plot_model(lm_1st_child4, type="pred",terms=c("YPLL_sum","gender"))

lm_1st_child5 <- glm(age_first_child ~ n_deaths*gender + age + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress),data=d)
summary(lm_1st_child5)

plot_model(lm_1st_child5, type="pred",terms=c("n_deaths","gender")) 

lm_1st_child6 <- glm(age_first_child ~ n_prem*gender + age + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress),data=d3)
summary(lm_1st_child6)
plot_model(lm_1st_child6, type="pred",terms=c("n_prem","gender")) 

lm_1st_child7 <- lmrob(age_first_child ~ youngest_death + gender + age + personal_income + SES_subj + stress + extrinsic_risk,data=d)
summary(lm_1st_child7)
plot_model(lm_1st_child7, type="pred",terms="youngest_death") 
plot(d$age_first_child,d$youngest_death)

  ######## Ideal age at first child ########

lm_ideal_child1 <- glm(ideal_age ~ scale(log(YPLL_sum+1)) + age + gender, data=d3)
summary(lm_ideal_child1)
# nothing

lm_ideal_child2 <- glm(ideal_age ~scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income+1)) + scale(SES_subj), data=d3)
summary(lm_ideal_child2)
# nothing

lm_ideal_child3 <- glm(ideal_age ~scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_ideal_child3)
# small positive effect of stress lol
plot_model(lm_ideal_child3, type="pred",terms="YPLL_sum") 

lm_ideal_child4 <- glm(ideal_age ~n_deaths + age + gender + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d)
summary(lm_ideal_child4) # nothing
plot_model(lm_ideal_child4, type="pred",terms="n_deaths") 

lm_ideal_child5 <- glm(ideal_age ~n_prem + age + gender + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_ideal_child5) # nothing
plot_model(lm_ideal_child5, type="pred",terms="n_prem") 

lm_ideal_child6 <- lmrob(ideal_age ~ youngest_death + gender + age + personal_income + SES_subj + stress + extrinsic_risk,data=d)
summary(lm_ideal_child6)
plot_model(lm_ideal_child6, type="pred",terms="youngest_death") 
plot(d$ideal_age,d$youngest_death)

  ######## Green behaviour ####

lm_env <- glm(environment_1 ~ scale(log(YPLL_sum+1)) + age + gender, data=d3)
summary(lm_env) # nothing

lm_env2 <- glm(environment_1 ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income+1)) + scale(SES_subj), data=d3)
summary(lm_env2) # nothing

lm_env3 <- glm(environment_1 ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_env3) # nothing

lm_env_tpt <-glm(env_transport ~scale(log(YPLL_sum+1)) + age + gender, data=d3)
summary(lm_env_tpt) # Lol there's something but it's in the wrong direction

# fig6 = ggplot(d3, aes(x=YPLL_sum, y=env_transport)) + 
#   theme_bw() + 
#   geom_point() + 
#   geom_smooth(method="lm") +
#   #facet_wrap(~gender) +
#   xlab("YPLL sum") + 
#   ylab("Effort to look after the environment in transport choices")
# #fig6

#plot_model(lm_env_tpt, type="pred",terms="YPLL_sum") 

lm_env_tpt2 <-glm(env_transport ~scale(log(YPLL_sum+1)) + age + gender+ scale(log(personal_income+1)) + scale(SES_subj), data=d3)
summary(lm_env_tpt2) # small, but resisting

lm_env_tpt3 <-glm(env_transport ~scale(log(YPLL_sum+1)) + age + gender+ scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_env_tpt3) # same

plot_model(lm_env_tpt3, type="pred",terms="YPLL_sum")
plot_model(lm_env_tpt3, type="est",show.values=T)

lm_env_tpt4 <-glm(env_transport ~n_deaths + age + gender+ scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d)
summary(lm_env_tpt4) # marginal for 5-6 deaths

lm_env_tpt5 <-glm(env_transport ~n_prem + age + gender+ scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_env_tpt5) # strong effect

plot_model(lm_env_tpt5, type="pred",terms="n_prem")
plot_model(lm_env_tpt5, type="est",show.values=T)

  ######## Smoker status ####
d$smoker <- as.factor(d$smoker)
lm_smoker <- glm(smoker ~scale(log(YPLL_sum+1)) + age + gender, data=d3, family = binomial)
summary(lm_smoker) # Hallelujah dos
fig7 = ggplot(d3, aes(x=YPLL_sum, y=smoker)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("YPLL sum") +
  ylab("Smoker status")
fig7

plot_model(lm_smoker, type = "pred", terms = "YPLL_sum")

lm_smoker2 <- glm(smoker ~scale(log(YPLL_sum+1)) + age + gender+ scale(log(personal_income+1)) + scale(SES_subj), data=d3, family = binomial)
summary(lm_smoker2) # Hallelujah it's still there

lm_smoker3 <- glm(smoker ~scale(log(YPLL_sum+1)) + age + gender+ scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3, family = binomial)
summary(lm_smoker3) # et en plus y a pas le stress, ça me gusta

plot_model(lm_smoker3, type="est", show.values=T)
plot_model(lm_smoker3, type = "pred", terms = "YPLL_sum")

lm_smoker4 <- glm(smoker ~n_deaths + age + gender, data=d, family = binomial)
summary(lm_smoker4) # nicht mehr there

lm_smoker5 <- glm(smoker ~n_prem + age + gender, data=d3, family = binomial)
summary(lm_smoker5) # ah c'est reparti

lm_smoker6 <- glm(smoker ~n_prem + age + gender+ scale(log(personal_income+1)) + scale(SES_subj), data=d3, family = binomial)
summary(lm_smoker6) # Hallelujah it's still there

lm_smoker7 <- glm(smoker ~n_prem + age + gender+ scale(log(personal_income+1)) + scale(SES_subj) + scale(stress), data=d3, family = binomial)
summary(lm_smoker7) # et en plus y a pas le stress, ça me gusta

plot_model(lm_smoker7, type="est",show.values=T)
plot_model(lm_smoker7, type = "pred", terms = "n_prem")

lm_smoker8 <- glm(smoker ~ youngest_death + gender + age + personal_income + SES_subj + stress, data =d, family = binomial)
summary(lm_smoker8)

bptest(lm_smoker8) # p<0.01
coeftest(lm_smoker8, vcov = vcovHC(lm_smoker8))
plot_model(lm_smoker8, type = "pred", terms = "youngest_death [all]")

### Check-up #### et breastfeed length : à refaire en ordinal logistic regression --> compliqué
# polr_checkup <- polr(checkup ~scale(log(YPLL_sum+1))+gender + age, data=d3)
# summary(polr_checkup) # nein
# 
# polr_checkup <- polr(checkup ~n_prem+gender + age, data=d)
# summary(polr_checkup) # nein
# 
  ######## Breastfeed length ####
# d3 <- d %>% filter(!is.na(breastfeed_length))
# summary(d3$breastfeed_length)
# lm_breastfeed <- glm(breastfeed_length ~ scale(YPLL_sqrt) + age + ethnicity, data=d3)
# summary(lm_breastfeed) # no effect


####### Effects of controllability and closeness to the deceased relative on main outcome variables ####
  ##### On effort in looking after health ####
lm_health_control <- glm(look_after_health ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability), data=d3)
summary(lm_health_control)
# a higher controllability means "they could have done more to look after their health"
# The more people feel their dead relatives were responsible for their death, the more they look after their health
# The more people feel their relatives' death was uncontrollable, the less they look after their health

lm_health_control2 <- glm(look_after_health ~ scale(log(YPLL_sum+1))*controllability + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress), data=d3)
summary(lm_health_control2)

plot_model(lm_health_control, type="est",show.values=TRUE)
plot_model(lm_health_control_close, type="pred",terms="controllability") 
plot_model(lm_health_control, type="pred",terms="YPLL_sum") 
  plot(d3$controllability,d3$look_after_health)
hist(d3$controllability)
d3$n_deaths

lm_health_control_close <- glm(look_after_health ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability) + scale(closeness), data=d3)
summary(lm_health_control_close) # no impact of closeness to the relative
plot_model(lm_health_control_close, type="est",show.values=TRUE)

  ##### On temporal discounting ####
lm_patience_control <- glm(patience_score ~ scale(log(YPLL_sum+1)) + age + gender + ethnicity + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability), data=d3)
summary(lm_patience_control) # no impact on future discounting mrmrm

lm_patience_control_close <- glm(patience_score ~ scale(log(YPLL_sum+1)) + age + gender + scale(log(personal_income)) + scale(SES_subj) + scale(stress) + scale(controllability) + scale(closeness), data=d3)
summary(lm_patience_control_close) # significant impact of closeness!
# The more the participants felt close to their relatives who died, the less patient they are --> the more they discount the future

plot_model(lm_patience_control_close, type="est",show.values=TRUE)
plot_model(lm_patience_control_close, type="pred",terms="closeness") 
plot_model(lm_patience_control_close, type="pred",terms="YPLL_sum") 


###### Effects of the raw ages at death of the deceased relatives on the main outcome variables #####

lm_he_ages <- glm(look_after_health ~ parent1_age_2 + parent2_age_2 + gp1_age_2 + gp2_age_2 + gp3_age_2 + gp4_age_2, data =d)
summary(lm_he_ages)
plot_model(lm_he_ages, type="pred",terms="gp3_age_2") 

lm_td_ages <- glm(time_discounting ~ parent1_age_2 + parent2_age_2 + gp1_age_2 + gp2_age_2 + gp3_age_2 + gp4_age_2, data =d3)
summary(lm_td_ages)
plot_model(lm_td_ages, type="pred",terms="parent1_age_2") 

fig8 = ggplot(d3, aes(x=parent1_age_2, y=time_discounting)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method="lm") +
  #facet_wrap(~gender) +
  xlab("age at death of parent 1") +
  ylab("Time discounting")
fig8

cor.test(d$SES_subj,d$YPLL_sum)
cor.test(d$extrinsic_risk,d$YPLL_sum)
cor.test(d$extrinsic_risk,d$youngest_death)

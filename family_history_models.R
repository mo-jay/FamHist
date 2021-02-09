# Data analysis script for 'Premature mortality and timing of your life: An exploratory correlational study'
# Mona Joly and colleagues
# 09/02/21

rm(list=ls())
#install.packages("tidyverse")
#install.packages("tidylog")
#install.packages("rmarkdown")
# install.packages("multilevel")
# install.packages("bda")
library(tidyverse)
library(tidylog)
library(rmarkdown)
# library(e1071)      # to calculate skewness
# library(dlookr)     # to transform data
# library(ggpubr)     # for density plots
# library(car)        # for qqPlots
library(multilevel) # for sobel test
library(bda)        # another way to calculate mediation

# setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona
# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

d <- read.table("data_famhist.txt",dec=",",sep="\t",header=TRUE)        # Read final data

######## Looking after health #######

  ## Checking distribution of look_after_health var
hist(d$look_after_health)
skewness(d$look_after_health) #-1.28 --> highly negatively skewed

d$look_after_health_x2 <- transform(
  d$look_after_health,
  method = "x^2")
hist(d$look_after_health_x2)
skewness(d$look_after_health_x2) #-0.38
ggdensity(d$look_after_health_x2)
qqPlot(d$look_after_health_x2)
d$look_after_health_x3 <- transform(
  d$look_after_health,
  method = "x^3")
hist(d$look_after_health_x3)
skewness(d$look_after_health_x3) #0.18 the best
ggdensity(d$look_after_health_x3)
qqPlot(d$look_after_health_x3) # also looks better graphically

  ## Checking distribution of YPLL_sum var
hist(d$YPLL_sum) 
skewness(d$YPLL_sum) #1.58 --> highly positively skewed
d$YPLL_sqrt <- transform(
  d$YPLL_sum,
  method = "sqrt"
)
skewness(d$YPLL_sqrt) #0.08, better
hist(d$YPLL_sqrt)
ggdensity(d$YPLL_sqrt) # could honestly look better but can't find how
qqPlot(d$YPLL_sqrt)

  ## Checking the age distribution
hist(d$age)   # uniform distribution, don't really know what to do about it
skewness(d$age) # 0.002, well at least it's symmetric
ggdensity(d$age) # quite terrible. Can't see how to improve a uniform distrib.

lm_health1 <- lm(scale(look_after_health_x3) ~ scale(YPLL_sqrt),data=d)
summary(lm_health1)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       4.304e-17  4.196e-02   0.000   1.0000  
# scale(YPLL_sqrt)  1.028e-01  4.200e-02   2.447   0.0147 *
AIC(lm_health1) #1597

lm_health2 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + age + gender + ethnicity,data=d)
summary(lm_health2)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.115061   0.187430  -0.614  0.53954    
# scale(YPLL_sqrt)             0.052754   0.042980   1.227  0.22018    
# age                        0.011754   0.002741   4.289 2.12e-05 ***
# genderMale                  -0.238766   0.082032  -2.911  0.00375 ** 
# ethnicityBlack  0.158012   0.249017   0.635  0.52599    
# ethnicityMixed -0.188880   0.292285  -0.646  0.51840    
# ethnicityOther -0.335652   0.308322  -1.089  0.27679    
# ethnicityWhite -0.362045   0.158227  -2.288  0.02251 *

d$personal_income <- as.numeric(d$personal_income)
hist(d$personal_income)
skewness(d$personal_income) #1.44
ggdensity(d$personal_income)

d$income_sqrt <- transform(
  d$personal_income,
  method = "sqrt"
)
skewness(d$income_sqrt) #0.4
ggdensity(d$income_sqrt)
qqPlot(d$income_sqrt) # Not bad

d$income_log <- transform(
  d$personal_income,
  method = "log"
)
skewness(d$income_log) #-0.52
ggdensity(d$income_log)
qqPlot(d$income_log) # hard to say which is best

skewness(d$SES_subj)
ggdensity(d$SES_subj)
qqPlot(d$SES_subj) # Not that bad? Transformations do not help anyway

lm_health3 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj, data=d)
summary(lm_health3)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.459702   0.240314  -1.913  0.05628 .  
# scale(YPLL_sqrt)             0.063921   0.043143   1.482  0.13902    
# age                        0.010823   0.002752   3.932 9.48e-05 ***
# genderMale                  -0.251831   0.081995  -3.071  0.00224 ** 
# ethnicityBlack  0.153225   0.248196   0.617  0.53726    
# ethnicityMixed -0.210696   0.291343  -0.723  0.46987    
# ethnicityOther -0.352581   0.307026  -1.148  0.25131    
# ethnicityWhite -0.345857   0.158418  -2.183  0.02944 *  
# scale(income_log)            0.008156   0.044593   0.183  0.85495    
# SES_subj                     0.069104   0.027456   2.517  0.01212 * 

# The older ppl are, the more they look after their health: for one additional year, they take 0.01sd better care of their health
# Man take 0.25 sd less care of their health
# White ppl take 0.35 sd less care of their health
# More affluent people take better care of their health (0.07 sd more for any increase in the 10-point social ladder)

lm_health4 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress, data=d)
summary(lm_health4)
# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  0.176850   0.293336   0.603 0.546828    
# scale(YPLL_sqrt)             0.064502   0.042656   1.512 0.131067    
# age                        0.008543   0.002790   3.062 0.002307 ** 
# genderMale                  -0.322386   0.083280  -3.871 0.000121 ***
# ethnicityBlack  0.068011   0.246469   0.276 0.782697    
# ethnicityMixed -0.163930   0.288327  -0.569 0.569890    
# ethnicityOther -0.271163   0.304352  -0.891 0.373344    
# ethnicityWhite -0.314388   0.156858  -2.004 0.045529 *  
# scale(income_log)            0.002713   0.044113   0.061 0.950988    
# SES_subj                     0.049622   0.027651   1.795 0.073269 .  
# stress                      -0.134927   0.036464  -3.700 0.000237 ***
# The more stressed people are, the less they take care of their health (0.13 sd less care for any increase in the 6-point stress scale)

  ##### Does extrinsic mortality risk mediate the relationship between SES and looking after health?

lm_SES_hb <-glm(scale(look_after_health_x3) ~ SES_subj + age + gender + ethnicity, data=d)
summary(lm_SES_hb)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.493676   0.227282  -2.172  0.03027 *  
# SES_subj                     0.066955   0.025642   2.611  0.00927 ** 
# age                        0.012088   0.002622   4.610    5e-06 ***
# genderMale                  -0.251758   0.081753  -3.079  0.00218 ** 
# ethnicityBlack  0.128965   0.247189   0.522  0.60207    
# ethnicityMixed -0.230787   0.290806  -0.794  0.42776    
# ethnicityOther -0.348307   0.306907  -1.135  0.25691    
# ethnicityWhite -0.358928   0.157269  -2.282  0.02285 * 
# The subjective SES has a significant effect on looking after health, but the effect is very small:
# For any increase in the 10-point social ladder, they take 0.07 sd better care of their health

hist(d$extrinsic_risk)
skewness(sqrt(d$extrinsic_risk))
ggdensity(sqrt(d$extrinsic_risk))
qqPlot(sqrt(d$extrinsic_risk))
d$extrinsic_risk_sqrt <- transform(
  d$extrinsic_risk,
  method = "sqrt"
)

lm_SES_extrinsic <- glm(scale(extrinsic_risk_sqrt) ~ SES_subj + age + gender + ethnicity, data=d)
summary(lm_SES_extrinsic)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  0.445850   0.231734   1.924 0.054870 .  
# SES_subj                    -0.096448   0.026145  -3.689 0.000247 ***
# age                       -0.005304   0.002673  -1.984 0.047744 *  
# genderMale                   0.056608   0.083355   0.679 0.497346    
# ethnicityBlack  0.078794   0.252031   0.313 0.754673    
# ethnicityMixed  0.539461   0.296502   1.819 0.069388 .  
# ethnicityOther  0.584956   0.312919   1.869 0.062101 .  
# ethnicityWhite  0.326012   0.160349   2.033 0.042515 * 

lm_extrinsic_hb <- glm(scale(look_after_health_x3)~scale(extrinsic_risk_sqrt) + age + gender + ethnicity,data=d)
summary(lm_extrinsic_hb)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.157582   0.182296  -0.864  0.38773    
# scale(extrinsic_risk_sqrt)  -0.188483   0.040638  -4.638 4.39e-06 ***
# age                        0.011555   0.002589   4.463 9.80e-06 ***
# genderMale                  -0.232688   0.080602  -2.887  0.00404 ** 
# ethnicityBlack  0.148734   0.244016   0.610  0.54242    
# ethnicityMixed -0.109815   0.287617  -0.382  0.70275    
# ethnicityOther -0.225754   0.303753  -0.743  0.45767    
# ethnicityWhite -0.308754   0.155782  -1.982  0.04798 *  

lm_SES_hb <-glm(scale(look_after_health_x3) ~ SES_subj + age + gender+ ethnicity,data=d)
summary(lm_SES_hb)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.493676   0.227282  -2.172  0.03027 *  
# SES_subj                     0.066955   0.025642   2.611  0.00927 ** 
# age                        0.012088   0.002622   4.610    5e-06 ***
# genderMale                  -0.251758   0.081753  -3.079  0.00218 ** 
# ethnicityBlack  0.128965   0.247189   0.522  0.60207    
# ethnicityMixed -0.230787   0.290806  -0.794  0.42776    
# ethnicityOther -0.348307   0.306907  -1.135  0.25691    
# ethnicityWhite -0.358928   0.157269  -2.282  0.02285 *

lm_SES_extrinsic_hb <-glm(scale(look_after_health_x3) ~ SES_subj + scale(extrinsic_risk_sqrt) + age + gender + ethnicity,data=d)
summary(lm_SES_extrinsic_hb) 
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -0.415180   0.224538  -1.849   0.0650 .  
# SES_subj                     0.049975   0.025557   1.955   0.0510 .  
# scale(extrinsic_risk_sqrt)  -0.176060   0.041030  -4.291 2.10e-05 ***
# age                        0.011154   0.002591   4.305 1.97e-05 ***
# genderMale                  -0.241792   0.080532  -3.002   0.0028 ** 
# ethnicityBlack  0.142838   0.243415   0.587   0.5576    
# ethnicityMixed -0.135809   0.287195  -0.473   0.6365    
# ethnicityOther -0.245319   0.303147  -0.809   0.4187    
# ethnicityWhite -0.301530   0.155430  -1.940   0.0529 .

## The SES effect on looking after health loses its significance once extrinsic mortality risk is added to the model --> completely mediated?

sobel(d$SES_subj,d$extrinsic_risk_sqrt,d$look_after_health_x3)
mediation.test(d$extrinsic_risk_sqrt,d$SES_subj,d$look_after_health_x3)
#               Sobel      Aroian     Goodman
# z.value 2.999783276 2.961187716 3.039928416
# p.value 0.002701718 0.003064551 0.002366344

# PEMR is a mediator between subjective SES and looking after health (p<.01, z=3). Yay!

####### PATIENCE SCORE ######
hist(d$patience_score) # Not good, but again don't see how to normalise an uniform distrib

lm_discounting1 <- lm(scale(patience_score) ~ scale(YPLL_sqrt),data=d)
summary(lm_discounting1)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)       1.856e-16  4.218e-02   0.000    1.000
# scale(YPLL_sqrt) -9.000e-03  4.222e-02  -0.213    0.831

# The nullest of the null results

lm_discounting2 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity,data=d)
summary(lm_discounting2)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 -0.450813   0.189922  -2.374 0.017952 *  
# scale(YPLL_sqrt)            -0.063894   0.043551  -1.467 0.142913    
# age                        0.010422   0.002777   3.753 0.000193 ***
# genderMale                   0.172466   0.083123   2.075 0.038463 *  
# ethnicityBlack -0.554994   0.252328  -2.199 0.028255 *  
# ethnicityMixed -0.103921   0.296171  -0.351 0.725811    
# ethnicityOther  0.026894   0.312421   0.086 0.931433    
# ethnicityWhite -0.099965   0.160331  -0.623 0.533216 

# Well, the hypothesis is confirmed, at a p-value of .15. .30 with the Holm-Bonferroni correction.

lm_discounting3 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj,data=d)
summary(lm_discounting3)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 -0.812781   0.242121  -3.357 0.000842 ***
# scale(YPLL_sqrt)            -0.053487   0.043468  -1.230 0.219035    
# age                        0.009030   0.002773   3.256 0.001197 ** 
# genderMale                   0.148692   0.082611   1.800 0.072421 .  
# ethnicityBlack -0.576615   0.250061  -2.306 0.021486 *  
# ethnicityMixed -0.116084   0.293533  -0.395 0.692647    
# ethnicityOther  0.016034   0.309334   0.052 0.958680    
# ethnicityWhite -0.096346   0.159608  -0.604 0.546330    
# scale(income_log)            0.058002   0.044928   1.291 0.197239    
# SES_subj                     0.078944   0.027662   2.854 0.004481 **

# Only effect that remains is the positive effect of SES on patience (conversely on delay discounting)

lm_discounting4 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress,data=d)
summary(lm_discounting4)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 -0.872775   0.299158  -2.917  0.00367 **
# scale(YPLL_sqrt)            -0.053541   0.043503  -1.231  0.21894   
# age                        0.009245   0.002845   3.249  0.00123 **
# genderMale                   0.155342   0.084933   1.829  0.06794 . 
# ethnicityBlack -0.568584   0.251361  -2.262  0.02408 * 
# ethnicityMixed -0.120492   0.294051  -0.410  0.68214   
# ethnicityOther  0.008360   0.310394   0.027  0.97852   
# ethnicityWhite -0.099312   0.159972  -0.621  0.53498   
# scale(income_log)            0.058515   0.044989   1.301  0.19392   
# SES_subj                     0.080781   0.028200   2.865  0.00434 **
# stress                       0.012717   0.037187   0.342  0.73251 

lm_discounting5 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress + extrinsic_risk_sqrt,data=d)
summary(lm_discounting5)
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 -0.807563   0.307792  -2.624  0.00894 **
# scale(YPLL_sqrt)            -0.050267   0.043661  -1.151  0.25011   
# age                        0.009067   0.002853   3.179  0.00156 **
# genderMale                   0.160295   0.085124   1.883  0.06022 . 
# ethnicityBlack -0.560712   0.251554  -2.229  0.02622 * 
# ethnicityMixed -0.101064   0.294885  -0.343  0.73194   
# ethnicityOther  0.027334   0.311155   0.088  0.93003   
# ethnicityWhite -0.086512   0.160624  -0.539  0.59038   
# scale(income_log)            0.057114   0.045023   1.269  0.20514   
# SES_subj                     0.078284   0.028340   2.762  0.00593 **
# stress                       0.017473   0.037564   0.465  0.64201   
# extrinsic_risk_sqrt         -0.017760   0.019658  -0.903  0.36668 

### Even the extrinsic risk effect on delay discounting is absent!

########### Age at first child ##########

lm_1st_child1 <- glm(d2$age_first_child ~ d2$YPLL_sum)
summary(lm_1st_child1)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 27.979714   0.541063   51.71   <2e-16 ***
# d2$YPLL_sum -0.008565   0.011121   -0.77    0.442

lm_1st_child2 <- glm(d2$age_first_child ~ d2$YPLL_sum + d2$age + d2$gender + d2$ethnicity)
summary(lm_1st_child2)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               23.87482    2.06838  11.543  < 2e-16 ***
# d2$YPLL_sum                              -0.01037    0.01107  -0.937 0.349602    
# d2$age                                  0.01639    0.02903   0.565 0.572781    
# d2$gender                                 2.59062    0.71857   3.605 0.000364 ***
# d2$ethnicityBlack            1.75483    2.46426   0.712 0.476942    
# d2$ethnicityCONSENT REVOKED  6.09764    6.44500   0.946 0.344847    
# d2$ethnicityMixed           -3.01310    2.66094  -1.132 0.258383    
# d2$ethnicityOther            5.67592    2.94088   1.930 0.054537 .  
# d2$ethnicityWhite            2.38235    1.50171   1.586 0.113681

lm_1st_child3 <- glm(d2$age_first_child ~ d2$YPLL_sum + d2$age + d2$gender + d2$ethnicity + d2$personal_income + d2$SES_subj)
summary(lm_1st_child3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.962e+01  2.389e+00   8.215 6.27e-15 ***
# d2$YPLL_sum                              -4.847e-03  1.101e-02  -0.440 0.659985    
# d2$age                                  1.412e-02  2.866e-02   0.493 0.622529    
# d2$gender                                 2.428e+00  7.145e-01   3.398 0.000770 ***
# d2$ethnicityBlack            1.754e+00  2.424e+00   0.724 0.469854    
# d2$ethnicityCONSENT REVOKED  6.477e+00  6.350e+00   1.020 0.308564    
# d2$ethnicityMixed           -3.377e+00  2.621e+00  -1.288 0.198609    
# d2$ethnicityOther            5.792e+00  2.898e+00   1.998 0.046577 *  
# d2$ethnicityWhite            2.811e+00  1.490e+00   1.886 0.060279 .  
# d2$personal_income                       -5.412e-05  4.191e-05  -1.291 0.197539    
# d2$SES_subj                               8.406e-01  2.409e-01   3.489 0.000557 ***

lm_1st_child4 <- glm(d2$age_first_child ~ d2$YPLL_sum + d2$age + d2$gender + d2$ethnicity + d2$personal_income + d2$SES_subj + d2$stress)
summary(lm_1st_child4)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.637e+01  2.856e+00   5.730 2.44e-08 ***
# d2$YPLL_sum                              -4.782e-03  1.095e-02  -0.437 0.662550    
# d2$age                                  2.788e-02  2.929e-02   0.952 0.341882    
# d2$gender                                 2.833e+00  7.376e-01   3.841 0.000149 ***
# d2$ethnicityBlack            1.958e+00  2.413e+00   0.811 0.417820    
# d2$ethnicityCONSENT REVOKED  6.352e+00  6.317e+00   1.006 0.315445    
# d2$ethnicityMixed           -3.666e+00  2.611e+00  -1.404 0.161311    
# d2$ethnicityOther            5.621e+00  2.884e+00   1.949 0.052226 .  
# d2$ethnicityWhite            2.719e+00  1.483e+00   1.833 0.067728 .  
# d2$personal_income                       -5.316e-05  4.169e-05  -1.275 0.203249    
# d2$SES_subj                               9.155e-01  2.424e-01   3.777 0.000191 ***
# d2$stress                                 6.588e-01  3.208e-01   2.054 0.040857 *


############## Ideal age at first child

lm_ideal_child1 <- glm(d2$ideal_age ~ d2$YPLL_sum)
summary(lm_1st_child1)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   27.979714   0.541063   51.71   <2e-16 ***
# d2$YPLL_sum   -0.008565   0.011121   -0.77    0.442 

lm_ideal_child2 <- glm(d2$ideal_age ~ d2$YPLL_sum + d2$age + d2$gender + d2$ethnicity)
summary(lm_1st_child2)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               23.87482    2.06838  11.543  < 2e-16 ***
# d2$YPLL_sum                              -0.01037    0.01107  -0.937 0.349602    
# d2$age                                  0.01639    0.02903   0.565 0.572781    
# d2$gender                                 2.59062    0.71857   3.605 0.000364 ***
# d2$ethnicityBlack            1.75483    2.46426   0.712 0.476942    
# d2$ethnicityCONSENT REVOKED  6.09764    6.44500   0.946 0.344847    
# d2$ethnicityMixed           -3.01310    2.66094  -1.132 0.258383    
# d2$ethnicityOther            5.67592    2.94088   1.930 0.054537 .  
# d2$ethnicityWhite            2.38235    1.50171   1.586 0.113681 

lm_ideal_child3 <- glm(d2$ideal_age ~ d2$YPLL_sum + d2$age + d2$gender + d2$ethnicity + d2$personal_income + d2$SES_subj)
summary(lm_1st_child3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.962e+01  2.389e+00   8.215 6.27e-15 ***
# d2$YPLL_sum                              -4.847e-03  1.101e-02  -0.440 0.659985    
# d2$age                                  1.412e-02  2.866e-02   0.493 0.622529    
# d2$gender                                 2.428e+00  7.145e-01   3.398 0.000770 ***
# d2$ethnicityBlack            1.754e+00  2.424e+00   0.724 0.469854    
# d2$ethnicityCONSENT REVOKED  6.477e+00  6.350e+00   1.020 0.308564    
# d2$ethnicityMixed           -3.377e+00  2.621e+00  -1.288 0.198609    
# d2$ethnicityOther            5.792e+00  2.898e+00   1.998 0.046577 *  
# d2$ethnicityWhite            2.811e+00  1.490e+00   1.886 0.060279 .  
# d2$personal_income                       -5.412e-05  4.191e-05  -1.291 0.197539    
# d2$SES_subj                               8.406e-01  2.409e-01   3.489 0.000557 ***

lm_ideal_child3 <- glm(d2$ideal_age ~ d2$YPLL_sum + d2$age + d2$gender + d2$ethnicity + d2$personal_income + d2$SES_subj + d2$stress)
summary(lm_1st_child3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.962e+01  2.389e+00   8.215 6.27e-15 ***
# d2$YPLL_sum                              -4.847e-03  1.101e-02  -0.440 0.659985    
# d2$age                                  1.412e-02  2.866e-02   0.493 0.622529    
# d2$gender                                 2.428e+00  7.145e-01   3.398 0.000770 ***
# d2$ethnicityBlack            1.754e+00  2.424e+00   0.724 0.469854    
# d2$ethnicityCONSENT REVOKED  6.477e+00  6.350e+00   1.020 0.308564    
# d2$ethnicityMixed           -3.377e+00  2.621e+00  -1.288 0.198609    
# d2$ethnicityOther            5.792e+00  2.898e+00   1.998 0.046577 *  
# d2$ethnicityWhite            2.811e+00  1.490e+00   1.886 0.060279 .  
# d2$personal_income                       -5.412e-05  4.191e-05  -1.291 0.197539    
# d2$SES_subj                               8.406e-01  2.409e-01   3.489 0.000557 *** 


############### Mediation of extrinsic risk

lm_health_extrinsic <- glm(d2$look_after_health ~ d2$extrinsic_risk)
summary(lm_health_extrinsic)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         50.0706     3.3372   15.00  < 2e-16 ***
# d2$extrinsic_risk   0.2872     0.0412    6.97 8.89e-12 ***

lm_extrinsic_YPLL <- glm(d2$extrinsic_risk ~ d2$YPLL_sum)
summary(lm_extrinsic_YPLL)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         79.53124    1.12107   70.94   <2e-16 ***
# d2$YPLL_sum         -0.02357    0.02455   -0.96    0.337 

lm_extrinsic_YPLL_health <- glm(d2$look_after_health ~ d2$YPLL_sum * d2$extrinsic_risk)
summary(lm_extrinsic_YPLL_health)
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   43.958746   4.821345   9.118  < 2e-16 ***
# d2$YPLL_sum                    0.169035   0.098362   1.719   0.0863 .  
# d2$extrinsic_risk              0.341527   0.059246   5.765 1.35e-08 ***
# d2$YPLL_sum:d2$extrinsic_risk -0.001438   0.001219  -1.180   0.2386    


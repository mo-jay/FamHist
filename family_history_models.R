# Data analysis script for 'Premature mortality and timing of your life: An exploratory correlational study'
# Mona Joly and colleagues
# 12/02/21

rm(list=ls())
#install.packages("tidyverse")
#install.packages("tidylog")
#install.packages("rmarkdown")
# install.packages("multilevel")
# install.packages("bda")
# install.packages("lme4")
library(tidyverse)
library(tidylog)
library(rmarkdown)
# library(e1071)      # to calculate skewness
# library(dlookr)     # to transform data
# library(ggpubr)     # for density plots
# library(car)        # for qqPlots
library(multilevel) # for sobel test
library(bda)        # another way to calculate mediation
# library(lme4)

# setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona
# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

d <- read.table("data_famhist.txt",dec=",",sep="\t",header=TRUE)        # Read final data

lapply(d, class)
cols.num <- c("look_after_health_x2","YPLL_sqrt","income_sqrt","income_log","extrinsic_risk_sqrt","age_child1_sqrt")
d[cols.num] <- sapply(d[cols.num],as.numeric)
d$patience_score_bi <- as.factor(d$patience_score_bi)
d$YPLL_dummy <- as.factor(d$YPLL_dummy)
sapply(d[cols.num],class)

################################
######## MAIN ANALYSIS #########
################################

#####################################
######## Looking after health #######
#####################################


lm_health1 <- lm(scale(look_after_health_x3) ~ scale(YPLL_sqrt),data=d)
summary(lm_health1)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.006242   0.049631   0.126    0.900  
# scale(YPLL_sqrt)  0.119114   0.049695   2.397    0.017 *
AIC(lm_health1) #1597 before, 1106 after the correction of the YPLL_sum var

lm_health_dummy1 <- lm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + YPLL_dummy,data=d)
summary(lm_health_dummy1)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -0.02483    0.16976  -0.146   0.8838  
# scale(YPLL_sqrt)   0.11054    0.06694   1.651   0.0995 .
# YPLL_dummy1        0.03634    0.18986   0.191   0.8483
# doesn't help
AIC(lm_health_dummy1) #1108 --> no improvement from the previous model

lm_health_a <- lm(scale(look_after_health_x3) ~ n_deaths,data=d)
summary(lm_health_a)
AIC(lm_health_a) #1588, not as good

lm_health2 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + age + gender + ethnicity,data=d)
summary(lm_health2)
# Coefficients before YPLL_sum improvement:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.115061   0.187430  -0.614  0.53954    
# scale(YPLL_sqrt)0.052754   0.042980   1.227  0.22018    
# age             0.011754   0.002741   4.289 2.12e-05 ***
# genderMale     -0.238766   0.082032  -2.911  0.00375 ** 
# ethnicityBlack  0.158012   0.249017   0.635  0.52599    
# ethnicityMixed -0.188880   0.292285  -0.646  0.51840    
# ethnicityOther -0.335652   0.308322  -1.089  0.27679    
# ethnicityWhite -0.362045   0.158227  -2.288  0.02251 *
# AIC: 1571 --> new: 1092

# Afterwards, the gender effect disappears and the white ethnicity effect becomes stronger

lm_health_dummy2 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + YPLL_dummy + age + gender + ethnicity,data=d)
summary(lm_health_dummy2)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.14368    0.25036  -0.574 0.566383    
# scale(YPLL_sqrt)  0.04925    0.06804   0.724 0.469562    
# YPLL_dummy1       0.02812    0.18869   0.149 0.881616    
# age               0.01230    0.00326   3.773 0.000187 ***
# genderMale       -0.08723    0.09795  -0.891 0.373686    
# ethnicityBlack    0.14347    0.35290   0.407 0.684565    
# ethnicityMixed   -0.11127    0.33922  -0.328 0.743070    
# ethnicityOther   -0.25651    0.35044  -0.732 0.464641    
# ethnicityWhite   -0.43619    0.18231  -2.393 0.017212 * 
# AIC: 1094
# slightly diminishes the effect of YPLL_sum, so definitely doesn't help. Plus increases the AIC.

lm_health3 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj, data=d)
summary(lm_health3)
# Coefficients before YPLL_sum improvement:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.459702   0.240314  -1.913  0.05628 .  
# scale(YPLL_sqrt)  0.063921   0.043143   1.482  0.13902    
# age               0.010823   0.002752   3.932 9.48e-05 ***
# genderMale       -0.251831   0.081995  -3.071  0.00224 ** 
# ethnicityBlack    0.153225   0.248196   0.617  0.53726    
# ethnicityMixed   -0.210696   0.291343  -0.723  0.46987    
# ethnicityOther   -0.352581   0.307026  -1.148  0.25131    
# ethnicityWhite   -0.345857   0.158418  -2.183  0.02944 *  
# scale(income_log) 0.008156   0.044593   0.183  0.85495    
# SES_subj          0.069104   0.027456   2.517  0.01212 * 
# AIC = 1567 ; now 1087

# The older ppl are, the more they look after their health: for one additional year, they take 0.01sd better care of their health
#The gender effect disappears after YPLL_sum var improvement
# White ppl take 0.40 sd less care of their health (stronger than before)
# More affluent people take better care of their health (0.08 sd more for any increase in the 10-point social ladder)

lm_health4 <- glm(scale(look_after_health_x3) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress, data=d)
summary(lm_health4)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.176850   0.293336   0.603 0.546828    
# scale(YPLL_sqrt)   0.064502   0.042656   1.512 0.131067    
# age                0.008543   0.002790   3.062 0.002307 ** 
# genderMale        -0.322386   0.083280  -3.871 0.000121 ***
# ethnicityBlack     0.068011   0.246469   0.276 0.782697    
# ethnicityMixed    -0.163930   0.288327  -0.569 0.569890    
# ethnicityOther    -0.271163   0.304352  -0.891 0.373344    
# ethnicityWhite    -0.314388   0.156858  -2.004 0.045529 *  
# scale(income_log)  0.002713   0.044113   0.061 0.950988    
# SES_subj           0.049622   0.027651   1.795 0.073269 .  
# stress            -0.134927   0.036464  -3.700 0.000237 ***

# With the YPLL_Sum correction, the gender effect loses its significance, 
# ethnicity unchanged, subjective SES effect remains (beta=.05, p<.05),
# the stress effect remains (beta = -.12, p<.01). AIC=1081

lm_health_n_premdeaths1 <- lm(scale(look_after_health_x3) ~ prem_sum,data=d)
summary(lm_health_n_premdeaths1)

lm_health_n_premdeaths2 <- lm(scale(look_after_health_x3) ~ prem_sum + age + gender + ethnicity,data=d)
summary(lm_health_n_premdeaths2)

lm_health_n_premdeaths3 <- lm(scale(look_after_health_x3) ~ prem_sum + age + gender + ethnicity + income_log + SES_subj,data=d)
summary(lm_health_n_premdeaths3)

lm_health_n_premdeaths4 <- lm(scale(look_after_health_x3) ~ prem_sum + age + gender + ethnicity + income_log + SES_subj + stress,data=d)
summary(lm_health_n_premdeaths4)

  ##### Does extrinsic mortality risk mediate the relationship between SES and looking after health? ####

lm_SES_hb <-glm(scale(look_after_health_x3) ~ SES_subj + age + gender + ethnicity, data=d)
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

lm_SES_extrinsic <- glm(scale(extrinsic_risk_sqrt) ~ SES_subj + age + gender + ethnicity, data=d)
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

lm_extrinsic_hb <- glm(scale(look_after_health_x3)~scale(extrinsic_risk_sqrt) + age + gender + ethnicity,data=d)
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

lm_SES_hb <-glm(scale(look_after_health_x3) ~ SES_subj + age + gender+ ethnicity,data=d)
summary(lm_SES_hb)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.493676   0.227282  -2.172  0.03027 *  
# SES_subj        0.066955   0.025642   2.611  0.00927 ** 
# age             0.012088   0.002622   4.610    5e-06 ***
# genderMale     -0.251758   0.081753  -3.079  0.00218 ** 
# ethnicityBlack  0.128965   0.247189   0.522  0.60207    
# ethnicityMixed -0.230787   0.290806  -0.794  0.42776    
# ethnicityOther -0.348307   0.306907  -1.135  0.25691    
# ethnicityWhite -0.358928   0.157269  -2.282  0.02285 *

lm_SES_extrinsic_hb <-glm(scale(look_after_health_x3) ~ SES_subj + scale(extrinsic_risk_sqrt) + age + gender + ethnicity,data=d)
summary(lm_SES_extrinsic_hb) 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               -0.415180   0.224538  -1.849   0.0650 .  
# SES_subj                   0.049975   0.025557   1.955   0.0510 .  
# scale(extrinsic_risk_sqrt)-0.176060   0.041030  -4.291 2.10e-05 ***
# age                        0.011154   0.002591   4.305 1.97e-05 ***
# genderMale                -0.241792   0.080532  -3.002   0.0028 ** 
# ethnicityBlack             0.142838   0.243415   0.587   0.5576    
# ethnicityMixed            -0.135809   0.287195  -0.473   0.6365    
# ethnicityOther            -0.245319   0.303147  -0.809   0.4187    
# ethnicityWhite            -0.301530   0.155430  -1.940   0.0529 .

## The SES effect on looking after health loses its significance once extrinsic mortality risk is added to the model --> completely mediated?

sobel(d$SES_subj,d$extrinsic_risk_sqrt,d$look_after_health_x3)
mediation.test(d$extrinsic_risk_sqrt,d$SES_subj,d$look_after_health_x3)
#               Sobel      Aroian     Goodman
# z.value 2.999783276 2.961187716 3.039928416
# p.value 0.002701718 0.003064551 0.002366344

# PEMR is a mediator between subjective SES and looking after health (p<.01, z=3). Yay!

##############################
####### PATIENCE SCORE #######
##############################

lm_discounting1 <- lm(scale(patience_score) ~ scale(YPLL_sqrt),data=d)
summary(lm_discounting1)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)       1.856e-16  4.218e-02   0.000    1.000
# scale(YPLL_sqrt) -9.000e-03  4.222e-02  -0.213    0.831

# The nullest of the null results. NOt drastically changes after YPLL_sum correction

lm_discounting2 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity,data=d)
summary(lm_discounting2)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)     -0.450813   0.189922  -2.374 0.017952 *  
# scale(YPLL_sqrt)-0.063894   0.043551  -1.467 0.142913    
# age              0.010422   0.002777   3.753 0.000193 ***
# genderMale       0.172466   0.083123   2.075 0.038463 *  
# ethnicityBlack  -0.554994   0.252328  -2.199 0.028255 *  
# ethnicityMixed  -0.103921   0.296171  -0.351 0.725811    
# ethnicityOther   0.026894   0.312421   0.086 0.931433    
# ethnicityWhite  -0.099965   0.160331  -0.623 0.533216 

# After YPLL_sum correction, the YPLL_sum effect becomes stronger (0.09), marginally significant (p=0.08)
# But not with the Holm-Bonferroni correction.
# The gender and ethnicity effects disappear.

lm_discounting3 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj,data=d)
summary(lm_discounting3)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)      -0.812781   0.242121  -3.357 0.000842 ***
# scale(YPLL_sqrt) -0.053487   0.043468  -1.230 0.219035    
# age               0.009030   0.002773   3.256 0.001197 ** 
# genderMale        0.148692   0.082611   1.800 0.072421 .  
# ethnicityBlack   -0.576615   0.250061  -2.306 0.021486 *  
# ethnicityMixed   -0.116084   0.293533  -0.395 0.692647    
# ethnicityOther    0.016034   0.309334   0.052 0.958680    
# ethnicityWhite   -0.096346   0.159608  -0.604 0.546330    
# scale(income_log) 0.058002   0.044928   1.291 0.197239    
# SES_subj          0.078944   0.027662   2.854 0.004481 **

# Only effect that remains is the positive effect of SES on patience (conversely on delay discounting)

lm_discounting4 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress,data=d)
summary(lm_discounting4)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)     -0.872775   0.299158  -2.917  0.00367 **
# scale(YPLL_sqrt)-0.053541   0.043503  -1.231  0.21894   
# age              0.009245   0.002845   3.249  0.00123 **
# genderMale       0.155342   0.084933   1.829  0.06794 . 
# ethnicityBlack  -0.568584   0.251361  -2.262  0.02408 * 
# ethnicityMixed  -0.120492   0.294051  -0.410  0.68214   
# ethnicityOther   0.008360   0.310394   0.027  0.97852   
# ethnicityWhite  -0.099312   0.159972  -0.621  0.53498   
# scale(income_log)0.058515   0.044989   1.301  0.19392   
# SES_subj         0.080781   0.028200   2.865  0.00434 **
# stress           0.012717   0.037187   0.342  0.73251 

lm_discounting5 <- lm(scale(patience_score) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress + extrinsic_risk_sqrt,data=d)
summary(lm_discounting5)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)         -0.807563   0.307792  -2.624  0.00894 **
# scale(YPLL_sqrt)    -0.050267   0.043661  -1.151  0.25011   
# age                  0.009067   0.002853   3.179  0.00156 **
# genderMale           0.160295   0.085124   1.883  0.06022 . 
# ethnicityBlack      -0.560712   0.251554  -2.229  0.02622 * 
# ethnicityMixed      -0.101064   0.294885  -0.343  0.73194   
# ethnicityOther       0.027334   0.311155   0.088  0.93003   
# ethnicityWhite      -0.086512   0.160624  -0.539  0.59038   
# scale(income_log)    0.057114   0.045023   1.269  0.20514   
# SES_subj             0.078284   0.028340   2.762  0.00593 **
# stress               0.017473   0.037564   0.465  0.64201   
# extrinsic_risk_sqrt -0.017760   0.019658  -0.903  0.36668 

### Even the extrinsic risk effect on delay discounting is absent!

lm_discounting6 <- lm(scale(patience_score) ~ extrinsic_risk_sqrt + SES_subj,data=d)
summary(lm_discounting6)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -0.45335    0.18104  -2.504 0.012559 *  
# extrinsic_risk_sqrt -0.02115    0.01946  -1.087 0.277579    
# SES_subj             0.09775    0.02633   3.713 0.000225 ***

#################################
###### Secondary analyses #######
#################################

########### Age at first child ##########

lm_1st_child1 <- glm(scale(age_child1_sqrt) ~ scale(YPLL_sqrt), data=d)
summary(lm_1st_child1)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)      -0.001084   0.057291  -0.019    0.985
# scale(YPLL_sqrt)  0.007712   0.058267   0.132    0.895

lm_1st_child2 <- glm(scale(age_child1_sqrt) ~ scale(YPLL_sqrt) + age + gender + ethnicity,data=d)
summary(lm_1st_child2)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.659735   0.321063  -2.055 0.040751 *  
# scale(YPLL_sqrt) -0.006203   0.058381  -0.106 0.915454    
# age               0.002135   0.004572   0.467 0.640785    
# genderMale        0.381506   0.112184   3.401 0.000762 ***
# ethnicityBlack    0.317338   0.383345   0.828 0.408427    
# ethnicityMixed   -0.484947   0.414477  -1.170 0.242912    
# ethnicityOther    0.935423   0.458405   2.041 0.042157 *  
# ethnicityWhite    0.419631   0.233676   1.796 0.073525 .

lm_1st_child3 <- glm(scale(age_child1_sqrt) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj, data=d)
summary(lm_1st_child3)
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.411090   0.392548  -3.595 0.000379 ***
# scale(YPLL_sqrt)   0.020819   0.057929   0.359 0.719552    
# age                0.001499   0.004507   0.333 0.739605    
# genderMale         0.352728   0.111835   3.154 0.001773 ** 
# ethnicityBlack     0.319813   0.376994   0.848 0.396932    
# ethnicityMixed    -0.549676   0.408231  -1.346 0.179159    
# ethnicityOther     0.946140   0.452133   2.093 0.037221 *  
# ethnicityWhite     0.478516   0.232734   2.056 0.040639 *  
# scale(income_log) -0.061012   0.068127  -0.896 0.371200    
# SES_subj           0.131486   0.038122   3.449 0.000643 ***

lm_1st_child4 <- glm(scale(age_child1_sqrt) ~ scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress,data=d)
summary(lm_1st_child4)
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.888061   0.456166  -4.139 4.54e-05 ***
# scale(YPLL_sqrt)   0.019821   0.057636   0.344 0.731160    
# age                0.003571   0.004599   0.776 0.438114    
# genderMale         0.412386   0.115105   3.583 0.000397 ***
# ethnicityBlack     0.349706   0.375362   0.932 0.352266    
# ethnicityMixed    -0.596154   0.406797  -1.465 0.143837    
# ethnicityOther     0.914794   0.450093   2.032 0.042988 *  
# ethnicityWhite     0.458620   0.231756   1.979 0.048742 *  
# scale(income_log) -0.050126   0.067993  -0.737 0.461563    
# SES_subj           0.140646   0.038196   3.682 0.000274 ***
# stress             0.101678   0.050248   2.024 0.043908 *


########### Ideal age at first child ########

lm_ideal_child1 <- glm(ideal_age ~ scale(YPLL_sqrt), data=d)
summary(lm_1st_child1)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.001084   0.057291  -0.019    0.985
# scale(YPLL_sqrt)  0.007712   0.058267   0.132    0.895

lm_ideal_child2 <- glm(ideal_age ~ scale(YPLL_sqrt) + age + gender + ethnicity, data=d)
summary(lm_1st_child2)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.659735   0.321063  -2.055 0.040751 *  
# scale(YPLL_sqrt) -0.006203   0.058381  -0.106 0.915454    
# age               0.002135   0.004572   0.467 0.640785    
# genderMale        0.381506   0.112184   3.401 0.000762 ***
# ethnicityBlack    0.317338   0.383345   0.828 0.408427    
# ethnicityMixed   -0.484947   0.414477  -1.170 0.242912    
# ethnicityOther    0.935423   0.458405   2.041 0.042157 *  
# ethnicityWhite    0.419631   0.233676   1.796 0.073525 . 

lm_ideal_child3 <- glm(ideal_age ~scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj, data=d)
summary(lm_1st_child3)
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.411090   0.392548  -3.595 0.000379 ***
# scale(YPLL_sqrt)   0.020819   0.057929   0.359 0.719552    
# age                0.001499   0.004507   0.333 0.739605    
# genderMale         0.352728   0.111835   3.154 0.001773 ** 
# ethnicityBlack     0.319813   0.376994   0.848 0.396932    
# ethnicityMixed    -0.549676   0.408231  -1.346 0.179159    
# ethnicityOther     0.946140   0.452133   2.093 0.037221 *  
# ethnicityWhite     0.478516   0.232734   2.056 0.040639 *  
# scale(income_log) -0.061012   0.068127  -0.896 0.371200    
# SES_subj           0.131486   0.038122   3.449 0.000643 ***

lm_ideal_child4 <- glm(ideal_age ~scale(YPLL_sqrt) + age + gender + ethnicity + scale(income_log) + SES_subj + stress, data=d)
summary(lm_1st_child4)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.888061   0.456166  -4.139 4.54e-05 ***
# scale(YPLL_sqrt)   0.019821   0.057636   0.344 0.731160    
# age                0.003571   0.004599   0.776 0.438114    
# genderMale         0.412386   0.115105   3.583 0.000397 ***
# ethnicityBlack     0.349706   0.375362   0.932 0.352266    
# ethnicityMixed    -0.596154   0.406797  -1.465 0.143837    
# ethnicityOther     0.914794   0.450093   2.032 0.042988 *  
# ethnicityWhite     0.458620   0.231756   1.979 0.048742 *  
# scale(income_log) -0.050126   0.067993  -0.737 0.461563    
# SES_subj           0.140646   0.038196   3.682 0.000274 ***
# stress             0.101678   0.050248   2.024 0.043908 * 


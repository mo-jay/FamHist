# Data analysis script for 'Premature mortality and timing of your life: An exploratory correlational study'
# Mona Joly and colleagues
# 08/02/21

rm(list=ls())
#install.packages("tidyverse")
#install.packages("MASS")
#install.packages("tidylog")
#install.packages("rmarkdown")
library(tidyverse)
library(tidylog)
library(rmarkdown)

# setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona
# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

d <- read.table("data_famhist.txt",dec=",",sep="\t",header=TRUE)        # Read final data

######## Looking after health
  ## Checking distribution of look_after_health var
hist(d2$look_after_health_1)
qqnorm(d2$look_after_health_1)
qqline(d2$look_after_health_1)
hist(sqrt(max(d2$look_after_health_1+1) - d2$look_after_health_1))
look_after_health_sqrt<- sqrt(max(d2$look_after_health_1+1) - d2$look_after_health_1)
qqnorm(look_after_health_sqrt)
qqline(look_after_health_sqrt,col="red")
hist(sqrt(max(d2$look_after_health_1+1) - d2$look_after_health_1))

  ## Checking distribution of YPLL_sum var
hist(d2$YPLL_sum)  
hist(log(d2$YPLL_sum))
log(d2$YPLL_sum+1)
hist(log(d2$YPLL_sum+1))

lm_health1 <- lm(look_after_health_sqrt ~ log(d2$YPLL_sum+1))
summary(lm_health1)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 71.09517    1.13838   62.45   <2e-16 ***
# d2$YPLL_sum  0.04962    0.02493    1.99    0.047 * 

lm_health2 <- glm(d2$look_after_health_1 ~ log(d2$YPLL_sum) + d2$age.x + d2$gender + d2$Ethnicity..Simplified.)
summary(lm_health2)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               71.59969    3.61099  19.828  < 2e-16 ***
# d2$YPLL_sum                                0.02511    0.02530   0.993 0.321243    
# d2$age.x                                   0.20016    0.05209   3.842 0.000136 ***
# d2$gender                                 -4.09375    1.58237  -2.587 0.009932 ** 
# d2$Ethnicity..Simplified.Black             1.00876    4.81233   0.210 0.834042    
# d2$Ethnicity..Simplified.CONSENT REVOKED -37.24146   18.97518  -1.963 0.050187 .  
# d2$Ethnicity..Simplified.Mixed            -3.54480    5.64616  -0.628 0.530377    
# d2$Ethnicity..Simplified.Other            -5.64531    5.95134  -0.949 0.343248    
# d2$Ethnicity..Simplified.White            -8.05480    3.05785  -2.634 0.008671 **

lm_health3 <- glm(d2$look_after_health_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified. + d2$personal_income + d2$SES_subj)
summary(lm_health3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               6.295e+01  4.433e+00  14.201  < 2e-16 ***
# d2$YPLL_sum                               3.346e-02  2.525e-02   1.325 0.185570    
# d2$age.x                                  1.796e-01  5.207e-02   3.449 0.000605 ***
# d2$gender                                -4.379e+00  1.574e+00  -2.782 0.005580 ** 
# d2$Ethnicity..Simplified.Black            9.372e-01  4.779e+00   0.196 0.844610    
# d2$Ethnicity..Simplified.CONSENT REVOKED -3.573e+01  1.884e+01  -1.896 0.058441 .  
# d2$Ethnicity..Simplified.Mixed           -4.042e+00  5.607e+00  -0.721 0.471318    
# d2$Ethnicity..Simplified.Other           -6.054e+00  5.905e+00  -1.025 0.305718    
# d2$Ethnicity..Simplified.White           -7.652e+00  3.050e+00  -2.509 0.012387 *  
# d2$personal_income                        1.682e-05  8.070e-05   0.208 0.834935    
# d2$SES_subj                               1.606e+00  5.253e-01   3.057 0.002340 **

# The older ppl are, the more they look after their health.
# Women take better care of their health (maybe I should leave it qualitative) 1=Male;0=Female
# White ppl take less care of their health
# More affluent people take better care of their health

lm_health4 <- glm(d2$look_after_health_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified. + d2$personal_income + d2$SES_subj + d2$stress)
summary(lm_health4)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               7.221e+01  5.564e+00  12.978  < 2e-16 ***
# d2$YPLL_sum                               3.209e-02  2.511e-02   1.278 0.201672    
# d2$age.x                                  1.472e-01  5.312e-02   2.771 0.005774 ** 
# d2$gender                                -5.405e+00  1.609e+00  -3.359 0.000837 ***
# d2$Ethnicity..Simplified.Black           -3.217e-01  4.774e+00  -0.067 0.946304    
# d2$Ethnicity..Simplified.CONSENT REVOKED -3.522e+01  1.873e+01  -1.880 0.060621 .  
# d2$Ethnicity..Simplified.Mixed           -3.370e+00  5.580e+00  -0.604 0.546143    
# d2$Ethnicity..Simplified.Other           -4.874e+00  5.887e+00  -0.828 0.408013    
# d2$Ethnicity..Simplified.White           -7.245e+00  3.036e+00  -2.387 0.017342 *  
# d2$personal_income                        1.588e-05  8.024e-05   0.198 0.843181    
# d2$SES_subj                               1.311e+00  5.334e-01   2.458 0.014268 *  
# d2$stress                                -1.920e+00  7.045e-01  -2.725 0.006626 **

# The more stressed people are, the less they take care of their health

### Does extrinsic mortality risk mediates the relationship between SES and looking after health?

lm_SES_hb <-glm(d2$look_after_health_1 ~ d2$SES_subj + d2$age.x + d2$gender)
summary(lm_SES_hb)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   58.07189    3.56813  16.275  < 2e-16 ***
# d2$SES_subj   1.68692    0.49486   3.409 0.000699 ***
# d2$age.x      0.16350    0.04941   3.309 0.000996 ***
# d2$gender     -4.57196    1.57955  -2.894 0.003946 **
sd(d2$look_after_health_1)

lm_SES_extrinsic <- glm(d2$extrinsic_risk ~ d2$SES_subj + d2$age.x + d2$gender)
summary(lm_SES_extrinsic)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  34.64825    3.53599   9.799  < 2e-16 ***
# d2$SES_subj  -2.14615    0.49040  -4.376 1.44e-05 ***
# d2$age.x     -0.05121    0.04896  -1.046    0.296    
# d2$gender     1.61831    1.56533   1.034    0.302

lm_extrinsic_hb <- glm(d2$look_after_health_1~d2$extrinsic_risk)
summary(lm_extrinsic_hb)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        78.7923     1.1693   67.38  < 2e-16 ***
# d2$extrinsic_risk  -0.2872     0.0412   -6.97 8.89e-12 ***

lm_SES_hb <-glm(d2$look_after_health_1 ~ d2$SES_subj + d2$age.x + d2$gender)
summary(lm_SES_hb)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   58.07189    3.56813  16.275  < 2e-16 ***
# d2$SES_subj   1.68692    0.49486   3.409 0.000699 ***
# d2$age.x      0.16350    0.04941   3.309 0.000996 ***
# d2$gender     -4.57196    1.57955  -2.894 0.003946 **
sd(d2$look_after_health_1)

lm_extrinsic_SES_hb <- glm(d2$look_after_health_1~d2$SES_subj+d2$extrinsic_risk +d2$age.x+d2$gender)
summary(lm_extrinsic_SES_hb) 
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       67.02322    3.73654  17.937  < 2e-16 ***
# d2$SES_subj        1.13246    0.48691   2.326  0.02039 *  
# d2$extrinsic_risk -0.25835    0.04126  -6.262 7.59e-10 ***
# d2$age.x           0.15027    0.04785   3.140  0.00178 ** 
# d2$gender         -4.15387    1.52973  -2.715  0.00682 **

sobel(d2$SES_subj,d2$extrinsic_risk,d2$look_after_health_1)

########### Age at first child

lm_1st_child1 <- glm(d2$age_first_child_1 ~ d2$YPLL_sum)
summary(lm_1st_child1)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 27.979714   0.541063   51.71   <2e-16 ***
# d2$YPLL_sum -0.008565   0.011121   -0.77    0.442

lm_1st_child2 <- glm(d2$age_first_child_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified.)
summary(lm_1st_child2)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               23.87482    2.06838  11.543  < 2e-16 ***
# d2$YPLL_sum                              -0.01037    0.01107  -0.937 0.349602    
# d2$age.x                                  0.01639    0.02903   0.565 0.572781    
# d2$gender                                 2.59062    0.71857   3.605 0.000364 ***
# d2$Ethnicity..Simplified.Black            1.75483    2.46426   0.712 0.476942    
# d2$Ethnicity..Simplified.CONSENT REVOKED  6.09764    6.44500   0.946 0.344847    
# d2$Ethnicity..Simplified.Mixed           -3.01310    2.66094  -1.132 0.258383    
# d2$Ethnicity..Simplified.Other            5.67592    2.94088   1.930 0.054537 .  
# d2$Ethnicity..Simplified.White            2.38235    1.50171   1.586 0.113681

lm_1st_child3 <- glm(d2$age_first_child_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified. + d2$personal_income + d2$SES_subj)
summary(lm_1st_child3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.962e+01  2.389e+00   8.215 6.27e-15 ***
# d2$YPLL_sum                              -4.847e-03  1.101e-02  -0.440 0.659985    
# d2$age.x                                  1.412e-02  2.866e-02   0.493 0.622529    
# d2$gender                                 2.428e+00  7.145e-01   3.398 0.000770 ***
# d2$Ethnicity..Simplified.Black            1.754e+00  2.424e+00   0.724 0.469854    
# d2$Ethnicity..Simplified.CONSENT REVOKED  6.477e+00  6.350e+00   1.020 0.308564    
# d2$Ethnicity..Simplified.Mixed           -3.377e+00  2.621e+00  -1.288 0.198609    
# d2$Ethnicity..Simplified.Other            5.792e+00  2.898e+00   1.998 0.046577 *  
# d2$Ethnicity..Simplified.White            2.811e+00  1.490e+00   1.886 0.060279 .  
# d2$personal_income                       -5.412e-05  4.191e-05  -1.291 0.197539    
# d2$SES_subj                               8.406e-01  2.409e-01   3.489 0.000557 ***

lm_1st_child4 <- glm(d2$age_first_child_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified. + d2$personal_income + d2$SES_subj + d2$stress)
summary(lm_1st_child4)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.637e+01  2.856e+00   5.730 2.44e-08 ***
# d2$YPLL_sum                              -4.782e-03  1.095e-02  -0.437 0.662550    
# d2$age.x                                  2.788e-02  2.929e-02   0.952 0.341882    
# d2$gender                                 2.833e+00  7.376e-01   3.841 0.000149 ***
# d2$Ethnicity..Simplified.Black            1.958e+00  2.413e+00   0.811 0.417820    
# d2$Ethnicity..Simplified.CONSENT REVOKED  6.352e+00  6.317e+00   1.006 0.315445    
# d2$Ethnicity..Simplified.Mixed           -3.666e+00  2.611e+00  -1.404 0.161311    
# d2$Ethnicity..Simplified.Other            5.621e+00  2.884e+00   1.949 0.052226 .  
# d2$Ethnicity..Simplified.White            2.719e+00  1.483e+00   1.833 0.067728 .  
# d2$personal_income                       -5.316e-05  4.169e-05  -1.275 0.203249    
# d2$SES_subj                               9.155e-01  2.424e-01   3.777 0.000191 ***
# d2$stress                                 6.588e-01  3.208e-01   2.054 0.040857 *


############## Ideal age at first child

lm_ideal_child1 <- glm(d2$ideal_age_1 ~ d2$YPLL_sum)
summary(lm_1st_child1)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   27.979714   0.541063   51.71   <2e-16 ***
# d2$YPLL_sum   -0.008565   0.011121   -0.77    0.442 

lm_ideal_child2 <- glm(d2$ideal_age_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified.)
summary(lm_1st_child2)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               23.87482    2.06838  11.543  < 2e-16 ***
# d2$YPLL_sum                              -0.01037    0.01107  -0.937 0.349602    
# d2$age.x                                  0.01639    0.02903   0.565 0.572781    
# d2$gender                                 2.59062    0.71857   3.605 0.000364 ***
# d2$Ethnicity..Simplified.Black            1.75483    2.46426   0.712 0.476942    
# d2$Ethnicity..Simplified.CONSENT REVOKED  6.09764    6.44500   0.946 0.344847    
# d2$Ethnicity..Simplified.Mixed           -3.01310    2.66094  -1.132 0.258383    
# d2$Ethnicity..Simplified.Other            5.67592    2.94088   1.930 0.054537 .  
# d2$Ethnicity..Simplified.White            2.38235    1.50171   1.586 0.113681 

lm_ideal_child3 <- glm(d2$ideal_age_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified. + d2$personal_income + d2$SES_subj)
summary(lm_1st_child3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.962e+01  2.389e+00   8.215 6.27e-15 ***
# d2$YPLL_sum                              -4.847e-03  1.101e-02  -0.440 0.659985    
# d2$age.x                                  1.412e-02  2.866e-02   0.493 0.622529    
# d2$gender                                 2.428e+00  7.145e-01   3.398 0.000770 ***
# d2$Ethnicity..Simplified.Black            1.754e+00  2.424e+00   0.724 0.469854    
# d2$Ethnicity..Simplified.CONSENT REVOKED  6.477e+00  6.350e+00   1.020 0.308564    
# d2$Ethnicity..Simplified.Mixed           -3.377e+00  2.621e+00  -1.288 0.198609    
# d2$Ethnicity..Simplified.Other            5.792e+00  2.898e+00   1.998 0.046577 *  
# d2$Ethnicity..Simplified.White            2.811e+00  1.490e+00   1.886 0.060279 .  
# d2$personal_income                       -5.412e-05  4.191e-05  -1.291 0.197539    
# d2$SES_subj                               8.406e-01  2.409e-01   3.489 0.000557 ***

lm_ideal_child3 <- glm(d2$ideal_age_1 ~ d2$YPLL_sum + d2$age.x + d2$gender + d2$Ethnicity..Simplified. + d2$personal_income + d2$SES_subj + d2$stress)
summary(lm_1st_child3)
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               1.962e+01  2.389e+00   8.215 6.27e-15 ***
# d2$YPLL_sum                              -4.847e-03  1.101e-02  -0.440 0.659985    
# d2$age.x                                  1.412e-02  2.866e-02   0.493 0.622529    
# d2$gender                                 2.428e+00  7.145e-01   3.398 0.000770 ***
# d2$Ethnicity..Simplified.Black            1.754e+00  2.424e+00   0.724 0.469854    
# d2$Ethnicity..Simplified.CONSENT REVOKED  6.477e+00  6.350e+00   1.020 0.308564    
# d2$Ethnicity..Simplified.Mixed           -3.377e+00  2.621e+00  -1.288 0.198609    
# d2$Ethnicity..Simplified.Other            5.792e+00  2.898e+00   1.998 0.046577 *  
# d2$Ethnicity..Simplified.White            2.811e+00  1.490e+00   1.886 0.060279 .  
# d2$personal_income                       -5.412e-05  4.191e-05  -1.291 0.197539    
# d2$SES_subj                               8.406e-01  2.409e-01   3.489 0.000557 *** 


############### Mediation of extrinsic risk

lm_health_extrinsic <- glm(d2$look_after_health_1 ~ d2$extrinsic_risk)
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

lm_extrinsic_YPLL_health <- glm(d2$look_after_health_1 ~ d2$YPLL_sum * d2$extrinsic_risk)
summary(lm_extrinsic_YPLL_health)
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   43.958746   4.821345   9.118  < 2e-16 ***
# d2$YPLL_sum                    0.169035   0.098362   1.719   0.0863 .  
# d2$extrinsic_risk              0.341527   0.059246   5.765 1.35e-08 ***
# d2$YPLL_sum:d2$extrinsic_risk -0.001438   0.001219  -1.180   0.2386    


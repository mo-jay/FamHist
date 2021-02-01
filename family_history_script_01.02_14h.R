# Data analysis script for 'Premature mortality and timing of your life: An exploratory correlational study'
# Mona Joly and colleagues
# 01/02/21

rm(list=ls())
#install.packages("tidyverse")
#install.packages("MASS")
#library(tidyverse)
#library(MASS)

setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona
# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis") # Macbook Mona

d <- read.csv("Qualtrics_fulldata.csv")                           # Read Qualtrics data
p <- read.csv("Prolific_fulldata.csv")                            # Read Prolific data
names(p)[names(p) == "participant_id"] <- "prolific_id"           # Rename ID in Prolific data for merging

d_merged <- merge(d,p,by="prolific_id")                           # Merging both datasets ; N = 632

###### CLEANING ######

d_merged$Status <- NULL
d_merged$ResponseId <- NULL
d_merged$RecipientLastName <- NULL
d_merged$RecipientFirstName <- NULL
d_merged$RecipientEmail <- NULL
d_merged$ExternalReference <- NULL
d_merged$DistributionChannel <- NULL
d_merged$prolific_id...Topics <- NULL
d_merged$prolific_id...Parent.Topics <- NULL
d_merged$session_id <- NULL

names(d_merged)[names(d_merged) == "Q85_1"] <- "env_transport"
names(d_merged)[names(d_merged) == "Q60_1"] <- "extrinsic_risk"

####### SORTING #######

  ### Removing participants who didn't give consent
d2 <- d_merged[which(d_merged$consent=="Yes, I consent to participate."),]          # N = 630

  ### Removing data of participants who didn't finish the survey
d2 <- d2[which(d2$Finished=="True"),]                                               # N = 612

  ### Removing data of participants who didn't give the same smoking status twice
d2 <- d2[which(d2$smoker==d2$attention_smoker),]                                    # N = 608

  ### Removing data of participants who failed two attention checks
d2 <- d2[-c(which(d2$attention_4_1 !="4" & d2$attention_fruit!="Strongly agree")),] # N = 605

  ### Removing data of participants with a Prolific score <95
d2 <- d2[-c(which(d2$prolific_score<95)),] # N = 602

  ### Removing data of participants who gave a significantly different age on Prolific and on Qualtrics
d2$age.x <- as.numeric(d2$age.x)
d2$age.y <- as.numeric(d2$age.y)
d2 <- d2[-c(which(d2$age.x != d2$age.y & d2$age.x != d2$age.y+1 & d2$age.x != d2$age.y - 1)),]  # N=591

  ### Removing data of participants who gave a different gender on Prolific and on Qualtrics
nrow(d2[which(d2$gender=="Female" & d2$Sex=="Male"),])      #0
nrow(d2[which(d2$gender=="Male" & d2$Sex=="Female"),])      #1
d2 <- d2[-c(which(d2$gender=="Male" & d2$Sex=="Female")),]                                      # N=590

  ### Removing data of participants who did not understand we asked for grandparents age at death and not their own

      ### Recoding of parental and grandparental age at death

d2$parent1_age_2[which(d2$parent1_age_2=="<20")] <- 19
d2$parent2_age_2[which(d2$parent2_age_2=="<20")] <- 19
d2$gp1_age_2[which(d2$gp1_age_2=="<20")] <- 19
d2$gp2_age_2[which(d2$gp2_age_2=="<20")] <- 19
d2$gp3_age_2[which(d2$gp3_age_2=="<20")] <- 19
d2$gp4_age_2[which(d2$gp4_age_2=="<20")] <- 19

d2$parent1_age_2[which(d2$parent1_age_2==">90")] <- 91
d2$parent2_age_2[which(d2$parent2_age_2=="90")] <- 91
d2$gp1_age_2[which(d2$gp1_age_2=="90")] <- 91
d2$gp2_age_2[which(d2$gp2_age_2=="90")] <- 91
d2$gp3_age_2[which(d2$gp3_age_2=="90")] <- 91
d2$gp4_age_2[which(d2$gp4_age_2=="90")] <- 91

# d2$parent1_age_2[which(d2$parent1_age_2=="Don't know")] <- NA
# d2$parent2_age_2[which(d2$parent2_age_2=="Don't know")] <- NA
# d2$gp1_age_2[which(d2$gp1_age_2=="Don't know")] <- NA
# d2$gp2_age_2[which(d2$gp2_age_2=="Don't know")] <- NA
# d2$gp3_age_2[which(d2$gp3_age_2=="Don't know")] <- NA
# d2$gp4_age_2[which(d2$gp4_age_2=="Don't know")] <- NA
# 
# d2$parent1_age_2[which(d2$parent1_age_2=="Rather not say")] <- NA
# d2$parent2_age_2[which(d2$parent2_age_2=="Rather not say")] <- NA
# d2$gp1_age_2[which(d2$gp1_age_2=="Rather not say")] <- NA
# d2$gp2_age_2[which(d2$gp2_age_2=="Rather not say")] <- NA
# d2$gp3_age_2[which(d2$gp3_age_2=="Rather not say")] <- NA
# d2$gp4_age_2[which(d2$gp4_age_2=="Rather not say")] <- NA

d2$parent1_age_2 <- as.numeric(d2$parent1_age_2)
d2$parent2_age_2 <- as.numeric(d2$parent2_age_2)
d2$gp1_age_2 <- as.numeric(d2$gp1_age_2)
d2$gp2_age_2 <- as.numeric(d2$gp2_age_2)
d2$gp3_age_2 <- as.numeric(d2$gp3_age_2)
d2$gp4_age_2 <- as.numeric(d2$gp4_age_2)

      ### Removing data of participants who answered 3 times that their gp died before the age of 25
nrow(d2[which(d2$gp1_age_2<25 & d2$gp2_age_2<25 & d2$gp3_age_2<25),])               #7     
d2 <- d2[-c(which(d2$gp1_age_2<25 & d2$gp2_age_2<25 & d2$gp3_age_2<25)),]           # N = 583
nrow(d3 <- d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$gp2_age_2<25),])     #0
nrow(d3 <- d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$gp1_age_2<25),]) #0

  ### Removing data of participants who answered twice that their relatives died before the age of 25 and failed an attention test
nrow(d2[which(d2$gp1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp1_age_2<25 & d2$gp2_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" ),])                   #1
d2 <- d2[-c(which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" )),]               # N = 582

nrow(d2[which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp1_age_2<25 & d2$gp4_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp1_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" ),])                   #1
d2 <- d2[-c(which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" )),]               # N = 581

nrow(d2[which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp3_age_2<25 & d2$gp4_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp3_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp2_age_2<25 & d2$gp4_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp2_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$attention_4_1 !="4"),])                #0
nrow(d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$attention_fruit!="Strongly agree"),])  #0
nrow(d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_4_1 !="4" ),])           #1
d2 <- d2[-c(which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_4_1 !="4" )),]       # N = 580

nrow(d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_fruit !="Strongly agree" ),])  #0
nrow(d2[which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4_1 !="4" ),])               #1
d2 <- d2[-c(which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4_1 !="4")),]            # N = 579

nrow(d2[which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_fruit!="Strongly agree"),])  #0

summary(d2$parent1_age_2)
summary(d2$parent2_age_2)
summary(d2$gp1_age_2)
summary(d2$gp2_age_2)
summary(d2$gp3_age_2)
summary(d2$gp4_age_2)

#d2[which(d2$parent2_age_2<25),] 

  ### Removing data of participants who were very quick and failed an attentional task
d2$Duration..in.seconds. <- as.numeric(d2$Duration..in.seconds.)
mean(d2$Duration..in.seconds.)-sd(d2$Duration..in.seconds.)     # mean - 1 sd = 139.4 seconds
nrow(d2[which(d2$attention_4_1 !="4" & d2$Duration..in.seconds. < mean(d2$Duration..in.seconds.)-sd(d2$Duration..in.seconds.)),]) #1
d2 <- d2[-c(which(d2$attention_4_1 !="4" & d2$Duration..in.seconds. 
                  < mean(d2$Duration..in.seconds.)-sd(d2$Duration..in.seconds.))),]             # N = 578

  ### Removing data of participants who were extremely long and failed an attentional task
mean(d2$Duration..in.seconds.)+3*sd(d2$Duration..in.seconds.)   # mean + 3 sd = 1486.2 seconds
nrow(d2[which(d2$attention_4_1 !="4" & d2$Duration..in.seconds. > mean(d2$Duration..in.seconds.)+3*sd(d2$Duration..in.seconds.)),]) #0

  ### Removing data of participants who didn't say whether their parents or grandparents were alive or not
table(d2$parents_dead)
nrow(d2[which(d2$parents_dead=="Rather not say"),]) #1
d2 <- d2[-c(which(d2$parents_dead=="Rather not say")),]                                         # N = 577
nrow(d2[which(d2$gp_dead==""),]) #10
d2 <- d2[-c(which(d2$gp_dead=="")),]                                                            # N = 567
nrow(d2[which(d2$gp_dead=="Rather not say"),]) #3
d2 <- d2[-c(which(d2$gp_dead=="Rather not say")),]                                              # N = 564

  ### Ejections to add: lack of variability in answers
d3 <- d2[which(d2$status=="REJECTED"),] #2 

###### Recoding of character variables into numeric variables #####

  ### Recode number of deaths
table(d2$parents_dead)
d2$parents_dead[which(d2$parents_dead=="Yes")] <- 0
d2$parents_dead[which(d2$parents_dead=="No: one is dead")] <- 1
d2$parents_dead[which(d2$parents_dead=="No: both are dead")] <- 2
d2$parents_dead <- as.numeric(d2$parents_dead)
summary(d2$parents_dead)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.0000  0.7148  1.0000  2.0000       3 

table(d2$gp_dead)
d2$gp_dead[which(d2$gp_dead=="Yes, they are all alive.")] <- 0
d2$gp_dead[which(d2$gp_dead=="No, one is dead")] <- 1
d2$gp_dead[which(d2$gp_dead=="No, two are dead.")] <- 2
d2$gp_dead[which(d2$gp_dead=="No, three are dead.")] <- 3
d2$gp_dead[which(d2$gp_dead=="No, all four are dead.")] <- 4
d2$gp_dead <- as.numeric(d2$gp_dead)
summary(d2$gp_dead)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.000   3.000   4.000   3.401   4.000   4.000 

  ### Recoding of gender
table(d2$gender)
d2$gender[which(d2$gender=="Female")] <- 0
d2$gender[which(d2$gender=="Male")] <- 1
d2$gender <- as.numeric((d2$gender))
summary(d2$gender)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   0.484   1.000   1.000 

  ### Recoding of interest in looking after health var
d2$look_after_health_1 <- as.numeric(d2$look_after_health_1)
summary(d2$look_after_health_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    65.0    75.0    72.7    85.0   100.0 

  ### Recoding of checkup - maybe choose a number lower than the maximum of the given range?
table(d2$checkup)
d2$checkup[which(d2$checkup=="Within the past year")] <- 1
d2$checkup[which(d2$checkup=="Within the last 2 years")] <- 2
d2$checkup[which(d2$checkup=="Within the last 3 years")] <- 3
d2$checkup[which(d2$checkup=="Within the last 5 years")] <- 5
d2$checkup[which(d2$checkup=="Within the last 10 years")] <- 10
d2$checkup[which(d2$checkup=="10 years ago or more")] <- 15
d2$checkup <- as.numeric(d2$checkup)
table(d2$checkup)
summary(d2$checkup)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   2.000   3.823   5.000  15.000      26 

  ### Recoding of smoker status
d2$smoker[which(d2$smoker=="No")] <- 0
d2$smoker[which(d2$smoker=="Yes")] <- 1
d2$smoker <- as.numeric(d2$smoker)
table(d2$smoker)
nrow(d2[which(d2$smoker==1),]) #58 (/564)
summary(d2$smoker) #10,28% of smokers

  ### Recoding of environment variables
d2$environment_1 <- as.numeric(d2$environment_1)
summary(d2$environment_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   55.00   70.00   65.43   80.00  100.00 

d2$env_transport <- as.numeric(d2$env_transport)
summary(d2$env_transport)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   29.00   53.50   50.53   72.00  100.00 

cor(d2$environment_1,d2$env_transport) # r = .47

  ### Recoding of perceived extrinsic mortality risk (PEMR) var
d2$extrinsic_risk <- as.numeric(d2$extrinsic_risk)
for (i in 1:nrow(d2)){                                # PEMR = 100 - perceived likelihood to survive to age 75 with max effort
  d2$extrinsic_risk[i] <- 100 - d2$extrinsic_risk[i]
}
summary(d2$extrinsic_risk)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   10.00   18.00   21.23   28.00  100.00

  ### Recoding of children presence
# table(d2$children)
d2$children[which(d2$children=="No")] <- 0
d2$children[which(d2$children=="Yes")] <- 1
d2$children <- as.numeric(d2$children)
nrow(d2[which(d2$children==1),]) #313 (/564)
summary(d2$children) #55,5% of participants with children

  ### Recoding of age at first child var for participants w/ children
# table(d2$age_first_child_1)
d2$age_first_child_1[which(d2$age_first_child_1=="<16")] <- 15 # arbitrary
d2$age_first_child_1 <- as.numeric(d2$age_first_child_1)
summary(d2$age_first_child_1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   15.00   23.00   28.00   27.67   32.00   51.00     251

  ### Recoding of history of breastfeeding
# table(d2$breastfeed_yesno)
d2$breastfeed_yesno[which(d2$breastfeed_yesno=="No")] <- 0
d2$breastfeed_yesno[which(d2$breastfeed_yesno=="Yes")] <- 1
d2$breastfeed_yesno <- as.numeric(d2$breastfeed_yesno)
d3<-d2[which(d2$breastfeed_yesno==1),] #113 (/564)
(nrow(d2[which(d2$breastfeed_yesno==1),]))/(nrow(d2[which(d2$children==1 & d2$gender==0),])) #64,6% of women with children breastfed their 1st child
(nrow(d2[which(!is.na(d2$breastfeed_yesno)),]))/nrow(d2) #30,9% of the participants answered the breastfeed question

  ### Recoding of breastfeed length
# table(d2$breastfeed_length_1)
d2$breastfeed_length_1 <- as.numeric(d2$breastfeed_length_1)
summary(d2$breastfeed_length_1)     # maybe to binarize (less than 6 months and more than 6 months)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   3.000   6.000   7.239   8.000  24.000     451

  ### Recoding of ideal age var for participants w/o children
# table(d2$ideal_age_1)
d2$ideal_age_1[which(d2$ideal_age_1=="<16")] <- 15 #arbitrary
d2$ideal_age_1 <- as.numeric(d2$ideal_age_1)
summary(d2$ideal_age_1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   15.00   27.00   30.00   29.86   32.00   45.00     314  

  ### Recoding of control and closeness with relative variables (change of class)
d2$parent1_control_1 <- as.numeric(d2$parent1_control_1)
d2$parent1_close_1 <- as.numeric(d2$parent1_close_1)
d2$parent2_control_1 <- as.numeric(d2$parent2_control_1)
d2$parent2_close_1 <- as.numeric(d2$parent2_close_1)
d2$gp1_control_1 <- as.numeric(d2$gp1_control_1)
d2$gp1_close_1 <- as.numeric(d2$gp1_close_1)
d2$gp2_control_1 <- as.numeric(d2$gp2_control_1)
d2$gp2_close_1 <- as.numeric(d2$gp2_close_1)
d2$gp3_control_1 <- as.numeric(d2$gp3_control_1)
d2$gp3_close_1 <- as.numeric(d2$gp3_close_1)
d2$gp4_control_1 <- as.numeric(d2$gp4_control_1)
d2$gp4_close_1 <- as.numeric(d2$gp4_close_1)

d2 <- d2 %>% mutate(across(c("parent1_control_1","parent1_close_1")))

  ### Recoding of stress variable: from categorical to numeric
table(d2$stress)
d2$stress[which(d2$stress=="Never")] <- 0
d2$stress[which(d2$stress=="Very rarely")] <- 1
d2$stress[which(d2$stress=="Rarely")] <- 2
d2$stress[which(d2$stress=="Occasionally")] <- 3
d2$stress[which(d2$stress=="Frequently")] <- 4
d2$stress[which(d2$stress=="Very frequently")] <- 5
table(d2$stress)
d2$stress <- as.numeric(d2$stress)

  ### Recoding of income
table(d2$income_1)
plot(table(d2$income_1),lwd=30,type="h")
d2$income_1[which(d2$income_1=="£10,000 - £15,999")] <- (10000+15999)/2
d2$income_1[which(d2$income_1=="£16,000 - £19,999")] <- (16000+19999)/2
d2$income_1[which(d2$income_1=="£20,000 - £29,999")] <- (20000+29999)/2
d2$income_1[which(d2$income_1=="£30,000 - £39,999")] <- (30000+39999)/2
d2$income_1[which(d2$income_1=="£40,000 - £49,999")] <- (40000+49999)/2
d2$income_1[which(d2$income_1=="£50,000 - £59,999")] <- (50000+59999)/2
d2$income_1[which(d2$income_1=="£60,000 - £74,999")] <- (60000+74999)/2
d2$income_1[which(d2$income_1=="£75,000 - £99,999")] <- (75000+99999)/2
d2$income_1[which(d2$income_1=="£100,000 - £149,999")] <- (100000+149999)/2
d2$income_1[which(d2$income_1=="More than £150,000")] <- 175000 #arbitrary
d2$income_1[which(d2$income_1=="Less than £10,000")] <- 7500    #arbitrary
table(d2$income_1)
d2$income_1 <- as.numeric(d2$income_1)
summary(d2$income_1)

  ### Recoding of number of people in the household var
table(d2$household_1)
d2$household_1 <- as.numeric(d2$household_1)

#####################################
##### Creation of new variables #####
#####################################
  
  ### creation of var personal annual income

d2$personal_income <- NA
for (i in 1:nrow(d2)){
  d2$personal_income[i] <- d2$income_1[i] / d2$household_1[i]
}
summary(d2$personal_income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1250    9000   13750   16885   22500  100000

hist(d2$personal_income,breaks=15)

nrow(d2[which(d2$personal_income < mean(d2$personal_income)-sd(d2$personal_income)),])    # 59 declare an annual income of less than £6188
nrow(d2[which(d2$personal_income < 3000 & d2$attention_4_1 != "4"),])           #4 - to delete?

  ### Creation of var subj SES

d2 <- d2 %>% mutate(
  SES_subj = case_when(
    subjective_SES_1=="On" ~ 10
    subjective_SES_2="On" ~ 9
    subjective_SES_3=="On" ~ 8
    subjective_SES_4=="On" ~ 7
    subjective_SES_5=="On" ~ 6
    subjective_SES_6=="On" ~ 5
    subjective_SES_7=="On" ~ 4
    subjective_SES_8=="On" ~ 3
    subjective_SES_9=="On" ~ 2
    subjective_SES_10=="On" ~ 1
  ),
)

d2$SES_subj <- NULL
for(i in 1:nrow(d2)){
  if (d2$subjective_SES_1[i]=="On" & d2$subjective_SES_2[i]=="Off" 
      & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
      & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
      & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
      & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
  {d2$SES_subj[i] <- 10}else{
    if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="On" 
        & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
        & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
        & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
        & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
    {d2$SES_subj[i] <- 9}else{
      if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off"
          & d2$subjective_SES_3[i]=="On" & d2$subjective_SES_4[i]=="Off" 
          & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
          & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
          & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
      {d2$SES_subj[i] <- 8}else{
        if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off"
            & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="On" 
            & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
            & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
            & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
        {d2$SES_subj[i] <- 7}else{
          if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off" 
              & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
              & d2$subjective_SES_5[i]=="On" & d2$subjective_SES_6[i]=="Off" 
              & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
              & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
          {d2$SES_subj[i] <- 6}else{
            if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off"  
                & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
                & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="On" 
                & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
                & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
            {d2$SES_subj[i] <- 5}else{
              if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off"
                  & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
                  & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
                  & d2$subjective_SES_7[i]=="On" & d2$subjective_SES_8[i]=="Off"
                  & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
              {d2$SES_subj[i] <- 4}else{
                if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off" 
                    & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
                    & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
                    & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="On"
                    & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="Off")
                {d2$SES_subj[i] <- 3}else{
                  if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off" 
                      & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
                      & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
                      & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
                      & d2$subjective_SES_9[i]=="On" & d2$subjective_SES_10[i]=="Off")
                  {d2$SES_subj[i] <- 2}else{
                    if (d2$subjective_SES_1[i]=="Off" & d2$subjective_SES_2[i]=="Off" 
                        & d2$subjective_SES_3[i]=="Off" & d2$subjective_SES_4[i]=="Off" 
                        & d2$subjective_SES_5[i]=="Off" & d2$subjective_SES_6[i]=="Off" 
                        & d2$subjective_SES_7[i]=="Off" & d2$subjective_SES_8[i]=="Off"
                        & d2$subjective_SES_9[i]=="Off" & d2$subjective_SES_10[i]=="On")
                    {d2$SES_subj[i] <- 1}
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
summary(d2$SES_subj)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    5.00    6.00    5.52    7.00   10.00 
hist(d2$SES_subj) # distribution more normal than personal_income var

  ### Creation of YPLL for each relative
  # 1st dead parent

d2$YPLL_p1 <- NULL
for(i in 1:nrow(d2)){
  if (is.na(d2$parent1_age_2[i])){d2$YPLL_p1[i] <- 0}else{    # have to replace NA with 0 for now
    if (d2$parent1_age_2[i]>=75) {d2$YPLL_p1[i] <- 0} else{ 
      if (d2$parent1_age_2[i]<75){
        d2$YPLL_p1[i] <- 75 - d2$parent1_age_2[i]
        }
    }
  }
}
summary(d2$YPLL_p1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    4.00   10.84   16.50   56.00     293 

  # 2nd dead parent

d2$YPLL_p2 <- NULL
for(i in 1:nrow(d2)){
  if (is.na(d2$parent2_age_2[i])){d2$YPLL_p2[i] <- 0}else{
    if (d2$parent2_age_2[i]>=75) {d2$YPLL_p2[i] <- 0} else{ 
      if (d2$parent2_age_2[i]<75){
        d2$YPLL_p2[i] <- 75 - d2$parent2_age_2[i]
      }
    }
  }
}
summary(d2$YPLL_p2)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   0.000   6.529  10.000  50.000     445 

  # 1st dead grandparent
d2$YPLL_gp1 <- NULL
for(i in 1:nrow(d2)){
  if (is.na(d2$gp1_age_2[i])){d2$YPLL_gp1[i] <- 0}else{
    if (d2$gp1_age_2[i]>=75) {d2$YPLL_gp1[i] <- 0} else{ 
      if (d2$gp1_age_2[i]<75){d2$YPLL_gp1[i] <- 75 - d2$gp1_age_2[i]
      }
    }
  }
}
summary(d2$YPLL_gp1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.0     0.0     9.0    12.4    20.0    56.0      52 

  #2nd dead grandparent
d2$YPLL_gp2 <- NULL
for(i in 1:nrow(d2)){
  if (is.na(d2$gp2_age_2[i])){d2$YPLL_gp2[i] <- 0}else{
    if (d2$gp2_age_2[i]>=75) {d2$YPLL_gp2[i] <- 0} else{ 
      if (d2$gp2_age_2[i]<75){d2$YPLL_gp2[i] <- 75 - d2$gp2_age_2[i]
      }
    }
  }
}
summary(d2$YPLL_gp2)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.000   0.000   3.000   8.069  11.000  56.000     112

  #3rd dead grandparent
d2$YPLL_gp3 <- NULL
for(i in 1:nrow(d2)){
  if (is.na(d2$gp3_age_2[i])){d2$YPLL_gp3[i] <- 0}else{
    if (d2$gp3_age_2[i]>=75) {d2$YPLL_gp3[i] <- 0} else{ 
      if (d2$gp3_age_2[i]<75){d2$YPLL_gp3[i] <- 75 - d2$gp3_age_2[i]
      }
    }
  }
}
summary(d2$YPLL_gp3)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.000   0.000   0.000   6.715  10.000  50.000     161

  #4th dead grandparent
d2$YPLL_gp4 <- NULL
for(i in 1:nrow(d2)){
  if (is.na(d2$gp4_age_2[i])){d2$YPLL_gp4[i] <- 0}else{
    if (d2$gp4_age_2[i]>=75) {d2$YPLL_gp4[i] <- 0} else{ 
      if (d2$gp4_age_2[i]<75){d2$YPLL_gp4[i] <- 75 - d2$gp4_age_2[i]
      }
    }
  }
}
summary(d2$YPLL_gp4)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.000   0.000   0.000   5.897   7.250  51.000     264

  ### Creation of sum of YPLL var - only works without NA
d2$YPLL_sum <- NA
for (i in 1:nrow(d2)){
  d2$YPLL_sum[i] <- d2$YPLL_p1[i] + d2$YPLL_p2[i] + d2$YPLL_gp1[i] + d2$YPLL_gp2[i] + d2$YPLL_gp3[i] + d2$YPLL_gp4[i]} 
summary(d2$YPLL_sum)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    7.00   25.00   32.24   48.00  208.00 
hist(d2$YPLL_sum)  
hist(log(d2$YPLL_sum))

    ####### PATIENCE SCORE

######################################
############# MODELS #################
######################################

  ######## Looking after health

lm_health1 <- glm(d2$look_after_health_1 ~ log(d2$YPLL_sum))
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

  
# Preparation of the data from the study 'Premature mortality and timing of your life: 
# An exploratory correlational study' for analysis
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
#render("1-example.Rmd")
#library(MASS)


# setwd("/Users/monou/Nextcloud/Family history questionnaire/Data analysis") # Mac France Mona
# setwd("/Users/Monouille/Nextcloud/Shared/HSI/Family history questionnaire/Data analysis/FamHist") # Macbook Mona

d <- read.csv("Qualtrics_fulldata.csv")                           # Read Qualtrics data
p <- read.csv("Prolific_fulldata.csv")                            # Read Prolific data
names(p)[names(p) == "participant_id"] <- "prolific_id"           # Rename ID in Prolific data for merging

d_merged <- merge(d,p,by="prolific_id")                           # Merging both datasets ; N = 632

###### CLEANING ######

d_merged$StartDate <- NULL
d_merged$EndDate <- NULL
d_merged$Status <- NULL
d_merged$IPAddress <- NULL
d_merged$ResponseId <- NULL
d_merged$RecipientLastName <- NULL
d_merged$RecipientFirstName <- NULL
d_merged$RecipientEmail <- NULL
d_merged$ExternalReference <- NULL
d_merged$LocationLatitude <- NULL
d_merged$LocationLongitude <- NULL
d_merged$DistributionChannel <- NULL
d_merged$prolific_id...Topics <- NULL
d_merged$prolific_id...Parent.Topics <- NULL
d_merged$session_id <- NULL
d_merged$started_datetime <- NULL
d_merged$completed_date_time <- NULL
d_merged$reviewed_at_datetime <- NULL
d_merged$entered_code <- NULL

names(d_merged)[names(d_merged) == "Q85_1"] <- "env_transport"
names(d_merged)[names(d_merged) == "Q60_1"] <- "extrinsic_risk"

####### SORTING #######

  ### Removing participants who didn't give consent
d2 <- d_merged %>% filter(consent=="Yes, I consent to participate.")              # N = 630

  ### Removing data of participants who didn't finish the survey
d2 <- d2 %>% filter(Finished=="True")                                             # N = 612

  ### Removing data of participants who didn't give the same smoking status twice
d2 <- d2 %>% filter(smoker==attention_smoker)                                     # N = 608

  ### Removing data of participants who failed two attention checks
d2 <- d2 %>% filter(!(attention_4_1 !=4 & attention_fruit!="Strongly agree"))     # N = 605

  ### Removing data of participants with a Prolific score <95
d2 <- d2 %>% filter(prolific_score>=95)                                           # N = 602

  ### Removing data of participants who gave a significantly different age on Prolific and on Qualtrics
# d2$age.x <- as.numeric(d2$age.x)
# d2$age.y <- as.numeric(d2$age.y)
d2 <- d2 %>% filter(age.x == age.y | age.x == age.y+1 | age.x == age.y - 1)       # N = 590

  ### Removing data of participants who gave a different gender on Prolific and on Qualtrics
d2 <- d2 %>% filter(!(gender=="Male" & Sex=="Female") | (gender=="Female" & Sex=="Male")) # N = 589

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

d2 <- d2 %>% mutate(across(contains("age_2"), as.numeric))

      ### Removing data of participants who answered 3 times that their gp died before the age of 25
count(d2 %>% filter(gp1_age_2 <25 & gp2_age_2<25 & gp3_age_2<25))       # 7
# It actually doesn't give the number of rows unless I use nrow() or count()

d2 <- d2[-c(which(d2$gp1_age_2<25 & d2$gp2_age_2<25 & d2$gp3_age_2<25)),]           # N = 582
# d2 <- d2 %>%
#   mutate(drop = gp1_age_2<25 & d2$gp2_age_2<25 & d2$gp3_age_2<25) %>%
#   filter(drop == FALSE | is.na(drop)) %>%
#   select(-drop)

count(d2 %>% filter(parent1_age_2<25 & gp1_age_2<25 & gp2_age_2<25))     #0
count(d2 %>% filter(parent1_age_2<25 & parent2_age_2<25 & gp1_age_2<25)) #0

  ### Removing data of participants who answered twice that their relatives died before the age of 25 and failed an attention test
count(d2 %>% filter(gp1_age_2<25 & gp2_age_2<25 & attention_4_1 !="4" ))                   #0
count(d2 %>% filter(gp1_age_2<25 & gp2_age_2<25 & attention_fruit!="Strongly agree"))      #0
count(d2 %>% filter(gp1_age_2<25 & gp3_age_2<25 & attention_4_1 !="4" ))                   #1
d2 <- d2[-c(which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" )),]               # N = 581

nrow(d2[which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp1_age_2<25 & d2$gp4_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp1_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" ),])                   #1
d2 <- d2[-c(which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_4_1 !="4" )),]               # N = 580

nrow(d2[which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp3_age_2<25 & d2$gp4_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp3_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$gp2_age_2<25 & d2$gp4_age_2<25 & d2$attention_4_1 !="4" ),])                   #0
nrow(d2[which(d2$gp2_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
nrow(d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$attention_4_1 !="4"),])                #0
nrow(d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$attention_fruit!="Strongly agree"),])  #0
nrow(d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_4_1 !="4" ),])           #1
d2 <- d2[-c(which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_4_1 !="4" )),]       # N = 579

nrow(d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_fruit !="Strongly agree" ),])  #0
nrow(d2[which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4_1 !="4" ),])               #1
d2 <- d2[-c(which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4_1 !="4")),]            # N = 578

nrow(d2[which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_fruit!="Strongly agree"),])  #0

summary(d2$parent1_age_2)
summary(d2$parent2_age_2)
summary(d2$gp1_age_2)
summary(d2$gp2_age_2)
summary(d2$gp3_age_2)
summary(d2$gp4_age_2)

d2 %>% 
  summarise(across(contains("age_2"), ~mean(.x, na.rm = TRUE)))

  ### Removing data of participants who were very quick and failed an attentional task
d2$Duration..in.seconds. <- as.numeric(d2$Duration..in.seconds.)
mean(d2$Duration..in.seconds.)-sd(d2$Duration..in.seconds.)     # mean - 1 sd = 139.4 seconds
nrow(d2[which(d2$attention_4_1 !="4" & d2$Duration..in.seconds. < mean(d2$Duration..in.seconds.)-sd(d2$Duration..in.seconds.)),]) #1
d2 <- d2[-c(which(d2$attention_4_1 !="4" & d2$Duration..in.seconds. 
                  < mean(d2$Duration..in.seconds.)-sd(d2$Duration..in.seconds.))),]             # N = 577

  ### Removing data of participants who were extremely long and failed an attentional task
mean(d2$Duration..in.seconds.)+3*sd(d2$Duration..in.seconds.)   # mean + 3 sd = 1486.2 seconds
nrow(d2[which(d2$attention_4_1 !="4" & d2$Duration..in.seconds. > mean(d2$Duration..in.seconds.)+3*sd(d2$Duration..in.seconds.)),]) #0

  ### Removing data of participants who didn't say whether their parents or grandparents were alive or not
table(d2$parents_dead)
nrow(d2[which(d2$parents_dead=="Rather not say"),]) #1
d2 <- d2[-c(which(d2$parents_dead=="Rather not say")),]                                         # N = 576
nrow(d2[which(d2$gp_dead==""),]) #10
d2 <- d2[-c(which(d2$gp_dead=="")),]                                                            # N = 566
nrow(d2[which(d2$gp_dead=="Rather not say"),]) #3
d2 <- d2[-c(which(d2$gp_dead=="Rather not say")),]                                              # N = 563

  ### Ejections to add: lack of variability in answers
d3 <- d2[which(d2$status=="REJECTED"),] #2 

###### Recoding of character variables into numeric variables #####

  ### Recode number of deaths
table(d2$parents_dead)
d2 <- d2 %>% mutate(
  parents_dead = case_when(
    parents_dead == "Yes" ~ 0,
    parents_dead == "No: one is dead" ~ 1,
    parents_dead == "No: both are dead" ~ 2),
) 

d2$parents_dead <- as.numeric(d2$parents_dead)
table(d2$parents_dead)
summary(d2$parents_dead)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.0000  0.7125  1.0000  2.0000       3 

table(d2$gp_dead)
d2 <- d2 %>% mutate(
  gp_dead = case_when(
    gp_dead == "Yes, they are all alive." ~ 0,
    gp_dead == "No, one is dead" ~ 1,
    gp_dead == "No, two are dead." ~ 2,
    gp_dead == "No, three are dead." ~ 3,
    gp_dead == "No, all four are dead." ~ 4),
)
d2$gp_dead <- as.numeric(d2$gp_dead)
table(d2$gp_dead)
summary(d2$gp_dead)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.000   3.000   4.000   3.401   4.000   4.000 

  ### Recoding of gender
# table(d2$gender)
# d2 <- d2 %>% mutate(      # Actually, we can keep "Male" and "Female", maybe that's clearer
#   gender = case_when(
#     gender == "Female"~ 0,
#     gender == "Male" ~ 1),
# )
d2$gender <- as.factor((d2$gender))
table(d2$gender)

  ### Recoding of interest in looking after health var
d2$look_after_health_1 <- as.numeric(d2$look_after_health_1)
summary(d2$look_after_health_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    65.0    75.0    72.7    85.0   100.0 

  ### Recoding of checkup - maybe choose a number lower than the maximum of the given range?
table(d2$checkup)
d2 <- d2 %>% mutate(
  checkup = case_when(
    checkup == "Within the past year" ~ 1,
    checkup == "Within the last 2 years" ~ 2,
    checkup == "Within the last 3 years" ~ 3,
    checkup == "Within the last 5 years" ~ 5,
    checkup == "Within the last 10 years" ~ 10,
    checkup == "10 years ago or more" ~ 15),
)
d2$checkup <- as.numeric(d2$checkup)
table(d2$checkup)
summary(d2$checkup)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   2.000   3.827   5.000  15.000      26 

  ### Recoding of smoker status
table(d2$smoker)
d2 <- d2 %>% mutate(
  smoker = case_when(
    smoker == "No" ~ 0,
    smoker == "Yes" ~ 1),
)
d2$smoker <- as.factor(d2$smoker)
table(d2$smoker)
# nrow(d2[which(d2$smoker==1),]) #58 (/564)
# summary(d2$smoker) #10,28% of smokers

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
table(d2$children)
d2 <- d2 %>% mutate(
  children = case_when(
    children == "No" ~ 0,
    children == "Yes" ~ 1),
)
d2$children <- as.factor(d2$children)
table(d2$children)
#nrow(d2[which(d2$children==1),]) #312 (/563)
#summary(d2$children) #55,5% of participants with children

  ### Recoding of age at first child var for participants w/ children
# table(d2$age_first_child_1)
d2$age_first_child_1[which(d2$age_first_child_1=="<16")] <- 15 # arbitrary
d2$age_first_child_1 <- as.numeric(d2$age_first_child_1)
summary(d2$age_first_child_1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   15.00   23.00   28.00   27.67   32.00   51.00     251

  ### Recoding of history of breastfeeding
table(d2$breastfeed_yesno)
d2 <- d2 %>% mutate(
  breastfeed_yesno = case_when(
    breastfeed_yesno =="No" ~ 0,
    breastfeed_yesno =="Yes" ~ 1),
)
d2$breastfeed_yesno <- as.factor(d2$breastfeed_yesno)
table(d2$breastfeed_yesno)
#d3<-d2[which(d2$breastfeed_yesno==1),] #113 (/564)
#(nrow(d2[which(d2$breastfeed_yesno==1),]))/(nrow(d2[which(d2$children==1 & d2$gender==0),])) #64,6% of women with children breastfed their 1st child
#(nrow(d2[which(!is.na(d2$breastfeed_yesno)),]))/nrow(d2) #30,9% of the participants answered the breastfeed question

  ### Recoding of breastfeed length
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

  ### Change of control and closeness var to numeric class
d2 <- d2 %>% mutate(across(contains("control"), as.numeric))
d2 <- d2 %>% mutate(across(contains("close"), as.numeric))
class(d2$gp1_control_1)

  ### Recoding of stress variable: from categorical to numeric
table(d2$stress)
d2 <- d2 %>% mutate(
  stress = case_when(
    stress == "Never" ~ 0,
    stress == "Very rarely" ~ 1,
    stress == "Rarely" ~ 2,
    stress == "Occasionally" ~ 3,
    stress == "Frequently" ~ 4,
    stress == "Very frequently" ~ 5),
)
table(d2$stress)
d2$stress <- as.numeric(d2$stress)

  ### Recoding of income
table(d2$income_1)
plot(table(d2$income_1),lwd=30,type="h")
d2 <- d2 %>% mutate(
  income_1 = case_when(
    income_1  == "£10,000 - £15,999" ~ (10000+15999)/2,
    income_1  == "£16,000 - £19,999" ~ (16000+19999)/2,
    income_1  == "£20,000 - £29,999" ~ (20000+29999)/2,
    income_1  == "£30,000 - £39,999" ~ (30000+39999)/2,
    income_1  == "£40,000 - £49,999" ~ (40000+49999)/2,
    income_1  == "£50,000 - £59,999" ~ (50000+59999)/2,
    income_1  == "£60,000 - £74,999" ~ (60000+74999)/2,
    income_1  == "£75,000 - £99,999" ~ (75000+99999)/2,
    income_1  == "£100,000 - £149,999" ~ (100000+149999)/2,
    income_1  == "More than £150,000" ~ 175000, #arbitrary
    income_1  == "Less than £10,000" ~ 7500),  #arbitrary
) 
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
d2 <- d2 %>% mutate(
  personal_income = income_1 / household_1
)
summary(d2$personal_income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1250    9000   13750   16821   22500   87500

hist(log(d2$personal_income))

nrow(d2[which(d2$personal_income < mean(d2$personal_income)-sd(d2$personal_income)),])    # 72 declare an annual income of less than £6188
nrow(d2[which(d2$personal_income < 3000 & d2$attention_4_1 != "4"),])           #4 - to delete?

  ### Creation of var subj SES

d2 <- d2 %>% mutate(
  SES_subj = case_when(
    subjective_SES_1  == "On" ~ 10,
    subjective_SES_2  == "On" ~ 9,
    subjective_SES_3  == "On" ~ 8,
    subjective_SES_4  == "On" ~ 7,
    subjective_SES_5  == "On" ~ 6,
    subjective_SES_6  == "On" ~ 5,
    subjective_SES_7  == "On" ~ 4,
    subjective_SES_8  == "On" ~ 3,
    subjective_SES_9  == "On" ~ 2,
    subjective_SES_10  == "On" ~ 1),
) 
summary(d2$SES_subj)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    5.00    6.00    5.52    7.00   10.00 
hist(d2$SES_subj) # distribution more normal than personal_income var
d2$subjective_SES_1 <- NULL
d2$subjective_SES_2 <- NULL
d2$subjective_SES_3 <- NULL
d2$subjective_SES_4 <- NULL
d2$subjective_SES_5 <- NULL
d2$subjective_SES_6 <- NULL
d2$subjective_SES_7 <- NULL
d2$subjective_SES_8 <- NULL
d2$subjective_SES_9 <- NULL
d2$subjective_SES_10 <- NULL

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
summary(d2$YPLL_p1) # with NA
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
summary(d2$YPLL_p2) #with NA
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
summary(d2$YPLL_gp1) #with NA
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

    ####### PATIENCE SCORE

d2 <- d2 %>% mutate(
  patience_score = case_when(
    Q23  == "Today" ~ 1,
    Q23  == "In 12 months" ~ 2,
    Q24  == "Today" ~ 3,
    Q24  == "In 12 months" ~ 4,
    Q20  == "Today" ~ 5,
    Q20  == "In 12 months" ~ 6,
    Q21  == "Today" ~ 7,
    Q21  == "In 12 months" ~ 8,
    Q31  == "Today" ~ 9,
    Q31  == "In 12 months" ~ 10,
    Q30  == "Today" ~ 11,
    Q30  == "In 12 months" ~ 12,
    Q28  == "Today" ~ 13,
    Q28  == "In 12 months" ~ 14,
    Q27  == "Today" ~ 15,
    Q27  == "In 12 months" ~ 16,
    Q16  == "Today" ~ 17,
    Q16  == "In 12 months" ~ 18,
    Q15  == "Today" ~ 19,
    Q15  == "In 12 months" ~ 20,
    Q13  == "Today" ~ 21,
    Q13  == "In 12 months" ~ 22,
    Q12  == "Today" ~ 23,
    Q12  == "In 12 months" ~ 24,
    Q8  == "Today" ~ 25,
    Q8  == "In 12 months" ~ 26,
    Q9  == "Today" ~ 27,
    Q9  == "In 12 months" ~ 28,
    Q6  == "Today" ~ 29,
    Q6  == "In 12 months" ~ 30,
    Q5  == "Today" ~ 31,
    Q5  == "In 12 months" ~ 32),
) 
summary(d2$patience_score)

d2$Q1 <- NULL
d2$Q2 <- NULL
d2$Q3 <- NULL
d2$Q4 <- NULL
d2$Q5 <- NULL
d2$Q6 <- NULL
d2$Q7 <- NULL
d2$Q8 <- NULL
d2$Q9 <- NULL
d2$Q10 <- NULL
d2$Q11 <- NULL
d2$Q12 <- NULL
d2$Q13 <- NULL
d2$Q14 <- NULL
d2$Q15 <- NULL
d2$Q16 <- NULL
d2$Q17 <- NULL
d2$Q18 <- NULL
d2$Q19 <- NULL
d2$Q20 <- NULL
d2$Q21 <- NULL
d2$Q22 <- NULL
d2$Q23 <- NULL
d2$Q24 <- NULL
d2$Q25 <- NULL
d2$Q26 <- NULL
d2$Q27 <- NULL
d2$Q28 <- NULL
d2$Q29 <- NULL
d2$Q30 <- NULL
d2$Q31 <- NULL

    ######### Creation of the final data table ########

write.table(d2,"data_famhist.txt",dec=".",sep="\t", row.names = F)

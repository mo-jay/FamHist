# Preparation of the data from the study 'Premature mortality and timing of your life: 
# An exploratory correlational study' for analysis
# Mona Joly and colleagues
# 25/05/21

rm(list=ls())

### Loading of required packages ####

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(tidylog)){
  install.packages("tidylog")
  library(tidylog)
}                           # to get output msgs like in Stata
if(!require(rmarkdown)){
  install.packages("rmarkdown")
  library(rmarkdown)
}
if(!require(e1071)){
  install.packages("e1071")
  library(e1071)
}                           # to calculate skewness
# if(!require(dlookr)){
#   install.packages("dlookr")
#   library(dlookr)
# }                         # to transform data
if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}                           # for density Plots
if(!require(car)){
  install.packages("car")
  library(car)
}                           # for qqPlots

#render("1-example.Rmd")    # Supposedly for Rmarkdown

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

  ### Renaming of some columns
names(d_merged)[names(d_merged) == "Duration..in.seconds."] <- "Duration"
names(d_merged)[names(d_merged) == "look_after_health_1"] <- "look_after_health"
names(d_merged)[names(d_merged) == "Q85_1"] <- "env_transport"
names(d_merged)[names(d_merged) == "Q60_1"] <- "extrinsic_risk"
names(d_merged)[names(d_merged) == "Ethnicity..Simplified."] <- "ethnicity"
d_merged$ethnicity[which(d_merged$ethnicity=="CONSENT REVOKED")] <- NA
table(d_merged$ethnicity)

names(d_merged)[names(d_merged) == "age_first_child_1"] <- "age_first_child"
names(d_merged)[names(d_merged) == "breastfeed_length_1"] <- "breastfeed_length"
names(d_merged)[names(d_merged) == "ideal_age_1"] <- "ideal_age"
names(d_merged)[names(d_merged) == "household_1"] <- "household"
names(d_merged)[names(d_merged) == "income_1"] <- "income"
names(d_merged)[names(d_merged) == "household_1"] <- "household"
names(d_merged)[names(d_merged) == "attention_4_1"] <- "attention_4"

####### SORTING #######

  ### Removing participants who didn't give consent
d2 <- d_merged %>% filter(consent=="Yes, I consent to participate.")              # N = 630

  ### Removing data of participants who didn't finish the survey
d2 <- d2 %>% filter(Finished=="True")    

#### N = 612 participants recruited

  ### Removing data of participants who didn't give the same smoking status twice
d2 <- d2 %>% filter(smoker==attention_smoker)                                     # N = 608

  ### Removing data of participants who failed two attention checks
d2 <- d2 %>% filter(!(attention_4 !=4 & attention_fruit!="Strongly agree"))       # N = 605

  ### Removing data of participants with a Prolific score <95 who failed the attention check
d2 <- d2 %>% filter(!(prolific_score<95 & attention_4 !=4))                       # N = 603

  ### Removing data of participants who gave a significantly different age on Prolific and on Qualtrics
# d2$age.x <- as.numeric(d2$age.x)
# d2$age.y <- as.numeric(d2$age.y)
d2 <- d2 %>% filter(age.x == age.y | age.x == age.y+1 | age.x == age.y - 1)       # N = 591

d2$age.y <- NULL
names(d2)[names(d2) == "age.x"] <- "age"

  ### Removing data of participants who gave a different gender on Prolific and on Qualtrics
d2 <- d2 %>% filter(!(gender=="Male" & Sex=="Female") | (gender=="Female" & Sex=="Male")) # N = 590
d2$Sex <- NULL

  ### Removing data of participants who were very quick and failed an attentional task
d2$Duration <- as.numeric(d2$Duration)
mean(d2$Duration)-sd(d2$Duration)     # mean - 1 sd = 139.2 seconds
nrow(d2[which(d2$attention_4 !="4" & d2$Duration < mean(d2$Duration)-sd(d2$Duration)),]) #1
d2 <- d2[-c(which(d2$attention_4 !="4" & d2$Duration 
                  < mean(d2$Duration)-sd(d2$Duration))),]             # N = 589

  ### Removing data of participants who were extremely long and failed an attentional task
# mean(d2$Duration)+3*sd(d2$Duration)   # mean + 3 sd = 1487 seconds
# nrow(d2[which(d2$attention_4 !="4" & d2$Duration > mean(d2$Duration)+3*sd(d2$Duration)),]) #0

##### 588 participants after the basic exclusions
612-589 # 23 excluded

#d2[which(d2$parent1_age_2 == 19 | d2$parent2_age_2 == 19 | d2$gp1_age_2 == 19 | d2$gp2_age_2 == 19 | d2$gp3_age_2 == 19 | d2$gp4_age_2 == 19),]

### Removing data of participants who did not understand we asked for grandparents age at death and not their own

### Recoding of parental and grandparental age at death

d2$parent1_age_2[which(d2$parent1_age_2=="<20")] <- 19
d2$parent2_age_2[which(d2$parent2_age_2=="<20")] <- 19
d2$gp1_age_2[which(d2$gp1_age_2=="<20")] <- 19
d2$gp2_age_2[which(d2$gp2_age_2=="<20")] <- 19
d2$gp3_age_2[which(d2$gp3_age_2=="<20")] <- 19
d2$gp4_age_2[which(d2$gp4_age_2=="<20")] <- 19

#d2[which(d2$parent1_age_2 == 19 | d2$parent2_age_2 == 19 | d2$gp1_age_2 == 19 | d2$gp2_age_2 == 19 | d2$gp3_age_2 == 19 | d2$gp4_age_2 == 19),]


d2$parent1_age_2[which(d2$parent1_age_2==">90")] <- 91
d2$parent2_age_2[which(d2$parent2_age_2=="90")] <- 91
d2$gp1_age_2[which(d2$gp1_age_2=="90")] <- 91
d2$gp2_age_2[which(d2$gp2_age_2=="90")] <- 91
d2$gp3_age_2[which(d2$gp3_age_2=="90")] <- 91
d2$gp4_age_2[which(d2$gp4_age_2=="90")] <- 91

d2 <- d2 %>% mutate(across(contains("age_2"), as.numeric))

### Removing data of participants who answered 3 times that their gp died before the age of 25
# count(d2 %>% filter(gp1_age_2 <25 & gp2_age_2<25 & gp3_age_2<25))       # 7
# It actually doesn't give the number of rows unless I use nrow() or count()

# d2 <- d2[-c(which(d2$gp1_age_2<25 & d2$gp2_age_2<25 & d2$gp3_age_2<25)),]           # N = 582
# d2 <- d2 %>%
#   mutate(drop = gp1_age_2<25 & d2$gp2_age_2<25 & d2$gp3_age_2<25) %>%
#   filter(drop == FALSE | is.na(drop)) %>%
#   select(-drop)

# count(d2 %>% filter(parent1_age_2<25 & gp1_age_2<25 & gp2_age_2<25))     #0
# count(d2 %>% filter(parent1_age_2<25 & parent2_age_2<25 & gp1_age_2<25)) #0

### Removing data of participants who answered twice that their relatives died before the age of 25 and failed an attention test
# count(d2 %>% filter(gp1_age_2<25 & gp2_age_2<25 & attention_4 !="4" ))                   #0
# count(d2 %>% filter(gp1_age_2<25 & gp2_age_2<25 & attention_fruit!="Strongly agree"))      #0
# count(d2 %>% filter(gp1_age_2<25 & gp3_age_2<25 & attention_4 !="4" ))                   #1
# d2 <- d2[-c(which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_4 !="4" )),]               # N = 581
# 
# nrow(d2[which(d2$gp1_age_2<25 & d2$gp3_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
# nrow(d2[which(d2$gp1_age_2<25 & d2$gp4_age_2<25 & d2$attention_4 !="4" ),])                   #0
# nrow(d2[which(d2$gp1_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
# nrow(d2[which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_4 !="4" ),])                   #1
# d2 <- d2[-c(which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_4 !="4" )),]               # N = 580
# 
# nrow(d2[which(d2$gp2_age_2<25 & d2$gp3_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
# nrow(d2[which(d2$gp3_age_2<25 & d2$gp4_age_2<25 & d2$attention_4 !="4" ),])                   #0
# nrow(d2[which(d2$gp3_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
# nrow(d2[which(d2$gp2_age_2<25 & d2$gp4_age_2<25 & d2$attention_4 !="4" ),])                   #0
# nrow(d2[which(d2$gp2_age_2<25 & d2$gp4_age_2<25 & d2$attention_fruit!="Strongly agree"),])      #0
# nrow(d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$attention_4 !="4"),])                #0
# nrow(d2[which(d2$parent1_age_2<25 & d2$gp1_age_2<25 & d2$attention_fruit!="Strongly agree"),])  #0
# nrow(d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_4 !="4" ),])           #1
# d2 <- d2[-c(which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_4 !="4" )),]       # N = 579
# 
# nrow(d2[which(d2$parent1_age_2<25 & d2$parent2_age_2<25 & d2$attention_fruit !="Strongly agree" ),])  #0
# nrow(d2[which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4 !="4" ),])               #1
# d2 <- d2[-c(which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_4 !="4")),]            # N = 578, 583 without first triage
# 
# nrow(d2[which(d2$parent1_age_2<25 & d2$gp2_age_2<25 & d2$attention_fruit!="Strongly agree"),])  #0

summary(d2$parent1_age_2)
summary(d2$parent2_age_2)
summary(d2$gp1_age_2)
summary(d2$gp2_age_2)
summary(d2$gp3_age_2)
summary(d2$gp4_age_2)

d2 %>% 
  summarise(across(contains("age_2"), ~mean(.x, na.rm = TRUE)))                                             # N = 563

  ### Ejections to add: lack of variability in answers
d3 <- d2[which(d2$status=="REJECTED"),] #8 

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

  ### Recoding of checkup - maybe choose a number lower than the maximum of the given range?
table(d2$checkup)
class(d2$checkup)
### Checkup
# 215/551 (39%) had their last checkup within a year.
# 353/582 (64%) within the last 2 years
# 198 (36%) in a longer time
# d2 <- d2 %>% mutate(
#   checkup = case_when(
#     checkup == "Within the past year" ~ 0.5,
#     checkup == "Within the last 2 years" ~ 1.5,
#     checkup == "Within the last 3 years" ~ 2.5,
#     checkup == "Within the last 5 years" ~ 4,
#     checkup == "Within the last 10 years" ~ 7.5,
#     checkup == "10 years ago or more" ~ 12),
# )
d2 <- d2 %>% mutate(
  checkup = case_when(
    checkup =="Within the last 3 years" | checkup == "Within the last 5 years" | 
      checkup =="Within the last 10 years" | checkup == "10 years ago or more" ~ "2 years ago or more",
    checkup == "Don't know" | checkup == "Rather not say" ~ NA_character_,
    TRUE ~ as.character(checkup)),
)
#d2$checkup <- as.numeric(d2$checkup)
table(d2$checkup)
# d2$checkup <- as.factor(d2$checkup)
# 2 years ago or more Within the last 2 years    Within the past year 
# 198                     138                     215

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
# table(d2$age_first_child)
d2$age_first_child[which(d2$age_first_child=="<16")] <- 15 # arbitrary
d2$age_first_child <- as.numeric(d2$age_first_child)
summary(d2$age_first_child)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   15.00   23.00   28.00   27.66   32.00   51.00     251

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
summary(d2$breastfeed_length)
d2 <- d2 %>% mutate(
  breastfeed_length = case_when(
    breastfeed_yesno ==0 ~ 0,
    TRUE ~ as.numeric(breastfeed_length)),
)
d2$breastfeed_length <- as.numeric(d2$breastfeed_length)
summary(d2$breastfeed_length)
count(d2 %>% filter(breastfeed_length ==0))/(582-402) # 34% didn't exclusively breastfeed their 1st child at all
count(d2 %>% filter(breastfeed_length > 0 & breastfeed_length < 6))/(582-402) #28% exclusively breastfed their child for less than 6 months
count(d2 %>% filter(breastfeed_length >= 6))/(582-402) # 37% breastfed their 1st child for 6 months or more

d2 <- d2 %>% mutate(
  breastfeed_length = case_when(
    breastfeed_length ==0 ~ "No breastfeeding",
    breastfeed_length > 0 & breastfeed_length < 6 ~ "Less than 6 months",
    breastfeed_length >= 6 ~ "6 months or more"),
)
table(d2$breastfeed_length)
# 6 months or more Less than 6 months   No breastfeeding 
#               67                 51                 62
# d2$breastfeed_length <- as.factor(d2$breastfeed_length)

  ### Recoding of ideal age var for participants w/o children
# table(d2$ideal_age)
d2$ideal_age[which(d2$ideal_age=="<16")] <- 15 #arbitrary
d2$ideal_age <- as.numeric(d2$ideal_age)
summary(d2$ideal_age)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   15.00   27.00   30.00   29.86   32.00   45.00     314  

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

  ### Recoding of income var
table(d2$income)
# plot(table(d2$income),lwd=30,type="h")
d2 <- d2 %>% mutate(
  income = case_when(
    income  == "£10,000 - £15,999" ~ (10000+15999)/2,
    income  == "£16,000 - £19,999" ~ (16000+19999)/2,
    income  == "£20,000 - £29,999" ~ (20000+29999)/2,
    income  == "£30,000 - £39,999" ~ (30000+39999)/2,
    income  == "£40,000 - £49,999" ~ (40000+49999)/2,
    income  == "£50,000 - £59,999" ~ (50000+59999)/2,
    income  == "£60,000 - £74,999" ~ (60000+74999)/2,
    income  == "£75,000 - £99,999" ~ (75000+99999)/2,
    income  == "£100,000 - £149,999" ~ (100000+149999)/2,
    income  == "More than £150,000" ~ 175000, #arbitrary
    income  == "Less than £10,000" ~ 7500),  #arbitrary
) 
table(d2$income)
d2$income <- as.numeric(d2$income)
summary(d2$income)

### Numerisation of remaining variables
d2 <- d2 %>% mutate(across(contains("control"), as.numeric))
d2 <- d2 %>% mutate(across(contains("close"), as.numeric))
class(d2$gp1_control_1)

lapply(d2, class)
cols.num <- c("age","look_after_health","environment_1","env_transport","age_first_child","household","attention_4")
d2[cols.num] <- sapply(d2[cols.num],as.numeric)
sapply(d2[cols.num],class)

#####################################
##### Creation of new variables #####
#####################################
  
  ### creation of var personal annual income
d2 <- d2 %>% mutate(
  personal_income = income / household
)
d2$personal_income <- as.numeric(d2$personal_income)
summary(d2$personal_income)
class(d2$personal_income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1250    9000   13750   16821   22500   87500

# hist(log(d2$personal_income))

nrow(d2[which(d2$personal_income < mean(d2$personal_income)-sd(d2$personal_income)),])    # 72 declare an annual income of less than £6188
nrow(d2[which(d2$personal_income < 3000 & d2$attention_4 != "4"),])           #4 - to delete?

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
class(d2$SES_subj)
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
d2 <- d2 %>% mutate(
  YPLL_p1 = case_when(
    parents_dead == 0 ~ 0,
    parent1_age_2 >= 75 ~ 0,
    parent1_age_2 < 75 ~ 75 - parent1_age_2),
)
# d2 %>% filter(is.na(YPLL_p1))
summary(d2$YPLL_p1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.000   0.000   0.000   5.224   3.000  56.000       4 

  # 2nd dead parent
d2 <- d2 %>% mutate(
  YPLL_p2 = case_when(
    parents_dead == 0 | parents_dead == 1 ~ 0,
    parent2_age_2 >= 75 ~ 0,
    parent2_age_2 < 75 ~ 75 - parent2_age_2),
)
# d2 %>% filter(is.na(YPLL_p2))
summary(d2$YPLL_p2) #with NA
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.00    0.00    0.00    1.41    0.00   50.00      12 

  # 1st dead grandparent
d2 <- d2 %>% mutate(
  YPLL_gp1 = case_when(
    gp_dead == 0 ~ 0,
    gp1_age_2 >= 75 ~ 0,
    gp1_age_2 < 75 ~ 75 - gp1_age_2),
)
# d2 %>% filter(is.na(YPLL_gp1))
summary(d2$YPLL_gp1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.00    0.00    8.00   12.07   20.00   56.00      37 

  #2nd dead grandparent
d2 <- d2 %>% mutate(
  YPLL_gp2 = case_when(
    gp_dead == 0 | gp_dead == 1 ~ 0,
    gp2_age_2 >= 75 ~ 0,
    gp2_age_2 < 75 ~ 75 - gp2_age_2),
)
d2 %>% filter(is.na(YPLL_gp2))
summary(d2$YPLL_gp2)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.000   0.000   0.000   7.353  10.000  56.000      67

  #3rd dead grandparent
d2 <- d2 %>% mutate(
  YPLL_gp3 = case_when(
    gp_dead == 0 | gp_dead == 1 | gp_dead == 2 ~ 0,
    gp3_age_2 >= 75 ~ 0,
    gp3_age_2 < 75 ~ 75 - gp3_age_2),
)
d2 %>% filter(is.na(YPLL_gp3))
summary(d2$YPLL_gp3)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.000   0.000   0.000   5.348   7.000  50.000      57

  #4th dead grandparent
d2 <- d2 %>% mutate(
  YPLL_gp4 = case_when(
    gp_dead == 0 | gp_dead == 1 | gp_dead == 2 | gp_dead == 3 ~ 0,
    gp4_age_2 >= 75 ~ 0,
    gp4_age_2 < 75 ~ 75 - gp4_age_2),
)
d2 %>% filter(is.na(YPLL_gp4))
summary(d2$YPLL_gp4)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.00    0.00    0.00    3.74    2.00   51.00      90

  ### Creation of sum of YPLL var
d2 <- d2 %>% mutate(
  YPLL_sum = YPLL_p1 + YPLL_p2 + YPLL_gp1 + YPLL_gp2 + YPLL_gp3 + YPLL_gp4
)
d2 %>% filter(is.na(YPLL_sum))
summary(d2$YPLL_sum)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    9.00   28.00   37.04   50.75  334.00     184  

  ### Creation of a dummy YPLL_sum var
d2 <- d2 %>% mutate(
  YPLL_dummy = case_when(
    YPLL_sum == 0 ~ 0,
    YPLL_sum > 0 ~ 1),
)
summary(d2$YPLL_dummy)

  ### Creation of sum of deaths in the family
d2 <- d2 %>% mutate(
  n_deaths = parents_dead + gp_dead
)
d2 %>% filter(is.na(n_deaths))
table(d2$n_deaths)
summary(d2$n_deaths)
hist(d2$n_deaths)
count(d2 %>% filter(n_deaths <= 3))/(582-17) # 29% had 3 deaths or less in their family
count(d2 %>% filter(n_deaths == 4))/(582-17) #26% had 4 deaths in their family
count(d2 %>% filter(n_deaths >= 5))/(582-17) # 45% had 5 to 6 deaths in their family

d2 <- d2 %>% mutate(
  n_deaths_cat = case_when(
    n_deaths <= 3 ~ "0-3 deaths",
    n_deaths == 4 ~ "4 deaths",
    n_deaths >= 5 ~ "5-6 deaths"),
)
table(d2$n_deaths_cat)

  ### Creation of sum of premature deaths within the family
d2 <- d2 %>% mutate(
  p1_prem = case_when(
    YPLL_p1 > 0 ~ 1,
    YPLL_p1 == 0 ~ 0),
  p2_prem = case_when(
    YPLL_p2 > 0 ~ 1,
    YPLL_p2 == 0 ~ 0),
  gp1_prem = case_when(
    YPLL_gp1 > 0 ~ 1,
    YPLL_gp1 == 0 ~ 0),
  gp2_prem = case_when(
    YPLL_gp2 > 0 ~ 1,
    YPLL_gp2 == 0 ~ 0),
  gp3_prem = case_when(
    YPLL_gp3 > 0 ~ 1,
    YPLL_gp3 == 0 ~ 0),
  gp4_prem = case_when(
    YPLL_gp4 > 0 ~ 1,
    YPLL_gp4 == 0 ~ 0),
)
summary(d2$p1_prem)
summary(d2$p2_prem)

d2 <- d2 %>% mutate(
  n_prem = p1_prem + p2_prem + gp1_prem + gp2_prem + gp3_prem + gp4_prem
)
summary(d2$n_prem)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.000   1.000   2.000   2.276   3.000   6.000     184

hist(d2$n_prem)
count(d2 %>% filter(n_prem <= 1))/(582-184) # 35% had 0 or 1 premature deaths in their family
count(d2 %>% filter(n_prem > 1 & n_prem < 4))/(582-184) #43% had 2 or 3 premature deaths in their family
count(d2 %>% filter(n_prem >= 4))/(582-184) # 21% had 4 to 6 premature deaths in their family

d2 <- d2 %>% mutate(
  n_prem_cat = case_when(
    n_prem <= 1 ~ "0-1 prem deaths",
    n_prem > 1 & n_prem < 4 ~ "2-3 prem deaths",
    n_prem >= 4 ~ "4-6 prem deaths"),
)
table(d2$n_prem_cat)

    ### Creation of youngest death experienced var
d2 <- d2 %>% mutate(
  youngest_death = min(parent1_age_2,parent2_age_2,gp1_age_2,gp2_age_2,gp3_age_2,gp4_age_2,na.rm=TRUE)
)
summary(d2$youngest_death)

for (i in 1:nrow(d2)){
  d2$youngest_death[i] <- pmin(d2$parent1_age_2[i],d2$parent2_age_2[i],d2$gp1_age_2[i],d2$gp2_age_2[i],d2$gp3_age_2[i],d2$gp4_age_2[i],na.rm=TRUE)
}
summary(d2$youngest_death)
hist(d2$youngest_death)
mean(d2$youngest_death,na.rm=TRUE)

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
hist(d2$patience_score)

d2 <- d2 %>% mutate(
  time_discounting = 33-patience_score
)
summary(d2$time_discounting)
hist(d2$time_discounting)

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

    #### CONTROLLABILITY
for (i in 1:nrow(d2)){
  d2$controllability[i] <- mean(c(d2$parent1_control_1[i],d2$parent2_control_1[i],d2$gp1_control_1[i],d2$gp2_control_1[i],d2$gp3_control_1[i],d2$gp4_control_1[i]), na.rm=TRUE)
}
summary(d2$controllability)
hist(d2$controllability)

    #### CLOSENESS
for (i in 1:nrow(d2)){
  d2$closeness[i] <- mean(c(d2$parent1_close_1[i],d2$parent2_close_1[i],d2$gp1_close_1[i],d2$gp2_close_1[i],d2$gp3_close_1[i],d2$gp4_close_1[i]), na.rm=TRUE)
}
summary(d2$closeness)
hist(d2$closeness)

  #### Data transformations ####

# d3 <- d2 %>% filter(YPLL_sum < mean(d2$YPLL_sum, na.rm = TRUE) + 3*sd(d2$YPLL_sum,na.rm=TRUE)) 
# removes 14 participants with a YPLL_sum > 121,8
# equivalent of having all relatives who died before the age of 55
# also removes the 184 NA --> 384 left

    ## Look_after_health var
hist(d2$look_after_health)
ggdensity(d2$look_after_health)
qqPlot(d2$look_after_health)
skewness(d2$look_after_health) #-1.28 --> highly negatively skewed
hist(-log(max(d2$look_after_health)-d2$look_after_health+1))
ggdensity(-log(max(d2$look_after_health)-d2$look_after_health+1))
d2$look_after_health_log <- -log(max(d2$look_after_health)-d2$look_after_health+1)
skewness(d2$look_after_health_log) #1.57, worse
qqPlot(d2$look_after_health_log)

d2$look_after_health_sqrt <- -sqrt(max(d2$look_after_health+1)-d2$look_after_health)
hist(d2$look_after_health_sqrt)
skewness(d2$look_after_health_sqrt)  #-0.17, great
qqPlot(d2$look_after_health_sqrt) # besser

# d2$look_after_health_sqrt <- transform(
#   d2$look_after_health,
#   method = "log")
# hist(d2$look_after_health_sqrt)
# skewness(d2$look_after_health_x2) #-0.38
# ggdensity(d2$look_after_health_x2)
# qqPlot(d2$look_after_health_x2)
# d2$look_after_health_x3 <- transform(
#   d2$look_after_health,
#   method = "x^3")
# hist(d2$look_after_health_x3)
# skewness(d2$look_after_health_x3) #0.18 the best
# ggdensity(d2$look_after_health_x3)
# qqPlot(d2$look_after_health_x3) # also looks better graphically


    ## YPLL_sum var
# hist(d2$YPLL_sum) 
# skewness(d2$YPLL_sum) #1.51 (before: 1.58) --> highly positively skewed
# hist(d3$YPLL_sum)
# d2$YPLL_sqrt <- transform(
#   d2$YPLL_sum,
#   method = "sqrt"
# )
# skewness(d2$YPLL_sqrt) #0.03, better
# hist(d2$YPLL_sqrt)
# ggdensity(d2$YPLL_sqrt) # looks great!
# qqPlot(d2$YPLL_sqrt)    # amazing
# 
# d3$YPLL_sqrt <- transform(
#   d3$YPLL_sum,
#   method = "sqrt"
# )
# skewness(d3$YPLL_sqrt) #0.03, better
# hist(d3$YPLL_sqrt)
# ggdensity(d3$YPLL_sqrt) # looks great!
# qqPlot(d3$YPLL_sqrt)    # amazing
# 
# d2$YPLL_log <- transform(
#   d2$YPLL_sum,
#   method = "log+1"
# )
# skewness(d2$YPLL_log) #-0.9, small improvement
# hist(d2$YPLL_log) 
# ggdensity(d2$YPLL_log)  # still a lot of 0
# qqPlot(d2$YPLL_log)     # looks terrible
# 
# d3$YPLL_log <- transform(
#   d3$YPLL_sum,
#   method = "log+1"
# )
# skewness(d3$YPLL_log) #-0.9, small improvement
# hist(d3$YPLL_log) 
# ggdensity(d3$YPLL_log)  # still a lot of 0
# qqPlot(d3$YPLL_log)     # looks terrible
# hist(log(max(d3$YPLL_sum)-d3$YPLL_sum+1))

#### For YPLL_sum, the best is to sqrt. Log+1 provides a little improvement.
#### Better to only use d3 for models with YPLL_sum bc of the crazy outliers

    ## age var
class(d2$age)
hist(d2$age)   # uniform distribution, don't really know what to do about it
skewness(d2$age) # 0.002, well at least it's symmetric
ggdensity(d2$age) # quite terrible. Can't see how to improve a uniform distrib.

    ## personal income var
d2$personal_income <- as.numeric(d2$personal_income)
hist(d2$personal_income)
skewness(d2$personal_income) #1.44
ggdensity(d2$personal_income)

# d2$income_sqrt <- transform(
#   d2$personal_income,
#   method = "sqrt"
# )
# skewness(d2$income_sqrt) #0.4
# ggdensity(d2$income_sqrt)
# qqPlot(d2$income_sqrt) # Not bad
# 
# d2$income_log <- transform(
#   d2$personal_income,
#   method = "log"
# )
# skewness(d2$income_log) #-0.52
# ggdensity(d2$income_log)
qqPlot(d2$income_log) # log is fine, let's use this one

    ## Subjective SES var
skewness(d2$SES_subj)
ggdensity(d2$SES_subj)
qqPlot(d2$SES_subj) # Not that bad? Transformations do not help anyway

    ## Extrinsic risk var
hist(d2$extrinsic_risk)
skewness(d2$extrinsic_risk)
# skewness(sqrt(d3$extrinsic_risk))
# skewness(log(d3$extrinsic_risk+1))
# ggdensity(sqrt(d2$extrinsic_risk))
# qqPlot(sqrt(d2$extrinsic_risk))
# d2$extrinsic_risk_sqrt <- transform(
#   d2$extrinsic_risk,
#   method = "sqrt"
# )
# d2$extrinsic_risk_log <- transform(
#   d2$extrinsic_risk,
#   method = "log+1"
# )
# ggdensity(d2$extrinsic_risk_log)
# ggdensity(d2$extrinsic_risk_sqrt)
# skewness(d2$extrinsic_risk_log)
# hist(d2$extrinsic_risk_log)
# hist(d2$extrinsic_risk_sqrt)

# The skewness of extrinsic_var is terrible with log. But graphically it looks
# just as fine as sqrt.

    ## Patience score var
hist(d2$patience_score) 
ggdensity(d2$patience_score) # kind of bimodal
# Maybe I can binarise it!
d2 <- d2 %>% mutate(
  patience_score_bi = case_when(
    patience_score  <16  ~ 0,
    patience_score  >=16  ~ 1),
) 
d2$patience_score_bi <- as.factor(d2$patience_score_bi)
summary(d2$patience_score_bi)
# high  low 
# 301  281
plot(d2$patience_score_bi)

    ### Age 1st child var
ggdensity(d2$age_first_child) #positively skewed. It's fine though
hist(d2$age_first_child)
skewness(d2$age_first_child)  #0.25
# d2$age_child1_sqrt <- transform(
#   d2$age_first_child,
#   method = "sqrt"
# )
# ggdensity(d2$age_child1_sqrt)
# skewness(d2$age_child1_sqrt) #-0.08

    ### Ideal age var
ggdensity(d2$ideal_age) #good
skewness(d2$ideal_age)  #0.15

    ### Green behaviour
hist(d2$environment_1) # negatively skewed, but fine
hist(d2$env_transport) # quite uniform. Maybe bimodal

###################################################
######### Creation of the final data table ########
###################################################

write.table(d2,"data_famhist.txt",dec=".",sep="\t", row.names = F)


#Install Packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(MatchIt)
library(cobalt)
library(naniar)
library(pwr)
library(forcats)

setwd("R:/PBS/MHSPP/GENERAL/Nosherwan/Tawny/Tawny Edits")

#Read in data files

TPR_group <- read.csv("TPRCANS_group043025.csv", header = TRUE)

Comp_group <- read.csv("CompCANS_group043025.csv", header = TRUE)

#https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#exact-matching-exact

#Prep dataframes for combining into same dataframe (in turn, prepping for matching)

#Check col names; same names will bind together
colnames(TPR_group)
colnames(Comp_group)

##Starting edits with TPR CANS group first
##Changing column names before binding dataframe by rows
head(TPR_group$assessment_date.x) #second (.x is the second CANS)
head(TPR_group$assessment_date.y) #first (.y is the first CANS)

TPR_group_newCols <- TPR_group %>% 
  rename("assessment_2_date" =  "assessment_date.x",
         "assessment_1_date" = "assessment_date.y",
          "age_at_2_assessment" = "age_at_assessment",
         "days_btw_assess2_tpr" = "days_btw_assess_tpr.x",
         "days_btw_assess1_tpr" = "days_btw_assess_tpr.y")
    

##Adding treatment group variable to track where observations came from
TPR_group_newCols$Tx_Group <- "TPR"

colnames(TPR_group_newCols)

#replacing CANS suffixes with more intuitive labels (for fist and second CANS)
names(TPR_group_newCols) <- gsub(x = names(TPR_group_newCols), pattern = "x",
                                     replacement = "2")
names(TPR_group_newCols) <- gsub(x = names(TPR_group_newCols), pattern = "y",
                                     replacement = "1")
colnames(TPR_group_newCols)

#Fixing column names
TPR_group_newCols <- TPR_group_newCols %>% 
  rename("Tx_Group" =  "T2_Group",
         "Perm_Exit" = "permanenc1e2it",
         "Livar_date_exit" = "livar_datee2it",
         "CANS_type_2" = "cans_t1pe.2",
         "Sub_type_2" = "sub_t1pe",
         "desc_org_ent_type" = "desc_org_ent_t1pe",
         "id_cycis" = "id_c1cis", 
         "cans_type_1" = "cans_t1pe.1",
         "days_btw_assess2_tpr" = "da1s_btw_assess2_tpr",
         "days_btw_assess1_tpr" = "da1s_btw_assess1_tpr",
         "days_btw_assess_1_2" = "da1s_btw_assess_1_2", 
         "days_seq_open" = "da1s_seq_open")


colnames(TPR_group_newCols)

#Drop unnecessary columns
TPR_group_newCols <- subset(TPR_group_newCols, select = -c(X.1, 
                                                                   X, 
                                                                   id_spell,
                                                                   id_case,
                                                                   spell_open_date,
                                                                   spell_close_date,
                                                                   id_pers,
                                                                   original_id,
                                                                   aprv_recv_date,
                                                                   id_assessor,
                                                                   name_assessor,
                                                                   id_approver,
                                                                   name_approver,
                                                                   name_supervisor,
                                                                   TPR_flag.2,
                                                                   CANS_after_tpr.id_c1cis,
                                                                   seq_open_date.1,
                                                                   seq_close_date.1,
                                                                   CANS_before_tpr.id_c1cis,
                                                                   TPR_flag.1,
                                                                   open_date_match
                                                                   ))

colnames(TPR_group_newCols)

TPR_group_newCols$dt_lgl_1.1 <- ymd(TPR_group_newCols$dt_lgl_1.1)
TPR_group_newCols$seq_open_date.2 <- ymd(TPR_group_newCols$seq_open_date.2)
TPR_group_newCols$days_open_tpr <- as.numeric(TPR_group_newCols$dt_lgl_1.1 - TPR_group_newCols$seq_open_date)
head(TPR_group_newCols$days_open_tpr)
mean(TPR_group_newCols$days_open_tpr)

#Editing comp group dataframe next
head(Comp_group$assessment_date.y) #First ( is the first CANS)
head(Comp_group$assessment_date.x) ##Second (.x is the second CANS)

#Change to class "date" and add variable for days_open_assessment_2 to use with match
class(Comp_group$assessment_date.x)
Comp_group$assessment_date.x <- ymd(Comp_group$assessment_date.x)
Comp_group$assessment_date.y <- ymd(Comp_group$assessment_date.y)
Comp_group$seq_open_date.x <- ymd(Comp_group$seq_open_date.x)

class(Comp_group$assessment_date.x)
class(Comp_group$assessment_date.y)
class(Comp_group$seq_open_date.x)

Comp_group$days_open_assessment_2 <- as.numeric(Comp_group$assessment_date.x - Comp_group$seq_open_date.x)
class(Comp_group$days_open_assessment_2)
range(Comp_group$days_open_assessment_2)

Comp_group$days_open_assessment_1 <- as.numeric(Comp_group$assessment_date.y - Comp_group$seq_open_date.x)
class(Comp_group$days_open_assessment_1)
range(Comp_group$days_open_assessment_1)

colnames(Comp_group)

Comp_group_newCols <- Comp_group %>% 
  rename("assessment_2_date" =  "assessment_date.x",
         "assessment_1_date" = "assessment_date.y",
         "age_at_2_assessment" = "age_at_assessment")

colnames(Comp_group_newCols)

head(Comp_group_newCols$days_open_assessment_2)
head(Comp_group_newCols$days_open_assessment_1)

##Adding treatment group variable to track where observations came from
Comp_group_newCols$Tx_Group <- "Comp"

colnames(Comp_group_newCols)

#replacing CANS suffixes with more intuitive labels (for fist and second CANS)
names(Comp_group_newCols) <- gsub(x = names(Comp_group_newCols), pattern = "y",
                                     replacement = "1")
names(Comp_group_newCols) <- gsub(x = names(Comp_group_newCols), pattern = "x",
                                     replacement = "2")
colnames(Comp_group_newCols)

#Fix Column names

Comp_group_newCols <- Comp_group_newCols %>% 
  rename("Tx_Group" =  "T2_Group",
         "Perm_Exit" = "permanenc1e2it",
         "Livar_date_exit" = "livar_datee2it",
         "CANS_type_1" = "cans_t1pe.1",
         "Sub_type_1" = "sub_t1pe",
         "desc_org_ent_type" = "desc_org_ent_t1pe",
         "id_cycis" = "id_c1cis", 
         "cans_type_2" = "cans_t1pe.2",
         "days_open_assessment_1" = "da1s_open_assessment_1",
         "days_btw_assess_1_2" = "da1s_btw_assess_1_2", 
         "days_open_assessment_2" = "da1s_open_assessment_2",
         "days_seq_open" = "da1s_seq_open",
         "days_btw_assess_CompTPRDate.1" = "da1s_btw_assess_CompTPRDate.1",
         "days_btw_assess_CompTPRDate.2" = "da1s_btw_assess_CompTPRDate.2")
         
head(Comp_group_newCols$days_btw_assess_1_2)
head(Comp_group_newCols$da1s_btw_assess)


head(Comp_group_newCols$da1s_open_assessment)
head(Comp_group_newCols$days_open_assessment_1)
head(Comp_group_newCols$days_open_assessment_2)

colnames(Comp_group_newCols)

Comp_group_newCols <- subset(Comp_group_newCols, select = -c(X.1,
                                                             X, 
                                                             id_spell,
                                                             id_case,
                                                             spell_open_date,
                                                             spell_close_date,
                                                             id_pers,
                                                             original_id,
                                                             aprv_recv_date,
                                                             id_assessor,
                                                             name_assessor,
                                                             id_approver,
                                                             name_approver,
                                                             name_supervisor,
                                                             TPR_flag.2,
                                                             seq_open_date.2,
                                                             seq_close_date.2,
                                                             CANS_before_CompTPRDate.id_c1cis,
                                                             TPR_flag.1,
                                                             open_date_match,
                                                             da1s_btw_assess,
                                                             da1s_open_assessment
                                                             ))

colnames(Comp_group_newCols)



#Next,
#Check col names and make sure same columns have same names for bind_rows
colnames(Comp_group_newCols)
colnames(TPR_group_newCols)

#Column names match except for 2 so changing the names of those
Comp_group_newCols <- Comp_group_newCols %>% 
  rename("seq_open_date" =  "seq_open_date.1",
         "seq_close_date" = "seq_close_date.1")
         
TPR_group_newCols <- TPR_group_newCols %>% 
  rename("seq_open_date" =  "seq_open_date.2",
         "seq_close_date" = "seq_close_date.2")

colnames(Comp_group_newCols)
colnames(TPR_group_newCols)

##Checking means for comp group and TPR group
mean(Comp_group_newCols$days_open_assessment_1)
mean(Comp_group_newCols$days_open_assessment_2)
mean(Comp_group_newCols$days_btw_assess_CompTPRDate.2)
mean(Comp_group_newCols$days_btw_assess_CompTPRDate.1)

head(TPR_group_newCols$days_open_assessment_1)
head(TPR_group_newCols$days_open_assessment_2)
head(TPR_group_newCols$assessment_1_date)
head(TPR_group_newCols$assessment_2_date)
head(TPR_group_newCols$seq_open_date)
mean(TPR_group_newCols$days_btw_assess2_tpr)
mean(TPR_group_newCols$days_btw_assess1_tpr)


##Merge the dataframes
TPR_group_newCols$assessment_2_date <- ymd(TPR_group_newCols$assessment_2_date)
class(TPR_group_newCols$assessment_2_date)

TPR_group_newCols$assessment_1_date <- ymd(TPR_group_newCols$assessment_1_date)
class(TPR_group_newCols$assessment_1_date)

Comp_group_newCols$seq_open_date <- ymd(Comp_group_newCols$seq_open_date)
class(Comp_group_newCols$seq_open_date)

TPRCompGroupJoined <- bind_rows(Comp_group_newCols, TPR_group_newCols)
colnames(TPRCompGroupJoined)
#write.csv(TPRCompGroupJoined, "TPRCompGroupJoined_050225.csv")

##Change -9999, -9997, -9996 values to NA
TPRCompGroupJoined[TPRCompGroupJoined == -9999] <- NA
TPRCompGroupJoined[TPRCompGroupJoined == -9997] <- NA
TPRCompGroupJoined[TPRCompGroupJoined == -9996] <- NA
#write.csv(TPRCompGroupJoined, "TPRCompGroupJoined_050225.csv")

##Drop cases where CANS values are NA in the 1st AND 5th question of relevant sections (first and middle ish)
#In CANS1 or CANS2
#Removing cases with large sections of NAs / total domains with NAs while trying not to overcorrect 


#######################################################################################################################################
# before removing cases due to NAs : n = [redacted]

#Trauma Experiences Domain
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans1.1 & TPRCompGroupJoined$cans5.1), ]
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans1.2 & TPRCompGroupJoined$cans5.2), ]

#Traumatic Stress Symptoms Domain
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans14.1 & TPRCompGroupJoined$cans18.1), ]
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans14.2 & TPRCompGroupJoined$cans18.2), ]

#Child Strengths
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans20.1 & TPRCompGroupJoined$cans25.1), ]
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans20.2 & TPRCompGroupJoined$cans25.2), ]

#Life Domain Functioning Domain
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans31.1 & TPRCompGroupJoined$cans35.1), ]
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans31.2 & TPRCompGroupJoined$cans35.2), ]

#Child Bev / Emo Needs Domain
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans48.1 & TPRCompGroupJoined$cans52.1), ]
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans48.2 & TPRCompGroupJoined$cans52.2), ]

#Child Risk
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans61.1 & TPRCompGroupJoined$cans65.1), ]
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$cans61.2 & TPRCompGroupJoined$cans65.2), ]

# after removing cases due to NAs : n= [redacted]

#write.csv(TPRCompGroupJoined, "TPRCompGroupJoined_050225.csv")

table(TPRCompGroupJoined$gender)

#Drop "unidentified" since only 1 case
TPRCompGroupJoined <- TPRCompGroupJoined[TPRCompGroupJoined$gender != "U", ]
#n= [redacted]

table(TPRCompGroupJoined$Tx_Group)

#Create covariate variables (trauma exp. combined score (sum of trauma exp. sect))
colnames(TPRCompGroupJoined)

#Change date variables to dates
TPRCompGroupJoined$dt_birth <- ymd(TPRCompGroupJoined$dt_birth)
TPRCompGroupJoined$seq_open_date <- ymd(TPRCompGroupJoined$seq_open_date)
class(TPRCompGroupJoined$seq_open_date)
TPRCompGroupJoined$seq_close_date <- ymd(TPRCompGroupJoined$seq_close_date)
class(TPRCompGroupJoined$seq_close_date)
TPRCompGroupJoined$assessment_1_date <- ymd(TPRCompGroupJoined$assessment_1_date)
class(TPRCompGroupJoined$assessment_1_date)
TPRCompGroupJoined$assessment_2_date <- ymd(TPRCompGroupJoined$assessment_2_date)
class(TPRCompGroupJoined$assessment_2_date)

TPRCompGroupJoined$age_at_1_assessment <- as.numeric(round((TPRCompGroupJoined$assessment_1_date - TPRCompGroupJoined$dt_birth)/365, digits = 1))
head(TPRCompGroupJoined$age_at_1_assessment)

TPRCompGroupJoined$age_at_2_assessment <- as.numeric(round((TPRCompGroupJoined$assessment_2_date - TPRCompGroupJoined$dt_birth)/365, digits = 1))
head(TPRCompGroupJoined$age_at_2_assessment)
range(TPRCompGroupJoined$age_at_2_assessment)

TPRCompGroupJoined$days_open_assessment_1 <- as.numeric(TPRCompGroupJoined$assessment_1_date - TPRCompGroupJoined$seq_open_date)
head(TPRCompGroupJoined$days_open_assessment_1)
class(TPRCompGroupJoined$days_open_assessment_1)
range(TPRCompGroupJoined$days_open_assessment_1)

TPRCompGroupJoined$days_open_assessment_2 <- as.numeric(TPRCompGroupJoined$assessment_2_date - TPRCompGroupJoined$seq_open_date)
head(TPRCompGroupJoined$days_open_assessment_2)
class(TPRCompGroupJoined$days_open_assessment_2)
range(TPRCompGroupJoined$days_open_assessment_2)

##Drop cases where days_open_assessment_1 negative 
TPRCompGroupJoined <- TPRCompGroupJoined[TPRCompGroupJoined$days_open_assessment_1 > 0, ]
range(TPRCompGroupJoined$days_open_assessment_1)

##Drop cases where days_open_assessment_1 is NA
TPRCompGroupJoined <- TPRCompGroupJoined[!is.na(TPRCompGroupJoined$days_open_assessment_1), ]
head(TPRCompGroupJoined$days_open_assessment_1)

table(TPRCompGroupJoined$Tx_Group)

#Drop cases where age at first or second assessment is less than 2 years
TPRCompGroupJoined <- TPRCompGroupJoined[TPRCompGroupJoined$age_at_2_assessment >= 2, ]
range(TPRCompGroupJoined$age_at_2_assessment)

TPRCompGroupJoined <- TPRCompGroupJoined[TPRCompGroupJoined$age_at_1_assessment >= 2, ]
range(TPRCompGroupJoined$age_at_1_assessment)

#Create variables
TPRCompGroupJoined$rounded_age_at_1_assess <- round(TPRCompGroupJoined$age_at_1_assessment, digits = 0)
head(TPRCompGroupJoined$rounded_age_at_1_assess)

TPRCompGroupJoined$rounded_age_at_2_assess <- round(TPRCompGroupJoined$age_at_2_assessment, digits = 0)
head(TPRCompGroupJoined$rounded_age_at_2_assess)

TPRCompGroupJoined$assessment_1_year <- year(TPRCompGroupJoined$assessment_1_date)
head(TPRCompGroupJoined$assessment_1_year)

TPRCompGroupJoined$assessment_2_year <- year(TPRCompGroupJoined$assessment_2_date)
head(TPRCompGroupJoined$assessment_2_year)

TPRCompGroupJoined$CANS_TE_Sum_A1 <- rowSums(TPRCompGroupJoined[,c("cans1.1",
                                                                   "cans2.1",
                                                                   "cans3.1",
                                                                   "cans4.1",
                                                                   "cans5.1",
                                                                   "cans6.1",
                                                                   "cans7.1",
                                                                   "cans8.1",
                                                                   "cans9.1",
                                                                   "cans10.1",
                                                                   "cans11.1",
                                                                   "cans12.1",
                                                                   "cans13.1"
                                                                   )], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_TE_Sum_A1)
range(TPRCompGroupJoined$CANS_TE_Sum_A1)

TPRCompGroupJoined$CANS_TE_TotEX0s_A1  <-  
  if_else(is.na(TPRCompGroupJoined$cans1.1), 0,
    if_else(TPRCompGroupJoined$cans1.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans2.1), 0,
          if_else(TPRCompGroupJoined$cans2.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans3.1), 0,
          if_else(TPRCompGroupJoined$cans3.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans4.1), 0,
          if_else(TPRCompGroupJoined$cans4.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans5.1), 0,
          if_else(TPRCompGroupJoined$cans5.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans6.1), 0,
          if_else(TPRCompGroupJoined$cans6.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans7.1), 0,
          if_else(TPRCompGroupJoined$cans7.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans8.1), 0,
          if_else(TPRCompGroupJoined$cans8.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans9.1), 0,
          if_else(TPRCompGroupJoined$cans9.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans10.1), 0,
          if_else(TPRCompGroupJoined$cans10.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans11.1), 0,
          if_else(TPRCompGroupJoined$cans11.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans12.1), 0,
          if_else(TPRCompGroupJoined$cans12.1 >= 1, 1, 0)) + 
  if_else(is.na(TPRCompGroupJoined$cans13.1), 0,
          if_else(TPRCompGroupJoined$cans13.1 >= 1, 1, 0)) 
  
head(TPRCompGroupJoined$CANS_TE_TotEX0s_A1)
range(TPRCompGroupJoined$CANS_TE_TotEX0s_A1)
TPRCompGroupJoined$CANS_TE_TotEX0s_A1
  
#Collapse levels in race and ethnicity variables for matching
#Race of the client
#AO: Asian
#BL: black
#CV: could not be identified
#DI: Declined to identify
#NR: Not reported
#PI: Pacific Islander
#UK: Unknown
#WH: white

#NH Not Hispanic
#HS Hispanic South American
#HC Hispanic Cuban
#HM Hispanic Mexican
#HP Hispanic Puerto Rican
#HD Hispanic Spanish Descent
#NR Not Reported
#HO Hispanic Other
#HN Hispanic Dominican
#HA Hispanic Central American
#DI Declined to Identify
#CV Could not be verified
#UN Unknown

table(TPRCompGroupJoined$race)

TPRCompGroupJoined$race_collapsed <- fct_collapse(TPRCompGroupJoined$race, Unknown = c("", "CV", "NR", "UK"))
table(TPRCompGroupJoined$race_collapsed)

TPRCompGroupJoined$race_collapsedx2 <- fct_collapse(TPRCompGroupJoined$race_collapsed, Asian_or_PI = c("AO", "PI"))
table(TPRCompGroupJoined$race_collapsedx2)

table(TPRCompGroupJoined$ethnc)
TPRCompGroupJoined$ethnc_collapsed <- fct_collapse(TPRCompGroupJoined$ethnc, Unknown = c("CV", "DI", "NR", "UK"))
table(TPRCompGroupJoined$ethnc_collapsed)

TPRCompGroupJoined$ethnc_collapsedx2 <- fct_collapse(TPRCompGroupJoined$ethnc_collapsed, 
                                                     Hispanic = c("HA", "HC", "HD", "HM",
                                                                  "HN", "HO", "HP", "HS"))
table(TPRCompGroupJoined$ethnc_collapsedx2)

#NH Not Hispanic
#HS Hispanic South American
#HC Hispanic Cuban
#HM Hispanic Mexican
#HP Hispanic Puerto Rican
#HD Hispanic Spanish Descent
#HO Hispanic Other
#HN Hispanic Dominican
#HA Hispanic Central American

table(TPRCompGroupJoined$cans_type_1)
table(TPRCompGroupJoined$cans_type_2)

#Excluding intact cases 
TPRCompGroupJoined <- TPRCompGroupJoined[-(which(TPRCompGroupJoined$cans_type_1 %in% "Intact Family")), ]
table(TPRCompGroupJoined$cans_type_1)

TPRCompGroupJoined <- TPRCompGroupJoined[-(which(TPRCompGroupJoined$cans_type_2 %in% "Intact Family")), ]
table(TPRCompGroupJoined$cans_type_2)
#N = [redacted]

table(TPRCompGroupJoined$Tx_Group)

#write.csv(TPRCompGroupJoined, "TPRCompGroupJoined_050225.csv")

##Adding in variables for time 1 CANS domains

#Calculate domain sums -> Assessment 1
TPRCompGroupJoined$CANS_TSS_Sum_A1 <- rowSums(TPRCompGroupJoined[,c("cans14.1",
                                                                      "cans15.1",
                                                                      "cans16.1",
                                                                      "cans17.1",
                                                                      "cans18.1",
                                                                      "cans19.1")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_TSS_Sum_A1)
range(TPRCompGroupJoined$CANS_TSS_Sum_A1)

TPRCompGroupJoined$CANS_StrengthsD_Sum_A1 <- rowSums(TPRCompGroupJoined[,c("cans20.1",
                                                                             "cans21.1",
                                                                             "cans22.1",
                                                                             "cans23.1",
                                                                             "cans24.1",
                                                                             "cans25.1",
                                                                             "cans26.1",
                                                                             "cans27.1",
                                                                             "cans28.1",
                                                                             "cans29.1",
                                                                             "cans30.1")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_StrengthsD_Sum_A1)
range(TPRCompGroupJoined$CANS_StrengthsD_Sum_A1)

TPRCompGroupJoined$CANS_LDF_Sum_A1 <- rowSums(TPRCompGroupJoined[,c("cans31.1",
                                                                      "cans32.1",
                                                                      "cans33.1",
                                                                      "cans34.1",
                                                                      "cans35.1",
                                                                      "cans36.1",
                                                                      "cans37.1",
                                                                      "cans38.1",
                                                                      "cans39.1",
                                                                      "cans40.1",
                                                                      "cans41.1",
                                                                      "cans42.1",
                                                                      "cans43.1")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_LDF_Sum_A1)
range(TPRCompGroupJoined$CANS_LDF_Sum_A1)

TPRCompGroupJoined$CANS_CBEN_Sum_A1 <- rowSums(TPRCompGroupJoined[,c("cans48.1",
                                                                       "cans49.1",
                                                                       "cans50.1",
                                                                       "cans51.1",
                                                                       "cans52.1",
                                                                       "cans53.1",
                                                                       "cans54.1",
                                                                       "cans55.1",
                                                                       "cans56.1",
                                                                       "cans57.1",
                                                                       "cans58.1",
                                                                       "cans59.1",
                                                                       "cans60.1")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_CBEN_Sum_A1)
range(TPRCompGroupJoined$CANS_CBEN_Sum_A1)

TPRCompGroupJoined$CANS_CRB_Sum_A1 <- rowSums(TPRCompGroupJoined[,c("cans61.1",
                                                                      "cans62.1",
                                                                      "cans63.1",
                                                                      "cans64.1",
                                                                      "cans65.1",
                                                                      "cans66.1",
                                                                      "cans67.1",
                                                                      "cans68.1",
                                                                      "cans69.1",
                                                                      "cans70.1",
                                                                      "cans71.1")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_CRB_Sum_A1)
range(TPRCompGroupJoined$CANS_CRB_Sum_A1)

##Calculating domain sums -> Assessment 2 | for potential trimming purposes

TPRCompGroupJoined$CANS_TE_Sum_A2 <- rowSums(TPRCompGroupJoined[,c("cans1.2",
                                                                     "cans2.2",
                                                                     "cans3.2",
                                                                     "cans4.2",
                                                                     "cans5.2",
                                                                     "cans6.2",
                                                                     "cans7.2",
                                                                     "cans8.2",
                                                                     "cans9.2",
                                                                     "cans10.2",
                                                                     "cans11.2",
                                                                     "cans12.2",
                                                                     "cans13.2")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_TE_Sum_A2)
range(TPRCompGroupJoined$CANS_TE_Sum_A2)
table(TPRCompGroupJoined$CANS_TE_Sum_A2)

TPRCompGroupJoined$CANS_TSS_Sum_A2 <- rowSums(TPRCompGroupJoined[,c("cans14.2",
                                                                      "cans15.2",
                                                                      "cans16.2",
                                                                      "cans17.2",
                                                                      "cans18.2",
                                                                      "cans19.2")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_TSS_Sum_A2)
range(TPRCompGroupJoined$CANS_TSS_Sum_A2)

TPRCompGroupJoined$CANS_StrengthsD_Sum_A2 <- rowSums(TPRCompGroupJoined[,c("cans20.2",
                                                                             "cans21.2",
                                                                             "cans22.2",
                                                                             "cans23.2",
                                                                             "cans24.2",
                                                                             "cans25.2",
                                                                             "cans26.2",
                                                                             "cans27.2",
                                                                             "cans28.2",
                                                                             "cans29.2",
                                                                             "cans30.2")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_StrengthsD_Sum_A2)
range(TPRCompGroupJoined$CANS_StrengthsD_Sum_A2)

TPRCompGroupJoined$CANS_LDF_Sum_A2 <- rowSums(TPRCompGroupJoined[,c("cans31.2",
                                                                      "cans32.2",
                                                                      "cans33.2",
                                                                      "cans34.2",
                                                                      "cans35.2",
                                                                      "cans36.2",
                                                                      "cans37.2",
                                                                      "cans38.2",
                                                                      "cans39.2",
                                                                      "cans40.2",
                                                                      "cans41.2",
                                                                      "cans42.2",
                                                                      "cans43.2")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_LDF_Sum_A2)
range(TPRCompGroupJoined$CANS_LDF_Sum_A2)

TPRCompGroupJoined$CANS_CBEN_Sum_A2 <- rowSums(TPRCompGroupJoined[,c("cans48.2",
                                                                       "cans49.2",
                                                                       "cans50.2",
                                                                       "cans51.2",
                                                                       "cans52.2",
                                                                       "cans53.2",
                                                                       "cans54.2",
                                                                       "cans55.2",
                                                                       "cans56.2",
                                                                       "cans57.2",
                                                                       "cans58.2",
                                                                       "cans59.2",
                                                                       "cans60.2")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_CBEN_Sum_A2)
range(TPRCompGroupJoined$CANS_CBEN_Sum_A2)

TPRCompGroupJoined$CANS_CRB_Sum_A2 <- rowSums(TPRCompGroupJoined[,c("cans61.2",
                                                                      "cans62.2",
                                                                      "cans63.2",
                                                                      "cans64.2",
                                                                      "cans65.2",
                                                                      "cans66.2",
                                                                      "cans67.2",
                                                                      "cans68.2",
                                                                      "cans69.2",
                                                                      "cans70.2",
                                                                      "cans71.2")], na.rm=TRUE)

head(TPRCompGroupJoined$CANS_CRB_Sum_A2)
range(TPRCompGroupJoined$CANS_CRB_Sum_A2)


###########################################################################Matching Prep 
##Sources
# vignette("sampling-weights")
# vignette("estimating-effects")
# vignette("MatchIt")
#vignette("assessing-balance")

#Checking some stats before the match
TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(mean_days_open_assess_1 = mean(days_open_assessment_1))

TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(mean_days_open_assess_2 = mean(days_open_assessment_2))

TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(age_at_2_assess = range(age_at_2_assessment))

TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(age_at_1_assess = range(age_at_1_assessment))

TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(age_at_2_assess = mean(age_at_2_assessment))

TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(age_at_1_assess = mean(age_at_1_assessment))

TPRCompGroupJoined %>% 
  group_by(Tx_Group) %>% 
  summarise(days_btw_assess_1_2 = mean(days_btw_assess_1_2))

colnames(TPRCompGroupJoined)

#Relevel race and ethnicity variables before the match
levels(TPRCompGroupJoined$race_collapsed)
levels(TPRCompGroupJoined$race_collapsedx2)
levels(TPRCompGroupJoined$ethnc_collapsed)
levels(TPRCompGroupJoined$ethnc_collapsedx2)

TPRCompGroupJoined$race_collapsed <- fct_relevel(TPRCompGroupJoined$race_collapsed, "BL", "WH", "AO", "PI", "Unknown")
TPRCompGroupJoined$race_collapsedx2 <- fct_relevel(TPRCompGroupJoined$race_collapsedx2, "BL", "WH", "Asian_or_PI", "Unknown")
TPRCompGroupJoined$ethnc_collapsed <- fct_relevel(TPRCompGroupJoined$ethnc_collapsed, "Unknown", after = Inf)
TPRCompGroupJoined$ethnc_collapsedx2 <- fct_relevel(TPRCompGroupJoined$ethnc_collapsedx2, "Hispanic", "NH", "Unknown")
help(fct_relevel)

table(TPRCompGroupJoined$race_collapsedx2)


############################################################### Matching Groups

colnames(TPRCompGroupJoined)

###Match data and check balance statistics
TPRCompGroupMatched <- matchit(as.factor(Tx_Group) ~ rounded_age_at_1_assess + gender + 
                                 race_collapsedx2 + CANS_TE_TotEX0s_A1 + ethnc_collapsedx2 + CANS_TSS_Sum_A1 +
                                 CANS_StrengthsD_Sum_A1 + CANS_LDF_Sum_A1 + CANS_CBEN_Sum_A1 + CANS_CRB_Sum_A1,
                 data = TPRCompGroupJoined,
                 method = "nearest",
                 distance = "glm",
                 ratio = 1)

# Get the summary of balance for all data
summary(TPRCompGroupMatched)

#Use reference to help assess balance statistics
#https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html

#Pull out the balance summary post-match
matched_balance_summary <- summary(TPRCompGroupMatched)$sum.matched

#Save as csv
write.csv(matched_balance_summary, "matched_balance_summary 071925.csv", row.names = TRUE)

###Pull variable names and change for love plot creation
b1 <- bal.tab(as.factor(Tx_Group) ~ rounded_age_at_1_assess + gender + 
                 race_collapsed + CANS_TE_TotEX0s_A1 + ethnc_collapsedx2 + CANS_TSS_Sum_A1 +
                 CANS_StrengthsD_Sum_A1 + CANS_LDF_Sum_A1 + CANS_CBEN_Sum_A1 + CANS_CRB_Sum_A1,
              data = TPRCompGroupJoined)

v1 <- var.names(b1, type = "vec", minimal = TRUE)
v1["rounded_age_at_1_assess"] <- "Age (Years)"
v1["gender_M"] <- "Sex (Male)"
v1["race_collapsedx2_BL"] <- "Race (Black)"
v1["race_collapsedx2_WH"] <- "Race (White)"
v1["race_collapsedx2_Asian_or_PI"] <- "Race (Asian or PI)"
v1["race_collapsedx2_Unknown"] <- "Race (Unknown)"
v1["CANS_TE_TotEX0s_A1"] <- "Total Trauma Experiences"
v1["ethnc_collapsedx2_Hispanic"] <- "Ethnicity (Hispanic)"
v1["ethnc_collapsedx2_NH"] <- "Ethnicity (Non-Hispanic)"
v1["ethnc_collapsedx2_Unknown"] <- "Ethnicity (Unknown)"
v1["CANS_TSS_Sum_A1"] <- "TSS Domain Score"
v1["CANS_StrengthsD_Sum_A1"] <- "Strengths Domain Score"
v1["CANS_LDF_Sum_A1"] <- "LDF Domain Score"
v1["CANS_CBEN_Sum_A1"] <- "CBEN Domain Score"
v1["CANS_CRB_Sum_A1"] <- "CRB Domain Score"

#Create love plot
loveplot <- love.plot(TPRCompGroupMatched, 
                      binary = "std", 
                      shapes = c("triangle filled", "circle filled"), 
                      grid = TRUE, 
                      var.names = v1,
                      cex.axis = 0.8)
loveplot

##save love plot
ggsave("Love Plot 071925.jpeg", plot = loveplot, width = 8, height =6, dpi = 300)

#Another plot example
plot(TPRCompGroupMatched, type = "qq", which.xs = ~ rounded_age_at_1_assess +  
       CANS_TE_TotEX0s_A1)

#Check general info
 TPRCompGroupMatched
# A matchit object
# - method: 1:1 nearest neighbor matching without replacement
# - distance: Propensity score
# - estimated with logistic regression
# - number of obs.: [redacted]
# - target estimand: ATT
# - covariates: rounded_age_at_1_assess, gender, race_collapsed, CANS_TE_TotEX0s_A1, 
# ethnc_collapsedx2, CANS_TSS_Sum_A1, CANS_StrengthsD_Sum_A1, CANS_LDF_Sum_A1, 
# CANS_CBEN_Sum_A1, CANS_CRB_Sum_A1
# 

 
TPRCompGroupMatchesOnly <- get_matches(TPRCompGroupMatched)
#total n = [redacted]
write.csv(TPRCompGroupMatchesOnly, "TPRCompGroupMatchesOnly050225.csv")

###References, helpful code, and test code below
# #Visualizations for assessing balance
# #https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html
# #Get the love plot of covariates
# #Options are "matched", "unmatched", "data" (default), "alphabetical" 
# plot(DummySum, var.order = "unmatched")
# 
# love.plot(DummyMatched2, binary = "std")
# 
# #Other options with love plots
# # love.plot(m.out, stats = c("m", "ks"), poly = 2, abs = TRUE,
# #           weights = list(nn = m.out2),
# #           drop.distance = TRUE, thresholds = c(m = .1),
# #           var.order = "unadjusted", binary = "std",
# #           shapes = c("circle filled", "triangle", "square"), 
# #           colors = c("red", "blue", "darkgreen"),
# #           sample.names = c("Original", "Full Matching", "NN Matching"),
# #           position = "bottom")
# 
# #Creating QQ plots
# #eQQ plot
# plot(DummyMatched2, type = "qq", which.xs = ~ Sex + Age2 + Political.Party)
# 
# #eCDF plot
# plot(DummyMatched2, type = "ecdf", which.xs = ~ Sex + Age2 + Political.Party)
# 
# #density plot
# plot(DummyMatched2, type = "density", which.xs = ~ Sex + Age2 + Political.Party)
# 
# #Density plot for continuous variables
# bal.plot(DummyMatched2, var.name = "Age2", which = "both")
# 
# #Bar graph for categorical variables
# bal.plot(DummyMatched2, var.name = "Political.Party", which = "both")
# 
# #Mirrored histogram
# bal.plot(DummyMatched2, var.name = "distance", which = "both",
#          type = "histogram", mirror = TRUE)

# #Power
# pwr.anova.test(k=2,            # 2 groups are compared
#                f=.2,          # small effect size
#                sig.level=.05,  # alpha/sig. level = .05
#                power=.8)
# #      n = 99.08032 in each group when two groups are compared

# DATA WRANGLING - G2C DATA
#Load library
library(Hmisc)
#library(summarytools)
library(tidyverse)
library(readxl)
library(dplyr)
library(likert)
library(kableExtra)
library(formattable)
library(janitor)

#Import Data Files
#G2C Files from REDCap
redcap_g2c_raw1 = read.csv("/Users/jon/Downloads/TheInfluenceOfHealth_DATA_2021-07-12_1849.csv")  # <- Change file location here 

# ETO Cycle 2 data
eto2_demo = read_excel("/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/Data/ETO Data/JHU_Request022021_Demographics(cycle 2).xlsx", sheet="Demographics")
eto2_prog = read_excel("/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/Data/ETO Data/JHU_Request022021_Demographics(cycle 2).xlsx", sheet="Enrolled Current Program")
# ETO Cycle 3 data
eto3_demo = read_excel("/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/Data/ETO Data/JHURequest022021_Cycle3.xlsx", sheet="Demographics")
eto3_prog = read_excel("/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/Data/ETO Data/JHURequest022021_Cycle3.xlsx", sheet="Training")
# Salesforce data
salesforce1 = read_excel("/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/Data/Salesforce data/Salesforce Data for JHU 210208 - unlocked.xlsx")
salesforce2 = read_excel("/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/Data/Salesforce data/Salesforce Data for JHU 210209 - unlocked.xlsx")

# Content:
#  1. Keep only Completed Surveys
#  2. Merge (ETO-Cycle2, ETO-Cycle3, Salesforce) data with REDCap data
#  3. Manually replace some missing Names and Cycles 
#  4. Rename variables, add labels and assign order to answers
#       - Food security
#       - Anxiety
#       - PTSD / Trauma  
#       - Depression
#  5. Create Totals variables
#  6. Dichotomous variables
#--------------------------------------------------------

# 1. G2C REDCap - keep only completed surveys
redcap_g2c_raw2 = filter(redcap_g2c_raw1, heybmore_survey_complete==2)
redcap_g2c_raw2$name_first <- redcap_g2c_raw2$info_fn
redcap_g2c_raw2$name_last <- redcap_g2c_raw2$info_ln
redcap_g2c_raw2$dob <- redcap_g2c_raw2$dob

# 2. Merge (ETO-Cycle2, ETO-Cycle3, Salesforce) data with REDCap data
#--------------------------------------------------------
# ETO data - Cycle 2 - rename variables
eto2_demo$subjectid <- eto2_demo$`Subject Unique Identifier`
eto2_demo$cycle <- eto2_demo$`Cycle:`
eto2_demo$name_first <- eto2_demo$`First Name`
eto2_demo$name_last <- eto2_demo$`Last Name`
eto2_demo$gender <- eto2_demo$`Gender_3753`
eto2_demo$ethnic <- eto2_demo$`Ethnicity-Custom`
eto2_demo$race <- eto2_demo$`Race-Custom`
eto2_demo$dob <- eto2_demo$DOB
eto2_demo$source <- "eto2"
eto2_prog$subjectid <- eto2_prog$ID
eto2_prog$program <- eto2_prog$`Current Program`
# ETO data - Cycle 3 - rename variables
eto3_demo$subjectid <- eto3_demo$`Subject Id`
eto3_demo$cycle <- eto3_demo$`Cycle:`
eto3_demo$name_first <- eto3_demo$`First Name`
eto3_demo$name_last <- eto3_demo$`Last Name`
eto3_demo$gender <- eto3_demo$`Gender_3753`
eto3_demo$ethnic <- eto3_demo$`Ethnicity-Custom`
eto3_demo$race <- eto3_demo$`Race-Custom`
eto3_demo$dob <- eto3_demo$DOB
eto3_demo$source <- "eto3"
eto3_prog$subjectid <- eto3_prog$SubjectID
eto3_prog$program <- eto3_prog$`Program Name`
# Salesforce data - rename variables
salesforce2$name_first <- salesforce2$`Contact: First Name`
salesforce2$name_last <- salesforce2$`Contact: Last Name`
salesforce2$gender_sales <- salesforce2$`Gender`
salesforce2$ethnic_sales <- salesforce2$`Ethnicity`
salesforce2$race_sales <- salesforce2$`Race`
salesforce2$source_sales <- "salesforce"
# Select variables from Salesforce data
salesforce3 <- dplyr::select(salesforce2, name_first, name_last, gender_sales, ethnic_sales, race_sales, source_sales)
# Select variables from ETO data
eto2_demo_select <- dplyr::select(eto2_demo, subjectid, cycle, name_first, name_last, gender, ethnic, race, dob, source)
eto2_prog_select <- dplyr::select(eto2_prog, subjectid, `program`)
eto2 <- left_join(eto2_demo_select, eto2_prog_select, by="subjectid")
eto3_demo_select <- dplyr::select(eto3_demo, subjectid, cycle, name_first, name_last, gender, ethnic, race, dob, source)
eto3_prog_select <- dplyr::select(eto3_prog, subjectid, `program`)
eto3 <- left_join(eto3_demo_select, eto3_prog_select, by="subjectid")
# All ETO data
eto_data <- rbind(eto2, eto3)
# Merge REDCap data with ETO and Salesforce data
redcap_g2c_raw3 <- left_join(redcap_g2c_raw2, eto_data, by=c("name_first", "name_last"))
redcap_g2c_raw4 <- left_join(redcap_g2c_raw3, salesforce3, by=c("name_first", "name_last"))
redcap_g2c_raw <- unique(redcap_g2c_raw4)
redcap_g2c_raw$gender_merge <- if_else((is.na(redcap_g2c_raw$gender)), redcap_g2c_raw$gender_sales, redcap_g2c_raw$gender)

# 3. Manually replace missing gender and cycle data
#------------------------------------------------------------
redcap_g2c_raw[redcap_g2c_raw$name_last=="Porter" & redcap_g2c_raw$name_first=="Jared","gender_merge"] <- "Male"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Christian" & redcap_g2c_raw$name_first=="Ja'Daia","gender_merge"] <- "Female"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Christian" & redcap_g2c_raw$name_first=="Ja'Daia","cycle.y"] <- "Cycle 3"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Johnson" & redcap_g2c_raw$name_first=="Jamyra","gender_merge"] <- "Female"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Baker" & redcap_g2c_raw$name_first=="Amori","gender_merge"] <- "Female"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Collins" & redcap_g2c_raw$name_first=="Jatay","gender_merge"] <- "Male"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Collins" & redcap_g2c_raw$name_first=="Jatay","cycle.y"] <- "Cycle 2"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Collins" & redcap_g2c_raw$name_first=="Jatay","program"] <- "Civic Works"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Evans" & redcap_g2c_raw$name_first=="Dasean","gender_merge"] <- "Male"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Evans" & redcap_g2c_raw$name_first=="Dasean","cycle.y"] <- "Cycle 3"
redcap_g2c_raw[redcap_g2c_raw$name_last=="Gladney" & redcap_g2c_raw$name_first=="Kayla","gender_merge"] <- "Female"
# Handle cycle 
redcap_g2c_raw$cycle1<- ifelse(is.na(redcap_g2c_raw$cycle.y), "Undetermined", redcap_g2c_raw$cycle.y)
# Handle G2c Participant
redcap_g2c_raw$g2c_part <- ifelse(redcap_g2c_raw$g2cparticipant==1, "Yes", ifelse(redcap_g2c_raw$g2cparticipant==2, "No", ifelse(redcap_g2c_raw$g2cparticipant==999, "Don't Know","NA")))

# 4. Rename variables, add labels and assign order to answers
# G2C FOOD SECURITY VARIABLES
#---------------------------------------------------------------
# Assign labels to choices
redcap_g2c_raw$q20a_food <- if_else(redcap_g2c_raw$q20a==2, "A lot", if_else(redcap_g2c_raw$q20a==1,"Sometimes", if_else(redcap_g2c_raw$q20a==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20b_food <- if_else(redcap_g2c_raw$q20b==2, "A lot", if_else(redcap_g2c_raw$q20b==1,"Sometimes", if_else(redcap_g2c_raw$q20b==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20c_food <- if_else(redcap_g2c_raw$q20c==2, "A lot", if_else(redcap_g2c_raw$q20c==1,"Sometimes", if_else(redcap_g2c_raw$q20c==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20d_food <- if_else(redcap_g2c_raw$q20d==2, "A lot", if_else(redcap_g2c_raw$q20d==1,"Sometimes", if_else(redcap_g2c_raw$q20d==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20e_food <- if_else(redcap_g2c_raw$q20e==2, "A lot", if_else(redcap_g2c_raw$q20e==1,"Sometimes", if_else(redcap_g2c_raw$q20e==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20f_food <- if_else(redcap_g2c_raw$q20f==2, "A lot", if_else(redcap_g2c_raw$q20f==1,"Sometimes", if_else(redcap_g2c_raw$q20f==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20g_food <- if_else(redcap_g2c_raw$q20g==2, "A lot", if_else(redcap_g2c_raw$q20g==1,"Sometimes", if_else(redcap_g2c_raw$q20g==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20h_food <- if_else(redcap_g2c_raw$q20h==2, "A lot", if_else(redcap_g2c_raw$q20h==1,"Sometimes", if_else(redcap_g2c_raw$q20h==0,"Never", "Prefer not to answer")))
redcap_g2c_raw$q20i_food <- if_else(redcap_g2c_raw$q20i==2, "A lot", if_else(redcap_g2c_raw$q20i==1,"Sometimes", if_else(redcap_g2c_raw$q20i==0,"Never", "Prefer not to answer")))
# Assign variable names
redcap_g2c_raw$"...did you worry that food at home would run out before you/your household got money to buy more?" <-redcap_g2c_raw$q20a_food 
redcap_g2c_raw$"...did the food that you/your household bought run out, and you didnt have money to get more?" <- redcap_g2c_raw$q20b_food
redcap_g2c_raw$"...did your meals only include a few kinds of cheap foods because you/your household was running out of money to buy food?"<- redcap_g2c_raw$q20c_food 
redcap_g2c_raw$"...were you not able to eat a balanced meal because you/your household didnt have enough money?"<- redcap_g2c_raw$q20d_food 
redcap_g2c_raw$"...did you have to eat less because you/your household didnt have enough money to buy food?" <- redcap_g2c_raw$q20e_food 
redcap_g2c_raw$"...has the size of your meals been cut because you/your household didnt have enough money for food?"<- redcap_g2c_raw$q20f_food 
redcap_g2c_raw$"...did you have to skip a meal because you/your household didnt have enough money for food?"<- redcap_g2c_raw$q20g_food
redcap_g2c_raw$"...were you hungry but didnt eat because you/your household didnt have enough food?"<- redcap_g2c_raw$q20h_food 
redcap_g2c_raw$"...did you not eat for a whole day because you/your household didnt have enough money for food?"<- redcap_g2c_raw$q20i_food 

# Assign an order to the variables
redcap_g2c_raw$"...did you worry that food at home would run out before you/your household got money to buy more?"  = factor(redcap_g2c_raw$"...did you worry that food at home would run out before you/your household got money to buy more?" , levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...did the food that you/your household bought run out, and you didnt have money to get more?" = factor(redcap_g2c_raw$"...did the food that you/your household bought run out, and you didnt have money to get more?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...did your meals only include a few kinds of cheap foods because you/your household was running out of money to buy food?" = factor(redcap_g2c_raw$"...did your meals only include a few kinds of cheap foods because you/your household was running out of money to buy food?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...were you not able to eat a balanced meal because you/your household didnt have enough money?" = factor(redcap_g2c_raw$"...were you not able to eat a balanced meal because you/your household didnt have enough money?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...did you have to eat less because you/your household didnt have enough money to buy food?" = factor(redcap_g2c_raw$"...did you have to eat less because you/your household didnt have enough money to buy food?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...has the size of your meals been cut because you/your household didnt have enough money for food?" = factor(redcap_g2c_raw$"...has the size of your meals been cut because you/your household didnt have enough money for food?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...did you have to skip a meal because you/your household didnt have enough money for food?" = factor(redcap_g2c_raw$"...did you have to skip a meal because you/your household didnt have enough money for food?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...were you hungry but didnt eat because you/your household didnt have enough food?" = factor(redcap_g2c_raw$"...were you hungry but didnt eat because you/your household didnt have enough food?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)
redcap_g2c_raw$"...did you not eat for a whole day because you/your household didnt have enough money for food?" = factor(redcap_g2c_raw$"...did you not eat for a whole day because you/your household didnt have enough money for food?", levels = c("Never", "Sometimes", "A lot"), ordered = TRUE)

# G2C ANXIETY VARIABLES
#--------------------------------------------------------------
# Assign labels to choices
redcap_g2c_raw$q34a_anxiety <- if_else(redcap_g2c_raw$q34a==4, "Almost always", if_else(redcap_g2c_raw$q34a==3,"Often", if_else(redcap_g2c_raw$q34a==2,"Sometimes", if_else(redcap_g2c_raw$q34a==1,"Almost never", if_else(redcap_g2c_raw$q34a==0,"Never", "Prefer not to answer")))))
redcap_g2c_raw$q34b_anxiety <- if_else(redcap_g2c_raw$q34b==4, "Almost always", if_else(redcap_g2c_raw$q34b==3,"Often", if_else(redcap_g2c_raw$q34b==2,"Sometimes", if_else(redcap_g2c_raw$q34b==1,"Almost never", if_else(redcap_g2c_raw$q34b==0,"Never", "Prefer not to answer")))))
redcap_g2c_raw$q34c_anxiety <- if_else(redcap_g2c_raw$q34c==4, "Almost always", if_else(redcap_g2c_raw$q34c==3,"Often", if_else(redcap_g2c_raw$q34c==2,"Sometimes", if_else(redcap_g2c_raw$q34c==1,"Almost never", if_else(redcap_g2c_raw$q34c==0,"Never", "Prefer not to answer")))))
redcap_g2c_raw$q34d_anxiety <- if_else(redcap_g2c_raw$q34d==4, "Almost always", if_else(redcap_g2c_raw$q34d==3,"Often", if_else(redcap_g2c_raw$q34d==2,"Sometimes", if_else(redcap_g2c_raw$q34d==1,"Almost never", if_else(redcap_g2c_raw$q34d==0,"Never", "Prefer not to answer")))))
# Assign variable names
redcap_g2c_raw$"I felt like something awful might happen"<- redcap_g2c_raw$q34a_anxiety  
redcap_g2c_raw$"I felt nervous"<- redcap_g2c_raw$q34b_anxiety 
redcap_g2c_raw$"I felt worried"<- redcap_g2c_raw$q34c_anxiety 
redcap_g2c_raw$"I worried when I was at home"<- redcap_g2c_raw$q34d_anxiety 
# Assign an order to the variables
redcap_g2c_raw$"I felt like something awful might happen"= factor(redcap_g2c_raw$"I felt like something awful might happen", levels = c("Prefer not to answer", "Never", "Almost never", "Sometimes", "Often", "Almost always"), ordered = TRUE)
redcap_g2c_raw$"I felt nervous"= factor(redcap_g2c_raw$"I felt nervous", levels = c("Prefer not to answer", "Never", "Almost never", "Sometimes", "Often", "Almost always"), ordered = TRUE)
redcap_g2c_raw$"I felt worried"= factor(redcap_g2c_raw$"I felt worried", levels = c("Prefer not to answer", "Never", "Almost never", "Sometimes", "Often", "Almost always"), ordered = TRUE)
redcap_g2c_raw$"I worried when I was at home"= factor(redcap_g2c_raw$"I worried when I was at home", levels = c("Prefer not to answer", "Never", "Almost never", "Sometimes", "Often", "Almost always"), ordered = TRUE)

# G2C PTSD/TRAUMA VARIABLES
#---------------------------------------------------------------
# Assign labels to choices
redcap_g2c_raw$q35a_ptsd <- if_else(redcap_g2c_raw$q35a==4, "Extremely", if_else(redcap_g2c_raw$q35a==3,"Quite a bit", if_else(redcap_g2c_raw$q35a==2,"Moderately", if_else(redcap_g2c_raw$q35a==1,"A little bit", if_else(redcap_g2c_raw$q35a==0,"Not at all", "Prefer not to answer")))))
redcap_g2c_raw$q35b_ptsd <- if_else(redcap_g2c_raw$q35b==4, "Extremely", if_else(redcap_g2c_raw$q35b==3,"Quite a bit", if_else(redcap_g2c_raw$q35b==2,"Moderately", if_else(redcap_g2c_raw$q35b==1,"A little bit", if_else(redcap_g2c_raw$q35b==0,"Not at all", "Prefer not to answer")))))
redcap_g2c_raw$q35c_ptsd <- if_else(redcap_g2c_raw$q35c==4, "Extremely", if_else(redcap_g2c_raw$q35c==3,"Quite a bit", if_else(redcap_g2c_raw$q35c==2,"Moderately", if_else(redcap_g2c_raw$q35c==1,"A little bit", if_else(redcap_g2c_raw$q35c==0,"Not at all", "Prefer not to answer")))))
redcap_g2c_raw$q35d_ptsd <- if_else(redcap_g2c_raw$q35d==4, "Extremely", if_else(redcap_g2c_raw$q35d==3,"Quite a bit", if_else(redcap_g2c_raw$q35d==2,"Moderately", if_else(redcap_g2c_raw$q35d==1,"A little bit", if_else(redcap_g2c_raw$q35d==0,"Not at all", "Prefer not to answer")))))
# Assign variable names
redcap_g2c_raw$"Suddenly feeling or acting as if the stressful experience were actually happening again"<- redcap_g2c_raw$q35a_ptsd 
redcap_g2c_raw$"Avoiding external reminders of the stressful experience"<- redcap_g2c_raw$q35b_ptsd 
redcap_g2c_raw$"Feeling distant or cut off from other people"<- redcap_g2c_raw$q35c_ptsd 
redcap_g2c_raw$"Irritable behavior, angry outbursts, or acting aggressively"<- redcap_g2c_raw$q35d_ptsd 
# Select only the new variable names
ptsd_g2c <- dplyr::select(redcap_g2c_raw, cycle1,
                          "Suddenly feeling or acting as if the stressful experience were actually happening again"
                          ,"Avoiding external reminders of the stressful experience"
                          ,"Feeling distant or cut off from other people"
                          ,"Irritable behavior, angry outbursts, or acting aggressively")
# Assign an order to the variables
redcap_g2c_raw$"Suddenly feeling or acting as if the stressful experience were actually happening again"= factor(redcap_g2c_raw$"Suddenly feeling or acting as if the stressful experience were actually happening again", levels = c("Prefer not to answer", "Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"), ordered = TRUE)
redcap_g2c_raw$"Avoiding external reminders of the stressful experience"= factor(redcap_g2c_raw$"Avoiding external reminders of the stressful experience", levels = c("Prefer not to answer", "Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"), ordered = TRUE)
redcap_g2c_raw$"Feeling distant or cut off from other people"= factor(redcap_g2c_raw$"Feeling distant or cut off from other people", levels = c("Prefer not to answer", "Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"), ordered = TRUE)
redcap_g2c_raw$"Irritable behavior, angry outbursts, or acting aggressively"= factor(redcap_g2c_raw$"Irritable behavior, angry outbursts, or acting aggressively", levels = c("Prefer not to answer", "Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"), ordered = TRUE)

# G2C DEPRESSION VARIABLES
#----------------------------------------------------------------------------------------------------------
# Assign labels to choices
redcap_g2c_raw$q33a_dep <-  if_else( redcap_g2c_raw$q33a==3,"5-7 days", if_else(redcap_g2c_raw$q33a==2,"3-4 days", if_else(redcap_g2c_raw$q33a==1,"1-2 days", if_else(redcap_g2c_raw$q33a==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33b_dep <-  if_else(redcap_g2c_raw$q33b==3,"5-7 days", if_else(redcap_g2c_raw$q33b==2,"3-4 days", if_else(redcap_g2c_raw$q33b==1,"1-2 days", if_else(redcap_g2c_raw$q33b==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33c_dep <-  if_else(redcap_g2c_raw$q33c==3,"5-7 days", if_else(redcap_g2c_raw$q33c==2,"3-4 days", if_else(redcap_g2c_raw$q33c==1,"1-2 days", if_else(redcap_g2c_raw$q33c==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33d_dep <-  if_else(redcap_g2c_raw$q33d==3,"5-7 days", if_else(redcap_g2c_raw$q33d==2,"3-4 days", if_else(redcap_g2c_raw$q33d==1,"1-2 days", if_else(redcap_g2c_raw$q33d==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33e_dep <-  if_else(redcap_g2c_raw$q33e==3,"5-7 days", if_else(redcap_g2c_raw$q33e==2,"3-4 days", if_else(redcap_g2c_raw$q33e==1,"1-2 days", if_else(redcap_g2c_raw$q33e==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33f_dep <-  if_else(redcap_g2c_raw$q33f==3,"5-7 days", if_else(redcap_g2c_raw$q33f==2,"3-4 days", if_else(redcap_g2c_raw$q33f==1,"1-2 days", if_else(redcap_g2c_raw$q33f==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33g_dep <-  if_else(redcap_g2c_raw$q33g==3,"5-7 days", if_else(redcap_g2c_raw$q33g==2,"3-4 days",  if_else(redcap_g2c_raw$q33g==1,"1-2 days", if_else(redcap_g2c_raw$q33g==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33h_dep <-  if_else(redcap_g2c_raw$q33h==3,"5-7 days", if_else(redcap_g2c_raw$q33h==2,"3-4 days",  if_else(redcap_g2c_raw$q33h==1,"1-2 days", if_else(redcap_g2c_raw$q33h==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33i_dep <-  if_else(redcap_g2c_raw$q33i==3,"5-7 days", if_else(redcap_g2c_raw$q33i==2,"3-4 days",  if_else(redcap_g2c_raw$q33i==1,"1-2 days", if_else(redcap_g2c_raw$q33i==0,"less than 1 day", "Prefer not to answer"))))
redcap_g2c_raw$q33j_dep <-  if_else(redcap_g2c_raw$q33j==3,"5-7 days", if_else(redcap_g2c_raw$q33j==2,"3-4 days", if_else(redcap_g2c_raw$q33j==1,"1-2 days", if_else(redcap_g2c_raw$q33j==0,"less than 1 day", "Prefer not to answer"))))
#Assign variable names                   
redcap_g2c_raw$"I was bothered by things that usually don't bother me"<- redcap_g2c_raw$q33a_dep
redcap_g2c_raw$"I had trouble keeping my mind on what I was doing"<- redcap_g2c_raw$q33b_dep 
redcap_g2c_raw$"I felt lonely"<- redcap_g2c_raw$q33i_dep
redcap_g2c_raw$"I felt depressed" <- redcap_g2c_raw$q33c_dep
redcap_g2c_raw$"I felt that everything was an effort"<- redcap_g2c_raw$q33d_dep
redcap_g2c_raw$"I felt hopeful about the future"<- redcap_g2c_raw$q33e_dep
redcap_g2c_raw$"I felt fearful"<- redcap_g2c_raw$q33f_dep
redcap_g2c_raw$"My sleep was restless" <- redcap_g2c_raw$q33g_dep
redcap_g2c_raw$"I was happy"<- redcap_g2c_raw$q33h_dep
redcap_g2c_raw$"I could not get going" <- redcap_g2c_raw$q33j_dep
#Assign an order to the variables
redcap_g2c_raw$"I was bothered by things that usually don't bother me" = factor(redcap_g2c_raw$"I was bothered by things that usually don't bother me", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I had trouble keeping my mind on what I was doing" = factor(redcap_g2c_raw$"I had trouble keeping my mind on what I was doing", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I felt lonely" = factor(redcap_g2c_raw$"I felt lonely", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I felt depressed" = factor(redcap_g2c_raw$"I felt depressed", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I felt that everything was an effort" = factor(redcap_g2c_raw$"I felt that everything was an effort", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I felt hopeful about the future" = factor(redcap_g2c_raw$"I felt hopeful about the future", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I felt fearful" = factor(redcap_g2c_raw$"I felt fearful", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"My sleep was restless" = factor(redcap_g2c_raw$"My sleep was restless", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I was happy" = factor(redcap_g2c_raw$"I was happy", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)
redcap_g2c_raw$"I could not get going" = factor(redcap_g2c_raw$"I could not get going", levels = c("Prefer not to answer", "less than 1 day", "1-2 days", "3-4 days", "5-7 days"), ordered = TRUE)

redcap_g2c_raw$study <- "G2C"

# 5. Compare similar ages in G2C and Vanguard

#### Create TOTAL Food Security Score w/ and w/o "Prefer not to answer"
redcap_g2c_raw$food_total <- redcap_g2c_raw$q20a + redcap_g2c_raw$q20b + redcap_g2c_raw$q20c + redcap_g2c_raw$q20d + redcap_g2c_raw$q20e + redcap_g2c_raw$q20f + redcap_g2c_raw$q20g + redcap_g2c_raw$q20h + redcap_g2c_raw$q20i
redcap_g2c_raw$food_total_no_refused <- ifelse(redcap_g2c_raw$food_total>18, NA, redcap_g2c_raw$food_total)
redcap_g2c_raw$food_flagged <- if_else(redcap_g2c_raw$food_total>=2, 1,0)  # Flagged if score 2 or greater
redcap_g2c_raw$food_cat  <- if_else(redcap_g2c_raw$food_total>=2, "Moderate", if_else(redcap_g2c_raw$food_total>=6, "Severe","None"))

#### Create TOTAL Anxiety Score w/ and w/o "Prefer not to answer"
redcap_g2c_raw$anxiety_total <- redcap_g2c_raw$q34a + redcap_g2c_raw$q34b + redcap_g2c_raw$q34c + redcap_g2c_raw$q34d
redcap_g2c_raw$anxiety_total_no_refused <- ifelse(redcap_g2c_raw$anxiety_total>16, NA, redcap_g2c_raw$anxiety_total)
redcap_g2c_raw$anxiety_flagged <- if_else(redcap_g2c_raw$anxiety_total>=8, 1,0) # Flagged if score 8 or greater
#### Create TOTAL PTSD Score w/ and w/o "Prefer not to answer"
redcap_g2c_raw$ptsd_total <- redcap_g2c_raw$q35a + redcap_g2c_raw$q35b + redcap_g2c_raw$q35c + redcap_g2c_raw$q35d
redcap_g2c_raw$ptsd_total_no_refused <- ifelse(redcap_g2c_raw$ptsd_total>16, NA, redcap_g2c_raw$ptsd_total)
redcap_g2c_raw$ptsd_flagged <- if_else(redcap_g2c_raw$ptsd_total>=7, 1,0)
#### Create TOTAL Depression Score w/ and w/o "Prefer not to answer"
# Reverse Code 2 Variables
redcap_g2c_raw$q33e_reversed <- ifelse(redcap_g2c_raw$q33e==0,3,ifelse(redcap_g2c_raw$q33e==1,2,ifelse(redcap_g2c_raw$q33e==2,1,0)))
redcap_g2c_raw$q33h_reversed <- ifelse(redcap_g2c_raw$q33h==0,3,ifelse(redcap_g2c_raw$q33h==1,2,ifelse(redcap_g2c_raw$q33h==2,1,0)))

redcap_g2c_raw$depression_total <- redcap_g2c_raw$q33a + redcap_g2c_raw$q33b + redcap_g2c_raw$q33c + redcap_g2c_raw$q33d + redcap_g2c_raw$q33e_reversed + redcap_g2c_raw$q33f + redcap_g2c_raw$q33g + redcap_g2c_raw$q33h_reversed + redcap_g2c_raw$q33i + redcap_g2c_raw$q33j
redcap_g2c_raw$depression_total_no_refused <- ifelse(redcap_g2c_raw$depression_total>30, NA, redcap_g2c_raw$depression_total)
redcap_g2c_raw$depression_flagged <- if_else(redcap_g2c_raw$depression_total>=9, 1,0) # Flagged if score 9 or greater

## Save as .rds extension
saveRDS(redcap_g2c_raw, file = "/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/G2C and Vanguard Comparison/g2c.rds")

### Create files for GIS
g2c_gis <- dplyr::select(redcap_g2c_raw, streetaddress, city, state, zipcode, g2cparticipant)
write.csv(g2c_gis, "/Users/jon/Desktop/PFRH/Grads2Careers - HEYBMORE/G2C and Vanguard Comparison/g2c_gis.csv")




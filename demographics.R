#Demographics (updated on Nov 8th)
#Scarlett

library(readxl)
library(dplyr)
library(stringr)
library(data.table)

rawdata <- read_excel("/Users/Scarlett/Desktop/BU Healthy Minds/HMS raw data 2016.xlsx", sheet = 1, col_names = TRUE)


# 1. race

race <- rawdata[15:23]
text <- unique(race$race_other_text)
text_asian_indicator <- c(3,9,14,15,16,18,21,29,32,39,42,45,50,58,68,73,75,76,79,80)
text_biracial_indicator <- c(24, 30,40,41,44,55,74)
text_black_indicator <- c(6)
text_asian <- text[text_asian_indicator]
text_biracial <- text[text_biracial_indicator]
text_black <- text[text_black_indicator]

race$race_cleaned[race$race_white == 1] <- "white"
race$race_cleaned[race$race_asian == 1 | race$race_other_text %in% text_asian] <- "asian"
race$race_cleaned[race$race_black == 1 | race$race_other_text %in% text_black] <- "black"
race$race_cleaned[rowSums(race[1:7], na.rm = TRUE) ==2 | race$race_other_text %in% text_biracial] <- "biracial"
#race$race_cleaned[race$race_ainaan == 1] <- "native american"
#race$race_cleaned[race$race_mides == 1] <- "middle eastern"
race$race_cleaned[race$race_ainaan == 1 | race$race_mides == 1] <- "other"
race$race_cleaned[is.na(race$race_cleaned) & race$race_other == 1] <- "other"



# 2. academic status + year of school
educ <- rawdata[41:50]
educ$aca_status[educ$degree_bach == 1] <- "undergraduate"
educ$aca_status[rowSums(educ[2:5], na.rm = TRUE) >= 1] <- "graduate"

#dealing with text column
other <- educ %>% filter(degree_other == 1 & !is.na(degree_other_text))
other_text <- unique(other$degree_other_text)
other_text_indicator <- c(T, T, T, F, F, F, T, T, T, T, T, T, T, T, T, T, F, F, F, T, T, T, F, T, F, F, F, T, T, F)
grad <- other_text[other_text_indicator]
educ$aca_status[educ$degree_other_text %in% grad] <- "graduate"
educ$aca_status[is.na(educ$aca_status) & educ$degree_other == 1 & !is.na(educ$degree_other_text)] <- "other"

#year of school
educ$yr_sch[educ$aca_status == "graduate"] <- NA
educ$yr_sch[educ$yr_sch == 1] <- "freshman"
educ$yr_sch[educ$yr_sch == 2] <- "sophomore"
educ$yr_sch[educ$yr_sch == 3] <- "junior"
educ$yr_sch[educ$yr_sch == 4 | educ$yr_sch == 5] <- "senior"



#3. gender
gender <- rawdata$gender
#didn't include gender_text because the column is all NAs
gender[gender == 1] <- "male"
gender[gender == 2] <- "female"
gender[gender == 3 | gender == 4] <- "trans gender"
gender[gender == 5 | gender == 6] <- "other"


#4. citizenship
citizenship <- rawdata$citizen
citizenship[citizenship == 1] <- "domestic"
citizenship[citizenship == 0] <- "international"




demographics <- data.frame(citizenship, gender, educ$aca_status, educ$yr_sch, race$race_cleaned)
names(demographics) <- c("citizenship", "gender", "academic_status", "year_of_school", "race")

save(demographics, file = "demographics.rda")

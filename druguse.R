library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

rawdata <- read_excel("D:/My Documents/projects/MH/HMS raw data 2016.xlsx", sheet = 1, col_names = TRUE)
grep("drug", colnames(rawdata))
drug <- rawdata[188:198]
rowsumd <- rowSums(drug[1:8], na.rm = TRUE)

# Nobody filled anything in those 3 text columns, even though 22 filled the "other" option. So the 3 text columns could be deleted, because they have only 'NA's. 
sum(1*is.na(drug$drug_other)) # 2529
sum(1*is.na(drug$drug_other1_text)) # 2551
sum(1*is.na(drug$drug_other2_text)) # 2551
sum(1*is.na(drug$drug_other3_text)) # 2551
drug[9:11] <- NULL

# Rename the drug data frame.
druglist <- names(drug[1:8])
druglist <- str_replace(druglist, "drug_", "")
names(drug) <- druglist

# Sum the columns up and check. I will divide drug use into the following levels: "mar", "coc", "her", "met", "stim", "ect", "more than 2", "none", "other".
max(rowsumd, na.rm = T)
more2 <- drug %>% filter(rowsumd >= 2 | other ==1 ) # 68
other <- drug %>% filter(other ==1 & rowsumd == 1) # 3

# Creat a new column and summary in it.
drug$cleaned <- c(NA)
for (i in 1:2551) {
  if (rowsumd[i] == 1){
    drug$cleaned[i] <- druglist[which(drug[i,1:8] == 1)]
  } else if (rowsumd[i] >= 2) {
    drug$cleaned[i] <- "2 or more"
  }
}

# Pull the cleaned drug data out for analysis.
drug_use <- data_frame(drug$cleaned)
drug <- data.frame(demographics, drug_use)
names(drug)[4:5] <- c("race", "drug")
drug <- na.omit(drug)

# Pull the groups out and analyse them separately.
par(mfrow = c(2, 4))

cfu <- drug %>% filter(US_citizenship == "Yes" & gender == "female" & aca_status == "undergraduate")
ggplot(cfu, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for undergraduate female citizen")

cmu <- drug %>% filter(US_citizenship == "Yes" & gender == "male" & aca_status == "undergraduate")
ggplot(cmu, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for undergraduate male citizen")

cfg <- drug %>% filter(US_citizenship == "Yes" & gender == "female" & aca_status == "graduate")
ggplot(cfg, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for graduate female citizen")

cmg <- drug %>% filter(US_citizenship == "Yes" & gender == "male" & aca_status == "graduate")
ggplot(cmg, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for graduate male citizen")

ifu <- drug %>% filter(US_citizenship == "No" & gender == "female" & aca_status == "undergraduate")
ggplot(ifu, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for undergraduate female non-citizen")

ifg <- drug %>% filter(US_citizenship == "No" & gender == "female" & aca_status == "graduate")
ggplot(ifg, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for graduate female non-citizen")

imu <- drug %>% filter(US_citizenship == "No" & gender == "male" & aca_status == "undergraduate")
ggplot(cfu, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for undergraduate male non-citizen")

img <- drug %>% filter(US_citizenship == "No" & gender == "male" & aca_status == "graduate")
ggplot(img, aes(race)) + geom_bar(aes(fill = drug), position = "fill") + ggtitle("Drug use analysis for graduate male non-citizen")


Questions:
1. Can we have drug uses as numeric numbers. 
2. How to keep colors in ggplot constant.
# mental status + drug use (Nov 14)
# Scarlett

library(ggplot2)
load("rawdata.rda")
load("demographics.rda")

## mental module
mental <- data.frame(demographics, rawdata[117:204])

## drug use
mental_drug <- subset(mental, is.na(mental$drug_none) & rowSums(mental[, c(77:83)], na.rm = TRUE) > 0)
colSums(mental_drug[c(77:83)], na.rm = TRUE)
# drug_mar   drug_coc   drug_her   drug_met  drug_stim   drug_ect drug_other 
# 372         32          0          2         44         10         22 

# the logic here is, someone using any kind of drug will be categorized as "drug user"
mental$dataset <- "non drug user"
mental$dataset[is.na(mental$drug_none) & rowSums(mental[, c(77:83)], na.rm = TRUE) > 0] <- "drug user"

## 1. Positive mental health
# diener_score. the higher the better
ggplot(mental,aes(x=mental$diener_score,fill=dataset))+
    geom_histogram(aes(y=0.5*..density..),
                   alpha=0.5,position='identity',binwidth=0.5)
# non drug users have a more positive attitude in general


## 2. depression
# deprawsc (i don't understand why depression has 2 sections of some same questions so I just used the 1st section)
ggplot(mental,aes(x=mental$deprawsc,fill=dataset))+
    geom_histogram(aes(y=0.5*..density..),
                   alpha=0.5,position='identity',binwidth=0.5)
# drug users have higher depression score

## 3. anxiety score
# anx_score
ggplot(mental,aes(x=mental$anx_score,fill=dataset))+
    geom_histogram(aes(y=0.5*..density..),
                   alpha=0.5,position='identity',binwidth=0.5)

# 4. body image
# ed_scoff
ggplot(mental,aes(x=mental$ed_scoff,fill=dataset))+
    geom_histogram(aes(y=0.5*..density..),
                   alpha=0.5,position='identity',binwidth=0.5)
# not that related

# 5. Non-suicidal self-injury
suic <- mental[c(48:61)]

count(subset(suic, rowSums(suic[,c(1:11,13)], na.rm = TRUE) >= 1 )) # all suic data with values
count(subset(suic, sib_none == 1))  #1692 out of 2077, 81.46% are none suicidal

suic_vec <- names(na.omit(unlist(suic[,1:10])))
suic_vec <- gsub("[[:digit:]]","",suic_vec)
suic_table <- as.data.frame(table(suic_vec))
suic_table[1] <- c("Cut myself", "Burned myself", "Punched or banged myself", "Scratched myself", "Pulled my hair","Bit myself", "Interfered with wound healing", "Carved words or symbols into skin", "Rubbed sharp objects into skin", "Punched or banged an object to hurt myself")
ggplot(data = suic_table, aes(x = suic_vec, y = Freq)) + geom_bar(stat="identity") + coord_flip() + ggtitle("Non-suicidal self-injury")

# 6 & 7 "violence" and "sleep" section, didn't explore



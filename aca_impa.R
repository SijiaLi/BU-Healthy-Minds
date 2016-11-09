### Read the data.


```{r }
setwd("D:/My Documents/projects/MH")
hmraw <- read.csv("HMS raw data 2016.csv", header=TRUE)
hm <- data.frame(hmraw)
```

```{r }
race <- hm[,c(14:23)]
degree <- hm[, c(41:48)]
aca_impa <- hm$aca_impa
sex_birth <- hm$sex_birth
gender <- hm$gender
gender_text <- hm$gender_text
citizen <- hm$citizen
hm <- data.frame(aca_impa, sex_birth, gender, gender_text, citizen, race, degree)
```

### Clean the data

In this report, I just care about the "aca_impa", academic impact as a response; and gender, citizen, race and under/graduate as factors. So I will clean the data of them one by one. As a reminder, each time we put some rows aside, we should rename the rows, to avoid confusion. I cleaned 159 (2551 - 2392) rows in total. About 6.23% of the initial data.

# Clean the gender data 

Find the indexes of NA's in the gender column, delete those rows, so that we could ignore those samples without response. 66 in total. Then we rename the rows.

```{r }
which(is.na(hm$gender))
length(which(is.na(hm$gender)))
hm <- hm[-which(is.na(hm$gender)),]
rownames(hm) <- c(1:nrow(hm))
```

# Clean the citizen data

Find the indexes of NA's in the gender column, delete those rows, so that we could ignore those without response. 17 in total. Then rename the rows.

```{r }
which(is.na(hm$citizen))
length(which(is.na(hm$citizen)))
hm <- hm[-which(is.na(hm$citizen)),]
rownames(hm) <- c(1:nrow(hm))
```

# Clean the degree data

let all the NA's in each degree column to be 0.

```{r }
hm$degree_ass[is.na( hm$degree_ass)] <- 0 # 36
hm$degree_bach[is.na( hm$degree_bach)] <- 0 # 1308
hm$degree_ma[is.na( hm$degree_ma)] <- 0 # 701
hm$degree_jd[is.na( hm$degree_jd)] <- 0 # 64
hm$degree_md[is.na( hm$degree_md)] <- 0 # 77
hm$degree_phd[is.na( hm$degree_phd)] <- 0 # 256
hm$degree_other[is.na( hm$degree_other)] <- 0 # 68
```

Put the values into lists.
```{r }
asso <- hm$degree_ass
bach <- hm$degree_bach
ma <- hm$degree_ma
jd <- hm$degree_jd
md <- hm$degree_md
phd <- hm$degree_phd
dother <- hm$degree_other
```

Sum the lists up, and explore the data later.
```{r }
deg <- asso + bach + ma + jd + md + phd + dother
```

For the 29 samples without answer, we should ignore them. I am curious about those with 3 or more degrees, so I pull them out, and verify whether the response is reliable. There are 4 samples with 3 or more degrees. The sample 457,792 and 1865 claimed to be enrolled in both bachelor and master degree, so they clicked bachelor, master and other. I think it should be master only. I wonder whether we should correct it, I will not clean them at this stage. For index 1934, I think the responser did not take the survey seriously. We could put this sample aside. Therefore in the following chunck, I will ignore 29 + 1 = 30 samples. Then rename the rows.
```{r }
deg
sum(deg)
degno <- which(deg == 0)
degno
length(degno) #29
deg3 <- which(deg >= 3)
deg3
length(deg3) #4
hm <- hm[-c(degno, 1934),]
rownames(hm) <- c(1:nrow(hm))
```

To explore more about degree data. I am not sure how should we deal with these 63 rows. Some claimed they are enrolled in mastrer and phd(row 27), some claimed they are enrolled in bachelor and doctor of medicine(row 44), some claimed md amd phd (row 54). Since the client cares about the difference between the undergraduates and postgraduates, I think maybe we could treat those chose the degree_bach as the undergraduate group, all the rest as the postgraduate group. Or we could use age.
```{r }
asso <- hm$degree_ass
bach <- hm$degree_bach
ma <- hm$degree_ma
jd <- hm$degree_jd
md <- hm$degree_md
phd <- hm$degree_phd
dother <- hm$degree_other
deg <- asso + bach + ma + jd + md + phd + dother
deg2 <- which(deg >= 2)
deg2
length(deg2) #63
```

# Clean the race data

let all the NA's in each column to be 0.
```{r }
hm$race_pi[is.na(hm$race_pi)] <- 0
hm$race_his[is.na(hm$race_his)] <- 0
hm$race_white[is.na(hm$race_white)] <- 0
hm$race_black[is.na(hm$race_black)] <- 0
hm$race_asian[is.na(hm$race_asian)] <- 0
hm$race_ainaan[is.na(hm$race_ainaan)] <- 0
hm$race_mides[is.na(hm$race_mides)] <- 0
hm$race_haw[is.na(hm$race_haw)] <- 0
hm$race_other[is.na(hm$race_other)] <- 0
```

Put them into a list
```{r }
pi <- hm$race_pi
his <- hm$race_his
white <- hm$race_white
black <- hm$race_black
asian <- hm$race_asian
ainaan <- hm$race_ainaan
mides <- hm$race_mides
haw <- hm$race_haw
rother <- hm$race_other
```

Sum the columns up, remove those equal to 0 or 9. We removed 4 samples, because 4 people did not give any answer. 
```{r }
race <- pi + his + white + black + asian + ainaan + mides + haw + rother
length(race)
raceno <- which(race == 0)
raceno
length(raceno)
hm <- hm[-raceno,]
rownames(hm) <- c(1:nrow(hm))
```

Explore the race data a little bit more, see the distribution. When we are analysing this part, maybe we could have a group called mixed race. 
```{r }
length(race1 <- which(race == 1)) # 2067
length(race2 <- which(race == 2)) # 326
length(race3 <- which(race == 3)) # 37
length(race4 <- which(race >= 4)) # 4
```

# Clean the "aca_impa" data

Find the indexes of NA's in the gender 'aca_impa', delete those rows, so that we could ignore those without response. 42 in total. Then rename the rows.

```{r }
which(is.na(hm$aca_impa))
length(which(is.na(hm$aca_impa))) # 42
hm <- hm[-which(is.na(hm$aca_impa)),]
rownames(hm) <- c(1:nrow(hm))
```


Question:

Can we try the test online? It would be helpful to see how the data is collected.
Bach & asso. md & phd. 
gender & sex by birth. 
race_his and others. mixed-race? 
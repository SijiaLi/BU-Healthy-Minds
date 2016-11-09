hmraw <- read.csv("HMS raw data 2016.csv", header=TRUE)
hm <- data.frame(hmraw) 
hm[1:4] <- list(NULL)

which(is.na(hm$Q2_1))
which(is.na(hm$age))
which(is.na(hm$sex_birth))
which(is.na(hm$gender))
which(is.na(hm$sexual))

length(which(is.na(hm$Q2_1)))
length(which(is.na(hm$age)))
length(which(is.na(hm$sex_birth)))
length(which(is.na(hm$gender)))
length(which(is.na(hm$sexual)))

length(intersect(which(is.na(hm$Q2_1)),which(is.na(hm$age))))
length(intersect(which(is.na(hm$Q2_1)),which(is.na(hm$sex_birth))))
length(intersect(which(is.na(hm$Q2_1)),which(is.na(hm$gender))))
length(intersect(which(is.na(hm$Q2_1)),which(is.na(hm$sexual))))

hm <- hm[-which(is.na(hm$Q2_1)),]

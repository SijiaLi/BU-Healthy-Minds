library(psych)
hmraw <- read.csv("HMS raw data 2016.csv", header=TRUE)
describe=describe(hmraw)
attach(describe)
describe$missing=2551-n
detach(describe)
describe
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(caTools)
library(glmnet)
library(pROC)
library(ROCR)
library(Matching)
library(tableone)
library(cobalt)
library(rpart)
library(rpart.plot)
library(dplyr)



# Data Cleaning

## Clean PAC

pacs <- read.delim("pacs18.txt",header=FALSE,sep=",")

for (i in 1:4){
pacs[,i] <- gsub('^.|.$', '', pacs[,i])
}

for (i in 7:10){
pacs[,i] <- gsub('^.|.$', '', pacs[,i])
}

pacs <- pacs %>%
rename(
Cycle = V1,
FECRecNo = V2,
PACID = V3,
CID = V4,
Amount = V5,
Date = V6,
RealCode = V7,
Type = V8,
DI = V9,
FECCandID = V10
)

write_csv(pacs, "pacs18.csv")


## Read Multiseparator

read.multisep <- function(File,sep) {
Lines <- readLines(File)
Matrix <- do.call(rbind,strsplit(Lines, sep, fixed = TRUE))
DataFrame <- structure(data.frame(Matrix))
DataFrame[] <- lapply(DataFrame, type.convert)
DataFrame
}


##  Clean Candidate
cands <- read.multisep("cands18.txt",sep="|,|")

cands[,1] <- sub('.','', cands[,1])
cands[,12] <- gsub('.{1}$','', cands[,12])

cands <- cands %>%
rename(
Cycle = X1,
FECCandID = X2,
CID = X3,
FirstLastP = X4,
Party = X5,
DistIDRunFor = X6,
DistIDCurr = X7,
CurrCand = X8,
CycleCand = X9,
CRPICO = X10,
RecipCode = X11,
NoPacs = X12
)

write_csv(cands, "cands18.csv")


#We also applied the same cleaning steps to the 2014 and 2016 data tables.

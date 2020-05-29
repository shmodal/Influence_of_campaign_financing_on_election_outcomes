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


# Exploratory Data Analysis (EDA)



cands <- read.csv("cands18.csv",header=TRUE)
pacs <- read.csv("pacs18.csv",header=TRUE)


## Plot 1: Election Outcome by Party

for (i in which(cands$RecipCode == "3l")) {
  cands$RecipCode[i] <- "3L"
}

CandsOutcome <- cands %>%
  select(Party,RecipCode) %>%
  filter(str_sub(RecipCode,2,2)!="I") %>%
  filter(str_sub(RecipCode,2,2)!="C") %>%
  filter(str_sub(RecipCode,2,2)!="N") %>%
  filter(str_sub(RecipCode,2,2)!="O") %>%
  mutate(Outcome=str_sub(RecipCode,2,2)) %>%
  group_by(Party,Outcome) %>%
  summarise(NumCandidates=n())

ggplot(CandsOutcome,
       aes(x=Outcome,y=NumCandidates,fill=Party))+
  geom_bar(stat="identity",width=0.5)+
  scale_fill_manual("Party", values = c("3" = "#7CAE00",
                                        "D" = "#00BFC4",
                                        "R" = "#F8766D",
                                        "L" = "#BB9D00",
                                        "I" = "#E76BF3"))+
  xlab("Outcome")



## Plot 2: Election Outcome by Status

for (i in which(cands$RecipCode == "3l")) {
  cands$RecipCode[i] <- "3L"
}

CandsOutcome <- cands %>%
  select(CRPICO,RecipCode) %>%
  filter(str_sub(RecipCode,2,2)!="I") %>%
  filter(str_sub(RecipCode,2,2)!="C") %>%
  filter(str_sub(RecipCode,2,2)!="N") %>%
  filter(str_sub(RecipCode,2,2)!="O") %>%
  filter(CRPICO!=" ") %>%
  mutate(Outcome=str_sub(RecipCode,2,2)) %>%
  group_by(CRPICO,Outcome) %>%
  summarise(NumCandidates=n())

ggplot(CandsOutcome,
       aes(x=Outcome,y=NumCandidates,fill=CRPICO))+
  geom_bar(stat="identity",width=0.5)+
  scale_fill_manual("CRPICO", values = c("C" = "#7CAE00",
                                         "I" = "#00BFC4",
                                         "O" = "#F8766D"))+
  xlab("Outcome")


## Plot 3: Total PAC Contribution by Party

pacContribution <- pacs %>%
  select(CID,Amount,DI) %>%
  filter(DI=="D") %>%
  group_by(CID) %>%
  summarise(Contribution = sum(Amount))

PACtoCand <- left_join(cands,pacContribution,by="CID")


PACtoCandidate <- PACtoCand %>%
  select(Party,RecipCode,Contribution) %>%
  filter(!is.na(Contribution)) %>%
  filter(str_sub(RecipCode,2,2)!="I") %>%
  filter(str_sub(RecipCode,2,2)!="C") %>%
  filter(str_sub(RecipCode,2,2)!="N") %>%
  filter(str_sub(RecipCode,2,2)!="O") %>%
  mutate(Outcome=str_sub(RecipCode,2,2)) %>%
  group_by(Outcome,Party) %>%
  summarise(Contribution=sum(Contribution))

ggplot(PACtoCandidate,
       aes(x=Outcome,y=Contribution/1000000,fill=Party))+
  geom_bar(stat="identity",width=0.5)+
  scale_fill_manual("Party", values = c("3" = "#7CAE00",
                                        "D" = "#00BFC4",
                                        "R" = "#F8766D",
                                        "L" = "#BB9D00",
                                        "I" = "#E76BF3")) +
  ylab("Total Amount ($ in millions)")



## Plot 4: Average PAC Contribution by Party

PACtoCandidate <- PACtoCand %>%
  select(Party,RecipCode,Contribution) %>%
  filter(!is.na(Contribution)) %>%
  filter(str_sub(RecipCode,2,2)!="I") %>%
  filter(str_sub(RecipCode,2,2)!="C") %>%
  filter(str_sub(RecipCode,2,2)!="N") %>%
  filter(str_sub(RecipCode,2,2)!="O") %>%
  mutate(Outcome=str_sub(RecipCode,2,2)) %>%
  group_by(Outcome,Party) %>%
  summarise(AvgAmount=mean(Contribution)) %>%
  mutate(PartyOutcome=paste(Party,Outcome,sep=""))

ggplot(PACtoCandidate,
       aes(x=PartyOutcome,y=AvgAmount/1000,fill=Party))+
  geom_bar(stat="identity",width=0.75)+
  scale_fill_manual("Party", values = c("3" = "#7CAE00",
                                        "D" = "#00BFC4",
                                        "R" = "#F8766D",
                                        "L" = "#BB9D00",
                                        "I" = "#E76BF3")) +
  ylab("Average Amount ($ in thousands)")


## Plot 5: Total PAC Contribution by Status

PACtoCandidate <- PACtoCand %>%
  select(CRPICO,RecipCode,Contribution) %>%
  filter(CRPICO!=" ") %>%
  filter(!is.na(Contribution)) %>%
  filter(str_sub(RecipCode,2,2)!="I") %>%
  filter(str_sub(RecipCode,2,2)!="C") %>%
  filter(str_sub(RecipCode,2,2)!="N") %>%
  filter(str_sub(RecipCode,2,2)!="O") %>%
  mutate(Outcome=str_sub(RecipCode,2,2)) %>%
  group_by(Outcome,CRPICO)

ggplot(PACtoCandidate,
       aes(x=Outcome,y=Contribution/1000000,fill=CRPICO))+
  geom_bar(stat="identity",width=0.5)+
  scale_fill_manual("CRPICO", values = c("C" = "#7CAE00",
                                         "I" = "#00BFC4",
                                         "O" = "#F8766D")) +
  ylab("Total Amount ($ in millions)")

## Plot 6: Average PAC Contribution by Status

PACtoCandidate <- PACtoCand %>%
  select(CRPICO,RecipCode,Contribution) %>%
  filter(CRPICO!=" ") %>%
  filter(!is.na(Contribution)) %>%
  filter(str_sub(RecipCode,2,2)!="I") %>%
  filter(str_sub(RecipCode,2,2)!="C") %>%
  filter(str_sub(RecipCode,2,2)!="N") %>%
  filter(str_sub(RecipCode,2,2)!="O") %>%
  mutate(Outcome=str_sub(RecipCode,2,2)) %>%
  group_by(Outcome,CRPICO) %>%
  summarise(AvgAmount=mean(Contribution)) %>%
  mutate(CRPICOOutcome=paste(CRPICO,Outcome,sep=""))

ggplot(PACtoCandidate,
       aes(x=CRPICOOutcome,y=AvgAmount/1000,fill=CRPICO))+
  geom_bar(stat="identity",width=0.5)+
  scale_fill_manual("CRPICO", values = c("C" = "#7CAE00",
                                         "I" = "#00BFC4",
                                         "O" = "#F8766D")) +
  ylab("Average Amount ($ in thousands)")


# Further exploratory data analysis was conducted after the data processing and feature engineering.

# Data Preprocessing



## Merging PACs and industries

rm(pacs)

industries=read.csv("CRP_Categories.csv" )
colnames(industries)[1]="RealCode"

pacs18 <- read.csv("pacs18.csv",header=TRUE)  %>% filter(DI=="D")
pacs16 <- read.csv("pacs16.csv",header=TRUE )%>% filter(DI=="D")
pacs14 <- read.csv("pacs14.csv",header=TRUE) %>% filter(DI=="D")


pacs18$Date<- as.Date(pacs18$Date,format="%m/%d/%Y")
pacs16$Date<- as.Date(pacs16$Date,format="%m/%d/%Y")
pacs14$Date<- as.Date(pacs14$Date,format="%m/%d/%Y")
pacs18 <- pacs18%>% filter(Date<="2018-11-6")
pacs16 <- pacs16%>% filter(Date<="2016-11-8")
pacs14 <- pacs14%>% filter(Date<="2014-11-4")

pacs<- rbind(pacs18,pacs16,pacs14)

### We only keep the key and the name of the sector
pacs_and_industries <- merge(pacs,industries[,c(1,6)],by ="RealCode")

columns_to_keep = c("Cycle","CID","Amount","Date","Sector.Long")
pacs_and_industries<- pacs_and_industries[,columns_to_keep]


## Contributions by timeframe
timeline_contributions <- pacs_and_industries %>% group_by(CID) %>% filter(Cycle==2018) %>% select(Cycle,CID,Amount,Date) %>% 
  mutate(
    Amount_2018 = sum(Amount),
    Three_M_Cont_2018=sum(Amount[Date>="2018-8-6"]),
    Six_M_Cont_2018=sum(Amount[Date>="2018-5-6"]),
    Twelve_M_Cont_2018=sum(Amount[Date>="2017-11-6"])
  ) %>% select(CID,Amount_2018,Three_M_Cont_2018,Six_M_Cont_2018,Twelve_M_Cont_2018) %>% distinct(CID,.keep_all = TRUE)  %>%  ungroup()


amount_2016 <- pacs_and_industries %>% filter(Cycle==2016) %>% group_by(CID) %>% select(Cycle,CID,Amount) %>% mutate(Amount_2016=sum(Amount)) %>% select(CID,Amount_2016) %>% distinct(CID,.keep_all = TRUE)  %>%  ungroup()

amount_2014 <-pacs_and_industries %>% filter(Cycle==2014) %>% group_by(CID) %>% select(Cycle,CID,Amount) %>% mutate(Amount_2014=sum(Amount)) %>% select(CID,Amount_2014) %>% distinct(CID,.keep_all = TRUE)  %>%  ungroup()

## Competitiveness of race

number_candidates <-cands %>% filter(CurrCand=="Y") %>% 
  group_by(DistIDRunFor) %>% summarise(
    n_candidates_final=n() ) %>% ungroup() %>% as.data.frame()
head(number_candidates)

## Contributions by industries

ind<- pacs_and_industries %>% group_by(CID) %>% filter(Cycle==2018) %>% 
  
  
  mutate(Amount_Agribusiness_2018=sum(Amount[Sector.Long=="Agribusiness"]),
         Amount_Candidate_2018=sum(Amount[Sector.Long=="Candidate"]),
         Amount_Communications_Electronics_2018=sum(Amount[
           Sector.Long=="Communications/Electronics"]),
         Amount_Construction_2018=sum(Amount[
           Sector.Long=="Construction"]),
         Amount_Defense_2018=sum(Amount[
           Sector.Long=="Defense"]),
         Amount_Energy_Natural_Resources_2018=sum(Amount[
           Sector.Long=="Energy & Natural Resources"]),
         Amount_Finance_Insurance_Real_Estate_2018=sum(Amount[
           Sector.Long=="Finance, Insurance & Real Estate"]),
         Amount_Health_2018=sum(Amount[
           Sector.Long=="Health"]),
         Amount_Ideological_Single_Issue_2018=sum(Amount[
           Sector.Long=="Ideological/Single-Issue"]),
         Amount_Joint_Candidate_Cmtes_2018=sum(Amount[
           Sector.Long=="Joint Candidate Cmtes"]),
         Amount_Labor_2018=sum(Amount[
           Sector.Long=="Labor"]),
         Amount_Lawyers_Lobbyists_2018=sum(Amount[
           Sector.Long=="Lawyers & Lobbyists"]),
         Amount_Misc_Business_2018=sum(Amount[
           Sector.Long=="Misc Business"]),
         Amount_Non_contribution_2018=sum(Amount[
           Sector.Long=="Non-contribution"]),
         Amount_Other_2018=sum(Amount[
           Sector.Long=="Other"]),
         Amount_Party_Cmtes_2018=sum(Amount[
           Sector.Long=="Party Cmtes"]),
         Amount_Transportation_2018=sum(Amount[
           Sector.Long=="Transportation"]),
         Amount_Unknown_2018=sum(Amount[
           Sector.Long=="Unknown"])
         
  ) %>%
  
  distinct(CID,.keep_all = TRUE)  %>%  ungroup()

ind$Amount<- NULL
ind$Date <- NULL
ind$Sector.Long<- NULL
head(ind)

## Merging data

c<-left_join(cands,amount_2014,by="CID")
c$CID<- as.factor(c$CID)

c<- left_join(c,amount_2016,by="CID")
c$CID<- as.factor(c$CID)


c<- left_join(c,timeline_contributions,by="CID")
c$CID<- as.factor(c$CID)


c <- left_join(c,ind,by="CID")
c$CID<- as.factor(c$CID)

c <- left_join(c,number_candidates,by="DistIDRunFor")
c$DistIDRunFor <- as.factor(c$DistIDRunFor)
colnames(c)[1]<- "Cycle"


## NA contributions, Outcome and removing wrongly coded data

df <- c %>%
  mutate(State = substr(DistIDRunFor,1,2)) %>%
  mutate(Outcome = substr(RecipCode,2,2)) %>%
  filter(Outcome != "I") %>%
  filter(Outcome != "C") %>%
  filter(Outcome != "O") %>%
  filter(Outcome != "N")

for (i in which(df$Outcome=="W")){
  df$Outcome[i] <- 1
}
for (i in which(df$Outcome=="L")){
  df$Outcome[i] <- 0
}

df$Cycle.y<- NULL


df[,13:36][is.na(df[,13:36])]<- 0
df$Outcome[df$Outcome=="l"]<- 1
df$Outcome <- as.numeric(df$Outcome)

df$CRPICO[df$CRPICO==" "]<- NA
df<-df[!is.na(df$CRPICO),]

df <- df %>%
  distinct(CID,.keep_all=TRUE)


df <- df[!is.na(df$n_candidates_final),]


write.csv(df,"data.csv",row.names=F)





#We can now complete our exploration of the data, leveraging the interaction between PACs industries and types of candidates.


## Plot 7: Number of PAC Contributions by Sector

ggplot(data=pacs_and_industries %>% filter(Cycle==2018) %>% 
         group_by(Sector.Long) %>% 
         summarise(number_pacs_contributions=n() ))+
  
  geom_bar(aes(x=Sector.Long,y=number_pacs_contributions) ,stat = "identity"    ) + 
  coord_flip() + xlab("Sector")+ ylab("Number of PACs contributions") + ggtitle("Number of PACs contributions per sector in 2018")


## Plot 8: Total PAC Contribution by Sector

ggplot(data=pacs_and_industries %>% filter(Cycle==2018) %>% 
         group_by(Sector.Long) %>% 
         summarise(amount_contributions=sum(Amount) ))+
  
  geom_bar(aes(x=Sector.Long,y=amount_contributions/(1000)) ,stat = "identity"    ) + 
  
  coord_flip() + xlab("Sector")+ ylab("Amount (in 1000$)")+ ggtitle("Total amount of PACs contributions per sector")


## Plot 9: Average PAC Contribution by Sector


ggplot(data=pacs_and_industries %>% filter(Cycle==2018) %>% 
         group_by(Sector.Long) %>% 
         summarise(amount_contributions=sum(Amount),
                   number_pacs_contributions=n(),
                   amount_per_contribution=amount_contributions/number_pacs_contributions ))+
  
  geom_bar(aes(x=Sector.Long,y=amount_per_contribution) ,stat = "identity"    ) + 
  
  coord_flip() + xlab("Sector")+ ylab("Amount")+ ggtitle("Average amount of PACs contributions per sector")


## Plot 10: Distribution of Defense PAC Contributions,message=FALSE


ggplot(df %>% filter(Amount_Defense_2018>0),aes(x=Amount_Defense_2018 ,
                                                fill=as.factor(CRPICO))) +
  geom_histogram() +
  xlab("Amount of contributions")+ 
  ylab("Count of values")  + ggtitle("Distribution of Defense PAC contributions according to types of candidates")+
  scale_fill_discrete (name='Status of the candidate', labels=c('Challenger','Incumbent',"Open seat"))


## Plot 11: Distribution of Single-Issue PAC Contributions


ggplot(df %>% filter(Amount_Ideological_Single_Issue_2018>0),aes(x=Amount_Ideological_Single_Issue_2018 ,
                                                                 fill=as.factor(CRPICO))) +
  geom_histogram() +
  xlab("Amount of contributions")+ 
  ylab("Count of values")  + ggtitle("Distribution of Single-issue PAC contributions according to types of candidates")+
  scale_fill_discrete (name='Status of the candidate', labels=c('Challenger','Incumbent',"Open seat"))



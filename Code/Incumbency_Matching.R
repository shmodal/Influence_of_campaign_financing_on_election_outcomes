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



# Predicting Incumbency from contributions


## Tree

set.seed(02138)
df_split <- sample.split(df$CRPICO,SplitRatio=0.7)
train <- df[df_split==T,]
test <- df[df_split==F,]


formula_status_money <- as.formula(paste("CRPICO~",formula,collapse=""))
tree1 <- rpart(formula_status_money,
               data = train , method="class",
               minbucket=5, cp=0.004)

prp(tree1,digits = 3,varlen=0,faclen=0)



pred_train <- predict(tree1,newdata= train,type="class")

confusion <- table(train$CRPICO, pred_train)
confusion



accuracy <- (confusion[1,1]+confusion[2,2]+confusion[3,3])/sum(confusion)
print("Accuracy on train")
print(accuracy)
print("Percentage of Open seats candidates predicted as Challenger")
print(confusion[3,1]/sum(confusion[3,])    )





pred_test <- predict(tree1,newdata= test,type="class")

confusion <- table(test$CRPICO, pred_test)
confusion



accuracy <- (confusion[1,1]+confusion[2,2]+confusion[3,3])/sum(confusion)
print("Accuracy on test")
print(accuracy)
print("Percentage of Open seats candidates predicted as Challenger")
print(confusion[3,1]/sum(confusion[3,])    )




# Matching: Effect of Incumbency on Election Results


## Variables
num_variables




  
  
## Plot SMD between groups

df_matching <- df %>% filter(CRPICO !='O')
df_matching$incumb<- df_matching$CRPICO=="I"
bl_c_i <- bal.tab(df_matching[num_variables], treat = df_matching$incumb, estimand="ATT", continuous="std")
love.plot(x = bl_c_i, stat = "mean.diffs", abs = TRUE, var.order = "unadjusted", threshold = 0.1)

## Matching

match_c_i <- Match(Tr=df_matching$incumb, M=1, X=df_matching[num_variables])

matched_c_i<- df_matching[unlist(match_c_i[c("index.treated","index.control")]), ]




matched_c_i$incumb<- as.numeric(matched_c_i$incumb)

balance_c_i <- bal.tab(matched_c_i[num_variables], treat = matched_c_i$incumb, estimand="ATT", continuous="std")
love.plot(x = balance_c_i, stat = "mean.diffs", abs = TRUE, var.order = "unadjusted", threshold = 0.1)

## ATE
mean(df_matching$Outcome[df_matching$incumb==TRUE])-mean(df_matching$Outcome[df_matching$incumb==FALSE])


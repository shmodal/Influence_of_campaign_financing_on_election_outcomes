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

# Model Training & Validation


df <- read.csv("data.csv",header=T)



# First, we decided to perform a penalized logistic regression through LASSO 
# We did a stratified split of the candidates into training (70%) and testing (30%) set based on the outcome variable.
# We did a 10-fold cross validation, 


set.seed(123)
df_split <- sample.split(df$Outcome,SplitRatio=0.7)
train <- df[df_split==T,]
test <- df[df_split==F,]


## Model 1

#Party, status, and amount of PAC contributions received by the candidate from 2014, 2016, and 2018 as the independent variables. 
#Model 1 has an out-of-sample AUC of 0.9768.


set.seed(123)

X_train <- sparse.model.matrix(~0+factor(Party)+Amount_2014+Amount_2016+Amount_2018+factor(CRPICO)
                               ,data=train)
y_train <- train$Outcome

X_test <- sparse.model.matrix(~0+factor(Party)+Amount_2014+Amount_2016+Amount_2018+factor(CRPICO)
                              ,data=test)
y_test <- test$Outcome

cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial",
                      alpha = 1, nfolds = 10, type.measure = "auc")

model <- glmnet(X_train,y_train,alpha=1,family="binomial",lambda=cv_lasso$lambda.min)

coefs <- data.frame(variable=rownames(coef(model)),estimate=(coef(model)[,1]))
rownames(coefs) <- 1:nrow(coefs)
subset(coefs,estimate!=0)

yhat_lasso_train <- predict(model,newx=X_train,type="response")
roc <- roc(response=y_train,predictor=as.numeric(yhat_lasso_train))
coords(roc,"best",transpose=T,ret=c("threshold","accuracy","precision","recall"))


###

yhat_lasso_test <- predict(model,newx=X_test,type="response")
prop.table(table(as.numeric(yhat_lasso_test>=0.1193021),y_test))

rocr.pred <- prediction(yhat_lasso_test,y_test)
rocr.pred.1 <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]],
                          tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])

ggplot() +
  geom_line(data=rocr.pred.1,aes(x=fpr,y=tpr),lwd=1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw() +
  scale_color_manual(values = c('black')) +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

auc <- performance(rocr.pred, "auc")@y.values[[1]]
print(auc)


## Model 2

#Party, status of the candidate  
#Model 2 has an out-of-sample AUC of 0.9543.


set.seed(123)

X_train <- sparse.model.matrix(~0+factor(Party)+factor(CRPICO),data=train)
y_train <- train$Outcome

X_test <- sparse.model.matrix(~0+factor(Party)+factor(CRPICO),data=test)
y_test <- test$Outcome

cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial",
                      alpha = 1, nfolds = 10, type.measure = "auc")

model <- glmnet(X_train,y_train,alpha=1,family="binomial",lambda=cv_lasso$lambda.min)

coefs <- data.frame(variable=rownames(coef(model)),estimate=(coef(model)[,1]))
rownames(coefs) <- 1:nrow(coefs)
subset(coefs,estimate!=0)

yhat_lasso_train <- predict(model,newx=X_train,type="response")
roc <- roc(response=y_train,predictor=as.numeric(yhat_lasso_train))
coords(roc,"best",transpose=T,ret=c("threshold","accuracy","precision","recall"))

## 

yhat_lasso_test <- predict(model,newx=X_test,type="response")
prop.table(table(as.numeric(yhat_lasso_test>=0.3857497),y_test))
rocr.pred <- prediction(yhat_lasso_test,y_test)
rocr.pred.2 <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]],
                          tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])

ggplot() +
  geom_line(data=rocr.pred.2,aes(x=fpr,y=tpr),lwd=1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw() +
  scale_color_manual(values = c('black')) +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

auc <- performance(rocr.pred, "auc")@y.values[[1]]
print(auc)

## Comparison

#Baseline
baseline <- data.frame(x=c(0,1),y=c(0,1))
#Ideal
ideal <- data.frame(x=c(0,0,1),y=c(0,1,1))

ggplot() +
  geom_line(data=rocr.pred.1,aes(x=fpr,y=tpr,colour='1'),lwd=1) +
  geom_line(data=rocr.pred.2,aes(x=fpr,y=tpr,colour='2'),lwd=1) +
  geom_line(data=baseline, aes(x=x, y=y,colour='3'),lwd=1) +
  geom_line(data=ideal, aes(x=x, y=y,colour='4'),lwd=1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw() +
  scale_color_manual(values = c('black','red','blue','green'), name = "",
                     labels = c("Model 1","Model 2","Baseline","Ideal")) +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18),
        legend.text=element_text(size=18))


#It seems as though CRPICO variable (status of the candidate), plays a large role in determining the outcome. 


## Model 3


#Only used monetary contributions as variables. 

  
## Variables used
num_variables <- colnames(df)[c(13:14,16:36)]
num_variables



## Modeling

formula <- paste(num_variables,collapse="+ ")
formula_money <- as.formula(paste("Outcome~",formula,collapse=""))


X_train <- sparse.model.matrix(formula_money,data=train)
y_train <- train$Outcome

X_test <- sparse.model.matrix(formula_money,data=test)
y_test <- test$Outcome

cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial",
                      alpha = 1, nfolds = 10, type.measure = "auc")

model <- glmnet(X_train,y_train,alpha=1,family="binomial",lambda=cv_lasso$lambda.min)

coefs <- data.frame(variable=rownames(coef(model)),estimate=(coef(model)[,1]))
rownames(coefs) <- 1:nrow(coefs)
subset(coefs,estimate!=0)

yhat_lasso_train <- predict(model,newx=X_train,type="response")
roc <- roc(response=y_train,predictor=as.numeric(yhat_lasso_train))
coords(roc,"best",transpose=T,ret=c("threshold","accuracy","precision","recall"))


## 

yhat_lasso_test <- predict(model,newx=X_test,type="response")
prop.table(table(as.numeric(yhat_lasso_test>=0.0803195),y_test))
rocr.pred <- prediction(yhat_lasso_test,y_test)
rocr.pred.2 <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]],
tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])

ggplot() +
geom_line(data=rocr.pred.2,aes(x=fpr,y=tpr),lwd=1) +
xlab("False Positive Rate") +
ylab("True Positive Rate") +
theme_bw() +
scale_color_manual(values = c('black')) +
theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

auc <- performance(rocr.pred, "auc")@y.values[[1]]
print(auc)




#'
#'
load("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/BaseGood/n80/data80_p10.rda")
library(xlsx)
library(MASS)
library(tidyverse)
library(Metrics)
library(MLmetrics)       # Confusion Metrices
library(dplyr)
require(caret) 


attach(base.df)

for (i in 1:length(base.df)){           
  base.df[[i]]$Adoption<-as.factor(base.df[[i]]$Adoption)
  base.df[[i]]$OFIOui<-as.factor(base.df[[i]]$OFIOui)
  base.df[[i]]$MachineryOOui<-as.factor(base.df[[i]]$MachineryOOui)
#  base.df[[i]]$AninamlTOui <-as.numeric(base.df[[i]]$AninamlTOui)
#  base.df[[i]]$InterInstOui<-as.numeric(base.df[[i]]$InterInstOui)
  base.df[[i]]$Region<-as.factor(base.df[[i]]$Region)
}
attach(base.df[[i]])


#'           The stepwise model (p_val) Selection des variables
#'  


Cov<- vector("list", length(base.df))
MSE<-vector("list",length(base.df))
Dev<-vector("list",length(base.df))
Rsq<- vector("list", length(base.df))
RMSE<- vector("list", length(base.df))
Preci<- vector("list", length(base.df))
Accuracy_P<-vector("list",length(base.df))
varia<-vector("list",length(base.df))
varimp<-vector("list",length(base.df))
attr(Cov,'names')<-paste('Cova',1:length(base.df),sep = "")
attr(MSE,'names')<-paste('MSE',1:length(base.df),sep = "")
attr(Dev,'names')<-paste('Dev',1:length(base.df),sep = "")
attr(Accuracy_P,'names')<-paste('Accuracy_P',1:length(base.df),sep = "")
attr(Rsq,'names')<-paste('Rsq',1:length(base.df),sep = "")
attr(RMSE,'names')<-paste('RMSE',1:length(base.df),sep = "")
attr(Preci,'names')<-paste('Preci',1:length(base.df),sep = "")
attr(varia,'names')<-paste('varia',1:length(base.df),sep = "")
attr(varimp,'names')<-paste('varimp',1:length(base.df),sep = "")


for (i in 1:length(base.df)){
  #      Make training
  #Use 70% of dataset as training set and remaining 30% as testing set
  Sample<- sample(c(TRUE, FALSE), nrow(base.df[[i]]), replace=TRUE, prob=c(0.7,0.3))
  train <- base.df[[i]][Sample, ]
  test <- base.df[[i]][!Sample, ]
  #####################
  ytrain <- train$Adoption
  xtrain <- data.matrix(train[,-1])
  ## test
  ytest <- test$Adoption
  xtest <- data.frame(test[,-1])
  # Stepwise
  model <- glm(Adoption ~ .,data = train, family = binomial(logit))
  
  # Select the most contributive variables
  modelstepboth<-model%>%stepAIC(trace = FALSE)
  a<-summary(modelstepboth)
  Cov[[i]]<-(length(a$coefficients[,1])-1)     # nber of covariate selected
  MSE[[i]]<-mean(modelstepboth$residuals^2)   # mean square error (RMSE)
  Dev[[i]]<-modelstepboth$deviance             # Deviance
  #calculate McFadden's R-Squared
  Rsq[[i]]<-pscl::pR2(model)["McFadden"]
  #
  #     Make test set
  # Make predictions
  glm.probs <- predict(modelstepboth,method = "cv",number = 10,test, type = "response")
  glm.pred <- ifelse(glm.probs > 0.5, "1", "0")
  # Prediction accuracy
  observed.classes <- test$Adoption
  Accuracy_P[[i]]<-mean(glm.pred == observed.classes)
  # Compute RMSE
  RMSE[[i]]<-rmse(as.numeric(observed.classes),glm.probs)
  ################ 
  aa<-as.factor(glm.pred)
  cm<-confusionMatrix(data=aa,reference=test$Adoption) # cm is the confusion matrix
  Preci[[i]]<-cm$byClass['Pos Pred Value']
  varia[[i]]<-as.character(modelstepboth$coefficients[-1])
  varimp[[i]]<-varImp(model)
}


setwd("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/BaseGood/n80")
write.xlsx(Cov,file="Cov_stepw.xlsx")
write.xlsx(MSE,file="MSE_stepw.xlsx")
write.xlsx(Dev,file="Dev_step.xlsx")
write.xlsx(Accuracy_P,file="Accuracy_P.xlsx")
write.xlsx(Rsq,file="RsqStepw.xlsx")
write.xlsx(RMSE,file="rmseStepw.xlsx")
write.xlsx(Preci,file="Precision1.xlsx")
write.xlsx(varia,file="varia_stepw.xlsx")
write.xlsx(varimp,file="varimp_stepw.xlsx")



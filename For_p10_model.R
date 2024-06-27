#'
#'
load("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/BaseGenerate/n80/data80_p27.rda")
library(xlsx)
library(MASS)
library(tidyverse)
library(Metrics)
library(MLmetrics)       # Confusion Metrices
library(dplyr)
require(caret) 
require(utils)

attach(base.df)
for (i in 1:length(base.df)){           
  base.df[[i]]$Adoption<-as.factor(base.df[[i]]$Adoption)
  #base.df[[i]]$Education <-as.factor(base.df[[i]]$Education)
  base.df[[i]]$OFIOui<-as.factor(base.df[[i]]$OFIOui)
  #base.df[[i]]$MachineryOOui<-as.factor(base.df[[i]]$MachineryOOui)
  #base.df[[i]]$RAOui <-as.factor(base.df[[i]]$RAOui)
  base.df[[i]]$TRFOui<-as.factor(base.df[[i]]$TRFOui)
  base.df[[i]]$UFertilizerOui<-as.factor(base.df[[i]]$UFertilizerOui)
  base.df[[i]]$UPesticidesOui<-as.factor(base.df[[i]]$UPesticidesOui)
  #base.df[[i]]$AninamlTOui <-as.factor(base.df[[i]]$AninamlTOui)
  base.df[[i]]$RMCropOui <-as.factor(base.df[[i]]$RMCropOui)
  #base.df[[i]]$NGOSOui<-as.factor(base.df[[i]]$NGOSOui)
  #base.df[[i]]$InterInstOui<-as.factor(base.df[[i]]$InterInstOui)
  #base.df[[i]]$Region<-as.factor(base.df[[i]]$Region)
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
  ############################################################################
  ############################################################################
  
  if ((0 %in% ytrain == T)&&(0 %in% ytest == T)){
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
  } else {
    next
  }
}



setwd("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Results/80StepW")

write.csv(as.matrix(Cov), file = "Cov_stepw.csv")
write.csv(as.matrix(MSE),file="MSE_stepw.csv")
write.csv(as.matrix(Dev),file="Dev_step.csv")
write.csv(as.matrix(Accuracy_P),file="Accuracy_P.csv")
write.csv(as.matrix(Rsq),file="RsqStepw.csv")
write.csv(as.matrix(RMSE),file="rmseStepw.csv")
write.csv(as.matrix(Preci),file="Precision.csv")
write.csv(as.matrix(varia),file="varia_stepw.csv")
write.csv(as.matrix(varimp),file="varimp_stepw.csv")



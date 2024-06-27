
# For p=27

'#####################
#########################################################################
##
##
########################################################################
#'
load("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/BaseGenerate/n1000/data1000_p20.rda")

#' load package
library(glmnet)
library(MASS)
library(xlsx)
library(Metrics)
library(MLmetrics)
library(dplyr)
library(caret)
library(tidyverse)
require(utils)
require(ncvreg)
# 
attach(base.df)


#'           The lasso Selection des variables
#'  


Cov<- vector("list", length(base.df))
MSE<-vector("list",length(base.df))
Dev<-vector("list",length(base.df))
Rsq<- vector("list", length(base.df))
RMSE<- vector("list", length(base.df))
Preci<- vector("list", length(base.df))
Accuracy_P<-vector("list",length(base.df))
attr(Cov,'names')<-paste('Cova',1:length(base.df),sep = "")
attr(MSE,'names')<-paste('MSE',1:length(base.df),sep = "")
attr(Dev,'names')<-paste('Dev',1:length(base.df),sep = "")
attr(Accuracy_P,'names')<-paste('Accuracy_P',1:length(base.df),sep = "")
attr(Rsq,'names')<-paste('Rsq',1:length(base.df),sep = "")
attr(RMSE,'names')<-paste('RMSE',1:length(base.df),sep = "")
attr(Preci,'names')<-paste('Preci',1:length(base.df),sep = "")

for (i in 1:length(base.df)){
  #      Make training
  #Use 70% of dataset as training set and remaining 30% as testing set
  Sample<- sample(c(TRUE, FALSE), nrow(base.df[[i]]), replace=TRUE, prob=c(0.7,0.3))
  train <- base.df[[i]][Sample, ]
  test <- base.df[[i]][!Sample, ]
  #####################
  ytrain <- train$Adoption
  xtrain <- data.matrix(train[,-1])
  nco1<-(ncol(xtrain))
  xtrain<-model.matrix(~ Age+AninamlTOui+ Education+ Experience+FOPR+HouseSize+InterInstOui+
                       LivestockO+LRS+MachineryOOui+ NGOSOui+ OFIOui+RAOui+Region+ RMCropOui+
                         TFarmSize+TRFOui+TWF+UFertilizerOui+UPesticidesOui-1,data=as.data.frame(xtrain[,-nco1]))
  #xtrain<-model.matrix(~.,data = xtrain)
  ##### test
  ytest <- test$Adoption
  xtest <- data.matrix(test[,-1])
  nco2<-(ncol(xtest))
  xtest <-xtest[,-nco2]
  xtest<-model.matrix(~ Age+AninamlTOui+ Education+ Experience+FOPR+HouseSize+InterInstOui+
                         LivestockO+LRS+MachineryOOui+ NGOSOui+ OFIOui+RAOui+Region+ RMCropOui+
                         TFarmSize+TRFOui+TWF+UFertilizerOui+UPesticidesOui-1,data=as.data.frame(xtest[,-nco1]))
  #
  
  if ((0 %in% ytrain == T)&&(0 %in% ytest == T)){
      #fit lasso regression model using k-fold cross-validation
      cv_model <- cv.glmnet(xtrain, ytrain, alpha = 1,family = "binomial")
      best_lambda <- cv_model$lambda.min
      # best_lambda
      #view coefficients of best model
      best_model <-glmnet(xtrain, ytrain, alpha =1, lambda = best_lambda,family = "binomial")
      best_model1 <-glmnet(xtrain, ytrain, alpha =1,family = "binomial")
      #
      #coef(best_model)
      # To obtain the number of variables retain
      Cov[[i]]<-best_model$df         # nber of covariate selected
      Dev[[i]]<-deviance(best_model)                      # Deviance
      ####
      g<-assess.glmnet(best_model,           #in this case, we are evaluating the model
                       newx = xtrain,              #in the same data used to fit the model
                       newy = ytrain)             #so newx=X and newY=Y
      MSE[[i]]<-g$mse                        # mean square error (MSE)
      #calculate McFadden's R-Squared
      Rsq[[i]]<-best_model$dev.ratio
      
      #make a prediction for the response value of a new observation
      ###
      #y_predicted <- predict(best_model,s = best_lambda, newx = xtest)
      lasso_pred = predict(best_model, s = best_lambda, newx = xtest,type="response") # Use best lambda to predict test data
      lasso_pred2 <- ifelse(lasso_pred > 0.5, "1", "0")
      #mean((lasso_pred - ytest)^2) # Calculate test MSE
      RMSE[[i]]<-rmse(ytest,lasso_pred)
      #RMSE[[i]]<-sqrt(mean((lasso_pred - ytest)^2))
      #####################
      Accuracy_P[[i]]<-mean(lasso_pred2 == test$Adoption)
      # Build a confusion matrix
      cm<-confusion.glmnet(best_model, newx =xtest, newy =ytest, s =best_lambda ,family="binomial")
      #table(lasso_pred2,ytest)
      #aa<-as.factor(lasso_pred2)
      #vv<-as.factor(test$Adoption)
      #cm<-confusionMatrix(data=aa,reference =vv ) # cm is the confusion matrix
      #accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
      Preci[[i]] <- cm[4] / sum(cm[4],cm[2])
      #Preci[[i]]<- cm$byClass['Pos Pred Value'] 
      #  sensitiv[[i]]<- cm$byClass['Sensitivity']
      #  specifi[[i]]<-cm$byClass['Specificity']
      # varia[[i]]<-t$coefficients[-1]  # the variables selected
      # Lasso results (absolute values, sorted)
      #coefs = coef(best_model)[-1,1]
      #coefs = sort(abs(coefs), decreasing = F)
      #va<-(coefs!=0)
    } else {
      next
    }
}





#


setwd("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Results/Lasso/lasso_500/500_10Lass")

write.csv(as.matrix(Cov), file = "Cov_lasso.csv")
write.csv(as.matrix(MSE),file="MSE_lasso.csv")
write.csv(as.matrix(Dev),file="Dev_lasso.csv")
write.csv(as.matrix(Accuracy_P),file="AccuracyLasso.csv")
write.csv(as.matrix(Rsq),file="Rsqlasso.csv")
write.csv(as.matrix(RMSE),file="rmselasso.csv")
write.csv(as.matrix(Preci),file="PrecisionLa.csv")
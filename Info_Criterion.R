# 
#                     INFORMATION CRITERION: AIC and BIC  Methode             #
#' 
#'  @author Hubert A DOSSA

load("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/BaseGenerate/n80/data80_p20.rda")
# load necessary package
library(xlsx)
library(Metrics)
library(MLmetrics)
library(dplyr)
library(caret)

## Transform variable to categorie
attach(base.df)  # attach data

for (i in 1:length(base.df)){           
  base.df[[i]]$Adoption<-as.factor(base.df[[i]]$Adoption)
}

attach(base.df[[i]])

Cov<- vector("list", length(base.df))
MSE<-vector("list",length(base.df))
Dev<-vector("list",length(base.df))
Rsq<- vector("list", length(base.df))
RMSE<- vector("list", length(base.df))
Preci<- vector("list", length(base.df))
Accuracy_P<-vector("list",length(base.df))
#varia<-vector("list",length(base.df))
#varimp<-vector("list",length(base.df))
sensitiv<- vector("list", length(base.df))
#fscore<- vector("list", length(base.df))
specifi<- vector("list", length(base.df)) 
aic<-vector("list", length(base.df)) 
#########
attr(Cov,'names')<-paste('Cova',1:length(base.df),sep = "")
attr(MSE,'names')<-paste('MSE',1:length(base.df),sep = "")
attr(Dev,'names')<-paste('Dev',1:length(base.df),sep = "")
attr(Accuracy_P,'names')<-paste('Accuracy_P',1:length(base.df),sep = "")
attr(Rsq,'names')<-paste('Rsq',1:length(base.df),sep = "")
attr(RMSE,'names')<-paste('RMSE',1:length(base.df),sep = "")
attr(Preci,'names')<-paste('Preci',1:length(base.df),sep = "")
#attr(varia,'names')<-paste('varia',1:length(base.df),sep = "")
#attr(varimp,'names')<-paste('varimp',1:length(base.df),sep = "")
attr(sensitiv,'names')<-paste('sensitiv',1:length(base.df),sep = "")
#attr(fscore,'names')<-paste('fscore',1:length(base.df),sep = "")
attr(specifi,'names')<-paste('specifi',1:length(base.df),sep = "")
attr(aic,'names')<-paste('aic',1:length(base.df),sep = "")


for (i in 1:500){
  ###
  lowest_aic <- Inf     # added
  best_model <- NULL    # added
  ####
  #      Make training
  #Use 70% of dataset as training set and remaining 30% as testing set
  Sample<- sample(c(TRUE, FALSE), nrow(base.df[[i]]), replace=TRUE, prob=c(0.7,0.3))
  train <- base.df[[i]][Sample, ]
  test <- base.df[[i]][!Sample, ]
  #####################
  ytrain <- train$Adoption
  xtrain <- train[,-1]
  ## test
  ytest <- test$Adoption
  xtest <- test[,-1]
  ####################
  ###############
  mod_headers <- names(xtrain[1:ncol(xtrain)])  # Header of the covariate
#############################
#             Fit model Info Criterion 
#############################
  if ((0 %in% ytrain == T)&&(0 %in% ytest == T)){
    #######################
    for(j in 1:length(mod_headers)){
      tab <- combn(mod_headers,j)
      for(j in 1:ncol(tab)){
        tab_new <- tab[, j]
        mod_tab_new <- c(tab_new, "Adoption")
        model <- glm(Adoption ~., data=train[mod_tab_new], family= binomial(link ="logit"),
                     control = list(maxit = 50))
        if(AIC(model) < lowest_aic){ # added
          lowest_aic <- AIC(model)   # added
          best_model <- model        # added
        }
      }
      t=best_model
    }
    Cov[[i]]<-(length((t$coefficients))-1)     # number of variables selected
    Dev[[i]]<-t$deviance                       # Deviance
    MSE[[i]]<-mean(t$residuals^2)         # mean square error (MSE)
    aic[[i]]<-t$aic
    #calculate McFadden's R-Squared
    Rsq[[i]]<-pscl::pR2(t)["McFadden"]
    ###
    #     Make test set
    # Make predictions
    glm.probs <- predict(t,method = "cv",number = 10,train, type = "response")
    glm.pred <- ifelse(glm.probs > 0.5, "1", "0")
    # Prediction accuracy
    observed.classes <- train$Adoption
    Accuracy_P[[i]]<-mean(glm.pred == observed.classes)
    # Compute RMSE
    RMSE[[i]]<-rmse(as.numeric(observed.classes),glm.probs)
    aa<-as.factor(glm.pred)
    cm<-confusionMatrix(data=aa,reference = train$Adoption) # cm is the confusion matrix
    #accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
    Preci[[i]]<- cm$byClass['Pos Pred Value'] 
    sensitiv[[i]]<- cm$byClass['Sensitivity']
    specifi[[i]]<-cm$byClass['Specificity']
#    varia[[i]]<-t$coefficients[-1]  # the variables selected
#    varimp[[j]]<-varImp(t)
  } else {
    next
  }
}

   

################ 

setwd("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Results/80Info")

################ 

write.csv(as.matrix(Cov),file="Cov_aic.csv")
write.csv(as.matrix(MSE),file="MSE_aic.csv")
write.csv(as.matrix(Dev),file="Dev_aic.csv")
write.csv(as.matrix(Accuracy_P),file="Accuracy_aic.csv")
write.csv(as.matrix(Rsq),file="Rsq_aic.csv")
write.csv(as.matrix(RMSE),file="rmse_aic.csv")
write.csv(as.matrix(Preci),file="Precision_aic.csv")
#write.csv(as.matrix(varia),file="varia_aic.csv")
write.csv(as.matrix(sensitiv),file="sensitiv_aic.csv")
write.csv(as.matrix(specifi),file="specifi_aic.csv")
write.csv(as.matrix(aic),file="aic_aic.csv")



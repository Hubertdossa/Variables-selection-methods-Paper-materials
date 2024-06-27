#########################################################################
##
##
########################################################################

load("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/BaseGenerate/n1000/data1000_p27.rda")

#library(glmnet)
require(plsVarSel)
require(pls)
require(plsRglm)
library(glmnet)
library(MASS)
library(Metrics)
library(MLmetrics)
library(dplyr)
library(caret)
library(tidyverse)
require(utils)
require(ncvreg)
library(mdatools)
attach(base.df)
#Define predictor and response variables
####
Cov<- vector("list", length(base.df))
MSE<-vector("list",length(base.df))
#Va<-vector("list",length(base.df))
Rsq<- vector("list", length(base.df))
RMSE<- vector("list", length(base.df))
Preci<- vector("list", length(base.df))
Accuracy_P<-vector("list",length(base.df))
sensitiv<-vector("list",length(base.df))
attr(Cov,'names')<-paste('Cova',1:length(base.df),sep = "")
attr(MSE,'names')<-paste('MSE',1:length(base.df),sep = "")
#attr(Va,'names')<-paste('Va',1:length(base.df),sep = "")
attr(Accuracy_P,'names')<-paste('Accuracy_P',1:length(base.df),sep = "")
attr(Rsq,'names')<-paste('Rsq',1:length(base.df),sep = "")
attr(RMSE,'names')<-paste('RMSE',1:length(base.df),sep = "")
attr(Preci,'names')<-paste('Preci',1:length(base.df),sep = "")
attr(sensitiv,'names')<-paste('sensitiv',1:length(base.df),sep = "")

for (i in 1:length(base.df)){
#      Make training
#Use 70% of dataset as training set and remaining 30% as testing set
Sample<- sample(c(TRUE, FALSE), nrow(base.df[[i]]), replace=TRUE, prob=c(0.7,0.3))
train <- base.df[[i]][Sample, ]
test <- base.df[[i]][!Sample, ]
#####################
ytrain <- (train$Adoption)
xtrain <- train[,-1]
## test
ytest <- test$Adoption
xtest <- test[,-1]
if ((0 %in% ytrain == T)&&(0 %in% ytest == T)){
# ici nous utilisons 7 variables latentes et sélectionnons 3 premières comme nombre optimal en utilisant la même méthode, selectCompNum()

m1<-pls(xtrain,ytrain,1, cv = 10,ncomp.selcrit = "min")
nbcomp<-(m1$ncomp.selected) # Two different ways/criteria for automatic selection (either 'min' or 'wold')

# Meilleurs models sans les variables non importantes
# Pour calculer les valeurs sans tracer l'utilisation vipscores()et selratio()
vip = vipscores(m1, ncomp = m1$ncomp.selected)
# For VIP
m3<-pls(xtrain, ytrain, scale = T, cv = 10,ncomp =m1$ncomp.selected,exclcols = (vip< 1))
bb=as.numeric(m3$coeffs$values)
Cov[[i]]<-(sum(bb!=0))     # number of variables selected
MSE[[i]]<-((sum(m3$res$cal$rmse))^2) # mean square error (MSE)
#Dev[[i]]<-t$deviance                       # Deviance
#calculate McFadden's R-Squared
Rsq[[i]]<-sum(m3$res$cal$r2)
##################################
pls.ypred = predict(m3,xtest, type="response")
pls.ypred2 <- ifelse(pls.ypred$y.pred > 0.8, "1", "0")
#mean((lasso_pred - ytest)^2) # Calculate test MSE
RMSE[[i]]<-rmse(ytest,pls.ypred$y.pred)
#RMSE[[i]]<-sqrt(mean((lasso_pred - ytest)^2))
#####################
Accuracy_P[[i]]<-mean(pls.ypred2 == ytest)
# Build a confusion matrix
#cm<-getConfusionMatrix(m3,newx =xtest, newy =ytest)
#getConfusionMatrix(m3)
#table(lasso_pred2,ytest)
aa<-as.factor(pls.ypred2)
vv<-as.factor(ytest)
cm<-confusionMatrix(data=aa,reference =vv ) # cm is the confusion matrix
Preci[[i]]<- cm$byClass['Pos Pred Value'] 
sensitiv[[i]]<- cm$byClass['Sensitivity']
} else {
  next
}
}


#'The p-values, t-values and standard errors are stored each as a 3-way array 
#'similar to regression coefficients. The selection can be made by comparing 
#'e.g. p-values with a threshold similar to what we have done with VIP-scores 
#'and selectivity ratio.
# FOR p.value
#mjk = pls(xtrain, ytrain, scale = TRUE, cv = 10)
## New model
#exclcols<-mjk$coeffs$p.values[,nbcomp, 1] > 0.05
#newm = pls(xtrain, ytrain, 3, scale = TRUE, cv = 10, exclcols=exclcols)
#summary(newm)


setwd("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Results/Pls_method/1000_pls/1000_27_pls")

write.csv(as.matrix(Cov), file = "Cov_pls.csv")
write.csv(as.matrix(MSE),file="MSE_pls.csv")
#write.csv(as.matrix(Va),file="Va_pls.csv")
write.csv(as.matrix(Accuracy_P),file="Accuracypls.csv")
write.csv(as.matrix(Rsq),file="Rsqpls.csv")
write.csv(as.matrix(RMSE),file="rmsepls.csv")
write.csv(as.matrix(Preci),file="Precisionpls.csv")
write.csv(as.matrix(sensitiv),file="sensitipls.csv")









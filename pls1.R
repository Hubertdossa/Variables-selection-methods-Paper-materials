loko<-read.table("clipboard",header = T)

attach(loko)
#loko$Adoption =as.factor(loko$Adoption)
#library(glmnet)
require(plsVarSel)
require(pls)
require(ranger)
#require(plsRglm)
#library(glmnet)
library(MASS)
library(Metrics)
library(MLmetrics)
library(dplyr)
library(caret)
library(tidyverse)
require(utils)
require(ncvreg)
library(mdatools)

#      Make training
#Use 70% of dataset as training set and remaining 30% as testing set
set.seed(2022)
Sample<- sample(c(TRUE, FALSE), nrow(loko), replace=TRUE, prob=c(0.7,0.3))
train <- loko[Sample, ]
test <- loko[!Sample, ]
#####################
ytrain <- as.vector(train$Adoption)
xtrain <- as.data.frame(train[,-1])
## test
ytest <- test$Adoption
xtest <- test[,-1]

m1<-pls(xtrain,ytrain,1, cv = 10,ncomp.selcrit = "min")

nbcomp<-(m1$ncomp.selected) # Two different ways/criteria for automatic selection (either 'min' or 'wold')

# Meilleurs models sans les variables non importantes
# Pour calculer les valeurs sans tracer l'utilisation vipscores()et selratio()
vip = vipscores(m1, ncomp= m1$ncomp.selected)

#plot(vip,lty=2,xlab="Covariates",ylab="Variables Importance",
#     main="", lwd=1.5,type="o",pch=2)
# For VIP
m3<-pls(xtrain, ytrain, scale = T, cv = 10,ncomp =2,exclcols = (vip< 1))
bb=as.numeric(m3$coeffs$values)
Cov<-(sum(bb!=0))     # number of variables selected
MSE<-((sum(m3$res$cal$rmse))^2) # mean square error (MSE)
#Dev[[i]]<-t$deviance                       # Deviance
#calculate McFadden's R-Squared
Rsq<-sum(m3$res$cal$r2)
##################################
pls.ypred = predict(m3,xtest, type="response")
pls.ypred2 <- ifelse(pls.ypred$y.pred[,1,1] > 0.5, "1", "0")
#mean((lasso_pred - ytest)^2) # Calculate test MSE
RMSE<-rmse(ytest,pls.ypred$y.pred)
#RMSE[[i]]<-sqrt(mean((lasso_pred - ytest)^2))
#####################
Accuracy_P<-mean(pls.ypred2 == ytest)
# Build a confusion matrix
#cm<-getConfusionMatrix(m3,newx =xtest, newy =ytest)
#getConfusionMatrix(m3)
#table(lasso_pred2,ytest)
aa<-as.factor(pls.ypred2)
vv<-as.factor(ytest)
cm<-confusionMatrix(data=aa,reference =vv ) # cm is the confusion matrix
Preci<-cm$byClass['Pos Pred Value'] 
sensitiv<- cm$byClass['Sensitivity']


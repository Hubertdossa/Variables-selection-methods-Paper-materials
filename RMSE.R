
##                         RMSE 

# 1 ways 
sqrt(mean((data$actual - data$predicted)^2))
2.041241

# 2 ways
library(Metrics)
rmse(data$actual, data$predicted)
2.041241



################          MSE 
lm <- lm(MuscleMAss~Age,data)

MSE<-mean(lm$residuals^2)





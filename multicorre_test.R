#========  How to Calculate Variance Inflation Factor   =======================#             
#========              (VIF) in  R                      =======================#
#
# Importe the data
data<-read.table("clipboard", header = TRUE)
data 									# Affiche la matrice de données dans R
attach(data)# Individualise les colonnes (variables) de la matrice de sorte qu'on puisse les manipuler separement
str(data) # Permet d'avoir les types et la dimension de la base
names(data)  							# Affiche les noms des colonnes (variables) de la matrice
dim(data)
require(car)
#fit the regression model

convert.magic <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}

out <- convert.magic(data,c('character','factor','numeric'))
str(out)

model<-glm(~ .,data = data, family = binomial(logit))


















model <- glm(Use_improved_varieties ~ Age + Experience +Livestock_ownership + Land_rice_size
            + Hired_farm_labour + Membership_association +Use_pesticides+Farmers_output_rice+
              NGOS+Male_head+ Education + Off_farm_Income+Machinery_ownership+Total_workforce+
              Risk_aversion+Rice_main_crop+Animal_traction+Off_season_rice+International_Inst+
              Female_head+Household_Size+Land_ownership+Total_farm_size+
              Family_workforce+Training_rice_farming+Use_fertilizer+Irrigation+Gvt_extensions+
              Region, data = data)

#       Model with only qualitative variables
model <- lm(Age ~+Farmers_output_rice+
              Livestock_ownership+Land_rice_size+Total_farm_size+Experience+
              Household_Size+Total_workforce, data = data)
vif(model)

# Model with only Categorical variable
model1 <- lm(Use_improved_varieties ~ Experience +Livestock_ownership + Land_rice_size
            + Hired_farm_labour + Membership_association +Use_pesticides+
              NGOS+Male_head+ Education + Off_farm_Income+Machinery_ownership+Total_workforce+
              Risk_aversion+Rice_main_crop+Animal_traction+Off_season_rice+International_Inst+
              Female_head+Household_Size+Land_ownership+Total_farm_size+
              Family_workforce+Training_rice_farming+Use_fertilizer+Irrigation+Gvt_extensions+
              Region, data = data)
vif(lm(Use_improved_varieties ~ Experience +Livestock_ownership + Land_rice_size
       + Hired_farm_labour + Membership_association +Use_pesticides+
         NGOS+Male_head+ Education + Off_farm_Income+Machinery_ownership+Total_workforce+
         Risk_aversion+Rice_main_crop+Animal_traction+Off_season_rice+International_Inst+
         Female_head+Household_Size+Land_ownership+Total_farm_size+
         Family_workforce+Training_rice_farming+Use_fertilizer+Irrigation+Gvt_extensions+
         Region, data = data))

#view the output of the regression model
summary(model)

#load the car library
library(car)

#calculate the VIF for each predictor variable in the model
vif(model)

# we can visualise the VIF value

#create vector of VIF values
vif_values <- vif(model)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)


#Visualizing Correlations Between Predictor Variables
#To gain a better understanding of why one predictor variable may have a high VIF
#value, we can create a correlation matrix to view the linear correlation 
#coefficients between each pair of variables:
  
  #define the variables we want to include in the correlation matrix
  data <- mtcars[ , c("disp", "hp", "wt", "drat")]

#create correlation matrix
cor(data)


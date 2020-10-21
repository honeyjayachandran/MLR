computer <- read.csv(file = "C:/Users/Sony/Downloads/MLR assignment -5/Computer_Data.csv")   
View(computer)    
summary(computer)

 
attach(computer)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(computer)
computer$cd <- as.numeric(computer$cd)
computer$multi <- as.numeric(computer$multi)
computer$premium <- as.numeric(computer$premium)
# The Linear Mo$el of interest with all the columns
model_c<-lm(price ~ .,data=computer)


summary(model_c)
 

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
install.packages("car")
library(car)
vif(model_c) # Original model
## vif>10 then there exists collinearity among all the variables 
#trend shows collineity
## Added Variable(partial regression plot) plot to check correlation b/n variables and o/p variable
avPlots(model_c)
# The above plots will reveal whether the Output price
# has an effect by changing the inputs
# From the graph we can see there is no change at all with 
# respect to trend

 
startup <- read.csv(file = "C:/Users/Sony/Downloads/MLR assignment -5/50_Startups.csv")
View(startup)
summary(startup)

# 7. Find the correlation b/n Output -Scatter plot
library(psych)
pairs.panels(startup) # strong correlation b/w profit and r.d,profitvand marketing,R.D and marketing

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
attach(startup)
startup$State <- as.numeric(startup$State)# changing state to numeric
cor(startup)
 
### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup))# no collinearity b/w r.d and marketing
?cor2pcor

# The Linear Model of interest with all the columns
model_startup <- lm(Profit ~ . , data=startup)
 
summary(model_startup) #parameters values of administration,state and marketing are insignificant

# Multicollinearity check
# Model based on only state
model.startupS<-lm(Profit~State,data=startup)
summary(model.startupS) # state became insignificant

# Model based on only marketing
model.startupM<-lm(Profit~Marketing.Spend,data=startup)
summary(model.startupM) # Marketing became significant

# Model based on only  Administration
model.startupA<-lm(Profit~Administration,data=startup)
summary(model.startupA) # Administration became insignificant

# Model based on Volume and Weight
model.carMSA<-lm(Profit~Marketing.Spend+State+Administration ,data=startup)
summary(model.carMSA) # Both marketing and administration are significant

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
install.packages("car")
library(car)
vif(model_startup) # Original model
## vif>10 then there exists collinearity among all the variables 
 # HERE NO MULTI COLLINENEARITY EXISTS
## Added Variable(partial regression plot) plot to check correlation b/n variables and o/p variable
avPlots(model_startup)
# The above plots will reveal whether the Output PROFIT
# has an effect by changing the inputs
# From the graph we can see there is no change at all with 
# respect to State


## VIF and AV plot has given us an indication to delete "State" variable

# Preparing new models by excluding State from inputs


# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model_startup)
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model_startup) # index plots for infuence measures
influencePlot(model.startup) # A user friendly representation of the above

# Regression after deleting the 50th observation, which is influential observation
model_1<-lm(Profit ~ .-State,data=startup[-50,])
summary(model_1)
plot(model_1)


# Regression after deleting the 49th & 50th Observations
model_2<-lm(Profit ~.-State,data=startup[-c(49,50),])
summary(model_2)



## Final model
par(mfrow= c(2,2))
plot(lm(Profit ~.-State,data=startup[-c(50),])) #50
summary(lm(Profit ~.-State,data=startup[-c(50),]))
plot(lm(Profit ~.-State,data=startup[-c(50,49),])) # 50,49
summary(lm(Profit ~.-State,data=startup[-c(50,49),]))
 

# Its not a feasible solution if we remove all the 
# influential values 
# We need to consider other assumptions to likes
# Heteroscadasticity | Normal Distribution of Residuals


finalmodel<-lm(Profit ~.-State,data=startup[-c(50,49),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution

 


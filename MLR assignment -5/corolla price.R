Corolla <- read.csv(file = "C:/Users/Sony/Downloads/MLR assignment -5/ToyotaCorolla.csv") # choose the Corolla.csv data set
View(Corolla)
summary(Corolla)
attach(Corolla)
plot(Age_08_04,Price)
# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
 

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
Corolla$Fuel_Type <- as.numeric(Corolla$Fuel_Type)
Corolla$Color <- as.numeric(Corolla$Color)
Corolla$Model <- as.numeric(Corolla$Model)
cor(Corolla)


# The Linear Model of interest with all the columns
model_car <- lm(Price ~ . , data=Corolla) #multiple r sqred=0.9122


summary(model_car)

# Multicollinearity check
# Model based on only cc 
model.carcc<-lm(Price~cc,data=Corolla)
summary(model.carcc) # cc became significant multiple r sqred=0.01597

# Model based on only Doors
model.card<-lm(Price~Doors,data=Corolla)
summary(model.card) # Doors became significant mulptiple r sqred=0.03435

# Model based on only Gears
model.carg<-lm(Price~Gears,data=Corolla)
summary(model.carg) # Doors became significant mulptiple r sqred=0.003982

# Model based on cc , DOORS AND gears
model.carccdg<-lm(Price~cc+Doors+Gears,data=Corolla)
summary(model.carccdg) #  became significant 

# Model based on only Age_08_04,km,hp,Quaterly_Tax and Weight
model.carm<-lm(Price~Age_08_04+KM+HP+Quarterly_Tax+Weight,data=Corolla)
summary(model.carm) # Doors became significant mulptiple r sqred= 0.8627

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
install.packages("car")
library(car)
vif(model_car) # Original model
## vif>10 then there exists collinearity among all the variables 

## Added Variable(partial regression plot) plot to check correlation b/n variables and o/p variable
avPlots(model_car)

# The above plots will reveal whether the Output  price
# has an effect by changing the inputs
# From the graph we can see there is no change at all with 
# respect to doors

## VIF and AV plot has given us an indication to delete "Doors" variable
# So there exists a collinearity problem 

# Preparing new models by excluding doors from inputs


# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations

influenceIndexPlot(model_car) # index plots for infuence measures
influencePlot(model_car) # A user friendly representation of the above

# Regression after deleting the 81st observation, which is influential observation
model_1<-lm(Price ~ .-Doors,data=Corolla[-81,])
summary(model_1)
plot(model_1)


# Regression after deleting the 81st & 222nd Observations
model_2<-lm(Price ~ .-Doors,data=Corolla[-c(81,222),])
summary(model_2)



## Final model
lm(Price ~ .-Doors,data=Corolla[-c(81),]) # 81
summary(lm(Price ~ .-Doors,data=Corolla[-c(81),]))
lm(Price ~ .-Doors,data=Corolla[-c(81,222),]) # 81,222
summary(lm(Price ~ .-Doors,data=Corolla[-c(81,222),]))
 

# Its not a feasible solution if we remove all the 
# influential values 
# We need to consider other assumptions to likes
# Heteroscadasticity | Normal Distribution of Residuals


finalmodel<-lm(Price ~ .-Doors,data=Corolla[-c(81,222),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution
 


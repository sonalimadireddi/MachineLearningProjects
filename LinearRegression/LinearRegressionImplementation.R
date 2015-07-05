#Linear Reggresion 
#Data loading and initial analysis 
setwd("~/Desktop/DataAnalyticsWork")
auto_data<-read.csv("auto-miles-per-gallon.csv")
str(auto_data)
summary(auto_data)
head(auto_data)

#Data Cleaning - Variable Imputation
auto_data$HORSEPOWER<-as.numeric(auto_data$HORSEPOWER)
auto_data$HORSEPOWER[is.na(auto_data$HORSEPOWER)]<-mean(auto_data$HORSEPOWER,na.rm=TRUE)
summary(auto_data)

library(ggplot2)
#Exploratory Data Analysis 

ggplot(auto_data, aes(factor(CYLINDERS), MPG)) +
  geom_boxplot( aes(fill=factor(CYLINDERS)))

ggplot(auto_data, aes(factor(CYLINDERS), DISPLACEMENT)) +
  geom_boxplot( aes(fill=factor(CYLINDERS)))

#Correlation Co-efficient 
library(psych)
pairs.panels(auto_data)

#Modelling and Prediction 
lm_model<-lm(MPG ~.,auto_data[,-8])
lm_model
summary(lm_model)

#Testing
predicted<-predict.lm(lm_model,auto_data)
summary(predicted)

plot(auto_data$MPG,predicted,col="red")

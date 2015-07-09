#Decision Trees 
#Data loading and initial analysis 
setwd("~/Desktop/DataAnalyticsWork")
iris_data<-iris
str(iris_data)
summary(iris_data)
head(iris_data)

#No cleaning required 
#Exploratory Data Analysis 

library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris_data,colour=Species, size=3)
qplot(Sepal.Length,Sepal.Width,data=iris_data,colour=Species, size=3)
library(psych)
pairs.panels(iris_data)

#Modelling and Prediction
library(caret)
inTrain<-createDataPartition(y=iris_data$Species,p=.7,list=FALSE)
training<-iris_data[inTrain,]
testing<-iris_data[,-inTrain]
dim(training);dim(testing)
table(training$Species);table(training$Species)

library(C50)
install.packages("C50")
model<-C5.0(training[-5],training$Species)
summary(model)

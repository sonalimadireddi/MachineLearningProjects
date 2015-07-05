#Random Forests Implementation in R
#Techniques 
#1. Random Forests 
#2. Traning and Testing 
#3. Confusion Matrix 
#4. Indicator Variables
#5. Binning 
#6. Variable Reduction 

setwd("~/Desktop/DataAnalyticsWork")
bank_data <- read.table("bank.csv",header=TRUE,sep=";")
str(bank_data)
#Notes - Total 17 variable with 16 consisting of predictor variables and 1 consisting of target variable
summary(bank_data)

#Performing a correlation analysis to see if any of the large predictor variables can be dropped
library(psych)
#To load the psych library use the installation command of install.packages("psych")
pairs.panels(bank_data[,c(1:8,17)])#Checking the first 8 predictors 
pairs.panels(bank_data[,c(9:17)])

new_data<-bank_data[,c(1:4,7:9,12,14,15,17)]
str(new_data)
pairs.panels(new_data)

#Data Transformation
#Performing binning of age and creating indicator variables for marital status
new_data$age<-cut(new_data$age,c(1,20,40,60,100))

new_data$is_divorced<-ifelse(new_data$marital=="divorced",1,0)
new_data$is_married<-ifelse(new_data$marital=="married",1,0)
new_data$is_single<-ifelse(new_data$marital=="single",1,0)
new_data$marital<-NULL
str(new_data)

#Data Exploration using Exploratory Data Analysis 
par(mfrow=c(2,2),las=2)
plot(new_data$housing, new_data$y,xlab="Housing",ylab="Become customer?",col =c("darkgreen","red"))
plot(new_data$contact, new_data$y,xlab="contact",ylab="Become customer?",col =c("darkgreen","red"))

boxplot(duration ~ y,data=new_data,col="red")
boxplot(pdays ~ y,data=new_data,col="blue")

library(caret)
inTrain<-createDataPartition(y=new_data$y,p=0.7,list=FALSE)
#creating 70% of traning data
training<-new_data[inTrain,]
testing<-new_data[-inTrain,]
dim(training);dim(testing)

library(randomForest)
model<-randomForest(y~.,data=training)
model
importance(model)

#Testing 
library(caret)
predicted<-predict(model,testing)
table(predicted)

confusionMatrix(predicted, testing$y)


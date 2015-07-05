#Kaggle Titanic Data Set Project Implementation in R

setwd("~/Desktop/DataAnalyticsWork")
trainData<-read.csv("train.csv",header=TRUE)
testData<-read.csv("test.csv",header=TRUE)

#Data Exploration 
str(trainData)
str(testData)
head(trainData)

#Exploratory Data Analysis by plotting
plot(density(trainData$PassengerId,na.rm=TRUE))
plot(density(trainData$Survived,na.rm=TRUE))
plot(density(trainData$Pclass,na.rm=TRUE))
plot(density(trainData$Age,na.rm=TRUE))

#Survival rate by sex barplot
counts<-table(trainData$Survived, trainData$Sex)
barplot(counts,xlab='Gender',ylab='Number of people survived',main='survived and deceased between male and female')
counts
#K-means clustering implementation in R
#1)K-means clustering
#2)Centering and Scaling

#DATA ENGINEERING AND ANALYSIS
setwd("~/Desktop/DataAnalyticsWork")
auto_data <- read.csv("auto-data.csv")
str(auto_data)
summary(auto_data)

#DATA CLEANING 
scaled_num<-scale(auto_data[8:12])
auto_data[,8:12]<-scaled_num
summary(auto_data)

#EXPLORATORY DATA ANALYSIS 
par(mfrow=c(1,5))
boxplot(auto_data$HP,col="red")
title("HP")

boxplot(auto_data$RPM,col="green")
title("RPM")

boxplot(auto_data$MPG.CITY,col="cyan")
title("MPG.CITY")

boxplot(auto_data$MPG.HWY,col="blue")
title("MPG.HWY")

boxplot(auto_data$PRICE,col="gray")
title("PRICE")

#MODELLING AND PREDICTION 
library(class)
#Choose the seed for the random number generator to pick the centroid randomly from the data
set.seed(1111)
auto_subset<-auto_data[1:100,c(8,12)]
clusters<-kmeans(auto_subset,4)
clusters

#Visualling the cluster plot
par(mfrow=c(1,1))
plot(auto_subset$HP,auto_subset$PRICE,col=clusters$cluster,pch=20,cex=2)
points(clusters$centers,col="purple",pch=17,cex=2)

#converting the values of the variables into numeric data
for(i in 1:8){
  auto_data[,i]=as.numeric(auto_data[,i])
}
summary(auto_data)

set.seed(1111)
clusters<-kmeans(auto_data[,7:12],4)
clusters



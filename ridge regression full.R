# ETH Zürich
# Chair of Management Information Systems
# Department of Management, Technology and Economics
# Business Analytics Project in Machine Learning
# Applying analytic methods to the predictive modelling of housing prices in
# Melbourne
# Nebojša Duboèanin*, Jan Eric Bühlmann**, Pauline Offeringa***
# *Dept. of Civil, Environmental and Geomatic Engineering, ETH Zürich, dunebojs@student.ethz.ch
# **Dept. of Business, Economics and Informatics, UZH Zürich, janeric.buehlmann@uzh.ch
# ***Dept. of Materials Sciences, ETH Zürich, paulineo@student.ethz.ch
# June 18, 2019

library(caret)
library(tidyr)
library(dplyr)
library(sandwich)
library(ggplot2)
library(glmnet)
library(bigmemory)

## Defining the working directory
setwd("C:/Users/nebapy/Desktop/ETHZ/2_Semester/06_Business_Analytics/PROJECT/Data")

## Loading "Melbourne Housing Dataset"
df <- read.csv("clean_dataframe.csv")

## Loaing only data from Melbourne Housing Dataset that has assigned values
df_cleaned <- df[complete.cases(df),]

#contruct a new data fram consisting solely of important variables
df_sub<-subset(df_cleaned, select=c(Rooms,CouncilArea,Price,Type,Distance,Bathroom,Car,Landsize,Lattitude,Longtitude,BuildingArea,Regionname,Propertycount))

#devide the data in training and testing part
intrain<-createDataPartition(df_sub$Price, p=0.75, list=FALSE)
training<-df_sub[intrain,]
testing<-df_sub[-intrain,]

# ridge regression from lecture
x.train <- model.matrix(Price~.,training)
y.train <- training$Price
#dim(x.train)
#length(y.train)
#head(x.train)
#head(y.train)

# parameter tuning, find optimal lambda via cross validation
cv.ridge<-cv.glmnet(x.train,y.train,alpha=0,lower=0)
#optimal lambda and corresponding coef
cv.ridge$lambda.min
coef(cv.ridge, s="lambda.min")

#compare the means squared error across lambda
plot(cv.ridge, cex.axis = 1.25 , cex = 1.25, family = c("serif")
     ,xlab="", ylab="") ## cex is scaling (up/down), familiy is teh font and the rest to remove axis names 
mtext(side=1, line = 2, "log(Lambda)", cex = 1.25, family = c("serif")) ## editing x-axis name size etc
mtext(side=2, line = 3, "Mean-Squared Error", cex = 1.25, family = c("serif")) ## editing y-axis name size etc

#predictions
x.test <- model.matrix(Price~.,testing)
y.test <- testing$Price
pred.ridge<-predict(cv.ridge,newx=x.test,s="lambda.min")
# Mean absolute percentage error (MAPE)
mean(abs((y.test - pred.ridge)/y.test))


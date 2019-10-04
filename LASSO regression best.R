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

# LASSO regression from lecture
x.train <- model.matrix(Price~.,training)
y.train <- training$Price
#dim(x.train)
#length(y.train)
#head(x.train)
#head(y.train)

# parameter tuning, find optimal lambda via cross validation by minimizing the mean squared error
cv.lasso<-cv.glmnet(x.train,y.train,alpha=1,lower=0)
#optimal lambda and corresponding coeff
cv.lasso$lambda.min
coef(cv.lasso, s="lambda.min")
# total variables
nrow(coef(cv.lasso))
#omitted variables
dimnames(coef(cv.lasso,s="lambda.min"))[[1]][which(coef(cv.lasso,s="lambda.min")==0)]
#included variables
dimnames(coef(cv.lasso, s="lambda.min"))[[1]][which(coef(cv.lasso, s="lambda.min") != 0)]
#compare means squared error across lambda
plot(cv.lasso, cex.axis = 1.25 , cex = 1.25, family = c("serif")
     , xlab="",ylab="")
mtext(side=1, line = 2, "log(Lambda)", cex = 1.25, family = c("serif"))
mtext(side=2, line = 3, "Mean-Squared Error", cex = 1.25, family = c("serif"))
#predictions
x.test <- model.matrix(Price~.,testing)
y.test <- testing$Price
pred.lasso<-predict(cv.lasso,newx=x.test,s="lambda.min")
# Mean absolute percentage error (MAPE)
mean(abs((y.test - pred.lasso)/y.test))
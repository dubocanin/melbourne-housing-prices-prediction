## Defining the working directory
setwd("//data.bauwelt.ethz.ch/dunebojs/.Windows/Desktop/06_Business Analytics/PROJECT")

## Loading the necessary libraries
library(tidyverse)
library(rpart)
library(randomForest)
library(lattice)
library(ggplot2)
library(caret)
library(Metrics)

## Loading "Melbourne Housing Dataset"
df <- read.csv("Data/Melbourne_housing_FULL.csv")

## Loaing only data from Melbourne Housing Dataset that has assigned values
df_cleaned <- df[complete.cases(df),]

str(df_cleaned)
head(df_cleaned)

## Checking if there are some N.A. values left
sum(is.na(df_cleaned))

## Names of the columns we are working with [21 column in total]
colnames(df_cleaned)

## Distribution of df_cleaned dataset into training and testing datasets
index <- createDataPartition(df_cleaned$Price, p = 0.75, list=FALSE)

df_cleaned_train <- df_cleaned[index,]
df_cleaned_test <- df_cleaned[-index,]

## Look on a header of the training dataset "df_cleaned_training"
head(df_cleaned_training)
## Look on a header of the training dataset "df_cleaned_training"
head(df_cleaned_test)

prediction_model_RF <- randomForest(Price ~ Rooms 
                                    + Type 
                                    + CouncilArea 
                                    + Method,  data = df_cleaned_train)

## Plot Variable Importance
varImpPlot(prediction_model_RF)

## Price prediction
point = c(1:2220)
predict_prices <- predict(prediction_model_RF, df_cleaned_test)
error = predict_prices - df_cleaned_test$Price
pred_vs_test <- data.frame(predict_prices, df_cleaned_test$Price, point, error)

## Plot predicted prices and prices from the "df_cleaned_test"

p <- ggplot(data = pred_vs_test, aes(x = point))
p +  geom_point(aes(y = predict_prices, colour = "Predicted Prices"), fill = "pink") +
     geom_POINTsmooth(aes(y = df_cleaned_test$Price, colour = "Test set Prices"), fill = "lightblue") +
     scale_colour_manual("Legend",
                       breaks = c("Predicted Prices", "Test set Prices"),
                       values = c("red", "blue")) +
     coord_cartesian(xlim=c(0, 2250), ylim=c(8.5*10^5, 1.6*10^6)) +
     labs(y="Test set Prices vs Predicted Prices [-]", x="Year [will try to extract years]") +
     scale_x_continuous(breaks = seq(from = 0, to = 2250, by = 250)) +
     theme_light(base_size = 12, base_family = "serif")


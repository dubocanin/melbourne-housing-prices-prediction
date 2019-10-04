list_of_packages <- c("knitr",
                      "tidyverse",
                      "caret",
                      "e1071",
                      "ggplot2",
                      "forcats",
                      "dplyr",
                      "lmtest",
                      "car",
                      "pROC",
                      "arules",
                      "arulesViz",
                      "ElemStatLearn",
                      "ISLR",
                      "glmnet",
                      "gam",
                      "class",
                      "nnet",
                      "rpart",
                      "party",
                      "partykit",
                      "randomForest",
                      "ROCR",
                      "boot",
                      "bayesboot",
                      "mboost",
                      "ada",
                      "MDPtoolbox",
                      "tm",
                      "SnowballC",
                      "ggmap",
                      "plyr",
                      "ggthemes",
                      "reshape",
                      "nycflights13",
                      "kohonen"
)

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

## Defining the working directory
setwd("//data.bauwelt.ethz.ch/dunebojs/.Windows/Desktop/06_Business Analytics/PROJECT")

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

stat = summary(df)

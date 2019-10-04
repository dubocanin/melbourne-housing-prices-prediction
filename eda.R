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

## Loading necessary libraries (libraries suggested by the professor Stefan Feuerriegel)

for(i in c(1:1)){
  library(knitr)
  library(tidyverse)
  library(caret)
  library(e1071)
  library(ggplot2)
  library(forcats)
  library(dplyr)
  library(lmtest)
  library(car)
  library(pROC)
  library(arules)
  library(arulesViz)
  library(ElemStatLearn)
  library(ISLR)
  library(glmnet)
  library(gam)
  library(class)
  library(nnet)
  library(rpart)
  library(party)
  library(partykit)
  library(randomForest)
  library(ROCR)
  library(boot)
  library(bayesboot)
  library(mboost)
  library(ada)
  library(MDPtoolbox)
  library(tm)
  library(SnowballC)
  library(ggmap)
  library(plyr)
  library(ggthemes)
  library(reshape)
  library(nycflights13)
  library(kohonen)
  library(corrplot)
  library(tidyr)
}

## Defining the working directory
setwd("C:/Users/nebapy/Desktop/ETHZ/2_Semester/06_Business_Analytics/PROJECT/Data")

## Loading "Melbourne Housing Dataset"
df <- read.csv("melbourne_housing_full.csv")

## Converting all "#N/A" values within df dataframe into NA values
## It is done column by column since if function bellow is applied to the whole data frame
## integers would be converted into other variable type 
## making it unusefull in the later analysis
Regionname = c(sapply(df$Regionname, function(x) { gsub("#N/A", NA, x) } ) )
Propertycount = c(sapply(df$Propertycount, function(x) { gsub("#N/A", NA, x) } ) )
Postcode = c(sapply(df$Postcode, function(x) { gsub("#N/A", NA, x) } ) )
CouncilArea = c(sapply(df$CouncilArea, function(x) { gsub("#N/A", NA, x) } ) )
Distance = c(sapply(df$Distance, function(x) { gsub("#N/A", NA, x) } ) )

## Now we will overwrite df dataset with "#N/A" changed to N.A.
df$Regionname <- Regionname
df$Propertycount <- Propertycount
df$Postcode <- Postcode
df$CouncilArea <- CouncilArea
df$Distance <- Distance

## Names of the columns we are working with [21 column in total]
colnames(df)

## Short Data Summary
str(df)
head(df)

## Statistical Summary
stat = summary(df)
print(stat)

## Variable of interest, variable to predict "PRICE"
sum(is.na(df$Price)) # tell us how many N.A. values are inside the column "Price"

## Removing rows with N.A. values from column "Price"
## By examining the dataset, one price is very high 11'200'000.00
## This one will be removed and also the house that costs 9'000'000.00
## Since it costs too much and has way smaller building area than houses of similar price
df1 <- df[-which(is.na(df$Price)), ]
price = df1$Price
a = c(1:2)
for (i in a) {
  df1 <- df1[-c(which.max(df1[,"Price"])),]
}
## Printing number of missing values for each variable 
## this part is done using the code from the following webpage
## (https://www.kaggle.com/sugh93/analysis-of-melbourne-housing-data)
print("Number of missing values in each feature")
knitr::kable(sapply(df1, function(x) sum(is.na(x))))

## After the summary it can be observed that variables:
## "Distance", "Postcode", "CouncilArea", "Regionname" and "Propertycount"
## have least number of NA values, therefore those will be removed

df1 <- subset(df1, !is.na(df1$Regionname))
df1 <- subset(df1, !is.na(df1$Propertycount))
df1 <- subset(df1, !is.na(df1$Postcode))
df1 <- subset(df1, !is.na(df1$Distance))
df1 <- subset(df1, !is.na(df1$CouncilArea))

## Plot for the frequency distribution for the variable "Price"
p1 <- ggplot(df1, aes(x = df1$Price))      # Here dataframe df_f is used since "Price" variable
p1 +  geom_histogram(binwidth = 100000, # do not have any N.A. values
                     col="black",
                     size=0.1,
                     fill = "lightgrey") +
  scale_x_continuous(breaks = c(1000000, 2000000, 3000000, 4000000, 5000000),
                     labels = c("1", "2", "3", "4", "5")) +
  labs(y="Count", x="Price [Millions of $]") +
  theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(),
        # Remove panel background
        panel.background = element_blank(),
        legend.spacing.x = unit(5, "mm"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14)) +
        guides(colour = guide_legend(override.aes = list(size=4)))

skewness(price)
kurtosis(price)

## Plot the "Regionname" vs "Price" to get an insight into the price distributon for specific region

p2 <- ggplot(df1, aes(x = df1$Regionname)) 
p2 +  geom_violin(aes(y = df1$Price), fill="lightblue", draw_quantiles = TRUE, scale = "width") +
      labs(y="Price", x="Region") +
      theme_bw(base_size = 14, base_family = "serif") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(),
            # Remove panel background
            panel.background = element_blank(),
            legend.spacing.x = unit(5, "mm"),
            panel.border = element_rect(colour = "black", fill=NA),
            legend.background = element_blank(),
            axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      guides(colour = guide_legend(override.aes = list(size=4)))

## Property Count by "Type" and "Price" variables
p3 <- ggplot(df1, aes(x=as.numeric(as.character(Propertycount)), y=Price))
p3 +  geom_point(aes(colour=Type)) +
      labs(y="Price", x="Property Count") +
      theme_bw(base_size = 14, base_family = "serif") +
      theme(legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(),
            # Remove panel background
            panel.background = element_blank(),
            legend.spacing.x = unit(5, "mm"),
            panel.border = element_rect(colour = "black", fill=NA),
            legend.background = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      guides(colour = guide_legend(override.aes = list(size=4)))

# Loop for removing 50 highest "Landsize" values (outliers)
a = c(1:25)
for (i in a) {
  df1 <- df1[-c(which.max(df1[,"Landsize"])),]
}

## Interpolation of NA values for all relevant variables
## Spline Interpolation of "Landsize" NA values

int.1 <- with(df1[!is.na(df1$Landsize),], smooth.spline(Price, Landsize))
Landsize <- with(df1,predict(int.1,Price[is.na(Landsize)]))
filler = Landsize$y
df1$Landsize[is.na(df1$Landsize)] <- filler

## Spline Interpolation of "Building Area" NA values

int.2 <- with(df1[!is.na(df1$BuildingArea),], smooth.spline(Price, BuildingArea))
BuildingArea <- with(df1,predict(int.2,Price[is.na(BuildingArea)]))
filler1 = BuildingArea$y
df1$BuildingArea[is.na(df1$BuildingArea)] <- filler1

## Spline Interpolation of "Bathroom" NA values

int.3 <- with(df1[!is.na(df1$Bathroom),], smooth.spline(Price, Bathroom))
Bathroom <- with(df1,predict(int.3,Price[is.na(Bathroom)]))
filler2 = Bathroom$y
df1$Bathroom[is.na(df1$Bathroom)] <- filler2

## Spline Interpolation of "Car" NA values

int.4 <- with(df1[!is.na(df1$Car),], smooth.spline(Price, Car))
Car <- with(df1,predict(int.4,Price[is.na(Car)]))
filler3 = Car$y
df1$Car[is.na(df1$Car)] <- filler3

## Removing the max value of YearBuilt (there is an outlier, year built 2144, which is not possible)
## There is alos min value 1196, this is also removed since it could be a wrong entry
df1 <- df1[-c(which.max(df1[,"YearBuilt"])),]
df1 <- df1[-c(which.min(df1[,"YearBuilt"])),]

## Spline Interpolation of "YearBuilt" NA values

int.5 <- with(df1[!is.na(df1$YearBuilt),], smooth.spline(Price, YearBuilt ))
YearBuilt  <- with(df1,predict(int.5,Price[is.na(YearBuilt )]))
filler4 = YearBuilt $y
df1$YearBuilt[is.na(df1$YearBuilt )] <- filler4

## Spline Interpolation of "Longtitude" NA values
## "spelling of Longtitude is obviously wrong"!, but it doesn't matter

## Checking for the NA values after interpolation of variables

print("Number of missing values in each feature")
knitr::kable(sapply(df1, function(x) sum(is.na(x))))

# Plot of distance with Landsize
df1$Distance = as.numeric(as.character(df1$Distance))

p4 <- ggplot(df1, aes(x=df1$Distance))
p4 +  geom_point(aes(y = df1$Price, colour=as.numeric(as.character(df1$Price))), alpha=0.8) +
      scale_colour_gradient(low = "lightblue", high = "red") +
      labs(y="Price", x="Distance", color = "Price") +
      theme_bw(base_size = 14, base_family = "serif") +
      theme(legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(),
            # Remove panel background
            panel.background = element_blank(),
            legend.spacing.x = unit(5, "mm"),
            panel.border = element_rect(colour = "black", fill=NA),
            legend.background = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
## Plotting the relation between "Landsize" and "Price"   
p5 <- ggplot(df1, aes(x=log(df1$Landsize)))
p5 +  geom_point(aes(y = df1$Price, colour=as.numeric(as.character(df1$Price))), alpha=0.8) +
      scale_colour_gradient(low = "lightblue", high = "red") +
      theme_bw(base_size = 14, base_family = "serif") + 
      labs(y="Price", x="Landsize", color = "Price") +
      theme(legend.position = "right",
            legend.title = element_text(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(),
            # Remove panel background
            panel.background = element_blank(),
            legend.spacing.x = unit(5, "mm"),
            panel.border = element_rect(colour = "black", fill=NA),
            legend.background = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      guides(colour = guide_legend(override.aes = list(size=4)))

## Ploting the relationship between "Property Location" and "Price"
df1 <- subset(df1, !is.na(df1$Lattitude)) # Since the variables are connected, 
                                          # when, N.As are removed from Latitude
                                          # they are also removed from Longitude

p6 <- ggplot(df1, aes(y = as.numeric(as.character(df1$Lattitude)),
                      x = as.numeric(as.character(df1$Longtitude))))
p6 +  geom_point(aes(colour = as.numeric(as.character(df1$Price)))) +
      scale_colour_gradient(low = "lightblue", high = "red") +
      theme_bw(base_size = 14, base_family = "serif") + 
      labs(y="Latitude", x="Longitude", color = "Price")

## Plotting "Property Count"  vs "Region"

p7 <- ggplot(df1, y = df1$Propertycount)
p7 +  geom_bar(aes(x = df1$Regionname, fill = df1$Type)) +
      theme_bw(base_size = 14, base_family = "serif") + 
      labs(y="Count", x="Region Name", fill = "Type") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(),
            # Remove panel background
            panel.background = element_blank(),
            axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 14))
      
## Ploting the Correlation among the variables
## First we need to clean df_f from all N.A. values othervise we can't do a correlogram
## Removing all incomplete cases

dfc <- df[complete.cases(df),]
df_cor = dplyr::select_if(dfc, is.numeric)
CORR <- cor(df_cor)
corrplot(CORR, method="number")

## Ploting Price Distribution against Year Built

p8 <- ggplot(df1, aes(x=df1$YearBuilt))
p8 +  geom_point(aes(y = df1$Price, colour=as.numeric(as.character(df1$Price))), alpha=0.8) +
  scale_colour_gradient(low = "lightblue", high = "red") +
  labs(y="Price", x="Year Built", color = "Price") +
  theme_bw(base_size = 14, base_family = "serif") +
  scale_x_continuous(breaks = seq(from = 1800, to = 2020, by = 50)) +
  theme(legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(),
        # Remove panel background
        panel.background = element_blank(),
        legend.spacing.x = unit(5, "mm"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

## DATE vs Price variables relationship
## Sorting Date variable from oldest to newest
df1 <- df1[order(as.Date(df1$Date, format="%d/%m/%Y")),]
Date11 = df1$Date
Price = df1$Price
PLOT111  <- data.frame(Date11, Price)

p9 <- ggplot(PLOT111, aes(x=order(as.Date(Date11, format="%d/%m/%Y"))))
p9 +  geom_point(aes(y = Price, colour=as.numeric(as.character(Price))), alpha=0.8) +
      scale_colour_gradient(low = "lightblue", high = "red") +
      labs(y="Price", x="Date", color = "Price") +
      theme_bw(base_size = 14, base_family = "serif") +
      theme(legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(),
            # Remove panel background
            panel.background = element_blank(),
            legend.spacing.x = unit(5, "mm"),
            panel.border = element_rect(colour = "black", fill=NA),
            legend.background = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      guides(colour = guide_legend(override.aes = list(size=4)))



## Removing unnecessary columns(variables) from dataset df1
## YearBuilt is not removed from the dataset since it was necessary for a plot 8
df1 <- subset( df1, select = -c( Bedroom2, Suburb, Address, Postcode, Method, Date, SellerG) )

## Export csv file. Set filename & location (in the current working directory) with input file="...".
write.table(df1, file="clean_dataframe.csv", sep = ",", row.names = F)
write.table(stat, file="stat_summary.csv", sep = ",", row.names = F)
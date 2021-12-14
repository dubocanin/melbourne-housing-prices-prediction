# ETH Zürich
# Chair of Management Information Systems
# Department of Management, Technology and Economics
# Business Analytics Project in Machine Learning
# Applying analytic methods to the predictive modelling of housing prices in
# Melbourne
# Nebojsa Dubocanin*, Jan Eric Bühlmann**, Pauline Offeringa***
# *Dept. of Civil, Environmental and Geomatic Engineering, ETH Zürich, dunebojs@student.ethz.ch
# **Dept. of Business, Economics and Informatics, UZH Zürich, janeric.buehlmann@uzh.ch
# ***Dept. of Materials Sciences, ETH Zürich, paulineo@student.ethz.ch
# June 18, 2019

## Loading necessary libraries (libraries suggested by the professor Stefan Feuerriegel)
library(rpart)

## Defining the working directory
setwd("C:/melbourne/Data")

## Loading "Melbourne Housing Dataset"
df <- read.csv("clean_dataframe.csv")

str(df)
head(df)

## Checking if there are some N.A. values left
sum(is.na(df))

## Names of the columns we are working with [13 column in total]
colnames(df)

## Distribution of df_cleaned dataset into training and testing datasets
index <- createDataPartition(df$Price, p = 0.75, list=FALSE)
df_training <- df[index,]
df_test <- df[-index,]

## Decission Tree Model
## Loading all variables to the model
DT <- rpart(Price ~ Rooms + Distance + YearBuilt + BuildingArea + Bathroom + Landsize + Car, 
            data = df_training,
            method = "anova")
## Checking variable importance
plotcp(DT)
rsq.rpart(DT)
varImp(DT)
summary(DT)
printcp(DT)
## Price prediction
point = c(1:5239)
predict_prices <- predict(DT, df_test)
error = predict_prices - df_test$Price
MAE = sum(abs(error))/length(error)
mean_price = mean(df_test$Price)
pred_vs_test <- data.frame(predict_prices, df_test$Price, point, error)

## Pruning the Decision Tree Model
printcp(DT) 
plotcp(DT)
DT_pruned <- prune(DT, cp = 0.0105)
plot(DT_pruned, uniform = TRUE)
text(DT_pruned, cex = .8)
summary(DT_pruned)

predict_prices_pruned <- predict(DT_pruned, df_test)
error_pruned = predict_prices_pruned - df_test$Price
MAE_pruned = sum(abs(error_pruned))/length(error_pruned)
pred_vs_test_pruned <- data.frame(predict_prices_pruned, df_test$Price, point, error_pruned)
MAPE = mean(abs((df_test$Price - predict_prices_pruned)/df_test$Price))
printcp(DT_pruned)
plotcp(DT_pruned)
## Plot predicted prices and prices from the "df_cleaned_test"

p <- ggplot(data = pred_vs_test, aes(x = point))
p +  geom_point(aes(y = predict_prices_pruned, colour = "Predicted Prices"), size = 1) +
     geom_point(aes(y = df_test$Price, colour = "Test Prices"), size = 1) +
     scale_colour_manual("Legend",
                      breaks = c("Predicted Prices", "Test Prices"),
                      values = c("#f937fc", "#378cfc")) +
    coord_cartesian(xlim=c(0, 5240), ylim=c(2*10^5, 10*10^6)) +
    labs(y="Test vs Predicted Prices (Mil. $)", x="Point (-)") +
    scale_x_continuous(breaks = seq(from = 0, to = 5240, by = 1000)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          legend.spacing.x = unit(5, "mm"),
          panel.border = element_rect(colour = "black", fill=NA),
          legend.background = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.text = element_text(size = 14))+
    guides(colour = guide_legend(override.aes = list(size=4)))
ggsave("C:/melbourne/Plots/dt_&_rf/decision_tree_plot.png")
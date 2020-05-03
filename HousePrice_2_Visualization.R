# MMA 867 Individual Assignment 1 - Kaggle Competition
# House Price - Visualization
# Date: Apr 30, 2020
# Author: Zita Lo

#install.packages("fastDummies") # Creating dummies
#install.packages("corrplot") # Correlations plot

# Load libraries
library(corrplot)
library(dplyr)
library(sqldf)
library(ggplot2)


# Read csv
data.train <- read.csv(file.choose()) # train.csv
data.test <- read.csv(file.choose()) # test.csv
datatrain <- read.csv(file.choose()) # train.csv

# Remove SalePrice variable from train
data.test$SalePrice <- 0

# Create a full set
data.full <- rbind(data.train,data.test)

# Summarize and describe data
str(data.full) #show the structure of data types in the data.full dataframe
head(data.full, 4) #show the first 4 rows in the data.full dataframe
tail(data.full,4) #show the last 4 rows in the data.full dataframe
summary(data.full) #show summary statistics of the data.full dataframe

#
# Plot - data exploration
# --------------------------------------------

# Plot Full Set: Train + Test data
plot(SalePrice ~ GrLivArea, data=data.full) #plot of price vs GrLivArea for full set
hist(data.full$OverallQual) #histogram 
hist(data.full$GarageCars) #histogram 
hist(data.full$YrSold) #histogram 
hist(data.full$MoSold) #histogram 


# Plot Train Data
#plot(SalePrice ~ GrLivArea, data=data.train) #plot of price vs GrLivArea for train set
plot <- ggplot(data=data.train, aes(y=SalePrice, x=GrLivArea, col=as.factor(YrSold)))
plot <- plot + geom_point(aes(size = 0.1))
plot <- plot + ylab("Sale Price") + xlab("GrLivArea") +
  scale_color_discrete(name = "Year Sold")
plot


# Correlations plots on selected numeric variables from datatrain
str(data.train)
datatrain.selected <- sqldf('SELECT SalePrice,MSSubClass,LotFrontage,GrLivArea,OverallQual,LotArea,OverallCond,YearBuilt,YearRemodAdd,MasVnrArea,BsmtFinSF1,BsmtFinSF2,FullBath,X1stFlrSF,Fireplaces,GarageArea,MoSold, YrSold,GarageCars FROM datatrain;')
correlations <- cor(datatrain.selected[,], use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

cor(datatrain.selected) # correlation stats

# plot SalePrice against a few variables that are highly correlated with SalePrice
plot(data.train$SalePrice ~ data.train$GrLivArea)
plot(data.train$SalePrice ~ data.train$OverallQual) 
plot(data.train$SalePrice ~ data.train$GarageCars) 

#
#
# Plot simple predicted SalePrice with cleaned file
# --------------------------------------------------

data.fullClean <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE) # Cleaned_Data_Full_dummies3.csv

data.predicting<-subset(data.fullClean, Id > 1460) # withold 1000 datapoints into a "testing" data
data.training.last <-subset(data.fullClean,  Id <=1460) # redefine the training + testing data for final prediction

#data.test <- read.csv(filepath.test)
fit<-lm(SalePrice ~ ., data=data.training.last) # run a multiple linear regression model (lm) on the training data, call it "fit" 
predicted.SalePrice<-predict(fit, data.predicting) # use the "fit" model to predict sale prices for the prediction data


plot(predicted.SalePrice ~ data.test$GrLivArea) # plot predicted sale price vs GrLicArea for visual inspection
abline(0,0) # plot the horisontal line

summary(fit) # summary of the "fit" regression model

par(mfrow=c(1,4)) # this command sets the plot window to show 1 row of 4 plots
plot(fit) # diagnostic plots for the "fit" model 



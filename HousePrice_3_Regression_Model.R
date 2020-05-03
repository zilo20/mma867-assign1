# MMA 867 Individual Assignment 1 - Kaggle Competition
# House Price - Regression Model
# Date: May 1, 2020
# Author: Zita Lo

#install.packages("Boruta")

# Load libraries

library(tidyverse)
library(caret) # Predict
library (Boruta) # Selecting Best Features


# Read csv "Cleaned_Data_Full_dummies3.csv"
data.fullClean <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE)


data.predicting<-subset(data.fullClean, Id > 1460) #withold 1000 datapoints into a "testing" data
data.training<-subset(data.fullClean, Id <= 1000) #redefine the training data (around 70% vs 30%) 
data.testing<-subset(data.fullClean, (Id> 1000 & Id <=1460)) #redefine the training data
data.training.last <-subset(data.fullClean,  Id <=1460) #redefine the training + testing data for final prediction

data.fullClean.y <- data.fullClean$SalePrice
data.fullClean$SalePrice <- NULL

#---------------------
###
### Assessing the quality of the model -- cross-fold validation
###
#---------------------
fit<-lm(SalePrice ~ ., data=data.training) #build a model on training data
summary(fit)
predicted.prices.testing<-predict(fit, data.testing) #predict the sale prices for testing the model

percent.errors <- abs((data.testing$SalePrice-predicted.prices.testing)/data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE) [1] 11.6151




#---------------------
###
### Variable Selection (Forward/Backward/Stepwise regression)
###
#---------------------

## log attempts: result: [1] 13.42958
fit.log.ii<-lm(log(SalePrice)~ log(GrLivArea)+ GarageCars + log(LotFrontage) + log(LotArea) +  TotalBsmtSF + OverallQual + X1stFlrSF+ ExterQual_TA + FullBath,  data=data.training)
summary(fit.log.ii)
predicted.prices.testing<-exp(predict(fit.log.ii, data.testing)) #predict the prices for testing the model

percent.errors <- abs((data.testing$SalePrice-predicted.prices.testing)/data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE)

## log attempts: result: [1] 10.01783
fit.log.ii<-lm(log(SalePrice)~ log(GrLivArea)+ log(OverallQual) +.,  data=data.training)
summary(fit.log.ii)
predicted.prices.testing<-exp(predict(fit.log.ii, data.testing)) #predict the prices for testing the model

percent.errors <- abs((data.testing$SalePrice-predicted.prices.testing)/data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE)

## Step Backward
fit.log.step<-step(lm(log(SalePrice)~ log(GrLivArea)+ log(GarageArea) + GarageCars + log(TotalBsmtSF) + OverallQual + 1stFlrSF + ExterQual_TA + FullBath,  data=data.training),direction="backward")
summary(fit.log.step)
predicted.prices.testing<-exp(predict(fit.log.step, data.testing)) #predict the prices for testing the model

percent.errors <- abs((data.testing$SalePrice-predicted.prices.testing)/data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE)

## Step Both
fit.log.step<-step(lm(log(SalePrice)~ log(GrLivArea)+ log(GarageArea) + GarageCars + log(TotalBsmtSF) + OverallQual + 1stFlrSF + ExterQual_TA + FullBath,  data=data.training),direction="both")
summary(fit.log.step)

predicted.prices.testing<-exp(predict(fit.log.step, data.testing)) #predict the prices for testing the model

percent.errors <- abs((data.testing$SalePrice-predicted.prices.testing)/data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE)

#---------------------
###
### Variable Selection (Boruta)
###
#---------------------
# Run the model 
train <- Boruta(SalePrice~ ., data = data.training)

# Show the results
print(train)
head(train,1)
# Default Plot
plot(train,cex.axis=.7, las=2, xlab="", main="Variable Importance")



#-----------------------
###
### Regularizations (LASSO and ridge)
###
#-----------------------
#install.packages("glmnet")
library(glmnet)

#create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)
y<-log(data.training$SalePrice)
X<-model.matrix(Id~ .+log(GrLivArea)+ log(OverallQual) + GarageArea * KitchenQual_TA * GarageCars * FullBath , data.fullClean)[,-1]
X<-cbind(data.fullClean$Id,X)

# split X into testing, trainig/holdout and prediction as before
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.01,0.05)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-data.testing$SalePrice)/data.testing$SalePrice*100) #calculate and display MAPE is 9.508746

#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

# predicting the performance on the testing set
ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-data.testing$SalePrice)/data.testing$SalePrice*100)  #calculate and display MAPE is 9.452251

# comparing the performance on the testing set. LASSO is used for the final model
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
#predicted.prices.log.i.ridge <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.prediction))
write.csv(predicted.prices.log.i.lasso, file = "Predicted_Lasso.csv") # export the predicted prices into a CSV file


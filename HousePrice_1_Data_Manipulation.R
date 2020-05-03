# MMA 867 Individual Assignment 1 - Kaggle Competition
# House Price - Data Manipulation
# Date: Apr 30, 2020
# Author: Zita Lo

#install.packages("fastDummies") # Creating dummies

# Load libraries
library(dplyr)
library(mice) # Imputation
library(fastDummies) # Create dummies

# Read csv
data.train <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE) #train.csv
data.test <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE) #test.csv

# Remove SalePrice variable from train
SalePrice <- data.train$SalePrice
data.train$SalePrice <- NULL

# Create a full set
data.full <- rbind(data.train,data.test)

# Summarize and describe data
str(data.full) #show the structure of data types in the data.full dataframe
head(data.full, 4) #show the first 4 rows in the data.full dataframe
tail(data.full,4) #show the last 4 rows in the data.full dataframe
summary(data.full) #show summary statistics of the data.full dataframe

# Handling missing values
#---------------------------
# Summarize # of NA in each column
data.full %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))

# Total count of NA in the data frame
sum(is.na(data.full))

# Remove features - including PoolQC, MiscFeature, Alley, Fence --> these four variables have more than 2000 NA out of 2919
data.full$PoolQC<- NULL
data.full$MiscFeature<- NULL
data.full$Alley<- NULL
data.full$Fence <- NULL
 

# Assumption if na could mean no basement, thus cannot imputate these values
data.full$MasVnrType <- ifelse(is.na(data.full$MasVnrType), "None", data.full$MasVnrType) 
data.full$BsmtQual <- ifelse(is.na(data.full$BsmtQual), "None", data.full$BsmtQual)   
data.full$BsmtCond <- ifelse(is.na(data.full$BsmtCond), "None", data.full$BsmtCond) 
data.full$BsmtExposure <- ifelse(is.na(data.full$BsmtExposure), "None", data.full$BsmtExposure) 
data.full$BsmtFinType1 <- ifelse(is.na(data.full$BsmtFinType1), "None", data.full$BsmtFinType1) 
data.full$BsmtFinType2 <- ifelse(is.na(data.full$BsmtFinType2), "None", data.full$BsmtFinType2) 
data.full$Utilities <- ifelse(is.na(data.full$Utilities), "None", data.full$Utilities) 
data.full$Functional <- ifelse(is.na(data.full$Functional), "None", data.full$Functional) 
data.full$Exterior1st <- ifelse(is.na(data.full$Exterior1st), "None", data.full$Exterior1st) 
data.full$Exterior2nd <- ifelse(is.na(data.full$Exterior2nd), "None", data.full$Exterior2nd) 
data.full$Electrical <- ifelse(is.na(data.full$Electrical), "None", data.full$Electrical) 
data.full$KitchenQual <- ifelse(is.na(data.full$KitchenQual), "TA", data.full$KitchenQual)
data.full$FireplaceQu <- ifelse(is.na(data.full$FireplaceQu), "None", data.full$FireplaceQu) 
data.full$GarageType <- ifelse(is.na(data.full$GarageType), "None", data.full$GarageType) 
data.full$GarageYrBlt <- ifelse(is.na(data.full$GarageYrBlt), data.full$YearBuilt, data.full$GarageYrBlt) 
data.full$GarageFinish <- ifelse(is.na(data.full$GarageFinish), "None", data.full$GarageFinish) 
data.full$GarageQual <- ifelse(is.na(data.full$GarageQual), "None", data.full$GarageQual) 
data.full$GarageCond <- ifelse(is.na(data.full$GarageCond), "None", data.full$GarageCond) 
data.full$MSZoning  <- ifelse(is.na(data.full$MSZoning), "None", data.full$MSZoning)
data.full$SaleType  <- ifelse(is.na(data.full$SaleType), "Oth", data.full$SaleType)
data.full$MasVnrArea <- ifelse(is.na(data.full$MasVnrArea), 0, data.full$MasVnrArea) 
data.full$BsmtFinSF1 <- ifelse(is.na(data.full$BsmtFinSF1), 0, data.full$BsmtFinSF1) 
data.full$BsmtFinSF2 <- ifelse(is.na(data.full$BsmtFinSF2), 0, data.full$BsmtFinSF2) 
data.full$BsmtUnfSF <- ifelse(is.na(data.full$BsmtUnfSF), 0, data.full$BsmtUnfSF) 
data.full$GarageCars <- ifelse(is.na(data.full$GarageCars), 0, data.full$GarageCars) 
data.full$GarageArea <- ifelse(is.na(data.full$GarageArea), 1, data.full$GarageArea) 
data.full$BsmtFullBath <- ifelse(is.na(data.full$BsmtFullBath), 0, data.full$BsmtFullBath) 
data.full$BsmtHalfBath <- ifelse(is.na(data.full$BsmtHalfBath), 0, data.full$BsmtHalfBath) 
data.full$TotalBsmtSF <- ifelse(is.na(data.full$TotalBsmtSF), 0, data.full$TotalBsmtSF) 
        


# Imputation 
md.pattern(data.full)
imputed.data.full <- mice(data.full, m=5, maxit=30, meth='cart', seed=1)
summary(imputed.data.full)
cleaned.data.full <- complete(imputed.data.full,2)
md.pattern(cleaned.data.full)


# Create dummies for entire data frame
cleaned.data.full.dummies <- dummy_cols(cleaned.data.full)

# Drop original categorical features after dummies are generated
cleaned.data.full.dummies$SaleCondition <- NULL
cleaned.data.full.dummies$SaleType <- NULL
cleaned.data.full.dummies$PavedDrive <- NULL
cleaned.data.full.dummies$GarageFinish <- NULL
cleaned.data.full.dummies$GarageType <- NULL
cleaned.data.full.dummies$Functional <- NULL
cleaned.data.full.dummies$Electrical <- NULL
cleaned.data.full.dummies$Heating <- NULL

cleaned.data.full.dummies$BsmtFinType2 <- NULL
cleaned.data.full.dummies$BsmtFinType1 <- NULL
cleaned.data.full.dummies$BsmtExposure <- NULL
cleaned.data.full.dummies$Foundation <- NULL
cleaned.data.full.dummies$MasVnrType <- NULL
cleaned.data.full.dummies$Exterior2nd <- NULL
cleaned.data.full.dummies$Exterior1st <- NULL
cleaned.data.full.dummies$RoofMatl <- NULL

cleaned.data.full.dummies$RoofStyle <- NULL
cleaned.data.full.dummies$HouseStyle <- NULL
cleaned.data.full.dummies$BldgType <- NULL
cleaned.data.full.dummies$Condition2 <- NULL
cleaned.data.full.dummies$Condition1 <- NULL
cleaned.data.full.dummies$Neighborhood <- NULL
cleaned.data.full.dummies$LandSlope <- NULL
cleaned.data.full.dummies$LotConfig <- NULL

cleaned.data.full.dummies$Utilities <- NULL
cleaned.data.full.dummies$LandContour <- NULL
cleaned.data.full.dummies$LotShape <- NULL
cleaned.data.full.dummies$Street <- NULL
cleaned.data.full.dummies$MSZoning <- NULL
cleaned.data.full.dummies$ExterQual <- NULL
cleaned.data.full.dummies$ExterCond <- NULL
cleaned.data.full.dummies$BsmtQual <- NULL

cleaned.data.full.dummies$BsmtCond <- NULL
cleaned.data.full.dummies$GarageCond <- NULL
cleaned.data.full.dummies$KitchenQual <- NULL
cleaned.data.full.dummies$HeatingQC <- NULL
cleaned.data.full.dummies$GarageQual <- NULL
cleaned.data.full.dummies$FireplaceQu   <- NULL
cleaned.data.full.dummies$CentralAir <- NULL

        
data1 <- subset(cleaned.data.full.dummies, Id<=1460) 
data2 <- subset(cleaned.data.full.dummies, Id> 1460) 

data1$SalePrice <- SalePrice
data2$SalePrice <- NA

cleaned.data.full.dummies <- rbind(data1,data2)

write.csv(cleaned.data.full.dummies, file = "Cleaned_Data_Full_dummies300.csv") # export the cleaned data into a CSV file

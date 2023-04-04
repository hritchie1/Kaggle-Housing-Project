library(MASS)
library(dplyr)
library(ggplot2)
library(car) 
library(broom) 
library(moments)
library(tidyverse)
library(leaps)
library(forcats)
library(ggcorrplot)
library(glmnet)
library(tree)
library(randomForest)
library(jtools)
library(sjPlot)
library(sjmisc)
library(gbm)

#################
## Milestone 1 ##
#################

train <- read.csv("train.csv")
colnames(train)
test <- read.csv("test.csv")
test$SalePrice <- 0
combined <- rbind(train, test)
combined$SalePrice <- as.numeric(combined$SalePrice)
## I will use the fact that sale price = 0 to re-separate later

# set seed to create replicatable results
set.seed(800)

# create clean kaggle train dataset
clean <- combined

## Finding Duplicates
sum(clean[anyDuplicated(clean) == TRUE, ])
## 0 Duplicates in the clear


## go through each column of the training data set to find typos/missing values
## replace typos and missing values with corrected values (etc.)
colname <- names(clean) 
print(colname)
ncol(clean)
nrow(clean)
sum(is.na(clean))

## ID column by default should have no missing values
## but let's check
sum(is.na(clean$Id))
## result is 0 so this column is clean

## MSSubClass is next
str(clean$MSSubClass)
## class type is integer, but on the website the data type is the building class
## we'll need to change it to a factor
## so let's count is.na to cover our bases
sum(is.na(clean$MSSubClass))
## 61 nulls
clean$MSSubClass[is.na(clean$MSSubClass)] <- 20
## Finding uniques for building classes
unique(clean$MSSubClass)
## Factor
clean$MSSubClass <- as.factor(clean$MSSubClass)
levels(clean$MSSubClass)
clean$MSSubClass <- fct_collapse(clean$MSSubClass, "20/30" = c('20',"30"), "40/50" = c("40","45","50")
             , "60/70" = c("60", "70", "75"), "80/90" = c("80", "85", "90"), 
             "120/160" = c("120", "150", "160"), "180/190" = c("180", "190"))
levels(clean$MSSubClass)
summary(clean$MSSubClass)


## Next Column is MSZoning
str(clean$MSZoning)
sum(is.na(clean$MSZoning))
## 4 nulls
clean$MSZoning[is.na(clean$MSZoning)] <- "RL"
clean$MSZoning <- as.factor(clean$MSZoning)
levels(clean$MSZoning)
summary(clean$MSZoning)

## Next Column is LotFrontage
## This is an integer value of the linear feet of street connected to the property
str(clean$LotFrontage)
clean$LotFrontage <- as.numeric(clean$LotFrontage)
sum(is.na(clean$LotFrontage))
## result is 486 missing values
## we need to group by and then find mean values 
mean_lot <- clean %>%
  group_by(MSZoning) %>%
  summarise(mean = mean(LotFrontage, na.rm= TRUE))
str(mean_lot$mean)
## replace null values in lot frontage 
clean$LotFrontage[which(clean$MSZoning == 'C (all)' & is.na(clean$LotFrontage))] <- mean_lot[1,2]
clean$LotFrontage[which(clean$MSZoning == 'FV' & is.na(clean$LotFrontage))] <- mean_lot[2,2]
clean$LotFrontage[which(clean$MSZoning == 'RH' & is.na(clean$LotFrontage))] <- mean_lot[3,2]
clean$LotFrontage[which(clean$MSZoning == 'RL' & is.na(clean$LotFrontage))] <- mean_lot[4,2]
clean$LotFrontage[which(clean$MSZoning == 'RM' & is.na(clean$LotFrontage))] <- mean_lot[5,2]
## checking our work to see if we've removed all null values
sum(is.na(clean$LotFrontage))
## cover our bases
clean$LotFrontage <- as.numeric(clean$LotFrontage)
## looking at summary to get better picture
summary(clean$LotFrontage)
## obviously has outliers ## boxplot
ggplot(data = clean, aes(LotFrontage)) + 
  geom_boxplot()
## >300 
clean$LotFrontage <- ifelse(clean$LotFrontage > 300,
                                  mean(clean$LotFrontage),
                                  clean$LotFrontage)
## Skew
skewness(clean$LotFrontage)
ggplot(data = clean, aes(LotFrontage)) + geom_histogram()
## has a bit of a skew but not a ton


## Lot Area is next variable 
str(clean$LotArea)
clean$LotArea <- as.numeric(clean$LotArea)
sum(is.na(clean$LotArea))
## no null in Lot Area
## Outliers?
ggplot(data = clean, aes(LotArea)) + 
  geom_boxplot()
## Lot area > 100,000
clean$LotArea <- ifelse(clean$LotArea > 100000,
                            mean(clean$LotArea),
                            clean$LotArea)
## Skew 
skewness(clean$LotArea)
ggplot(data = clean, aes(LotArea)) + geom_histogram()
## >1 so highly skewed
ggplot(data = clean, aes(sqrt(LotArea))) + geom_histogram()
clean$LotArea <- sqrt(clean$LotArea)
## not quite enough
ggplot(data = clean, aes(sqrt(LotArea))) + geom_histogram()
clean$SqrtLotArea <- sqrt(clean$LotArea)
skewness(clean$SqrtLotArea)
clean <- subset(clean, select = -LotArea)
## skew is much better

## Street is Next # REMOVED
str(clean$Street)
unique(clean$Street)
## no nulls but let's check
sum(is.na(clean$Street))
## Factor with two levels (seems there are no typos)
clean$Street <- as.factor(clean$Street)
summary(clean$Street)
## might remove later

## Alley is next # REMOVED
### type of alley access
str(clean$Alley)
unique(clean$Alley)
sum(is.na(clean$Alley))
## That is a lot of Nulls
## Null is "No alley access"
clean$Alley[is.na(clean$Alley)] <- "No Access"
sum(is.na(clean$Alley))
## now that nulls are replaced we factor qualitative variable
clean$Alley <- as.factor(clean$Alley)
summary(clean$Alley)
## seem like a lot have no access might be a non-useful variable


## Lot Shape is Next #
str(clean$LotShape)
unique(clean$LotShape)
## doesn't appear to have nulls but let's check further
sum(is.na(clean$LotShape))
## factor
clean$LotShape <- as.factor(clean$LotShape)
summary(clean$LotShape)



## Land Contour is Next # REMOVED
### Flatness of Property
str(clean$LandContour)
unique(clean$LandContour)
## doesn't appear to have nulls but let's check
sum(is.na(clean$LotShape))
## Factor
clean$LandContour <- as.factor(clean$LandContour)
summary(clean$LandContour)

## Utilities is Next # REMOVED
### Types of Utilities available
unique(clean$Utilities)
sum(is.na(clean$Utilities))
## two nulls
summary(factor(clean$Utilities))
clean$Utilities[is.na(clean$Utilities)] <- "AllPub"
## Factor
clean$Utilities <- as.factor(clean$Utilities)
## Summarize
summary(factor(clean$Utilities))
## Useless variable so prob drop it



## Lot Config is next
### Lot Configuration
str(clean$LotConfig)
unique(clean$LotConfig)
## Check for nulls
sum(is.na(clean$LotConfig))
## 0 yeeee
## Factor
clean$LotConfig <- as.factor(clean$LotConfig)
summary(clean$LotConfig)


## Land Slope # REMOVED
### slope of land
str(clean$LandSlope)
unique(clean$LandSlope)
## Check for nulls
sum(is.na(clean$LandSlope))
# 0
## Factor
clean$LandSlope <- as.factor(clean$LandSlope)
summary(clean$LandSlope)
## might not be too useful


## Neighborhood
str(clean$Neighborhood)
unique(clean$Neighborhood)
## Check for nulls
sum(is.na(clean$Neighborhood))
## Factor
clean$Neighborhood <- as.factor(clean$Neighborhood)
summary(clean$Neighborhood)

## Condition 1
str(clean$Condition1)
unique(clean$Condition1)
## Check for nulls
sum(is.na(clean$Condition1))
## Factor
clean$Condition1 <- as.factor(clean$Condition1)
summary(clean$Condition1)
clean$Condition1 <- fct_collapse(clean$Condition1, "Artery/Feedr" = c('Artery',"Feedr"), 
                                 "PosA/N" = c("PosA","PosN"), 
                                 "RRAe/n//RRNn" = c("RRAe", "RRAn","RRNn"),
                                 "Norm" = "Norm")

## Condition 2 # SUBJECT FOR REMOVAL
str(clean$Condition2)
unique(clean$Condition2)
## Check for nulls
sum(is.na(clean$Condition2))
## Factor
clean$Condition2 <- as.factor(clean$Condition2)
summary(clean$Condition2)
clean$Condition2 <- fct_collapse(clean$Condition2, "Artery/Feedr" = c('Artery',"Feedr"), 
                                 "PosA/N" = c("PosA","PosN"), 
                                 "RRAe/n//RRNn" = c("RRAe", "RRAn","RRNn"),
                                 "Norm" = "Norm")
summary(clean$Condition2)
## might not be so useful

## Building Type
str(clean$BldgType)
unique(clean$BldgType)
## Check for nulls
sum(is.na(clean$BldgType))
## Factor
clean$BldgType <- as.factor(clean$BldgType)
summary(clean$BldgType)

## House Style
str(clean$HouseStyle)
unique(clean$HouseStyle)
## Check for nulls
sum(is.na(clean$HouseStyle))
## Factor
clean$HouseStyle <- as.factor(clean$HouseStyle)
summary(clean$HouseStyle)

## Overall Quality
str(clean$OverallQual)
summary(clean$OverallQual)
unique(clean$OverallQual)
## Check for nulls
sum(is.na(clean$OverallQual))
## Factor
clean$OverallQual <- factor(clean$OverallQual, order = TRUE, levels = seq(from = 1, to = 10, by = 1))
summary(clean$OverallQual)


## Overall Condition # REMOVED
str(clean$OverallCond)
summary(clean$OverallCond)
unique(clean$OverallCond)
## Check for nulls
sum(is.na(clean$OverallCond))
## Factor
clean$OverallCond <- factor(clean$OverallCond, order = TRUE, levels = seq(from = 1, to = 10, by = 1))
summary(clean$OverallCond)
## keeping 10 for interpretability reasons

## Year Built
str(clean$YearBuilt)
summary(clean$YearBuilt)
unique(clean$YearBuilt)
min(clean$YearBuilt)
max(clean$YearBuilt)
## I feel like this should be categorical just by vibes alone
## Check for nulls
sum(is.na(clean$YearBuilt))
## Factor
clean$YearBuilt <- factor(clean$YearBuilt, levels = c(seq(from = 1872, to = 2010, by = 1)))
levels(clean$YearBuilt)
clean$YearBuilt <- fct_collapse(clean$YearBuilt, "1870s" = c(seq(from = 1872, to = 1879, by = 1)), 
                                "1880s" = c(as.character(seq(from = 1880, to = 1889, by = 1)))
                                 , "1890s" = c(as.character(seq(from = 1890, to = 1899, by = 1))), 
                                "1900s" = c(as.character(seq(from = 1900, to = 1909, by = 1))), 
                                 "1910s" = c(as.character(seq(from = 1910, to = 1919, by = 1))), 
                                "1920s" = c(as.character(seq(from = 1920, to = 1929, by = 1))),
                                "1930s" = c(as.character(seq(from = 1930, to = 1939, by = 1))),
                                "1940s" = c(as.character(seq(from = 1940, to = 1949, by = 1))),
                                "1950s" = c(as.character(seq(from = 1950, to = 1959, by = 1))),
                                "1960s" = c(as.character(seq(from = 1960, to = 1969, by = 1))),
                                "1970s" = c(as.character(seq(from = 1970, to = 1979, by = 1))),
                                "1980s" = c(as.character(seq(from = 1980, to = 1989, by = 1))),
                                "1990s" = c(as.character(seq(from = 1990, to = 1999, by = 1))),
                                "2000s" = c(as.character(seq(from = 2000, to = 2010, by = 1))))
levels(clean$YearBuilt)
summary(clean$YearBuilt)

## Year Remodeled/Additions
clean$YearRemodAdd <- combined$YearRemodAdd
str(clean$YearRemodAdd)
summary(clean$YearRemodAdd)
unique(clean$YearRemodAdd)
## I feel like this should be categorical just by vibes alone
## Check for nulls
sum(is.na(clean$YearRemodAdd))
## Factor
clean$YearRemodAdd <- as.factor(clean$YearRemodAdd)
levels(clean$YearRemodAdd)
clean$YearRemodAdd <- fct_collapse(clean$YearRemodAdd,
                                "1950s" = c(as.character(seq(from = 1950, to = 1959, by = 1))),
                                "1960s" = c(as.character(seq(from = 1960, to = 1969, by = 1))),
                                "1970s" = c(as.character(seq(from = 1970, to = 1979, by = 1))),
                                "1980s" = c(as.character(seq(from = 1980, to = 1989, by = 1))),
                                "1990s" = c(as.character(seq(from = 1990, to = 1999, by = 1))),
                                "2000s" = c(as.character(seq(from = 2000, to = 2009, by = 1))))
levels(clean$YearBuilt)
summary(clean$YearRemodAdd)

## Roof Style
str(clean$RoofStyle)
summary(clean$RoofStyle)
unique(clean$RoofStyle)
## Check for nulls
sum(is.na(clean$RoofStyle))
## Factor
clean$RoofStyle <- as.factor(clean$RoofStyle)
summary(clean$RoofStyle)
clean$RoofStyle <- fct_collapse(clean$RoofStyle, "Gable" = "Gable",
                                "Gambrel/Hip" = c("Gambrel", "Hip"),
                                "Mansard/Shed/Flat" = c("Mansard", "Shed", "Flat"))
summary(clean$RoofStyle)


## Roof Material 
str(clean$RoofMatl)
summary(clean$RoofMatl)
unique(clean$RoofMatl)
## Check for nulls
sum(is.na(clean$RoofMatl))
## Factor
clean$RoofMatl <- as.factor(clean$RoofMatl)

## mostly COMPSHG so may be null in terms of predicting

## Exterior 1st
clean$Exterior1st <- combined$Exterior1st
str(clean$Exterior1st)
summary(factor(clean$Exterior1st))
unique(clean$Exterior1st)
## Check for nulls
sum(is.na(clean$Exterior1st))
# 1 null
summary(factor(clean$Exterior1st))
clean$Exterior1st[is.na(clean$Exterior1st)] <- "VinylSd"
## Factor
clean$Exterior1st <- as.factor(clean$Exterior1st)
levels(clean$Exterior1st)
summary(factor(clean$Exterior1st))
clean$Exterior1st <- fct_collapse(clean$Exterior1st, "Shingles" = c("AsbShng","AsphShn"), 
                                  "Brick" = c("BrkComm","BrkFace"), 
                                  "Cinder/Cement" = c("CBlock", "CemntBd"), 
                                  "Wood" = c("Wd Sdng", "WdShing","Plywood"), 
                                  "Hd/ImStucc" = c("HdBoard", "ImStucc"), 
                                  "Stone/Stucco" = c("Stone", "Stucco"),
                                  "Vinyl" = "VinylSd", "Metal" = "MetalSd")
levels(clean$Exterior1st)
summary(clean$Exterior1st)

## Exterior 2nd
str(clean$Exterior2nd)
summary(clean$Exterior2nd)
unique(clean$Exterior2nd)
## Check for nulls
sum(is.na(clean$Exterior2nd))
# 1 null
summary(factor(clean$Exterior2nd))
clean$Exterior2nd[is.na(clean$Exterior2nd)] <- "VinylSd"
## Factor
clean$Exterior2nd <- as.factor(clean$Exterior2nd)
levels(clean$Exterior2nd)
summary(factor(clean$Exterior2nd))
clean$Exterior2nd <- fct_collapse(clean$Exterior2nd, "Shingles" = c("AsbShng","AsphShn"), 
                                  "Brick" = c("Brk Cmn","BrkFace"), 
                                  "Cinder/Cement" = c("CBlock", "CmentBd"), 
                                  "Wood" = c("Wd Sdng", "Wd Shng","Plywood"), 
                                  "Hd/ImStucc" = c("HdBoard", "ImStucc"), 
                                  "Stone/Stucco" = c("Stone", "Stucco"),
                                  "Vinyl" = "VinylSd", "Metal" = "MetalSd",
                                  "Other" = "Other")
levels(clean$Exterior2nd)
summary(clean$Exterior2nd)
## probably have high correlation with exterior 1st = FALSE ASSUMPTION


## Masonry Venneer Type
str(clean$MasVnrType)
summary(factor(clean$MasVnrType))
unique(clean$MasVnrType)
## Check for nulls
sum(is.na(clean$MasVnrType))
## 24 Nulls gonna replace with None cause it makes the most sense
## 8 Null Values in this Subset replaced by most frequent
clean$MasVnrType[is.na(clean$MasVnrType)] <- "None"                       
## Factor
clean$MasVnrType <- as.factor(clean$MasVnrType)
summary(clean$MasVnrType)

## Masonry Veneer Area
str(clean$MasVnrArea)
summary(clean$MasVnrArea)
## there are nulls... how many
sum(is.na(clean$MasVnrArea))
### 8 nulls (most likely the same ones we replaced in above so replace with 0) but lets check
clean %>%
  select(MasVnrType, MasVnrArea) %>%
  filter(is.na(MasVnrArea))
## Insert 0
clean$MasVnrArea[is.na(clean$MasVnrArea)] <- 0    
## Outliers 
ggplot(data = clean, aes(MasVnrArea)) + 
  geom_boxplot()
## Lot area > 1000
clean$MasVnrArea <- ifelse(clean$MasVnrArea > 1500,
                        mean(clean$MasVnrArea),
                        clean$MasVnrArea)
## Skew 
skewness(clean$MasVnrArea)
ggplot(data = clean, aes(MasVnrArea)) + geom_histogram()
## skew isnt bad



## Exterior Quality
str(clean$ExterQual)
summary(factor(clean$ExterQual))
unique(clean$ExterQual)
## Check for nulls
sum(is.na(clean$ExterQual))
## Factor
clean$ExterQual <- as.factor(clean$ExterQual)
summary(clean$ExterQual)

## Exterior Condition # REMOVED
str(clean$ExterCond)
summary(factor(clean$ExterCond))
unique(clean$ExterCond)
## Check for nulls
sum(is.na(clean$ExterCond))
## Factor
clean$ExterCond <- factor(clean$ExterCond, order = T, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
summary(clean$ExterCond)


## Foundation
str(clean$Foundation)
summary(factor(clean$Foundation))
unique(clean$Foundation)
## Check for nulls
sum(is.na(clean$Foundation))
## Factor
clean$Foundation <- as.factor(clean$Foundation)
summary(clean$Foundation)

## Basement Height
str(clean$BsmtQual)
summary(factor(clean$BsmtQual))
unique(clean$BsmtQual)
## Check for nulls
sum(is.na(clean$BsmtQual))
## 81 nulls = No Basement
clean$BsmtQual[is.na(clean$BsmtQual)] <- "No Basement" 
## Factor
clean$BsmtQual <- factor(clean$BsmtQual, order = T, levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))
summary(clean$BsmtQual)


## Basement Condition # REMOVED
str(clean$BsmtCond)
summary(factor(clean$BsmtCond))
unique(clean$BsmtCond)
## Check for amount of nulls
sum(is.na(clean$BsmtCond))
## 37 nulls prob = No Basement
clean %>%
  select(BsmtQual, BsmtCond) %>%
  filter(is.na(BsmtCond))
clean$BsmtCond[which(is.na(clean$BsmtCond)& clean$BsmtQual == "No Basement")] <- "No Basement" 
clean$BsmtCond[which(is.na(clean$BsmtCond)& clean$BsmtQual == "TA")] <- "TA" 
clean$BsmtCond[which(is.na(clean$BsmtCond)& clean$BsmtQual == "Gd")] <- "Gd" 
## Factor
clean$BsmtQual <- factor(clean$BsmtQual, order = T, levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))
summary(clean$BsmtQual)
## Quality and Cond are similar so probably gonna drop one



## Basement Walkout or garden level basement walls
str(clean$BsmtExposure)
summary(factor(clean$BsmtExposure))
## Check for amount of nulls
sum(is.na(clean$BsmtExposure))
## 38 nulls prob = No Basement
clean %>%
  select(BsmtQual, BsmtCond, BsmtExposure) %>%
  filter(is.na(BsmtExposure))
## we can see that there is one value with a basement that has null value 
## we will put in value No
clean$BsmtExposure[which(is.na(clean$BsmtExposure)& clean$BsmtQual == "No Basement")] <- "No Basement" 
clean$BsmtExposure[is.na(clean$BsmtExposure)] <- "No" 
## Factor
clean$BsmtQual <- as.factor(clean$BsmtExposure)
summary(clean$BsmtExposure)

## basement finished type 1
str(clean$BsmtFinType1)
summary(factor(clean$BsmtFinType1))
unique(clean$BsmtFinType1)
## Check for amount of nulls
sum(is.na(clean$BsmtFinType1))
## 37 nulls prob = No Basement
clean %>%
  select(BsmtQual, BsmtFinType1) %>%
  filter(is.na(BsmtFinType1))
## all values that are null have no basement
clean$BsmtFinType1[is.na(clean$BsmtFinType1)] <- "No Basement" 
## Factor
clean$BsmtFinType1 <- as.factor(clean$BsmtFinType1)
levels(clean$BsmtFinType1)
clean$BsmtFinType1 <- fct_collapse(clean$BsmtFinType1, 
                                   "ALQ" = "ALQ", 
                                   "BLQ" = "BLQ", 
                                 "GLC" = "GLQ",
                                 "LwQ" = "LwQ",
                                 "Rec" = "Rec",
                                 "NoBSMT/UNF" = c("No Basement", "Unf"))
levels(clean$BsmtFinType1)
summary(clean$BsmtFinType1)

## Basement Finish Type 1 Square Footage # REMOVED
str(clean$BsmtFinSF1)
summary(clean$BsmtFinSF1)
## Check for amount of nulls
sum(is.na(clean$BsmtFinSF1))
## one null
clean %>%
  select(BsmtQual, BsmtFinSF1) %>%
  filter(is.na(BsmtFinSF1))
## null is no basement
clean$BsmtFinSF1[is.na(clean$BsmtFinSF1)] <- 0
## outlier
ggplot(data = clean, aes(BsmtFinSF1)) + 
  geom_boxplot()
## Lot area > 3000
clean$BsmtFinSF1 <- ifelse(clean$BsmtFinSF1 > 3000,
                           mean(clean$BsmtFinSF1),
                           clean$BsmtFinSF1)
## Skew 
skewness(clean$BsmtFinSF1)
ggplot(data = clean, aes(BsmtFinSF1)) + geom_histogram()
## skew .... mm not horible





## Basement Finish Type 2
str(clean$BsmtFinType2)
summary(factor(clean$BsmtFinType2))
unique(clean$BsmtFinType2)
## Check for amount of nulls
sum(is.na(clean$BsmtFinType2))
## 80 nulls prob = No Basement
clean %>%
  select(BsmtQual, BsmtFinType2) %>%
  filter(is.na(BsmtFinType2))
## Can realistically assume that the one null with basment type has only one 
## finish type
clean$BsmtFinType2[which(is.na(clean$BsmtFinType2)& clean$BsmtQual == "No Basement")] <- "No Basement" 
clean$BsmtFinType2[is.na(clean$BsmtFinType2)] <- "Unf" 
## Factor
clean$BsmtFinType2 <- as.factor(clean$BsmtFinType2)
levels(clean$BsmtFinType2)
clean$BsmtFinType2 <- fct_collapse(clean$BsmtFinType2, 
                                   "ALQ" = "ALQ", 
                                   "BLQ" = "BLQ", 
                                   "GLC" = "GLQ",
                                   "LwQ" = "LwQ",
                                   "Rec" = "Rec",
                                   "NoBSMT/UNF" = c("No Basement", "Unf"))
summary(clean$BsmtFinType2)

## Finish 2 sqr ft (REMOVED)
str(clean$BsmtFinSF2)
summary(clean$BsmtFinSF2)
## Check for amount of nulls
sum(is.na(clean$BsmtFinSF2))
## one null
clean %>%
  select(BsmtQual, BsmtFinSF2) %>%
  filter(is.na(BsmtFinSF2))
## null is no basement
clean$BsmtFinSF2[is.na(clean$BsmtFinSF2)] <- 0
## outlier
ggplot(data = clean, aes(BsmtFinSF2)) + 
  geom_boxplot()
## Lot area > 3000
clean$BsmtFinSF2 <- ifelse(clean$BsmtFinSF2 > 1250,
                           mean(clean$BsmtFinSF2),
                           clean$BsmtFinSF2)
## Skew 
skewness(clean$BsmtFinSF2)
ggplot(data = clean, aes(BsmtFinSF2)) + geom_histogram()
## skew is bad but probably gonna remove


## Unfinished Square Footage # REMOVED (?)
str(clean$BsmtUnfSF)
summary(clean$BsmtUnfSF)
## Check for amount of nulls
sum(is.na(clean$BsmtUnfSF))
## one null
clean %>%
  select(BsmtQual, BsmtUnfSF) %>%
  filter(is.na(BsmtUnfSF))
## null is no basement
clean$BsmtUnfSF[is.na(clean$BsmtUnfSF)] <- 0
## outlier
ggplot(data = clean, aes(BsmtUnfSF)) + 
  geom_boxplot()
## Lot area > 3000
clean$BsmtUnfSF <- ifelse(clean$BsmtUnfSF > 2250,
                           mean(clean$BsmtUnfSF),
                           clean$BsmtUnfSF)
## Skew 
skewness(clean$BsmtUnfSF)
ggplot(data = clean, aes(BsmtUnfSF)) + geom_histogram()
## mmm skew is ok not great but still within range but prob gonna 
## drop in favor of total



## Total Square Footage 
str(clean$TotalBsmtSF)
summary(clean$TotalBsmtSF)
## Check for amount of nulls
sum(is.na(clean$TotalBsmtSF))
## one null
clean %>%
  select(BsmtQual, TotalBsmtSF) %>%
  filter(is.na(TotalBsmtSF))
## null is no basement
clean$TotalBsmtSF[is.na(clean$TotalBsmtSF)] <- 0
## outlier
ggplot(data = clean, aes(TotalBsmtSF)) + 
  geom_boxplot()
## Lot area > 4000
clean$TotalBsmtSF <- ifelse(clean$TotalBsmtSF > 4000,
                          mean(clean$TotalBsmtSF),
                          clean$TotalBsmtSF)
## Skew 
skewness(clean$TotalBsmtSF)
ggplot(data = clean, aes(TotalBsmtSF)) + geom_histogram()
## i wish i had a thumbs up emoji ability in r


## Heating # REMOVED
str(clean$Heating)
summary(factor(clean$Heating))
unique(clean$Heating)
## no nulls
summary(factor(clean$Heating))
clean$Heating <- as.factor(clean$Heating)

## Heating Qual/Cond
str(clean$HeatingQC)
summary(factor(clean$HeatingQC))
unique(clean$HeatingQC)
sum(is.na(clean$HeatingQC))
## no nulls
clean$HeatingQC <- factor(clean$HeatingQC, order = T, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
clean$HeatingQC <- fct_collapse(clean$HeatingQC, "Po/Fa" = c("Po", "Fa"))
summary((clean$HeatingQC))


## Central Air
str(clean$CentralAir)
unique(clean$CentralAir)
summary(factor(clean$CentralAir))
# no nulls
clean$CentralAir <- as.factor(clean$CentralAir)
summary(clean$CentralAir)

## Electrical
str(clean$Electrical)
unique(clean$Electrical)
summary(factor(clean$Electrical))
sum(is.na(clean$Electrical))
# one null so replacing with most common SBrkr
clean$Electrical[is.na(clean$Electrical)] <- 'SBrkr'
clean$Electrical <- as.factor(clean$Electrical)
summary(clean$Electrical)

## First Floor Square Footage # REMOVED
str(clean$X1stFlrSF)
summary(clean$X1stFlrSF)
sum(is.na(clean$X1stFlrSF))
## 0 nulls
ggplot(data = clean, aes(X1stFlrSF)) + 
  geom_boxplot()
## X1stFlrSF > 3000
clean$X1stFlrSF <- ifelse(clean$X1stFlrSF > 3000,
                            mean(clean$X1stFlrSF),
                            clean$X1stFlrSF)
## Skew 
skewness(clean$X1stFlrSF)
ggplot(data = clean, aes(X1stFlrSF)) + geom_histogram()
## moderately skewed so 


## Second Floor Square Footage
str(clean$X2ndFlrSF)
summary(clean$X2ndFlrSF)
sum(is.na(clean$X2ndFlrSF))
## no nulls 
ggplot(data = clean, aes(X2ndFlrSF)) + 
  geom_boxplot()
## X2ndFlrSF > 2000
clean$X2ndFlrSF <- ifelse(clean$X2ndFlrSF > 2000,
                          mean(clean$X2ndFlrSF),
                          clean$X2ndFlrSF)
## Skew 
skewness(clean$X2ndFlrSF)
ggplot(data = clean, aes(X2ndFlrSF)) + geom_histogram()
## moderately skewed so 

## Low Quality finished square footage # REMOVED
str(clean$LowQualFinSF)
summary(clean$LowQualFinSF)
sum(is.na(clean$LowQualFinSF))
## no nulls 
ggplot(data = clean, aes(LowQualFinSF)) + 
  geom_boxplot()
## LowQualFinSF > 600
clean$LowQualFinSF <- ifelse(clean$LowQualFinSF > 600,
                          mean(clean$LowQualFinSF),
                          clean$LowQualFinSF)
## Skew 
skewness(clean$LowQualFinSF)
ggplot(data = clean, aes(LowQualFinSF)) + geom_histogram()
## moderately skewed so 




## above ground square footage # REMOVED
str(clean$GrLivArea)
summary(clean$GrLivArea)
sum(is.na(clean$GrLivArea))
## no nulls 
ggplot(data = clean, aes(GrLivArea)) + 
  geom_boxplot()
## GrLivArea > 4000
clean$GrLivArea <- ifelse(clean$GrLivArea > 4000,
                             mean(clean$GrLivArea),
                             clean$GrLivArea)
## Skew 
skewness(clean$GrLivArea)
ggplot(data = clean, aes(GrLivArea)) + geom_histogram()
## moderately skewed so 


## basment full baths # REMOVED
str(clean$BsmtFullBath)
unique(clean$BsmtFullBath)
## numeric 
summary(factor(clean$BsmtFullBath))
# one null going to replace with 0
clean$BsmtFullBath[is.na(clean$BsmtFullBath)] <- 0
skewness(clean$BsmtFullBath)
## moderately skewed so its ok


## Basement Half baths # REMOVED
str(clean$BsmtHalfBath)
unique(clean$BsmtHalfBath)
summary(factor(clean$BsmtHalfBath))
## 2 nulls
clean$BsmtHalfBath[is.na(clean$BsmtHalfBath)] <- 0
## Skew
skewness(clean$BsmtHalfBath)

## ^^ combine so not fixing skew or outliers

## Full Bath # REMOVED
str(clean$FullBath)
unique(clean$FullBath)
summary(factor(clean$FullBath))
## no Na s
## outliers
ggplot(data = clean, aes(FullBath)) + 
  geom_boxplot()
## outlier at 4 
clean$FullBath <- ifelse(clean$FullBath > 4,
                          mean(clean$FullBath),
                          clean$FullBath)
## Skew
skewness(clean$FullBath)
## not bad skew

## Half Bath # REMOVED
str(clean$HalfBath)
unique(clean$HalfBath)
summary(factor(clean$HalfBath))
## no Na s
## outliers
ggplot(data = clean, aes(HalfBath)) + 
  geom_boxplot()
## Skew
skewness(clean$HalfBath)
## not too bad skew

## Above Ground Bedrooms #
str(clean$BedroomAbvGr)
unique(clean$BedroomAbvGr)
summary(factor(clean$BedroomAbvGr))
## no Na s
## outliers
ggplot(data = clean, aes(BedroomAbvGr)) + 
  geom_boxplot()
## Skew
skewness(clean$BedroomAbvGr)
## not too bad skew

## Kitchen Above Ground # REMOVED
str(clean$KitchenAbvGr)
unique(clean$KitchenAbvGr)
summary(factor(clean$KitchenAbvGr))
## no Na s
## outliers?
ggplot(data = clean, aes(KitchenAbvGr)) + 
  geom_boxplot()
## can't really get rid of any?
## skew
skewness(clean$KitchenAbvGr)
ggplot(data = clean, aes(KitchenAbvGr)) + geom_histogram()
## really skewed but I'm gonna remove >.

## Kitchen Quality
str(clean$KitchenQual)
unique(clean$KitchenQual)
summary(factor(clean$KitchenQual))
## one null
clean %>%
  select(KitchenAbvGr, KitchenQual) %>%
  filter(is.na(KitchenQual))
clean$KitchenQual[is.na(clean$KitchenQual)] <- 'TA'
## replace more common
clean$KitchenQual <- factor(clean$KitchenQual, order = T, levels = c("Po", "Fa", "TA", "Gd", "Ex"))


## Total Rooms Above Ground
str(clean$TotRmsAbvGrd)
unique(clean$TotRmsAbvGrd)
summary(factor(clean$TotRmsAbvGrd))
sum(is.na(clean$TotRmsAbvGrd))
## no Na s
## Outlier
ggplot(data = clean, aes(TotRmsAbvGrd)) + 
  geom_boxplot()
## can't really get rid of any?
## skew
skewness(clean$TotRmsAbvGrd)
ggplot(data = clean, aes(TotRmsAbvGrd)) + geom_histogram()
## skew is fine


## Functional
str(clean$Functional)
unique(clean$Functional)
summary(factor(clean$Functional))
sum(is.na(clean$Functional))
## two nulls
clean$Functional[is.na(clean$Functional)] <- 'Typ'
## factor
clean$Functional <- as.factor(clean$Functional)
levels(clean$Functional)
clean$Functional <- fct_collapse(clean$Functional, "Sev/Mod" = c("Sev","Mod"), 
                                 "Majs" = c("Maj2", "Maj1"),
                                 "Mins" = c("Min1", "Min2"))
summary(factor(clean$Functional))


## Fireplaces
str(clean$Fireplaces)
unique(clean$Fireplaces)
summary(factor(clean$Fireplaces))
## no nulls
## outliers?
ggplot(data = clean, aes(Fireplaces)) + 
  geom_boxplot()
## mmm imma leave it
## skewness
skewness(clean$Fireplaces)
## skew is fine

## Fireplaces quality
str(clean$FireplaceQu)
unique(clean$FireplaceQu)
summary(factor(clean$FireplaceQu))
## 1420 nas most likely due to people not having a fire place
clean %>%
  select(Fireplaces, FireplaceQu) %>%
  filter(is.na(FireplaceQu))
## all have 0 
clean$FireplaceQu[is.na(clean$FireplaceQu)] <- 'No Fireplace'
clean$FireplaceQu <- factor(clean$FireplaceQu, order = T, levels = c("No Fireplace","Po", "Fa", "TA", "Gd", "Ex"))
summary(factor(clean$FireplaceQu))

## Garage Type 
str(clean$GarageType)
unique(clean$GarageType)
summary(factor(clean$GarageType))
sum(is.na(clean$GarageType))
## 157
clean$GarageType[is.na(clean$GarageType)] <- "No Garage"
clean$GarageType <- as.factor(clean$GarageType)

## Garage - Year Built # Removed 
# redundant variable i will remove
str(clean$GarageYrBlt)
unique(clean$GarageYrBlt)
summary(factor(clean$GarageYrBlt))
## 159 nulls
clean$GarageYrBlt[is.na(clean$GarageYrBlt)] <- "No Garage"
clean$GarageYrBlt <- as.factor(clean$GarageYrBlt)
summary(factor(clean$GarageYrBlt))


## Garage Finish
str(clean$GarageFinish)
unique(clean$GarageFinish)
summary(factor(clean$GarageFinish))
clean$GarageFinish[is.na(clean$GarageFinish)] <- "No Garage"
clean$GarageFinish <- as.factor(clean$GarageFinish)


## Garage Cars # REMOVED
str(clean$GarageCars)
unique(clean$GarageCars)
summary(factor(clean$GarageCars))
## one null replace with 0
clean$GarageCars[is.na(clean$GarageCars)] <- 0
## Outliers?
ggplot(data = clean, aes(GarageCars)) + 
  geom_boxplot()
## skew?
skewness(clean$GarageCars)
## not bad but im also not gonna use it


## Garage Qual
str(clean$GarageQual)
unique(clean$GarageQual)
summary(factor(clean$GarageQual))
sum(is.na(clean$GarageQual))
## 159
clean$GarageQual[is.na(clean$GarageQual)] <- "No Garage"
clean$GarageQual <- factor(clean$GarageQual, order = T, levels = c("No Garage","Po", "Fa", "TA", "Gd", "Ex"))
summary(factor(clean$GarageQual))

## Garage Area 
str(clean$GarageArea)
unique(clean$GarageArea)
summary((clean$GarageArea))
## one null replace with mean
clean$GarageArea[is.na(clean$GarageArea)] <- mean(clean$GarageArea, na.rm = TRUE)
## outlier
ggplot(data = clean, aes(GarageCars)) + 
  geom_boxplot()
## skew?
skewness(clean$GarageCars)


## Garage cond # REMOVED
# is similar to quality so I'm dropping


## Paved Driveway
str(clean$PavedDrive)
unique(clean$PavedDrive)
summary(factor(clean$PavedDrive))
clean$PavedDrive <- as.factor(clean$PavedDrive)


## Wood deck SF
str(clean$WoodDeckSF)
unique(clean$WoodDeckSF)
summary(factor(clean$WoodDeckSF))
sum(is.na(clean$WoodDeckSF))
## Outliers?
ggplot(data = clean, aes(WoodDeckSF)) + 
  geom_boxplot()
## around 750
clean$WoodDeckSF <- ifelse(clean$WoodDeckSF > 750,
                                 mean(clean$WoodDeckSF),
                                 clean$WoodDeckSF)
## skew?
ggplot(data = clean, aes(WoodDeckSF)) + geom_histogram()
skewness(clean$WoodDeckSF)
ggplot(data = clean, aes(log10(WoodDeckSF+1))) + geom_histogram()
skewness(log10(clean$WoodDeckSF +1))
clean$WoodDeckSF <- log10(clean$WoodDeckSF +1)
## not sure about this one

## Open Porch SF # REMOVED
str(clean$OpenPorchSF)
unique(clean$OpenPorchSF)
summary(factor(clean$OpenPorchSF))
sum(is.na(clean$OpenPorchSF))
## outliers 
ggplot(data = clean, aes(OpenPorchSF)) + 
  geom_boxplot()
## around 600
clean$OpenPorchSF <- ifelse(clean$OpenPorchSF > 600,
                                  mean(clean$OpenPorchSF),
                                  clean$OpenPorchSF)
## Skew
ggplot(data = clean, aes(OpenPorchSF)) + geom_histogram()
skewness(clean$OpenPorchSF)
## skewedddd but im gonna combine it with other so not fixing

## closed porch # REMOVED
clean$EnclosedPorch <- as.numeric(clean$EnclosedPorch)
sum(is.na(clean$EnclosedPorch))
#No missing value 
#Outliers
ggplot(data = clean, aes(EnclosedPorch)) + geom_boxplot()
#Outliers seem to be around 500.
#Replace outliers with mean.
clean$EnclosedPorch <- ifelse(clean$EnclosedPorch > 500,
                                           mean(clean$EnclosedPorch),
                                           clean$EnclosedPorch)
#Skewness
ggplot(data = clean, aes(EnclosedPorch)) + geom_histogram()
skewness(clean$EnclosedPorch)
#The value is 2.87, highly skewed.
#As I will combine EnclosedPorch with other variables to create a new variable,
#for now I won't do any transformation.


## X3SsnPorch # REMOVED
clean$X3SsnPorch <- as.numeric(clean$X3SsnPorch)
sum(is.na(clean$X3SsnPorch))
#Outliers
ggplot(data = clean, aes(X3SsnPorch)) + geom_boxplot()
#Outliers seem to be around 500.
#Replace outliers with mean.
clean$X3SsnPorch <- ifelse(clean$X3SsnPorch > 500,
                                        mean(clean$X3SsnPorch),
                                        clean$X3SsnPorch)
#Skewness
ggplot(data = clean, aes(X3SsnPorch)) + geom_histogram()
skewness(clean$X3SsnPorch)
#The value is 10.28, highly skewed.
#As I will combine EnclosedPorch with other variables to create a new variable,
#for now I won't do any transformation.


## ScreenPorch # REMOVED
clean$ScreenPorch <- as.numeric(clean$ScreenPorch)
sum(is.na(clean$ScreenPorch))
#No missing value.
#Outliers
ggplot(data = clean, aes(ScreenPorch)) + geom_boxplot()
#outliers > 500?
#Replace outliers with mean.
clean$ScreenPorch <- ifelse(clean$ScreenPorch > 500,
                           mean(clean$ScreenPorch),
                           clean$ScreenPorch)
#Skewness
ggplot(data = clean, aes(ScreenPorch)) + geom_histogram()
skewness(clean$ScreenPorch)
#The value is 4.114, highly skewed.
#As I will combine EnclosedPorch with other variables to create a new variable,
#for now I won't do any transformation.


## PoolArea # REMOVED
sum(is.na(clean$PoolArea))
#No missing value.
#Outliers
ggplot(data = clean, aes(PoolArea)) + geom_boxplot()
#I don't see very obvious outliers.
#However, 1453 out of 1460 case have the same value (i.e., 0), which
#won't provid much useful information in the modeling stage.
#PoolArea should be removed.
#Skewness
ggplot(data = clean, aes(PoolArea)) + geom_histogram()
skewness(clean$PoolArea)
#The value is 14.798, highly skewed.


##COLUMN 73: PoolQC # REMOVED
sum(is.na(clean$PoolQC))
summary(factor(clean$PoolQC))
#1453 NAs but they mean "No Pool".
#Replace NAs with "No Pool".
clean$PoolQC[is.na(clean$PoolQC)] <- "No Pool"
clean$PoolQC <- factor(clean$PoolQC, order = TRUE, 
                                    levels = c("No Pool", "Fa", "TA", "Gd", "Ex"))
#Similar to PoolArea, PoolQC should be removed.


## Fence
sum(is.na(clean$Fence))
summary(factor(clean$Fence))
#1179 NAs but they mean "No Fence".
#Replace NAs with "No Fence".
clean$Fence[is.na(clean$Fence)] <- "No Fence"
clean$Fence <- factor(clean$Fence, order = TRUE, 
                                   levels = c("No Fence", "MnWw", "GdWo", "MnPrv", "GdPrv"))



## MiscFeature # REMOVED
sum(is.na(clean$MiscFeature))
summary(factor(clean$MiscFeature))
#1406 NAs but they mean "None".
#Replace NAs with "None" to avoid confusion.
clean$MiscFeature[is.na(clean$MiscFeature)] <- "None"
clean$MiscFeature <- as.factor(clean$MiscFeature)
#MiscFeature should be removed because 1406 out of 1460 cases share the same value,
#which won't generate much useful information in data modeling.


## MiscVal REMOVED
sum(is.na(clean$MiscVal))
#Outliers
ggplot(data = clean, aes(MiscVal)) + geom_boxplot()
#Outliers seem to be around 7500.
#Replace outliers with mean.
clean$MiscVal <- ifelse(clean$MiscVal > 7500,
                                     mean(clean$MiscVal),
                                     clean$MiscVal)
#Skewness
ggplot(data = clean, aes(MiscVal)) + geom_histogram()
skewness(clean$MiscVal)
#The value is 10.487, highly skewed.
summary(factor(clean$MiscVal))
#MiscVal has 1408 out of 1460 cases with the same value (i.e., 0), 
#which won't provide much useful information during the modeling stage.
#Suggest removing this variable.


## MoSold
sum(is.na(clean$MoSold))
#No missing value 
#Similar to YearBuilt, it would be more reasonable to explain the coefficient of MoSold
#when MoSold is a categorical variable.
clean$MoSold <- as.factor(clean$MoSold)


## YrSold
sum(is.na(clean$YrSold))
#No missing value 
#Similar to YearBuilt, it would be more reasonable to explain the coefficient of YrSold
#when YrSold is a categorical variable.
clean$YrSold <- as.factor(clean$YrSold)
levels(clean$YrSold)


## SaleType
sum(is.na(clean$SaleType))
summary(factor(clean$SaleType))
#one null
clean$SaleType[is.na(clean$SaleType)] <- "WD"
clean$SaleType <- as.factor(clean$SaleType)
levels(clean$SaleType)


## SaleCondition
sum(is.na(clean$SaleCondition))
summary(factor(clean$SaleCondition))
#No missing value or typos.
clean$SaleCondition <- as.factor(clean$SaleCondition)


## SalePrice
sum(is.na(clean$SalePrice))
#No missing value.
skewness(clean$SalePrice)
#The value is 1.879, highly skewed.
#Try log transformation.
skewness(log10(clean$SalePrice+1))
clean$Log10SalePrice <- log10(clean$SalePrice+1)
clean <- subset(clean, select = -SalePrice)
#The value now is 0.121, good!



##  "HomeTotalSF" 
## combine SF variables 
clean$HomeTotalSF <- clean$TotalBsmtSF + clean$X1stFlrSF + clean$X2ndFlrSF
skewness(clean$HomeTotalSF)
#0.70, Only moderately skewed.


## "TotalPorchSF" 
## combine porch variables
clean$TotalPorchSF <- clean$OpenPorchSF + clean$EnclosedPorch + clean$X3SsnPorch + clean$ScreenPorch
skewness(clean$TotalPorchSF)
#The value is 1.479, highly skewed.
#Try log transformation.
skewness(log10(clean$TotalPorchSF+1))
clean$log10TotalPorchSF <- log10(clean$TotalPorchSF+1)
clean <- subset(clean, select = -TotalPorchSF)
#The value is -0.518, better!

## "TotalFullBath" 
## combine full bath variables
clean$TotalFullBath <- clean$BsmtFullBath + clean$FullBath 
skewness(clean$TotalFullBath)
#The value is 0.298, approximately symmetrical.

## "TotalHalfBath" 
## combine half bathroom variables
clean$TotalHalfBath <- clean$BsmtHalfBath + clean$HalfBath
skewness(clean$TotalHalfBath)
#The value is 0.937, moderately skewed.

## Removing Variables
clean <- subset(clean, select = -c(TotalBsmtSF, BsmtHalfBath, HalfBath,
                                   BsmtFullBath, FullBath, EnclosedPorch, 
                                   X3SsnPorch, ScreenPorch, PoolArea, 
                                   BsmtFinSF2, PoolQC, BsmtFinSF1,
                                   KitchenAbvGr, GarageCond, 
                                   MiscFeature, MiscVal, GarageYrBlt,
                                   X1stFlrSF,X2ndFlrSF, GarageCars, MiscVal,
                                   BsmtCond, ExterCond, OverallCond, 
                                   BsmtExposure, LowQualFinSF, Heating, Street, 
                                   Alley, Utilities, LandSlope, BsmtUnfSF,
                                   CentralAir))

## separating the data back out
clean_test <- clean %>%
  filter(Log10SalePrice == 0)

clean_train <- clean %>%
  filter(Log10SalePrice != 0)



#################
## Milestone 2 ##
#################

clean_train <- clean_train[ , -1]
ncol(clean_train)

model_plainvanila <- lm(Log10SalePrice ~., data = clean_train)
summary(model_plainvanila)

## dropping basement exposure
## NO NULLS :D

## combine with closest level to stop error in predictor model
## for different reasons (na coeficcients for train model) (warning for predict model)
## MSE
MSE_Train_Plain <- mean((10^model_plainvanila$residuals)^2)
MSE_Train_Plain

## year built is messing up calcs might need to remove a couple obs
## that is a large MSE
## levels for MSSubclass dont match ## went back and removed an obs


clean_test <- subset(clean_test, select = -c(Log10SalePrice))
test_pred <- predict(model_plainvanila, clean_test)
test_pred <- as.data.frame(test_pred)
test_pred$Id <- clean_test$Id
test_pred$SalePrice <- 10^test_pred$test_pred
test_pred <- subset(test_pred, select = -test_pred)

# write.csv(test_pred, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_feb24.csv", row.names = F)
## Best Score = 0.13857,


## vif
# vif(model_plainvanila)

alias_matrix <- alias(model_plainvanila)
levels(clean_train$GarageType)
## no Garage Type is highly correlated with finish Garage Finish
clean_train <- subset(clean_train, select = -GarageFinish)
clean_test <- subset(clean_test, select = -GarageFinish)
clean_train <- subset(clean_train, select = -GarageQual)
clean_test <- subset(clean_test, select = -GarageQual)
model_plainvanila <- lm(Log10SalePrice ~., clean_train)
summary(model_plainvanila)

alias(model_plainvanila)
vif(model_plainvanila)
## works now :)
## won't work
str(clean_train)
cormatrix <- cor(clean_train[,c(3,20,28,29,31,33,36,38,39,45,46,47,48,49,50)], method = "pearson")
# scatterplot against response and categorical variables
## if highly correl scatterplots will look similar

cormatrix1 <- ifelse(cormatrix < .5, '' , round(cormatrix,2))
cormatrix1
summary(model_plainvanila)
## TotRms and GrLivArea are correlated
summary(lm(Log10SalePrice ~. - TotRmsAbvGrd, data = clean_train))
summary(lm(Log10SalePrice ~. + TotRmsAbvGrd*GrLivArea, data = clean_train))
vif(lm(Log10SalePrice ~. + TotRmsAbvGrd*GrLivArea, data = clean_train))
vif(model_plainvanila)
# ADJR^2 doesnt really change ^^
summary(lm(Log10SalePrice ~. - GrLivArea, data = clean_train))
# ADJR^2 does really change ^^

# clean_train <- subset(clean_train, select = -c(TotRmsAbvGrd))
# clean_test <- subset(clean_test, select = -c(TotRmsAbvGrd))


model_plainvanila2 <- lm(Log10SalePrice ~., data = clean_train)
summary(model_plainvanila2)
vif(model_plainvanila)
## removing further to see if it helps model
## MSSubClass
summary(model_plainvanila2)
summary(lm(Log10SalePrice ~. -MSSubClass , data = clean_train))
## Increases adj r^2
clean_train <- subset(clean_train, select = -c(MSSubClass))
clean_test <- subset(clean_test, select = -c(MSSubClass))
## Home TotalSF 
model_plainvanila2 <- lm(Log10SalePrice ~., data = clean_train)
## Guessing I can't remove but let's check
summary(model_plainvanila2)
summary(lm(Log10SalePrice ~. -HomeTotalSF , data = clean_train))
## not taking it out it decreases model accuracy and adj decreases by a bunch

## BldgType
summary(model_plainvanila2)
summary(lm(Log10SalePrice ~. -BldgType , data = clean_train))
##Change model Accuracy



## Exterior 1st
summary(lm(Log10SalePrice ~. -Exterior1st , data = clean_train))
##  Change model Accuracy



## Exterior 2nd
summary(model_plainvanila)
summary(lm(Log10SalePrice ~. -Exterior2nd , data = clean_train))
## doesnt Change model Accuracy too much
clean_train <- subset(clean_train, select = -Exterior2nd)
clean_test <- subset(clean_test, select = -Exterior2nd)
model_plainvanila2 <- lm(Log10SalePrice ~., data = clean_train)
vif(model_plainvanila)



vif(model_plainvanila)


## Fireplaces
summary(lm(Log10SalePrice ~. -Fireplaces , data = clean_train))
## doesnt Change model Accuracy too much (0.0001)
clean_train <- subset(clean_train, select = -Fireplaces)
clean_test <- subset(clean_test, select = -Fireplaces)
model_plainvanila2 <- lm(Log10SalePrice ~., data = clean_train)
vif(model_plainvanila2)




model_plainvanila3 <- lm(Log10SalePrice ~., data = clean_train)



summary(model_plainvanila3)

## rerunning model to see if it helped
test_pred <- predict(model_plainvanila3, clean_test)
test_pred <- as.data.frame(test_pred)
test_pred$Id <- clean_test$Id
test_pred$SalePrice <- 10^test_pred$test_pred
test_pred3 <- subset(test_pred, select = -test_pred)
# write.csv(test_pred3, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_feb24collin.csv", row.names = F)
# Best Score = 0.13838

#model_plainvanila4 <- lm(Log10SalePrice ~. + YrSold*MoSold, data = clean_train)
#summary(model_plainvanila4)
#test_pred <- predict(model_plainvanila4, clean_test)
#test_pred <- as.data.frame(test_pred)
#test_pred$Id <- clean_test$Id
#test_pred$SalePrice <- 10^test_pred$test_pred
#test_pred <- subset(test_pred, select = -test_pred)
# write.csv(test_pred, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_test.csv", row.names = F)
## Best Score = 0.14374

# Non-constance in variance Terms
par(mfrow = c(1,1))
plot(model_plainvanila3)
# Residuals vs. Fitted
# Pretty normaly distributed (no fan distribution is shown)
# and also cooksd line isn't shown

#Normal Q-Q Plot
## Residuals don't quite follow that line closely...
## Might have to ask about how to fix that

## Scale-Location Plot
## a bit of a fan shape but might be ok



# Cooks Distance
plot(model_plainvanila3, 6)
cooksD_cutoff <- 0.5
cooksD <- cooks.distance(model_plainvanila3)
cooksD_dataframe <- data.frame(obs = names(cooksD), cooks = cooksD)
cooksD_dataframe[which(abs(cooksD) > cooksD_cutoff), ]
leverage = 1
lever = hatvalues(model_plainvanila3)
leverage_dataframe <- data.frame(obs = names(lever), leverages = lever)
leverage_dataframe[which(abs(lever) == 1), ]
anova(model_plainvanila3)
clean_train <- clean_train[-c(272, 399, 1276, 1299),]
## removing points with leverage one? drop 
model_plainvanila3 <- lm(Log10SalePrice ~., data = clean_train)
plot(model_plainvanila3, 6)
clean_train <- clean_train[-121,]
model_plainvanila3 <- lm(Log10SalePrice ~., data = clean_train)
plot(model_plainvanila3, 6)
cooksD_cutoff <- 0.5
cooksD <- cooks.distance(model_plainvanila3)
cooksD_dataframe <- data.frame(obs = names(cooksD), cooks = cooksD)
cooksD_dataframe[which(abs(cooksD) > cooksD_cutoff), ]

summary(model_plainvanila3)

test_pred <- predict(model_plainvanila3, clean_test)
test_pred <- as.data.frame(test_pred)
test_pred$Id <- clean_test$Id
test_pred$SalePrice <- 10^test_pred$test_pred
test_pred <- subset(test_pred, select = -test_pred)
# write.csv(test_pred, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_milestone2done.csv", row.names = F)
# Best Score = 0.13838


summary(model_plainvanila3)

test_pred <- predict(model_plainvanila3, clean_test)
test_pred <- as.data.frame(test_pred)
test_pred$Id <- clean_test$Id
test_pred$SalePrice <- 10^test_pred$test_pred
test_pred <- subset(test_pred, select = -test_pred)
# write.csv(test_pred, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_interactiontest.csv", row.names = F)
# Best Score = 0.13834

#################
## Milestone 3 ##
#################

# Feature Selection
summary(model_plainvanila3)
colnames(clean_train)
## Forward AIC
null_model <- lm(Log10SalePrice ~ 1, data = clean_train)
Forward_AIC <- stepAIC(null_model, direction = "forward", 
                       scope = list(lower = null_model, upper = ~ MSZoning +
                                      LotFrontage + LotShape + LandContour  +
                                      LotConfig + Neighborhood + Condition1 +
                                      Condition2 + BldgType + HouseStyle +  OverallQual + 
                                      YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + 
                                      Exterior1st + MasVnrType + MasVnrArea +
                                      ExterQual + Foundation + BsmtQual + BsmtFinType1 +
                                      BsmtFinType2 + HeatingQC +
                                       Electrical + GrLivArea + BedroomAbvGr +
                                      KitchenQual + Functional + 
                                      FireplaceQu + GarageType + GarageArea + PavedDrive + 
                                      WoodDeckSF + OpenPorchSF + Fence + MoSold + YrSold +
                                      SaleType + SaleCondition + SqrtLotArea + HomeTotalSF + 
                                      log10TotalPorchSF + TotalFullBath + TotalHalfBath), k = 2)
summary(Forward_AIC)

MSE_Train_ForAIC <- mean((clean_train$Log10SalePrice - 10^Forward_AIC$fitted.values)^2)
Forward_AIC_predict <- predict(Forward_AIC, clean_test)
Forward_AIC_predict <- as.data.frame(Forward_AIC_predict)
Forward_AIC_predict$Id <- clean_test$Id
Forward_AIC_predict$SalePrice <- 10^Forward_AIC_predict$Forward_AIC_predict
Forward_AIC_predict <- subset(Forward_AIC_predict, select = -Forward_AIC_predict)
# write.csv(Forward_AIC_predict, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_Feb28Forward.csv", row.names = F)
## Best Score = 0.13486


## Backward AIC
Backward_AIC <- stepAIC(model_plainvanila3, direction = "backward", k = 2)
summary(Backward_AIC)
MSE_Train_BackAIC <- mean((clean_train$Log10SalePrice - 10^Backward_AIC$fitted.values)^2)
Backward_AIC_predict <- predict(Backward_AIC, clean_test)
Backward_AIC_predict <- as.data.frame(Backward_AIC_predict)
Backward_AIC_predict$Id <- clean_test$Id
Backward_AIC_predict$SalePrice <- 10^Backward_AIC_predict$Backward_AIC_predict
Backward_AIC_predict <- subset(Backward_AIC_predict, select = -Backward_AIC_predict)
#write.csv(Backward_AIC_predict, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_Backward.csv", row.names = F)
## Best Score = 0.13541


## Hybrid AIC
Hybrid_AIC <- stepAIC(null_model, direction = "both", 
                      scope = list(lower = null_model, 
                                   upper = ~ MSZoning +
                                     LotFrontage + LotShape + LandContour  +
                                     LotConfig + Neighborhood + Condition1 +
                                     Condition2 + BldgType + HouseStyle +  OverallQual + 
                                     YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + 
                                     Exterior1st + MasVnrType + MasVnrArea +
                                     ExterQual + Foundation + BsmtQual + BsmtFinType1 +
                                     BsmtFinType2 + HeatingQC +
                                     Electrical + GrLivArea + BedroomAbvGr +
                                     KitchenQual + Functional + 
                                     FireplaceQu + GarageType + GarageArea + PavedDrive + 
                                     WoodDeckSF + OpenPorchSF + Fence + MoSold + YrSold +
                                     SaleType + SaleCondition + SqrtLotArea + HomeTotalSF + 
                                     log10TotalPorchSF + TotalFullBath + TotalHalfBath), k = 2)
summary(Hybrid_AIC)
MSE_Train_Hybrid_AIC <- mean((clean_train$Log10SalePrice - 10^Hybrid_AIC$fitted.values)^2)
Hybrid_AIC_predict <- predict(Hybrid_AIC, clean_test)
Hybrid_AIC_predict <- as.data.frame(Hybrid_AIC_predict)
Hybrid_AIC_predict$Id <- clean_test$Id
Hybrid_AIC_predict$SalePrice <- 10^Hybrid_AIC_predict$Hybrid_AIC_predict
Hybrid_AIC_predict <- subset(Hybrid_AIC_predict, select = -Hybrid_AIC_predict)
# write.csv(Hybrid_AIC_predict, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_Feb24Hybrid.csv", row.names = F)
## Best Score = 0.13541



# Ridge Regression
x_train <- model.matrix(Log10SalePrice ~ MSZoning +
                          LotFrontage + LotShape + LandContour  +
                          LotConfig + Neighborhood + Condition1 +
                          Condition2 + BldgType + HouseStyle +  OverallQual + 
                          YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + 
                          Exterior1st + MasVnrType + MasVnrArea +
                          ExterQual + Foundation + BsmtQual + BsmtFinType1 +
                          BsmtFinType2 + HeatingQC +
                          Electrical + GrLivArea + BedroomAbvGr +
                          KitchenQual + Functional + 
                          FireplaceQu + GarageType + GarageArea + PavedDrive + 
                          WoodDeckSF + OpenPorchSF + Fence + MoSold + YrSold +
                          SaleType + SaleCondition + SqrtLotArea + HomeTotalSF + 
                          log10TotalPorchSF + TotalFullBath + TotalHalfBath, data = clean_train)[,-1]
y_train <- clean_train$Log10SalePrice

clean_test$Log10SalePrice = 0
x_test <- model.matrix(Log10SalePrice ~ MSZoning +
                         LotFrontage + LotShape + LandContour  +
                         LotConfig + Neighborhood + Condition1 +
                         Condition2 + BldgType + HouseStyle +  OverallQual + 
                         YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + 
                         Exterior1st + MasVnrType + MasVnrArea +
                         ExterQual + Foundation + BsmtQual + BsmtFinType1 +
                         BsmtFinType2 + HeatingQC +
                         Electrical + GrLivArea + BedroomAbvGr +
                         KitchenQual + Functional + 
                         FireplaceQu + GarageType + GarageArea + PavedDrive + 
                         WoodDeckSF + OpenPorchSF + Fence + MoSold + YrSold +
                         SaleType + SaleCondition + SqrtLotArea + HomeTotalSF + 
                         log10TotalPorchSF + TotalFullBath + TotalHalfBath, data = clean_test)[,-1]
cv_out_ridge1 <- cv.glmnet(x_train, y_train, alpha = 0, 
                          type.measure = "mse", nfolds = 10)
plot (cv_out_ridge1)
bestlam1 <- cv_out_ridge1$lambda.min
model_ridge <- glmnet(x_train, y_train, alpha = 0, 
                       standardize = TRUE, lambda = bestlam1)
summary(model_ridge)
coef(model_ridge)
model_ridge$beta
model_ridge$a0


## R-Squared Value
y_predicted <- predict(model_ridge, s = bestlam1, newx = x_train)

#find SST and SSE
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq



model_ridge_predict <- predict(model_ridge, newx = x_test)
model_ridge_predict <- as.data.frame(model_ridge_predict)
model_ridge_predict$Id <- clean_test$Id
model_ridge_predict$SalePrice <- 10^model_ridge_predict$s0
model_ridge_predict <- subset(model_ridge_predict, select = -s0)
# write.csv(model_ridge_predict, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_ridge.csv", row.names = F)
## Best Score = 0.134313


# Lasso 
cv_out_ridge2 <- cv.glmnet(x_train, y_train, alpha = 1, 
                          type.measure = "mse", nfolds = 10)
plot (cv_out_ridge2)
bestlam2 <- cv_out_ridge2$lambda.min
model_Lasso <- glmnet (x_train, y_train, alpha = 1, 
                       standardize = TRUE, lambda = bestlam2)
model_Lasso$beta

model_Lasso$a0
## R-Squared Value
y_predicted <- predict(model_ridge, s = bestlam2, newx = x_train)

#find SST and SSE
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

model_Lasso_predict <- predict(model_Lasso, newx = x_test)
model_Lasso_predict <- as.data.frame(model_Lasso_predict)
model_Lasso_predict$Id <- clean_test$Id
model_Lasso_predict$SalePrice <- 10^model_Lasso_predict$s0
model_Lasso_predict <- subset(model_Lasso_predict, select = -s0)
 write.csv(model_Lasso_predict, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_Feb24Lasso.csv", row.names = F)
## Best Score = 0.13145



#################
## Milestone 4 ##
#################

# Regression Tree

## Random Bagging
clean_test <- clean %>%
  filter(Log10SalePrice == 0)

clean_train <- clean %>%
  filter(Log10SalePrice != 0)

clean_train <- clean_train[ , -1]


Simple_tree <- tree(formula = Log10SalePrice ~., data = clean_train)
summary(Simple_tree)
plot(Simple_tree)
text(Simple_tree)

test_pred_simple <- predict(Simple_tree, clean_test)
test_pred_simple <- as.data.frame(test_pred_simple)
test_pred_simple$Id <- clean_test$Id
test_pred_simple$SalePrice <- 10^test_pred_simple$test_pred_simple
test_pred_simple <- subset(test_pred_simple, select = -test_pred_simple)
 write.csv(test_pred_simple, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//newtreewoprune.csv", row.names = F)
## Best Score = .2173


## Tree Pruning
cv_train_Tree <- cv.tree(Simple_tree, K = 10)
plot(cv_train_Tree$size, cv_train_Tree$dev, type = 'b')
## 8 
Simple_tree_prune <- prune.tree(Simple_tree, best = 8)
plot(Simple_tree_prune)
text(Simple_tree_prune)
test_pred_prune <- predict(Simple_tree_prune, clean_test)
test_pred_prune <- as.data.frame(test_pred_prune)
test_pred_prune$Id <- clean_test$Id
test_pred_prune$SalePrice <- 10^test_pred_prune$test_pred_prune
test_pred_prune <- subset(test_pred_prune, select = -test_pred_prune)
# write.csv(test_pred_prune, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_Feb24PrunedTree.csv", row.names = F)
## Best Score = .2173

## Bagging
ncol(clean_train)
nrow(clean_train)
Tree_Bagging <- randomForest(Log10SalePrice ~ ., data = clean_train,
                             ntrees = 500, mtry = 52, replace = TRUE,
                             importance = TRUE)
Tree_Bagging
importance(Tree_Bagging)
varImpPlot(Tree_Bagging)

test_pred_bag <- predict(Tree_Bagging, clean_test)
test_pred_bag <- as.data.frame(test_pred_bag)
test_pred_bag$Id <- clean_test$Id
test_pred_bag$SalePrice <- 10^test_pred_bag$test_pred_bag
test_pred_bag <- subset(test_pred_bag, select = -test_pred_bag)
# write.csv(test_pred_bag, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_TreeBaggingRedo.csv", row.names = F)
## Best Score = 0.15352

## Random Forest
# m = p/3
Tree_RF <- randomForest(Log10SalePrice ~ ., data = clean_train,
                             ntrees = 500, mtry = (51/3), replace = TRUE,
                             importance = TRUE)
Tree_RF

importance(Tree_RF)
varImpPlot(Tree_RF)

test_pred_rf <- predict(Tree_RF, clean_test)
test_pred_rf <- as.data.frame(test_pred_rf)
test_pred_rf$Id <- clean_test$Id
test_pred_rf$SalePrice <- 10^test_pred_rf$test_pred_rf
test_pred_rf <- subset(test_pred_rf, select = -test_pred_rf)
# write.csv(test_pred_rf, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_TreeRF.csv", row.names = F)
## Best Score = 0.14714

## Split training set further 
nrow(clean_train)/2
ncol(clean_train)
train <- clean_train[1:730,]
test <- clean_train[731:nrow(clean_train),]
MSE_Test_RF <- NA
y <- 100000000
for (i in 1:51) {
  set.seed(1)
  Tree_RF <- randomForest(Log10SalePrice ~ ., data = train,
                          ntrees = 500, mtry = (i), replace = TRUE,
                          importance = TRUE)
  Test_pred_rf <- predict(Tree_RF, test)
  MSE_Test_RF[i] <- mean((test$Log10SalePrice - Test_pred_rf)^2)
  x <- MSE_Test_RF[i]
  if (x < y) {
    Best <- Tree_RF
    y = x
    m = i
  }
  else {
    Best <- Best
  }
}

Best
p = which.min(MSE_Test_RF)

Tree_RF <- randomForest(Log10SalePrice ~ ., data = clean_train,
                        ntrees = 500, mtry = (p), replace = TRUE,
                        importance = TRUE)
Tree_RF

importance(Tree_RF)
varImpPlot(Tree_RF)

test_pred_rf <- predict(Tree_RF, clean_test)
test_pred_rf <- as.data.frame(test_pred_rf)
test_pred_rf$Id <- clean_test$Id
test_pred_rf$SalePrice <- 10^test_pred_rf$test_pred_rf
test_pred_rf <- subset(test_pred_rf, select = -test_pred_rf)
# write.csv(test_pred_rf, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_TreeRFbest.csv", row.names = F)
## Best Score = 0.14659


## Boosting
Tree_Boosting <- gbm(Log10SalePrice ~., data = clean_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 1, cv.folds = 10,
                     shrinkage = 0.01)

Tree_Boosting
min(Tree_Boosting$cv.error)
which.min(Tree_Boosting$cv.error)


test_pred_boosting <- predict(Tree_Boosting, clean_test)
test_pred_boosting <- as.data.frame(test_pred_boosting)
test_pred_boosting$Id <- clean_test$Id
test_pred_boosting$SalePrice <- 10^test_pred_boosting$test_pred_boosting
test_pred_boosting <- subset(test_pred_boosting, select = -test_pred_boosting)
# write.csv(test_pred_boosting, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_Boosting.csv", row.names = F)
## Best Score = 0.13451

n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)
t = 5000
for(i in 1:6){
  set.seed(1)
  Tree_Boosting <- gbm(Log10SalePrice ~., data = clean_train, distribution = "gaussian",
                       n.trees = 5000, interaction.depth = (i), cv.folds = 10,
                       shrinkage = 0.01)
  
  n_trees[i] <- which.min(Tree_Boosting$cv.error)
  min_cv_error[i] <- min(Tree_Boosting$cv.error)
}


p <- which.min(min_cv_error)
## 3
n <- n_trees[p]
## 3346

Tree_Boosting <- gbm(Log10SalePrice ~., data = clean_train, distribution = "gaussian",
                     n.trees = 3346, interaction.depth = (3),
                     shrinkage = 0.01)
Tree_Boosting
summary(Tree_Boosting)
Tree_Boosting

test_pred_boosting <- predict(Tree_Boosting, clean_test)
test_pred_boosting <- as.data.frame(test_pred_boosting)
test_pred_boosting$Id <- clean_test$Id
test_pred_boosting$SalePrice <- 10^test_pred_boosting$test_pred_boosting
test_pred_boosting <- subset(test_pred_boosting, select = -test_pred_boosting)
# write.csv(test_pred_boosting, "/Users/hannahritchie/Desktop/BAT/Machine Learning/Kaggle//test_pred_BestBoosting.csv", row.names = F)
## Best Score = 0.12993


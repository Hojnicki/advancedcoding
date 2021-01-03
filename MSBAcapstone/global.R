library(keras)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(xlsx)
library(dplyr)
library(tidyr)
library(corrplot)
library(skimr)
library(caret)
library(DMwR)
library(xgboost)


#Data import 
df <- read.xlsx2('data/buad5762-m1-driver-raw-shot-data-with-outlier-flags3.xlsx', sheetIndex = 1, startRow = 1, password = NULL)
df <- df[-c(17707),]

#prep columns
df <- df %>% select(-c(Test_Type,Ball)) #All values in each column are the same therefore removing
df$Tester_ID <- as.factor(df$Tester_ID)
df$Group <- as.factor(df$Group)
df$Club <- as.factor(df$Club)
df$Full_Club_Name <- as.factor(df$Full_Club_Name)
df$Test_Date <- as.factor(df$Test_Date)
df$Test_Time <- substr(df$Test_Time, nchar(df$Test_Time) - 11 + 1, nchar(df$Test_Time)) 
df$Test_Time <- as.factor(df$Test_Time)
df[ , 7:30] <- apply(df[ , 7:30], 2, function(x) as.numeric(as.character(x)))


#correlation plot prep work
res <- cor(df[,7:29])
round(res, 2)

#build data frame with only numeric data
justNum <- df[,7:29]

#vuild data frame with only categorical data
catOnly <- df[,1:6]

#################################################################
## Load models built in modelTrainer.Rmd

ballSpeedFit <- xgb.load('ballSpeedFit')

launchAngleFit <- load_model_hdf5('launchAngleFit.h5')

sideAngleFit <- load_model_hdf5('sideAngleFit.h5')

backspinFit <- load_model_hdf5('backspinFit.h5')

sidespinFit <- xgb.load('sidespinFit')

tiltAngleFit <- xgb.load('tiltAngleFit')

totalSpinFit <- load_model_hdf5('totalSpinFit.h5')

carryYardsFit <- load_model_hdf5('carryYardsFit.h5')

totalYardsFit <- xgb.load('totalYardsFit')

offlineYardsFit <- xgb.load('offlineYardsFit')

descentAngleFit <- load_model_hdf5('descentAngleFit.h5')

peakHeightFit <- xgb.load('peakHeightFit')

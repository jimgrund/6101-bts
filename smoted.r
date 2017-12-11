#' ### Load Necessary Libraries

#install.packages("BSDA")
suppressPackageStartupMessages(library(BSDA))
#install.packages("MASS")
suppressPackageStartupMessages(library(MASS))
#install.packages("Hmisc")
suppressPackageStartupMessages(library(Hmisc))
#install.packages("caret")
suppressPackageStartupMessages(library(caret))
#install.packages("ROCR")
suppressPackageStartupMessages(library(ROCR))
#install.packages("rpart")
suppressPackageStartupMessages(library(rpart))
#install.packages("dplyr")
#suppressPackageStartupMessages(library(dplyr))
#install.packages("Amelia")
suppressPackageStartupMessages(library(Amelia))
#install.packages("openxlsx")
suppressPackageStartupMessages(library(openxlsx))
#install.packages("party")
library(party)

require(DMwR)
require(caret)
require(pROC)
library(caret)
library(pROC)
library(DMwR)
library(doMC)
registerDoMC(cores = 3)


#Remove any objects in the Environment
rm(list = ls())

#Set working directory
setwd("/Users/jimgrund/6101-bts/data/")

data <- data.frame(read.csv("DMV_On_Time_Performance_2015-2017.csv", header = TRUE))
d2 <- subset(data,select = c(Year,
                             Quarter,       #smote this
                             Month,         #smote this
                             DayofMonth,    #smote this
                             DayOfWeek,     #smote this
                             AirlineID,
                             Origin,        #smote this
                             Distance,
                             DepDel15,      #smote this
                             DepartureDelayGroups,
                             DepDelay,
                             Carrier,       #smote this
                             Dest,          #smote this
                             DestAirportID
                             ))
d2 <- subset(d2, Origin == "IAD" | Origin == "BWI" | Origin == "DCA")

#rm(data)

row.has.na <- apply(d2, 1, function(x){any(is.na(x))})
d2 <- d2[!row.has.na,]
rm(row.has.na)

#Knitr global options
library(knitr)
opts_chunk$set(eval = TRUE, echo = TRUE, warning = FALSE,
               tidy = TRUE, results = "hold", cache = TRUE)

#Set the overall seed for reproducibility
set.seed(6101)


#d2$DepDel15 <- as.numeric(d2$DepDel15)

#' ## Partition Data
subsamples <- createDataPartition(y=d2$DepDel15, p=0.7, list=FALSE)
TrainSet <- d2[subsamples, ]
TestSet <- d2[-subsamples, ]
rm(subsamples)
rm(d2)

#hist(TrainSet$DepDel15)

#TrainSet$DepDel15   <- as.factor(TrainSet$DepDel15)
#TrainSet$DayOfWeek  <- as.factor(TrainSet$DayOfWeek)
#TrainSet$DayofMonth <- as.factor(TrainSet$DayofMonth)
#TrainSet$Month      <- as.factor(TrainSet$Month)
#TrainSet$Quarter    <- as.factor(TrainSet$Quarter)
#TrainSet$Carrier    <- as.factor(TrainSet$Carrier)

#FactoredVariable = factor(df$Any)
#dumm = as.data.frame(model.matrix(~TrainSet$DayOfWeek)[,-1])
#dfWithDummies = cbind(TrainSet, dumm)
#str(dfWithDummies)

library(ade4)
library(data.table)
ohe_feats = c('DayOfWeek','DayofMonth','Month','Quarter','Carrier')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(TrainSet[f])
  TrainSet[f] = NULL
  TrainSet = cbind(TrainSet, df_all_dummy)
}
rm(df_all_dummy,ohe_feats)

#str(TrainSet)
#quit()
# $ Year                : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
# $ AirlineID           : int  19805 19805 19805 19805 19805 19805 19805 19805 19805 19805 ...
# $ Origin              : Factor w/ 98 levels "ABQ","ACK","ALB",..: 24 24 24 24 24 24 24 24 24 24 ...
# $ Distance            : int  1192 1192 1192 1192 1192 1192 1192 1192 1192 1192 ...
# $ DepDel15            : int  0 1 0 0 0 1 0 0 0 1 ...
# $ DepartureDelayGroups: int  0 2 -1 -1 0 4 0 0 -1 1 ...
# $ DepDelay            : int  9 30 -4 -5 8 61 3 2 -2 25 ...
# $ Dest                : Factor w/ 99 levels "ABQ","ACK","ALB",..: 26 26 26 26 26 26 26 26 26 26 ...
# $ DestAirportID       : int  11298 11298 11298 11298 11298 11298 11298 11298 11298 11298 ...

TrainSet$DepDel15 <- as.factor(TrainSet$DepDel15)

#newData <- SMOTE(DepDel15 ~ Quarter+Month+DayofMonth+DayOfWeek+Carrier+Origin+Dest, TrainSet, perc.over = 260,perc.under=100, k=7)
newData <- SMOTE(DepDel15 ~ . - Year - AirlineID - Distance - DepartureDelayGroups - DepDelay - DestAirportID, TrainSet, perc.over = 100,perc.under=200, k=7)
write.csv(newData,"smoted-one-hot-test.csv")
prop.table(table(newData$DepDel15))

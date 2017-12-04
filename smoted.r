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
d2 <- subset(data,select = c(Year,Quarter,Month,DayofMonth,DayOfWeek,AirlineID,Origin,Distance,DepDel15))
d2 <- subset(d2, Origin == "IAD" | 
                 Origin == "BWI" |
                 Origin == "DCA")
                 
rm(data)

row.has.na <- apply(d2, 1, function(x){any(is.na(x))})
d2 <- d2[!row.has.na,]
rm(row.has.na)

#Knitr global options
library(knitr)
opts_chunk$set(eval = TRUE, echo = TRUE, warning = FALSE, 
               tidy = TRUE, results = "hold", cache = TRUE)

#Set the overall seed for reproducibility
set.seed(6101)

#' ## Partition Data
subsamples <- createDataPartition(y=d2$DepDel15, p=0.3, list=FALSE)
TrainSet <- d2[subsamples, ] 
TestSet <- d2[-subsamples, ]
rm(subsamples)

hist(TrainSet$DepDel15)

# look at the data prior to smote
TrainSet$DepDel15 <- as.factor(TrainSet$DepDel15)
table(TrainSet$DepDel15)

newData <- SMOTE(DepDel15 ~ ., TrainSet, perc.over = 400,perc.under=100, k=3)

# convert DepDel15 back to numeric
# unsure why, but smote appears to be converting 0 and 1 to 1 and 2 for DepDel15 /shrug
newData$DepDel15 <- as.numeric(newData$DepDel15)
newData$DepDel15 <- (newData$DepDel15-1)

# look at data after smote
hist(newData$DepDel15)
table(newData$DepDel15)


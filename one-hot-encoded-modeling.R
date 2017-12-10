#' ---
#' title: 6101 Project 2 b
#' author: TeamBestTeam
#' date: 31Oct17
#' output:
#'    html_document:
#'      toc: true
#'      highlight: haddock
#' ---
########################################
#http://rpubs.com/jassalak/TeamBestTeam_Proj2
########################################
#' ## Environment Preparation
#Remove any objects in the Environment
rm(list = ls())

#Set working directory
#setwd("C:/Users/akash/Desktop/GWU/6101_DataScience_RShah/Proj_2")
setwd("/Users/jimgrund/6101-bts/data/")


#Knitr global options
library(knitr)
opts_chunk$set(eval = TRUE, echo = TRUE, warning = FALSE,
               tidy = TRUE, results = "hold", cache = TRUE)

#Set the overall seed for reproducibility
set.seed(6101)

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


########################################
#' Load Original Data to grab the Test Data
Original_df <- data.frame(read.csv("DMV_On_Time_Performance_2015-2017.csv", header = TRUE))
Purged_Original_df <- subset(Original_df,select = c(Year,
                             Quarter,
                             Month,
                             DayofMonth,
                             DayOfWeek,
                             AirlineID,
                             Origin,
                             Distance,
                             DepDel15,
                             DepartureDelayGroups,
                             DepDelay,
                             Carrier,
                             Dest,
                             DestAirportID
                             ))
Purged_Original_df <- subset(Purged_Original_df, Origin == "IAD" | Origin == "BWI" | Origin == "DCA")

row.has.na <- apply(Purged_Original_df, 1, function(x){any(is.na(x))})
Purged_Original_df <- Purged_Original_df[!row.has.na,]
rm(row.has.na)

# compute baseline
sum(Purged_Original_df$DepDel15)/length(Purged_Original_df$DepDel15) *100

#Knitr global options
library(knitr)
opts_chunk$set(eval = TRUE, echo = TRUE, warning = FALSE,
               tidy = TRUE, results = "hold", cache = TRUE)

#Set the overall seed for reproducibility
set.seed(6101)

#' ## Partition Data
subsamples <- createDataPartition(y=Purged_Original_df$DepDel15, p=0.7, list=FALSE)
# we're getting the TrainSet from the smoted set being loaded manually
#TrainSet <- Purged_Original_df[subsamples, ]
TestSet <- Purged_Original_df[-subsamples, ]
rm(subsamples)

########################################
#' one-hot-encode the TestSet
library(ade4)
library(data.table)
ohe_feats = c('DayOfWeek','DayofMonth','Month','Quarter','Carrier')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(TestSet[f])
  TestSet[f] = NULL
  TestSet = cbind(TestSet, df_all_dummy)
}
rm(ohe_feats,df_all_dummy)

# destroy the large dataset as it's no longer needed
rm(Purged_Original_df)

########################################
#' Load Smoted Data .. this will be the training data
d3 <- data.frame(read.csv("smoted-one-hot-40-60.csv", header = TRUE))

#View(d3)
#' Create Random Sample
d3_rand15 <- d3[sample(nrow(d3),15),]

########################################

#' ### Convert Variables to Factor
#cols <- c("Year","Quarter","Month","DayofMonth","DayofWeek","AirlineID", "Origin")
#d3[cols] <- lapply(d3[cols], factor)
##+ eval = FALSE, echo = TRUE
#sapply(d3,class)

########################################

#' Histogram of DV
#hist(d3$DepDel15)
#' ## Baseline Model
#(length(d3$DepDelay15[d3$DepDelay15>=1]) / nrow(d3) * 100 )
#What is the percentage the plane will be delayed without a model (at random)

########################################

#' ## Partition Data
#subsamples <- createDataPartition(y=d3$DepDel15, p=0.7, list=FALSE)
#TrainSet <- d3[subsamples, ]
TrainSet <- d3
#TestSet <- d3[-subsamples, ]
#rm(subsamples)

########################################
#' ## Model 5 (Logistic Regression)

#sum(d3$DepDel15)/length(d3$DepDel15) *100

# remove ROA from testset since not in train set
#TestSet<-TestSet[!(TestSet$Dest=="ROA"),]

#' MODEL
#m5 <- glm(DepDel15 ~ Quarter+Month+DayofMonth+DayOfWeek+Carrier+Origin+Dest,data=TrainSet)
m5 <- glm(DepDel15 ~ . - AirlineID - Distance - DepartureDelayGroups - DepDelay - DestAirportID, family=binomial(link = "logit"), data=TrainSet)
summary(m5)

#' ### Using anova() for feature importance
anova(m5, test="Chisq")

#' ### Prediction
p5 <- predict(m5, newdata = TestSet,type = 'response')
p5 <- ifelse(p5 > 0.5,1,0)
str(p5)

#' ### Confusion Matrix to Check Accuracy
confusionMatrix(data=p5, reference = TestSet$DepDel15)

#' ### ROC and AUC
response_predict <- predict(m5, newdata=TestSet, type = "response")
link_predict <- predict(m5, newdata=TestSet, type = "link")
qplot(x=response_predict, geom="histogram")
predictions <- predict(m5, newdata=TestSet, type="response")
ROCRpred <- prediction(response_predict, TestSet$DepDel15)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

#plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7),print.cutoffs.at = seq(0,1,0.1))

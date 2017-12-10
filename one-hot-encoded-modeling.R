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
#install.packages('gputools', repos = 'http://cran.rstudio.com/')
#library(gputools)
#install.packages('speedglm', repos = 'http://cran.rstudio.com/')
require(speedglm)


########################################
#' Load Original Data to grab the Test Data
#Original_df <- data.frame(read.csv("DMV_On_Time_Performance_2015-2017.csv", header = TRUE))
#Purged_Original_df <- subset(Original_df,select = c(Year,
#                             Quarter,
#                             Month,
#                             DayofMonth,
#                             DayOfWeek,
#                             AirlineID,
#                             Origin,
#                             Distance,
#                             DepDel15,
#                             DepartureDelayGroups,
#                             DepDelay,
#                             Carrier,
#                             Dest,
#                             DestAirportID
#                             ))
#Purged_Original_df <- subset(Purged_Original_df, Origin == "IAD" | Origin == "BWI" | Origin == "DCA")
#
#row.has.na <- apply(Purged_Original_df, 1, function(x){any(is.na(x))})
#Purged_Original_df <- Purged_Original_df[!row.has.na,]
#rm(row.has.na)
#
## compute baseline
#sum(Purged_Original_df$DepDel15)/length(Purged_Original_df$DepDel15) *100
#
##Knitr global options
#library(knitr)
#opts_chunk$set(eval = TRUE, echo = TRUE, warning = FALSE,
#               tidy = TRUE, results = "hold", cache = TRUE)
#
##Set the overall seed for reproducibility
#set.seed(6101)
#
##' ## Partition Data
#subsamples <- createDataPartition(y=Purged_Original_df$DepDel15, p=0.7, list=FALSE)
## we're getting the TrainSet from the smoted set being loaded manually
##TrainSet <- Purged_Original_df[subsamples, ]
#TestSet <- Purged_Original_df[-subsamples, ]
#rm(subsamples)
##write.csv(TestSet,"TestSet.csv")
#
########################################
#' one-hot-encode the TestSet
#library(ade4)
#library(data.table)
#ohe_feats = c('DayOfWeek','DayofMonth','Month','Quarter','Carrier')
#for (f in ohe_feats){
#  df_all_dummy = acm.disjonctif(TestSet[f])
#  TestSet[f] = NULL
#  TestSet = cbind(TestSet, df_all_dummy)
#}
#rm(ohe_feats,df_all_dummy)
#write.csv(TestSet,"TestSet-OneHot.csv")

# destroy the large dataset as it's no longer needed
#rm(Purged_Original_df)

########################################
#' Load Test Data
TestSet <- data.frame(read.csv("TestSet-OneHot.csv", header = TRUE))

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


row.has.na <- apply(TrainSet, 1, function(x){any(is.na(x))})
TrainSet <- TrainSet[!row.has.na,]
rm(row.has.na)

#' MODEL
#m5 <- glm(DepDel15 ~ Quarter+Month+DayofMonth+DayOfWeek+Carrier+Origin+Dest,data=TrainSet)
m5 <- glm(DepDel15 ~ . - AirlineID - Distance - DepartureDelayGroups - DepDelay - DestAirportID, family=binomial(link = "logit"), data=TrainSet[1:250000,])
summary(m5)

#' ### Using anova() for feature importance
#anova(m5, test="Chisq")

# remove airports from testset since not in train set
# BTR, EGE, HPN, JAC, MSN, ROA, TTN, TUL
#TestSet<-TestSet[!(TestSet$Dest=="BTR"),]
#TestSet<-TestSet[!(TestSet$Dest=="EGE"),]
#TestSet<-TestSet[!(TestSet$Dest=="HPN"),]
#TestSet<-TestSet[!(TestSet$Dest=="JAC"),]
#TestSet<-TestSet[!(TestSet$Dest=="MSN"),]
#TestSet<-TestSet[!(TestSet$Dest=="ROA"),]
TestSet<-TestSet[!(TestSet$Dest=="TTN"),]
#TestSet<-TestSet[!(TestSet$Dest=="TUL"),]

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

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7),print.cutoffs.at = seq(0,1,0.1))


################################
#' ### Custom Prediction
options(warn=-1)
rm(customQuery)

#' construct the dataframe structure for the custom query
customQuery <- data.frame(matrix(ncol = 77, nrow = 0))
cols <- c("X", "Year", "AirlineID", "Origin", "Distance", "DepDel15", "DepartureDelayGroups", "DepDelay", "Dest", "DestAirportID", "DayOfWeek.1", "DayOfWeek.2", "DayOfWeek.3", "DayOfWeek.4", "DayOfWeek.5", "DayOfWeek.6", "DayOfWeek.7", "DayofMonth.1", "DayofMonth.2", "DayofMonth.3", "DayofMonth.4", "DayofMonth.5", "DayofMonth.6", "DayofMonth.7", "DayofMonth.8", "DayofMonth.9", "DayofMonth.10", "DayofMonth.11", "DayofMonth.12", "DayofMonth.13", "DayofMonth.14", "DayofMonth.15", "DayofMonth.16", "DayofMonth.17", "DayofMonth.18", "DayofMonth.19", "DayofMonth.20", "DayofMonth.21", "DayofMonth.22", "DayofMonth.23", "DayofMonth.24", "DayofMonth.25", "DayofMonth.26", "DayofMonth.27", "DayofMonth.28", "DayofMonth.29", "DayofMonth.30", "DayofMonth.31", "Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Quarter.1", "Quarter.2", "Quarter.3", "Quarter.4", "Carrier.AA", "Carrier.AS", "Carrier.B6", "Carrier.DL", "Carrier.EV", "Carrier.F9", "Carrier.MQ", "Carrier.NK", "Carrier.OO", "Carrier.UA", "Carrier.US", "Carrier.VX", "Carrier.WN")
colnames(customQuery) <- cols

#' initialize the customQuery df with zeros as base
customQuery <- data.frame(X=0,
                         Year=0,
                         AirlineID=0,
                         Origin="0",
                         Distance=0,
                         DepDel15=0,
                         DepartureDelayGroups=0,
                         DepDelay=0,
                         Dest="0",
                         DestAirportID=0,
                         DayOfWeek.1=0,
                         DayOfWeek.2=0,
                         DayOfWeek.3=0,
                         DayOfWeek.4=0,
                         DayOfWeek.5=0,
                         DayOfWeek.6=0,
                         DayOfWeek.7=0,
                         DayofMonth.1=0,
                         DayofMonth.2=0,
                         DayofMonth.3=0,
                         DayofMonth.4=0,
                         DayofMonth.5=0,
                         DayofMonth.6=0,
                         DayofMonth.7=0,
                         DayofMonth.8=0,
                         DayofMonth.9=0,
                         DayofMonth.10=0,
                         DayofMonth.11=0,
                         DayofMonth.12=0,
                         DayofMonth.13=0,
                         DayofMonth.14=0,
                         DayofMonth.15=0,
                         DayofMonth.16=0,
                         DayofMonth.17=0,
                         DayofMonth.18=0,
                         DayofMonth.19=0,
                         DayofMonth.20=0,
                         DayofMonth.21=0,
                         DayofMonth.22=0,
                         DayofMonth.23=0,
                         DayofMonth.24=0,
                         DayofMonth.25=0,
                         DayofMonth.26=0,
                         DayofMonth.27=0,
                         DayofMonth.28=0,
                         DayofMonth.29=0,
                         DayofMonth.30=0,
                         DayofMonth.31=0,
                         Month.1=0,
                         Month.2=0,
                         Month.3=0,
                         Month.4=0,
                         Month.5=0,
                         Month.6=0,
                         Month.7=0,
                         Month.8=0,
                         Month.9=0,
                         Month.10=0,
                         Month.11=0,
                         Month.12=0,
                         Quarter.1=0,
                         Quarter.2=0,
                         Quarter.3=0,
                         Quarter.4=0,
                         Carrier.AA=0,
                         Carrier.AS=0,
                         Carrier.B6=0,
                         Carrier.DL=0,
                         Carrier.EV=0,
                         Carrier.F9=0,
                         Carrier.MQ=0,
                         Carrier.NK=0,
                         Carrier.OO=0,
                         Carrier.UA=0,
                         Carrier.US=0,
                         Carrier.VX=0,
                         Carrier.WN=0
                         )

##########
#' populate your custom query attributes here
customQuery$X=1
customQuery$Year=2000
customQuery$DepartureDelayGroups=1
customQuery$DepDelay=1
customQuery$AirlineID=19805
customQuery$Origin="IAD"
customQuery$Distance=283
customQuery$Dest="DFW"
customQuery$DestAirportID=11298
customQuery$DayOfWeek.1=1
customQuery$DayofMonth.3=1
customQuery$Month.2=1
customQuery$Quarter.1=1
customQuery$Carrier.AA=1


#' perform the custom query prediction
round(predict(m5,newdata=customQuery,type = "response"))

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

#newData <- SMOTE(DepDel15 ~ . - Year - AirlineID - Distance - DepartureDelayGroups - DepDelay - DestAirportID, TrainSet, perc.over = 100,perc.under=200, k=7)
#write.csv(newData,"smoted-one-hot-test.csv")
#prop.table(table(newData$DepDel15))




##################################
##################################
# model starts here
##################################
##################################


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
#suppressPackageStartupMessages(library(Amelia))
#install.packages("openxlsx")
suppressPackageStartupMessages(library(openxlsx))
#install.packages("party")
#library(party)
library(pROC)




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

########################################
#' Load Test Data
TestSet <- data.frame(read.csv("TestSet-OneHot.csv", header = TRUE))

########################################
#' Load Smoted Data .. this will be the training data
d3 <- data.frame(read.csv("smoted-one-hot-test.csv", header = TRUE))

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

#+ echo = FALSE, fig.width=4, fig.height=4, dpi=100


########################################

#' ## Partition Data
#subsamples <- createDataPartition(y=d3$DepDel15, p=0.7, list=FALSE)
#TrainSet <- d3[subsamples, ]
TrainSet <- d3
#TestSet <- d3[-subsamples, ]
#rm(subsamples)

########################################
#' ## Model 5 (Logistic Regression)
# join the smoted data with the test test to create a complete dataset 
# for running the baseline model against
finalModel<-rbind(d3,TestSet)
sum(finalModel$DepDel15)/length(finalModel$DepDel15) *100


row.has.na <- apply(TrainSet, 1, function(x){any(is.na(x))})
TrainSet <- TrainSet[!row.has.na,]
rm(row.has.na)

#####################################
#' EDA diagrams
colors = rainbow(length(unique(d3)))

#' Histogram of DV
# Delay Flag Frequency after SMOTE
hist(finalModel$DepDel15,xlab="Delay Flag", breaks=10, main="Histogram of Delay Flag Frequency (Final Model Data)", col = c("Purple3","Pink3"), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


barchart(finalModel$Origin,ylab="Name of Airport", main="Barchart of Airport Name Frequency", col = rainbow(3), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#bartable <-- table(finalModel$DepDel15, finalModel$Origin)
#barplot(bartable,ylab="Name of Airport", main="Stacked barchart of Airport Name Frequency vs DepDelay15",
#        col = c("Green4","Blue4"), legend = rownames(bartable), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


#barchart(finalModel$DepDel15,finalModel$DayOfWeek.1)
#Monday <- finalModel$DayOfWeek.1[finalModel$DayOfWeek.1==1 & finalModel$DepDel15==1]
#Tuesday <- finalModel$DayOfWeek.2[finalModel$DayOfWeek.2==1 & finalModel$DepDel15==1]
#Wednesday <- finalModel$DayOfWeek.3[finalModel$DayOfWeek.3==1 & finalModel$DepDel15==1]
#DaysOfWeek <- data.frame(length(Monday), length(Tuesday), length(Wednesday))
#rm(barftable)
#barftable <-- table(Monday)
#barplot(barftable,ylab="Name of Airport", main="delay vs day of week",
#        col = c("Green4","Blue4"), legend = rownames(bartable), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
#plot(DaysOfWeek)




#' MODEL
# remove AirlineID, Distance, DepartureDelayGroups, DepDelay, DestAirportID from the model
# all other fields from the TrainSet are fair game for use in the model
m5 <- glm(DepDel15 ~ . - Year - Quarter.1 - Quarter.2 - Quarter.3 - Quarter.4 - AirlineID - Distance - DepartureDelayGroups - DepDelay - DestAirportID, family=binomial(link = "logit"), data=TrainSet)
summary(m5)

#' ### Using anova() for feature importance
#anova(m5, test="Chisq")

# remove TTN and MSN from TestSet since not found in TrainSet
TestSet<-TestSet[!(TestSet$Dest=="TTN"),]
TestSet<-TestSet[!(TestSet$Dest=="MSN"),]

#' ### Prediction
p5 <- predict(m5, newdata = TestSet,type = 'response')
p5 <- ifelse(p5 > 0.5,1,0)
str(p5)

#' ### Confusion Matrix to Check Accuracy
confusionMatrix(data=p5, reference = TestSet$DepDel15)

#' ### ROC and AUC
response_predict <- predict(m5, newdata=TestSet, type = "response")
link_predict <- predict(m5, newdata=TestSet, type = "link")
terms_predict <- predict(m5, newdata=TestSet, type = "terms")
qplot(x=response_predict, geom="histogram")
qplot(x=link_predict, geom="histogram")
qplot(x=terms_predict, geom="histogram")
predictions <- predict(m5, newdata=TestSet, type="response")

#' ROC prediction
ROCRpred <- prediction(response_predict, TestSet$DepDel15)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7),print.cutoffs.at = seq(0,1,0.1), main="Receiver Operator Characteristic Curve", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


################################
#' ### Custom Prediction
options(warn=-1)
rm(customQuery)
customQuery <- data.frame(matrix(ncol = 77, nrow = 0))
cols <- c("X", "Year", "AirlineID", "Origin", "Distance", "DepDel15", "DepartureDelayGroups", "DepDelay", "Dest", "DestAirportID", "DayOfWeek.1", "DayOfWeek.2", "DayOfWeek.3", "DayOfWeek.4", "DayOfWeek.5", "DayOfWeek.6", "DayOfWeek.7", "DayofMonth.1", "DayofMonth.2", "DayofMonth.3", "DayofMonth.4", "DayofMonth.5", "DayofMonth.6", "DayofMonth.7", "DayofMonth.8", "DayofMonth.9", "DayofMonth.10", "DayofMonth.11", "DayofMonth.12", "DayofMonth.13", "DayofMonth.14", "DayofMonth.15", "DayofMonth.16", "DayofMonth.17", "DayofMonth.18", "DayofMonth.19", "DayofMonth.20", "DayofMonth.21", "DayofMonth.22", "DayofMonth.23", "DayofMonth.24", "DayofMonth.25", "DayofMonth.26", "DayofMonth.27", "DayofMonth.28", "DayofMonth.29", "DayofMonth.30", "DayofMonth.31", "Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Quarter.1", "Quarter.2", "Quarter.3", "Quarter.4", "Carrier.AA", "Carrier.AS", "Carrier.B6", "Carrier.DL", "Carrier.EV", "Carrier.F9", "Carrier.MQ", "Carrier.NK", "Carrier.OO", "Carrier.UA", "Carrier.US", "Carrier.VX", "Carrier.WN")
colnames(customQuery) <- cols

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
customQuery$Origin="IAD"
#customQuery$Distance=283
customQuery$Dest="JFK"
#customQuery$DestAirportID=11298
customQuery$DayOfWeek.1=1
customQuery$DayofMonth.11=1
customQuery$Month.12=1
customQuery$Carrier.DL=1



predict(m5,newdata=customQuery,type = "response")





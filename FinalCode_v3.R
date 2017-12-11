#' ---
#' title: 6101 Project2 Final
#' author: TeamBestTeam (A.Aiuto, J.Imgrund, A.Jassal)
#' date: 11Dec2017
#' output:
#'    html_document:
#'      toc: true
#'      highlight: haddock
#' ---
#'#######################################
#' **Will Your Flight Be Delayed?**
#http://rpubs.com/jassalak/FinalProject_6101
#'#######################################
#'
#' ## Environment Preparation
# Remove any objects in the Environment
rm(list = ls())

# Set working directory
setwd("C:/Users/akash/Desktop/GWU/6101_DataScience_RShah/Proj_2")
#setwd("/Users/jimgrund/6101-bts/data/")


# Knitr global options
library(knitr)
opts_chunk$set(eval = TRUE, echo = TRUE, warning = FALSE,
               tidy = TRUE, results = "hold", cache = TRUE)

# Set the overall seed for reproducibility
set.seed(6101)

#'#######################################
#'
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
#install.packages("Amelia")
suppressPackageStartupMessages(library(Amelia))
#install.packages("openxlsx")
suppressPackageStartupMessages(library(openxlsx))
#install.packages("pROC")
suppressPackageStartupMessages(library(pROC))

#'#######################################
#'
#' ### Data Load Part-1
# Read in data to be filtered down to only DCA, IAD, BWI airports

data <- data.frame(read.csv("DMV_On_Time_Performance_2015-2017.csv", header = TRUE))
d2.0 <- subset(data,select = c(Year,
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
                             DestAirportID ))

# Parse this down to only where DCA, IAD, BWI are the origin airports to make a more manageable dataset
d2.1 <- subset(d2.0, Origin == "IAD" | Origin == "BWI" | Origin == "DCA")

# Write this out to a new CSV
write.csv(d2.1,"DMV-origin_On_Time_Performance_2015-2017.csv")

# Remove the old dataframes to save on memory consumption
rm(data,d2.0,d2.1)

# Read in the DVM origin data into a data frame for processing
d2 <- data.frame(read.csv("DMV-origin_On_Time_Performance_2015-2017.csv", header = TRUE))

#'#######################################
#'
#' ## Data Tidy
#'
# Remove any nulls and na data from the dataframe
row.has.na <- apply(d2, 1, function(x){any(is.na(x))})
d2 <- d2[!row.has.na,]
rm(row.has.na)
d2 <- d2[!apply(is.na(d2) | d2 == "", 1, all),]

# Perform garbage collection, to help processing speeds
gc()
#'
#'#######################################
#'
#' ## Exploratory Data Analysis
#' 
# Create graphics highlighting relationship between Dependent Variable (DepDel15) and Independent Variables 

colors = rainbow(length(unique(d2)))
barchart(d2$Origin, ylab="Name of Airport", 
         main="Barchart of Airport Name Frequency (pre-smote)", 
         col=rainbow(3), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

bartable <- table(d2$DepDel15, d2$Origin)
barplot(bartable,xlab="Name of Airport", ylab="Frequency", 
        main="Stacked barchart of Airport Name Frequency vs DepDelay15 (pre-smote)",
        col = c("Green4","Blue4"), legend = rownames(bartable), cex.lab=1.5,
        cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

bartable <- table(d2$DepDel15, d2$DayOfWeek)
barplot(bartable,xlab="Day Of Week", ylab="Frequency", 
        main="Day Of Week vs DepDelay15 (pre-smote)", col = c("Green2","Blue2"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

bartable <- table(d2$DepDel15, d2$Month)
barplot(bartable,xlab="Month", ylab="Frequency", 
        main="Month vs DepDelay15 (pre-smote)", col = c("Green2","Blue2"), 
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

bartable <- table(d2$DepDel15, d2$Carrier)
barplot(bartable,xlab="Carrier", ylab="Frequency", 
        main="Carrier vs DepDelay15 (pre-smote)", col = c("Green3","Blue3"),  
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

bartable <- table(d2$DepDel15, d2$DayofMonth)
barplot(bartable,xlab="Day of Month", ylab="Frequency", 
        main="Day of Month vs DepDelay15 (pre-smote)", col = c("Green2","Blue2"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

hist(d2$DepDel15,xlab="Delay Flag", breaks=10, 
     main="Histogram of Delay Flag Frequency",col = c("Gold3","Brown1"), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#'#######################################
#'
#' ## Partition Data
# Create a 70% training set, 30% test set
subsamples <- createDataPartition(y=d2$DepDel15, p=0.7, list=FALSE)
TrainSet <- d2[subsamples, ]
TestSet <- d2[-subsamples, ]
rm(subsamples)
rm(d2)

#'#######################################
#'
#' ##  One-Hot-Encode
# One-Hot-Encode the training set
# 
#library(ade4)
# 
#library(data.table)
#
#!ohe_feats = c('DayOfWeek','DayofMonth','Month','Quarter','Carrier')
#!for (f in ohe_feats){
#!  df_all_dummy = acm.disjonctif(TrainSet[f])
#!  TrainSet[f] = NULL
#!  TrainSet = cbind(TrainSet, df_all_dummy)
#}
#rm(df_all_dummy,ohe_feats)

#'#######################################
#'
#' ## SMOTE 
# SMOTE prefers the dependent variable to be a factor
TrainSet$DepDel15 <- as.factor(TrainSet$DepDel15)

# Use Smote to increase the minority to be better balanced with the majority.
# Use only the key independent variables that we care about 
# In execution, variables not used were removed (notice the usage of the minus sign)

#!newData <- SMOTE(DepDel15 ~ . - Year - AirlineID - Distance - DepartureDelayGroups - DepDelay - DestAirportID, TrainSet, perc.over = 100,perc.under=200, k=7)

#! write the smoked data out to disk so we can read it in later without having to re-run smote
#! write.csv(newData,"smoted-one-hot-trainset.csv")
#! prop.table(table(newData$DepDel15))

#'#######################################
#' ## Model Environment Preparation
# All Training Data has been One-Hot-Encoded and SMOTE'd. 

# Remove any objects in the Environment
rm(list = ls())

#' ### Data Load Part-2
#' 
# Load Training Data 
d3 <- data.frame(read.csv("smoted-one-hot-trainset.csv", header = TRUE))
TrainSet <- d3

# Load Test Data
TestSet <- data.frame(read.csv("TestSet-OneHot.csv", header = TRUE))

#'### Model Dataset
# Join the SMOTE dataset (TrainingSet) with the TestSet to create model dataset
finalModel<-rbind(d3,TestSet)

#' ### Create Random Sample
rand15 <- finalModel[sample(nrow(finalModel),15),]

#'#######################################

#' ## Baseline Model
sum(finalModel$DepDel15)/length(finalModel$DepDel15) *100

#'####################################
#' ## Exploratory Data Analysis 
colors = rainbow(length(unique(finalModel)))

# Airport frequency
barchart(finalModel$Origin, ylab="Name of Airport", 
         main="Barchart of Airport Name Frequency (post-smote)", col=rainbow(3),
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

# Airport on-time and delay stacked barchart
bartable <- table(finalModel$DepDel15, finalModel$Origin)
barplot(bartable,xlab="Name of Airport", ylab="Frequency",
        main="Stacked barchart of Airport Name Frequency vs DepDelay15 (post-smote)",
        col = c("Green4","Blue4"), legend = rownames(bartable), cex.lab=1.5, cex.axis=1.5, cex.main=1.0, cex.sub=1.5)

# Histogram of DV
# 
# Delay Flag Frequency after SMOTE
hist(finalModel$DepDel15,xlab="Delay Flag", breaks=10, main="Histogram of Delay Flag Frequency (Final Model Data)", col = c("Purple3","Pink3"), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#'####################################
#' ## Model -- Logistic Regression

#' ### ANOVA
# Using anova() for feature importance
#anova(m5, test="Chisq")

#' ### GLM
# Remove Year, Quarter AirlineID, Distance, DepartureDelayGroups, DepDelay, 
# DestAirportID from the model because those will not be used in Model
m5 <- glm(DepDel15 ~ . - Year - Quarter.1 - Quarter.2 - Quarter.3 - Quarter.4 -
            AirlineID - Distance - DepartureDelayGroups - 
            DepDelay - DestAirportID, family=binomial(link = "logit"), data=TrainSet)
summary(m5)

# Remove TTN and MSN from TestSet since not found in TrainSet, if not removed, the prediction will not work
TestSet<-TestSet[!(TestSet$Dest=="TTN"),]
TestSet<-TestSet[!(TestSet$Dest=="MSN"),]

#' ### Prediction
p5 <- predict(m5, newdata = TestSet,type = 'response')
p5 <- ifelse(p5 > 0.5,1,0)
str(p5)

#' ### Confusion Matrix to Check Accuracy
confusionMatrix(data=p5, reference = TestSet$DepDel15)

#' ### ROC
response_predict <- predict(m5, newdata=TestSet, type = "response")
link_predict <- predict(m5, newdata=TestSet, type = "link")
terms_predict <- predict(m5, newdata=TestSet, type = "terms")
#qplot(x=response_predict, geom="histogram")
#qplot(x=link_predict, geom="histogram")
#qplot(x=terms_predict, geom="histogram")
predictions <- predict(m5, newdata=TestSet, type="response")

#' ### ROC prediction
ROCRpred <- prediction(response_predict, TestSet$DepDel15)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7),print.cutoffs.at = seq(0,1,0.1), main="Receiver Operator Characteristic Curve", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#' ### Custom Prediction
options(warn=-1)

# Create a customQuery dataframe which defaults to zero for one-hot-encoded IVs

customQuery <- data.frame(matrix(ncol = 77, nrow = 0))
cols <- c("X", "Year", "AirlineID", "Origin", "Distance", "DepDel15", "DepartureDelayGroups", "DepDelay", "Dest", "DestAirportID", "DayOfWeek.1", "DayOfWeek.2", "DayOfWeek.3", "DayOfWeek.4", "DayOfWeek.5", "DayOfWeek.6", "DayOfWeek.7", "DayofMonth.1", "DayofMonth.2", "DayofMonth.3", "DayofMonth.4", "DayofMonth.5", "DayofMonth.6", "DayofMonth.7", "DayofMonth.8", "DayofMonth.9", "DayofMonth.10", "DayofMonth.11", "DayofMonth.12", "DayofMonth.13", "DayofMonth.14", "DayofMonth.15", "DayofMonth.16", "DayofMonth.17", "DayofMonth.18", "DayofMonth.19", "DayofMonth.20", "DayofMonth.21", "DayofMonth.22", "DayofMonth.23", "DayofMonth.24", "DayofMonth.25", "DayofMonth.26", "DayofMonth.27", "DayofMonth.28", "DayofMonth.29", "DayofMonth.30", "DayofMonth.31", "Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Quarter.1", "Quarter.2", "Quarter.3", "Quarter.4", "Carrier.AA", "Carrier.AS", "Carrier.B6", "Carrier.DL", "Carrier.EV", "Carrier.F9", "Carrier.MQ", "Carrier.NK", "Carrier.OO", "Carrier.UA", "Carrier.US", "Carrier.VX", "Carrier.WN")
colnames(customQuery) <- cols

customQuery <- data.frame(X=0, Year=0, AirlineID=0, Origin="0", Distance=0,
                          DepDel15=0,DepartureDelayGroups=0,DepDelay=0,Dest="0",
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
                          Carrier.WN=0)

#' #### customQuery attributes
customQuery$Origin="IAD"
# customQuery$Distance=283
customQuery$Dest="JFK"
#customQuery$DestAirportID=11298
customQuery$DayOfWeek.1=1
customQuery$DayofMonth.11=1
customQuery$Month.12=1
customQuery$Carrier.DL=1

#' ### Prediction Output
predict(m5,newdata=customQuery,type = "response")
#' ---
#' title: 6101 Project 2
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
setwd("C:/Users/akash/Desktop/GWU/6101_DataScience_RShah/Proj_2")

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
#' ###  Data Load # Load the dataset from the working directory 
data_all<- data.frame(read.csv("DMV_On_Time_Performance_2015-2017.csv",
header = TRUE))

#' ## Preprocess Data
#' ### Global Filter Data
 d1 <- subset(data_all,Year == "2015" | Year =="2016" | Year== "2017" & Origin =="IAD" | Origin=="BWI" | Origin =="DCA") 
 #!! d1 <-subset(data_all, Year == "2015" |Year =="2016" | Year == "2017")

#' ### Missing Values Clean-up #' #'
#### Check for missing data #+ eval = FALSE, echo = TRUE colSums(is.na(d1))

#' #### Remove Col if all values are NA
#'  d1[sapply(d1, function(x)all(is.na(x)))] <- NULL 
#+ eval = FALSE, echo = TRUE colSums(is.na(d1))

#' #### Remove Col if 80% of Rows are NA # Works for only Numerical 
d1 <-d1[,colSums(is.na(d1)) < (nrow(d1)*0.80)]
#+ eval = FALSE,echo = TRUE 
colSums(is.na(d1))

#' #### Check for Empty Data 
#+ eval = FALSE, echo = TRUE colSums(d1=='')

#' #### Remove Cols if high values of Blanks
d1 <- subset(d1,select = -c(TailNum, CancellationCode, Div1Airport, Div1TailNum,Div2Airport, Div2TailNum))
#+ eval = FALSE, echo = TRUE
colSums(d1=='')

#' #### Check number of uniquesvalues for each of the column 
#+ eval = FALSE, echo = TRUE
sapply(d1,function(x) length(unique(x)))

#' #### Remove Cols if Only OneUn ique Value Exists 
d1 <- subset(d1,select = -c(Flights))

#' #### Remove Rows if NA exists 
d1 <- d1[complete.cases(d1), ] 
#+ eval = FALSE, echo = TRUE colSums(is.na(d1))

#' #### Visulize Missing Data
#missmap(d1, main = "Missing vs Completed Values")
#^^ Takes too longto run on my comp

#' ### Top n #d1_top20 <- d1[1:20,]
d1_rand10 <- d1[sample(nrow(d1),10),]

#' #### Clean up Environment
rm(data_all)

########################################
d2 <- data.frame(read.csv("OriginAirport_data.csv", header = TRUE))

########################################
#' ## Create Model DataFrame
#!! d2 <- subset(d1,select = c(DepartureDelayGroups,DepDelay,Year,Quarter,Month,DayofMonth,DayOfWeek,AirlineID,OriginAirportID,Origin,DestAirportID,Dest))
d2_rand10 <- d2[sample(nrow(d2),10),]

#' ### Convert Variables to Factor
#+ eval = FALSE, echo = TRUE
str(d2)
#!! d2$DepDel15 <- factor(d2$DepDel15)
#!! levels(d2$DepDel15)
#!! cols <- c("AirlineID","OriginAirportID","DestAirportID")
cols <- c("Year","Quarter","Month","DayofMonth","DayOfWeek","AirlineID","OriginAirportID", "Origin", "DestAirportID", "Dest")
d2[cols] <- lapply(d2[cols], factor)
#+ eval = FALSE, echo = TRUE
sapply(d2,class)

#' ## Partition Data
subsamples <- createDataPartition(y=d2$DepartureDelayGroups, p=0.7, list=FALSE)
TrainSet <- d2[subsamples, ] 
TestSet <- d2[-subsamples, ]
rm(subsamples)

hist(d2$DepartureDelayGroups)
hist(d2$DepDelay)

#' ## Baseline Model 
(length(d2$DepartureDelayGroups[d2$DepartureDelayGroups>1]) / nrow(d2) * 100 )
#What is the percentage the plane will be delayed without a model (at random)
########################################
#' #' ## Export Data
#' write.csv(d2,file = "OriginAirport_data.csv")
#' #write.xlsx(d2,'OrginAirport_data.xlsx')
#' #Datafile to use for next time
########################################

########################################

#https://analyticsdefined.com/implementing-logistic-regression-using-titanic-dataset-r/
#https://stats.idre.ucla.edu/r/dae/logit-regression/
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

#' Target Variables -->
# DepDelay (Includes Negatives)
# DepDelayMinutes (Same as DepDelay but negs are 0)
# DepDel15 --> Flag if DepDelay >= 15
# DepartureDelayGroups --> Bins DepDelay (-2:12)

#'Questions -->
# We could focus on DepDep15 --> and use Logistic Regression too see what variables are most important...or predict whether or not a flight will be delayed
# Does Distance affect delay?
# Is there a difference in Delays by Airport? Is it signif?

# KAPPA Metric --> hurts your model if you are predicting correctly based on chance 
# if P =! 0 in confusion matrix --> that means that is your baselinemodel (the acc is what you are trying to beat)
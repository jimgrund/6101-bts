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


########################################
d2 <- data.frame(read.csv("OriginAirport_data.csv", header = TRUE))
d2 <- subset(d2,DepartureDelayGroups >= 0)
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
(length(d2$DepartureDelayGroups[d2$DepartureDelayGroups>=1]) / nrow(d2) * 100 )
#What is the percentage the plane will be delayed without a model (at random)

########################################
# ## Model 1 (Decision Tree)
# 
# m1 <- rpart(DepartureDelayGroups ~ AirlineID+OriginAirportID+DestAirportID, data=TrainSet, method="class")
# printcp(m1)
# plotcp(m1)
# summary(m1)
# plot(m1,uniform=TRUE, main="Classification Tree", extra=102, under=TRUE, faclen=0)
# 
# ###Prediction
# p1 <- predict(m1, TestSet, type = "class")
# summary(p1)
# plot(p1)

########################################
#' #' ## Model 2 (Logistic Regression)
#' 
#' #xtabs(~DepDel15 + AirlineID, data = d1)
#' 
#' #' MODEL
#' #m2 <- glm(DepDel15 ~ AirlineID+OriginAirportID+DestAirportID,data=TrainSet) 
#' #m2 <- glm(DepDel15 ~ (Quarter)+(Month)+(AirlineID+OriginAirportID+DestAirportID,data=TrainSet, family=binomial(link = "logit")) 
#' #m2 <- glm(DepDel15 ~.,data = TrainSet, family=binomial(link = "logit"))
#' #m2 <- glm(DepDel15 ~ factor(Year)+factor(Quarter)+factor(Month)+factor(DayofMonth)+factor(DayOfWeek)+factor(AirlineID)+factor(OriginAirportID)+factor(DestAirportID),data=TrainSet) 
#' summary(m2)
#' 
#' #' ### Using anova() for feature importance
#' #anova(m2, test="Chisq")
#' 
#' #' ### Prediction
#' p2 <- predict(m2, newdata = TestSet,type = 'response')
#' p2 <- ifelse(p2 > 0.5,1,0)
#' str(p2)
#' p2
#' 
#' #' ### Confusion Matrix to Check Accuracy 
#' confusionMatrix(data=p2, reference = TestSet$DepDel15)
#' 
#' #' ### ROC and AUC
#' response_predict <- predict(m2, newdata=TestSet, type = "response")
#' link_predict <- predict(m2, newdata=TestSet, type = "link")
#' qplot(x=response_predict, geom="histogram")
#' #predictions <- predict(m2, newdata=TestSet, type="response")
#' ROCRpred <- prediction(response_predict, TestSet$DepDel15)
#' ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
#' 
#' plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7)) #print.cutoffs.at = seq(0,1,0.1))
#' #plot(ROCRperf, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
########################################
#' Model 3 (Decision Tree)

png(file = "model3.png")
m3 <- ctree(DepartureDelayGroups ~ AirlineID+OriginAirportID+DestAirportID, data = TrainSet)
plot(m3, main = "Conditional Inference Tree")
dev.off()
p3 <- predict(m3, TestSet)
input.response <- (TestSet$DepartureDelayGroups)
mean(input.response != p3)

#p3 <- predict(m3, TestSet)
#summary(p3)
#plot(p3)
########################################
#' Model 4 (Linear Regression)
m4 <- lm(DepDelay ~ AirlineID+OriginAirportID+DestAirportID, data = TrainSet)
summary(m4)
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
# RECODE FLAG, 0 - 1 --> 
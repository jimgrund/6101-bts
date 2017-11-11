#' ---
#' title: 6101 Project 2
#' author: TeamBestTeam
#' date: 31Oct17
#' output:
#'    html_document:
#'      toc: true
#'      highlight: haddock
#' ---
#' ## Environment Preparation
#Remove any objects in the Environment
#rm(list = ls())

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

#' ###  Data Load
# Load the dataset from the working directory
#data_all <- data.frame(read.csv("DMV_On_Time_Performance_2015-2017.csv", header = TRUE))

#' ## Preprocess Data
#' ### Global Filter Data
d1 <- subset(data_all, Year == "2015" | Year =="2016" | Year == "2017")

#' ### Missing Values Clean-up
#' 
#' #### Check for missing data
#+ eval = FALSE, echo = TRUE
colSums(is.na(d1))

#' #### Remove Col if all values are NA
d1[sapply(d1, function(x) all(is.na(x)))] <- NULL
#+ eval = FALSE, echo = TRUE
colSums(is.na(d1))

#' #### Remove Col if 80% of Rows are NA
# Works for only Numerical
d1 <- d1[,colSums(is.na(d1)) < (nrow(d1)*0.80)]
#+ eval = FALSE, echo = TRUE
colSums(is.na(d1))

#' #### Remove Rows if NA exists 
d1 <- d1[complete.cases(d1), ]
#+ eval = FALSE, echo = TRUE
colSums(is.na(d1))
d1_top20 <- d1[1:20,]
d1_rand10 <- d1[sample(nrow(d1),10),]
#' #### Clean up Environment
rm(data_all)

#' Target Variables -->
# DepDelay (Includes Negatives)
# DepDelayMinutes (Same as DepDelay but negs are 0)
# DepDel15 --> Flag if DepDelay >15
# DepartureDelayGroups --> Bins DepDelay (-2:12)

#'Questions -->
# We could focus on DepDep15 --> and use Logistic Regression too see what variables are most important...or predict whether or not a flight will be delayed
# Does Distance affect delay?
# Is there a difference in Delays by Airport? Is it signif?
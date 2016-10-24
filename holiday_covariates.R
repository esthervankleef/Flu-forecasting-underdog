###### Flu Forecasting Challenge ####### 

## title: "Holidays"
## author: "Underdog"
## date: "19 October 2016"

# empty workspace
rm(list = ls())
script_name <- "holiday_covariates"

# libraries
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
# find a source for school holidays
library(timeDate)

s_script_name <- "school_holidays"
loadname <- paste0("./Data/", s_script_name, ".Rda")
load(file = loadname)

# load flu data
load("./Data/data_manip.Rda")
DF <- usflu
rm(usflu) # delete under diff name



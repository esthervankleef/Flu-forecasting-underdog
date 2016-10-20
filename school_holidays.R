###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "19 October 2016"

# empty workspace
rm(list = ls())
script_name <- "school_holidays"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)

# find a source for school holidays


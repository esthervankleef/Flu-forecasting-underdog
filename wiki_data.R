###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "19 October 2016"

# empty workspace
rm(list = ls())
script_name <- "wiki_data"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)

# source for wiki data
# wikipedia page statistics only 2015
# goes back to 2007: http://stats.grok.se/en/200712/Influenza

# download from here http://www.wikishark.com/title/en/Influenza#

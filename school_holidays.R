###### Flu Forecasting Challenge ####### 

## title: "Holidays
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
library(timeDate)
# List all holiday available
listHolidays()
# Seach in the holiday list 
listHolidays(pattern = "US")

isHoliday(x, holidays = holidayNYSE(), wday=1:5)


currentYear <- getRmetricsOptions("currentYear")
# set start and end date
start_date <- "2012-01-01"
end_date <- "2015-12-31"

# make a timeline with dates 

# match vectors with different holidays on these days

# bin them into weeks

# output

my_years <- currentYear:2010


# only very special days
ChristmasDay(year = my_years)
ChristmasEve(year = my_years)
BoxingDay(year = getRmetricsOptions("currentYear"))

Easter(year = my_years)
EasterSunday(year = getRmetricsOptions("currentYear"))
EasterMonday(year = getRmetricsOptions("currentYear"))
GoodFriday(year = getRmetricsOptions("currentYear"))

NewYearsDay(year = getRmetricsOptions("currentYear"))






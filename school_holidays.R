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
listHolidays(pattern = "Thanks")


currentYear <- getRmetricsOptions("currentYear")

# set start and end date
start_date <- ymd("2012-01-01")
end_date <- ymd("2015-12-31")
# make a timeline with dates 
my_time_line <- (seq(start_date,end_date, by="day"))

# these are the big holidays
my_years <- 2012:2015
#
christmas_e <- ChristmasEve(year = my_years) # you could add days before and after
christmas_d <- ChristmasDay(year = my_years) 
boxd <- BoxingDay(year = my_years) 
easter_s <- EasterSunday(year = my_years)
easter_m <- EasterMonday(year=my_years)
thanks_g <- USThanksgivingDay(year=my_years)
ny <- NewYearsDay(year = my_years) - 1
nyd <- NewYearsDay(year = my_years)

# vector for Christmas
big_holidays <- c(christmas_e, christmas_d, boxd,
                  easter_s, easter_m, thanks_g, ny, nyd) # 4 dates each year
big_holidays <- as.Date(big_holidays)
#
whenis_bigholiday <- (my_time_line%in%big_holidays)
# get back christmas
my_time_line[whenis_bigholiday]
sum(whenis_bigholiday)
# make numeric
whenis_bigholiday <- as.numeric(whenis_bigholiday)

#

## get all the other holidays
# weekends and holidays =TRUE, else FALSE
whenis_holiday <- isHoliday(as.timeDate(my_time_line), holidays = holidayNYSE(), wday=1:5)
table(whenis_holiday)
whenis_holiday <- as.numeric(as.vector(whenis_holiday))


# put into dataframe
date_frame <- data.frame(time_line=my_time_line,
                         weekday=weekdays(x=my_time_line),
                         whenis_bigholiday=whenis_bigholiday, 
                         whenis_holiday=whenis_holiday)

# [https://www.feiertagskalender.ch/ferien.php?geo=3537&jahr=2012&hl=en] kids in school

# christmas
chin2011 <- c("2011-12-25","2012-01-01")
chin2012 <- c("2012-12-25","2013-01-01")
chin2013 <- c("2013-12-25","2014-01-01")
chin2014 <- c("2014-12-25","2015-01-01")
chin2015 <- c("2015-12-21","2016-01-01") # different
chin2016 <- c("2016-12-19","2017-01-02") # different
chin2017 <- c("2017-12-18","2018-01-02") # different
# winter
wiin2012 <- c("2012-02-20","2012-02-26")
wiin2013 <- c("2013-02-18","2013-02-24")
wiin2014 <- c("2014-02-17","2014-02-23")
wiin2015 <- c("2015-02-16","2015-02-20")
wiin2016 <- c("2016-02-15","2016-02-19")
wiin2017 <- c("2017-02-13","2017-02-17")
# spring
spin2012 <- c("2012-04-09","2012-04-15")
spin2013 <- c("2013-04-08","2013-04-14")
spin2014 <- c("2014-04-14","2014-04-20")
spin2015 <- c("2015-03-25","2015-03-29")
spin2016 <- c("2016-03-25","2016-04-08") # now call it Easter holiday
spin20162 <- c("2016-05-30","2016-06-03") # also Spring
spin2017 <- c("2017-04-10","2017-04-21") # Easter
spin20172 <- c("2017-05-29","2017-06-02") # Spring time
# mayday
mayday2016 <- c("2016-05-02")
mayday2017 <- c("2017-05-01")
# Summer
suin2012 <- c("2012-06-03","2012-08-26")
suin2013 <- c("2013-06-03","2013-08-25")
suin2014 <- c("2014-06-02","2014-08-24")
suin2015 <- c("2015-06-22","2015-08-31")
suin2016 <- c("2016-06-20","2016-08-31")
suin2017 <- c("2017-07-24","2017-09-01")
# Autumn
auin2012 <- c("2012-11-19","2012-11-25")
auin2013 <- c("2013-11-25","2013-12-01")
auin2014 <- c("2014-11-24","2014-11-30")
auin2015 <- c("2015-10-26","2015-10-30")
auin2016 <- c("2016-10-24","2016-10-28")
auin2017 <- c("2017-10-23","2017-10-27")

# ch 
kidhol_ch <- c(seq(ymd(), ymd(),by="day"), seq(ymd(""), ymd(""),by="day"))
# winter
kidhol_wt <- seq(ymd(), ymd(),by="day")
# spring
kidhol_sp <- seq(ymd(), ymd(),by="day")
# Summer
kidhol_su <- seq(ymd(), ymd(),by="day")
# Autumn
kidhol_au <- seq(ymd(), ymd(),by="day")

# bin them into weeks

class(my_time_line) 
whenis_bigholiday

# output



# only very special days







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
library(randomForest)
library(caret)
library(stringr)
# the timeDate package contains public holidays of countries (Christmas, Ester etc)
library(timeDate)

# List all holiday available
listHolidays()
# Seach in the holiday list 
listHolidays(pattern = "Thanks")
# get the current year
currentYear <- getRmetricsOptions("currentYear")

### make a timeline and then add presence and absence of holidays
# set start and end date
wday("2011-12-17") # I want to start with a sunday to capture complete weeks
week("2011-12-16");week("2011-12-17")
start_date <- ymd("2011-12-20")
week("2017-08-07")
end_date <- ymd("2017-08-07") # choose past flu season
# make a timeline with dates 
my_time_line <- (seq(start_date,end_date, by="day"))

# these are the big holidays#
my_years <- 2011:2017 # I choose gererously
#
christmas_e <- ChristmasEve(year = my_years) # you could add days before and after
christmas_d <- ChristmasDay(year = my_years) 
boxd <- BoxingDay(year = my_years) 
easter_s <- EasterSunday(year = my_years)
easter_m <- EasterMonday(year=my_years)
thanks_g <- USThanksgivingDay(year=my_years)
ny <- NewYearsDay(year = my_years) - 1
nyd <- NewYearsDay(year = my_years)

# vector for all big holidays
big_holidays <- c(christmas_e, christmas_d, boxd,
                  easter_s, easter_m, thanks_g, ny, nyd) # 4 dates each year
big_holidays <- as.Date(big_holidays)
#
whenis_bigholiday <- (my_time_line%in%big_holidays)
# get back all holidays 
my_time_line[whenis_bigholiday]
sum(whenis_bigholiday)
# make numeric
whenis_bigholiday <- as.numeric(whenis_bigholiday)

## get all the other holidays
# weekends and holidays =TRUE, else FALSE
whenis_daysoffwork <- isHoliday(as.timeDate(my_time_line), holidays = holidayNYSE(), wday=1:5)
table(whenis_daysoffwork)
whenis_daysoffwork <- as.numeric(as.vector(whenis_daysoffwork))

### kids out of school # this data starts 2011-12-25
# [https://www.feiertagskalender.ch/ferien.php?geo=3537&jahr=2012&hl=en] kids in school
schoolhols <- data.frame(dates = c("start","end"))
# christmas
schoolhols$chin2011 <- c("2011-12-25","2012-01-01")
schoolhols$chin2012 <- c("2012-12-25","2013-01-01")
schoolhols$chin2013 <- c("2013-12-25","2014-01-01")
schoolhols$chin2014 <- c("2014-12-25","2015-01-01")
schoolhols$chin2015 <- c("2015-12-21","2016-01-01") # different
schoolhols$chin2016 <- c("2016-12-19","2017-01-02") # different
schoolhols$chin2017 <- c("2017-12-18","2018-01-02") # different
# winter
schoolhols$wiin2012 <- c("2012-02-20","2012-02-26")
schoolhols$wiin2013 <- c("2013-02-18","2013-02-24")
schoolhols$wiin2014 <- c("2014-02-17","2014-02-23")
schoolhols$wiin2015 <- c("2015-02-16","2015-02-20")
schoolhols$wiin2016 <- c("2016-02-15","2016-02-19")
schoolhols$wiin2017 <- c("2017-02-13","2017-02-17")
# spring
schoolhols$spin2012 <- c("2012-04-09","2012-04-15")
schoolhols$spin2013 <- c("2013-04-08","2013-04-14")
schoolhols$spin2014 <- c("2014-04-14","2014-04-20")
schoolhols$spin2015 <- c("2015-03-25","2015-03-29")
schoolhols$spin2016 <- c("2016-03-25","2016-04-08") # now call it Easter holiday
schoolhols$spin20162 <- c("2016-05-30","2016-06-03") # also Spring
schoolhols$spin2017 <- c("2017-04-10","2017-04-21") # Easter
schoolhols$spin20172 <- c("2017-05-29","2017-06-02") # Spring time
# mayday
schoolhols$mayday2016 <- c("2016-05-02")
schoolhols$mayday2017 <- c("2017-05-01")
# Summer
schoolhols$suin2012 <- c("2012-06-03","2012-08-26")
schoolhols$suin2013 <- c("2013-06-03","2013-08-25")
schoolhols$suin2014 <- c("2014-06-02","2014-08-24")
schoolhols$suin2015 <- c("2015-06-22","2015-08-31")
schoolhols$suin2016 <- c("2016-06-20","2016-08-31")
schoolhols$suin2017 <- c("2017-07-24","2017-09-01")
# Autumn
schoolhols$auin2012 <- c("2012-11-19","2012-11-25")
schoolhols$auin2013 <- c("2013-11-25","2013-12-01")
schoolhols$auin2014 <- c("2014-11-24","2014-11-30")
schoolhols$auin2015 <- c("2015-10-26","2015-10-30")
schoolhols$auin2016 <- c("2016-10-24","2016-10-28")
schoolhols$auin2017 <- c("2017-10-23","2017-10-27")
#
# 
diff_hols <- dim(schoolhols)[2]
# initiate holiday time
holiday_time <- as.Date(NA)
# collect all the school holidays
for (i in (2:diff_hols)){
  hol_start <- ymd(schoolhols[1,i]) 
  hol_end <- ymd(schoolhols[2,i])
  hol_seq <- seq(hol_start, hol_end, by="day")
  holiday_time <- c(holiday_time, hol_seq)
}
# remove NA
holiday_time <- holiday_time[!is.na(holiday_time)]
# make timeline vector
whenis_schoolfree <- as.numeric(my_time_line%in%holiday_time)

# put into dataframe
holiday_df <- data.frame(time_line=my_time_line,
                         weekday=weekdays(x=my_time_line),
                         whenis_bigholiday=whenis_bigholiday, 
                         whenis_daysoffwork=whenis_daysoffwork,
                         whenis_schoolfree=whenis_schoolfree,
                         year=year(my_time_line),
                         week=week(my_time_line)) %>%
  mutate(whenare_kidsoff=as.numeric(whenis_daysoffwork==1|whenis_schoolfree==1)) %>% 
  mutate(weekname = paste(year,str_pad(week, 2, pad = "0"),sep="-"))
# plot
plot(as.numeric(as.factor(holiday_df$weekname[1000:2000])),holiday_df$whenis_bigholiday[1000:2000],pch=8,col="darkblue")
points(holiday_df$whenare_kidsoff[1000:2000],col="darkred",pch=19)

##############################
# bin into week
holiday_perweek <- holiday_df %>% 
  group_by(weekname) %>% 
  summarise(big_holidays=as.numeric(any(whenis_bigholiday==1)),
            inschool=as.numeric(sum(whenare_kidsoff==0)))


########################################
#### save & load
savename <- paste0("./Data/", script_name, ".Rda")
save(holiday,holiday_perweek,file = savename)





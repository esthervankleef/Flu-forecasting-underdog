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
library(forecast)
library(caret)
# find a source for school holidays
library(timeDate)

s_script_name <- "school_holidays"
loadname <- paste0("./Data/", s_script_name, ".Rda")
load(file = loadname)

# load flu data
load("./Data/data_manip.Rda")
DF <- usflu; rm(usflu,usflu_allyears)
rm(usflu) # delete under diff name

# cut DF 
DF <- DF[DF$year>=2012,]
# cut holiday_perweek
holiday_perweek <- holiday_perweek[!holiday_perweek$weekname == "2016-40",]

#
DF1 <- dplyr::left_join(DF, holiday_perweek, by = "weekname")

############################### Look at linear models
# log scale
DF1$cases <- log(DF1$x.weighted.ili) + 1

### cases vs inschool
plot(DF1$cases~DF1$inschool)
# correlation
my.ccf <- ccf(DF1$cases,DF1$inschool, type="covariance", lag.max=10) # indicates a 2 weeks lag
# gives the pearson coefficient
my.cor0 <- cor(DF1$cases,DF1$inschool) # best lag (when log-scale)
my.cor1 <- cor(lag(DF1$cases,1),DF1$inschool,use = "complete") # 
my.cor2 <- cor(lag(DF1$cases,2),DF1$inschool,use = "complete") # best lag (when not log-scale)
my.cor3 <- cor(lag(DF1$cases,3),DF1$inschool,use = "complete") # 
my.cor4 <- cor(lag(DF1$cases,4),DF1$inschool,use = "complete") # 

plot(lag(DF1$cases,0)~DF1$inschool)


# cases vs big_holidays
plot(DF1$cases~DF1$big_holidays)
### pearson correlation with diff lags
# gives the pearson coefficient
my.cor5 <- cor(DF1$cases,DF1$big_holidays) 
my.cor6 <- cor(lag(DF1$cases,1),DF1$big_holidays,use = "complete") # best lag (both with and without log-scale)
my.cor7 <- cor(lag(DF1$cases,2),DF1$big_holidays,use = "complete") # 
my.cor8 <- cor(lag(DF1$cases,3),DF1$big_holidays,use = "complete") # 
my.cor9 <- cor(lag(DF1$cases,4),DF1$big_holidays,use = "complete") # 

### cases vs weeks
plot(abs(DF1$week - 31),DF1$cases)
plot(DF1$week ,DF1$cases)

### linear regression models
ili <- DF1$cases
cases_around <- lag(DF1$x.weighted.ili,4) # with lag 4 for forecasting
kids_cuddle <- lag(DF1$inschool,0) # with lag 0 from correlation analysis
time_sinceOB <- NA
big_hols <- lag(DF1$big_holidays,1) # with lag 1 from correlation analysis
magic_week <- abs(DF1$week - 31)

# lm with big_holidays + inschool
fit1 <- lm(ili ~ big_hols + kids_cuddle)
summary(fit1)

# 
fit2 <- lm(ili ~ big_hols + kids_cuddle + big_hols:kids_cuddle) # the interaction terms makes is worse
summary(fit2)

# H: flu cases increase because kids are in school, but only when there are cases around, only far away from last peak

#
fit3 <- lm(ili ~ cases_around + kids_cuddle + cases_around:kids_cuddle) 
summary(fit3)
#
fit4 <- lm(ili ~ cases_around + kids_cuddle + big_hols  + 
             cases_around:kids_cuddle + cases_around:big_hols) 
summary(fit4)

# 
fit5 <- lm(ili ~ magic_week + cases_around + kids_cuddle + big_hols  + 
             cases_around:kids_cuddle + cases_around:big_hols) 
summary(fit5)

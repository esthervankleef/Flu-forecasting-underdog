###### Flu Forecasting Challenge ####### 

## title: "Data Manipulation 2"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "data_manip_2"

# libraries
library(tidyverse)
library(lubridate)

########################################
#### load data
# flu data
load("./Data/data_manip.Rda")
DF <- usflu # already truncated and without NA (otherwise use usflu_allyears)
# holiday data
load("./Data/school_holidays.Rda")
load("./Data/clim_data.Rda")
load("./Data/seas_times.Rda")
load("./Data/google_data.Rda")

#######################################################
# log transform data
# DO WE NEED TO LOG-TRANSFORM?
# For ARIMA and LASSO yes. Is done in the below
DF1 <- DF %>% 
  dplyr::select(-region,-region.type) %>% mutate(cases = as.numeric(as.character(x.weighted.ili)),
                                                 cases = log(cases + 1)) # NA is one missing value which was coded as X
# only keep DF
rm(usflu, usflu_allyears, DF)

#######################################################
# join the data
DF2 <- dplyr::full_join(DF1,holiday_perweek,by = "weekname")
DF3 <- dplyr::left_join(DF2,seas_times,by = "weekname")
DF4 <- dplyr::left_join(DF3,clim, by="weekname")
DF5 <- dplyr::left_join(DF4,google, by="weekname")
# fill in the missing years and weeks
no_dates <- is.na(DF5$year)
DF5$year[no_dates] <- DF5$hyear[no_dates]
DF5$week[no_dates] <- DF5$hweek[no_dates]
#
DF1 <- DF5

#######################################################
### identify and remove week 53
# identify where there are 53 weeks 
week53 = DF1$year[which(DF1$week == 53)]
# get rid of 53 week
week53_names <- paste(week53,"53",sep="-")
where_week53 <- which(DF1$weekname %in% week53_names)
#
my_vars <- c("x.weighted.ili","ilitotal",
             "total.patients","cases","big_holidays","inschool","m_start_seas",
             "m_end_seas","m_peak_seas","temp_av","temp_anom_av",
             "gfever","gheadache","gdoctor","gshivering","gcough")

# take the mean
DF1[where_week53-1,my_vars] <- (DF1[where_week53-1,my_vars] + DF1[where_week53,my_vars])/2
# remove week 53
DF1 <- DF1[-where_week53,]

#######################################################
# give each timepoint the season name, assuming that the dataframe start at the start of a season
season=NULL
for(i in 1:(length(unique(DF1$year))-0)){
  s = rep(i,52)
  season = c(season,s)
}
# 
season <- season[1:dim(DF1)[1]]
DF1$season = season
# create a sum over seasons 
seas = DF1%>%group_by(season) %>% summarise(seas_total = sum(cases)) %>% mutate(seas_total_l1=lag(seas_total,1))
# add as covariate
DF1 = left_join(DF1, seas, by="season")

#######################################################
# mutate variables
end.timeline <- dim(DF1)[1]
### mutate predictive variables: derivatives
DF1 <-  DF1 %>% 
  mutate(
    cases = cases, # lag 4
    week = (week-31)%%52,
    dcases = c(NA,diff(cases)), # lag 4 # lag 5
    kids_cuddle = inschool, # lag 2
    big_hols= big_holidays, # lag 1
    sin_week = sin(2*pi*week/52),
    cos_week = cos(2*pi*week/52)
  )
# truncate the NA-beginning
# the season lag produces 53 NA
# but even more NA because holidays available form 2011-51
week_pos <- which(DF1$weekname == "2011-51") + 2 # plus because of lag
DF2 <- DF1[week_pos:end.timeline,] 

###################################
#### save
DF <- DF2
savename <- paste0("./Data/", script_name,".Rda")
save(DF, file = savename)

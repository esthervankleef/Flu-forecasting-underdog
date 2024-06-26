###### Flu Forecasting Challenge ####### 

## title: "Add season to points"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "add_season_targets_to_points"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(glmnet)
# load functions
source("./functions.R")

# magic and real week converter
real_week <- 1:52
magic_week <- (real_week - 31 ) %% 52
week_conv_fromreal <- data.frame(real_week=real_week,magic_week=magic_week)
week_conv_frommag <- dplyr::arrange(week_conv_fromreal,magic_week) 

########################################
#### load data
results_combined = read.csv("./Forecasts/combine_point_forecasts.csv")
#
load("./Data/data_manip.Rda") # to get most recent week
load("./Data/season_forecasts.Rda")

# Intensity
#########################

targets = c("Season peak percentage")
nat_peakweek = which(results_combined$Target==targets & results_combined$Location=="US National")
# Calculate bins mean season onset prediction
check  = (results_combined[nat_peakweek,])
bins = unique(check$Bin_start_incl)[!unique(check$Bin_start_incl)%in%c("none")]
bins = as.numeric(as.character(bins[2:length(bins)])) # remove NA
bins = c(bins)  

# Intensity will be above 3.4 (-5, as five bins below still count in scoring) for sure, so we can give 0 prob to all bins lower than that 
# Also, we don't expect the intensity to be higher than 7.6 (this is the max of the prediction
# interval of all models entering the competition)
# Do this to create more certainty in our prediction
# 
bins.rem.low = bins[bins<=3]
bins.rem.high = bins[bins>=7.7]

bins = bins[bins>3 & bins<7.7]
#
breaks.in = as.numeric(as.character(bins))
prob.forecast = data.frame(cbind(Bin_start_incl = breaks.in,prob = rep(NA,length(breaks.in))))
# 
prob.forecast$prob = gen.prob.distr(mean=mean_intensity, sd=sd_intensity, log.scale=F, breaks.in=breaks.in)
plot(breaks.in,prob.forecast$prob, type="l", ylab="density", main=targets, xlab="breaks")
# put into data frame
point = mean_intensity
results_combined$Value[nat_peakweek] = c(point,rep(0,length(bins.rem.low)),prob.forecast$prob,rep(0,length(bins.rem.high))) 

# Start week
#########################

targets = c("Season onset")
nat_peakweek = which(results_combined$Target==targets & results_combined$Location=="US National")
# Calculate bins mean season onset prediction
check  = (results_combined[nat_peakweek,])
# magic week intermission
breaks.in <- 0:51
prob.forecast = data.frame(cbind(Bin_start_incl = breaks.in,prob = rep(NA,length(breaks.in))))
prob.forecast$prob = gen.prob.distr(mean=mean_start, sd=sd_start, log.scale=F, breaks.in=breaks.in)
plot(breaks.in,prob.forecast$prob, type="l", ylab="density", main=targets, xlab="breaks")
prob.forecast$Bin_start_incl <- week_conv_frommag$real_week
# sum bins
weeks_sums <- 21:39
find_weeks <- prob.forecast$Bin_start_incl %in% weeks_sums
find_weeks_probs <- prob.forecast$prob[find_weeks]
none_bin <- sum(find_weeks_probs)
# remove
prob.forecast <- prob.forecast[!find_weeks,]
#
# put into data frame
point = week_conv_frommag$real_week[mean_start + 1]
results_combined$Value[nat_peakweek] = c(point,prob.forecast$prob,none_bin) 

# Season onset has occurred therefore:
onset = which(prob.forecast$Bin_start_incl==50)
point = 50
results_combined$Value[nat_peakweek] = c(point,rep(0,onset-1),1,rep(0,(length(prob.forecast$prob)-(onset-1)))) 

# Peak week
#########################

targets = c("Season peak week")
nat_peakweek = which(results_combined$Target==targets & results_combined$Location=="US National")
# Calculate bins mean season onset prediction
check  = (results_combined[nat_peakweek,])
# magic week intermission
breaks.in <- 0:51
prob.forecast = data.frame(cbind(Bin_start_incl = breaks.in,prob = rep(NA,length(breaks.in))))
prob.forecast$prob = gen.prob.distr(mean=mean_peak, sd=sd_peak, log.scale=F, breaks.in=breaks.in)
plot(breaks.in,prob.forecast$prob, type="l", ylab="density", main=targets, xlab="breaks")
prob.forecast$Bin_start_incl <- week_conv_frommag$real_week
# sum bins
weeks_sums <- 21:39
find_weeks <- prob.forecast$Bin_start_incl %in% weeks_sums
find_weeks_probs <- prob.forecast$prob[find_weeks]
none_bin <- sum(find_weeks_probs)
# remove
prob.forecast <- prob.forecast[!find_weeks,]
#
# put into data frame
point = week_conv_frommag$real_week[mean_peak + 1]
results_combined$Value[nat_peakweek] = c(point,prob.forecast$prob) 

# Save
# remove first to columns
results_combined <- results_combined[,-c(1,2)]
#####################################################
# Save file
latest_week <- as.numeric(gsub("2017-","",most_current_week))

date_today <- format(Sys.time(), "%Y-%m-%d") 
# remove where we did not fill in
check <- results_combined[is.na(results_combined$Value),]
check <- check$Location # should only contain HHS Region
#
results_combined <- results_combined[!is.na(results_combined$Value),]
savename <- paste0("./Forecasts/Submissions/NEW",latest_week,"-FORSEA_",date_today,".csv")
write.csv(results_combined,file = savename)


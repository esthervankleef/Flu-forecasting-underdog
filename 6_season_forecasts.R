###### Flu Forecasting Challenge ####### 

## title: "Season Forecasts"
## author: "Underdog"
## date: "7 November 2016"

######################
# Predict peak weeks

rm(list=ls())
script_name <- "season_forecasts"


# libraries
library(glmnet)
library(Hmisc)
# load data
load("./Data/data_manip.Rda")
source("./functions.R")
#
DF <- usflu_allyears

# Create season variable, each season starts at week 40
# identify where there are 53 weeks 
week53 = DF$year[which(DF$week == 53)]
# give each timepoint the season name
season=NULL
for(i in 1:(length(unique(DF$year)))){
  if(!unique(DF$year)[i] %in% week53){
    s = rep(i,52)
  }
  else{
    s = rep(i,53)
  }
  season = c(season,s)
}
DF$season = season[1:length(DF$region.type)]

# get rid of 53 week
week53_names <- paste(week53,"53",sep="-")
where_week53 <- which(DF$weekname %in% week53_names)
my_vars = c("x.weighted.ili","ilitotal", "total.patients","cases")  
# take mean
DF[where_week53-1,my_vars] <- (DF[where_week53-1,my_vars] + DF[where_week53,my_vars])/2
# remove week 53
DF <- DF[-where_week53,]

DF$week <- (DF$week - 31 ) %% 52

# magic-real week conversion
real_week <- 1:52
magic_week <- (real_week - 31 ) %% 52
week_conv_fromreal <- data.frame(real_week=real_week,magic_week=magic_week)
week_conv_frommag <- dplyr::arrange(week_conv_fromreal,magic_week) 

### Predefined parameters
national_baseline <- 2.2

intensity <- start_week <- peak_week <- array(dim = length(1998:2016))

# put this in manually cause coding is a massive pain
start_week <- c(17,19,21,26,22,16,22,21,21,23,27,5,21,21,20,21,19,27)
# the weeks we have in 2016

DF_this_season <- subset(DF, year%in%c(2016,2017))
most_recent_weeks <- 52
weeks_to_use <- tail(DF_this_season$week,most_recent_weeks)

X_data <- array(dim = c(length(1998:2015), length(weeks_to_use)))
# WE SHOULD USE SEASONS AND NOT YEARS!
for(years in 1998:2015){
  year_data <- subset(DF, year==years)
  peak_week[years-1997] <- year_data$week[which.max(year_data$x.weighted.ili)]
  intensity[years-1997] <- year_data$x.weighted.ili[which.max(year_data$x.weighted.ili)]
  #take the last 5 weeks as predictors
  X_data[years-1997,1:sum(year_data$week %in% weeks_to_use)] <- as.numeric(year_data$x.weighted.ili[year_data$week %in% weeks_to_use])
}

# remove years
years_to_remove <- c(1:5,12,19)
X_predict <- X_data[-years_to_remove,]
peak_week <- peak_week[-years_to_remove]
start_week <- start_week[-years_to_remove]
intensity <- intensity[-years_to_remove]

## TRY MODELLING WITH ACTUAL CASES FOR INTENSITY
r_fit_peak <- glmnet(y=peak_week, x= t(apply(X = X_predict , 1, diff)))
r_fit_start <- glmnet(y=start_week, x= t(apply(X = X_predict, 1, diff)))
r_fit_intensity <- glmnet(y=intensity, x= t(apply(X = X_predict, 1, diff)))

plot(r_fit_intensity, xvar='lambda', label=T)
plot(r_fit_start, xvar='lambda', label=T)
plot(r_fit_peak, xvar='lambda', label=T)

# cross validation to decide on lambda 
cv_fit <- cv.glmnet(y = as.numeric(peak_week), x = t(apply(X = X_predict , 1, diff)),
                    type.measure = "mae", nfolds = 12)
best_lambda_pea <- cv_fit$lambda.min

#
cv_fit <- cv.glmnet(y = as.numeric(start_week), x = t(apply(X = X_predict , 1, diff)),
                    type.measure = "mae", nfolds = 12)
best_lambda_sta <- cv_fit$lambda.min
#
cv_fit <- cv.glmnet(y = as.numeric(intensity), x = t(apply(X = X_predict , 1, diff)),
                    type.measure = "mae", nfolds = 12)
best_lambda_int <- cv_fit$lambda.min

# Plot model and actual
par(mfrow=c(3,1))
plot(predict(r_fit_intensity, t(apply(X = X_predict, 1, diff)), s = best_lambda_int), intensity,
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
abline(0,1) # add diagonal 

plot(predict(r_fit_start, t(apply(X = X_predict, 1, diff)), s = best_lambda_sta), start_week,
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
abline(0,1) # add diagonal

plot(predict(r_fit_peak, t(apply(X = X_predict, 1, diff)), s = best_lambda_pea), peak_week,
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
abline(0,1) # add diagonal

# Store fitted vs data
datfit = data.frame(obspeak = peak_week, fitpeak = predict(r_fit_peak, t(apply(X = X_predict, 1, diff)), s = best_lambda_pea), # I think these should be best fitted lambda rather than 0.5 right?
                    obsstart = start_week, fitstart = predict(r_fit_start, t(apply(X = X_predict, 1, diff)), s = best_lambda_sta),
                    obsinstensity = intensity, fitintensity = predict(r_fit_intensity, t(apply(X = X_predict, 1, diff)), s = best_lambda_int))
names(datfit) = c("obspeak" ,      "fitpeak"  ,          "obsstart"    ,  "fitstart"    ,      "obsinstensity" ,"fitintensity")
datfit$respeak = datfit$obspeak - datfit$fitpeak 
datfit$resstart = datfit$obsstart - datfit$fitstart 
datfit$resintensity = datfit$obsinstensity - datfit$fitintensity

## Mean prediction
# predict from this season
years <- c(2016,2017)
year_data <- subset(DF, year%in%years)
most_recent_week = which(duplicated(year_data$week)) # This is to make sure that the most recent weeks in the new year (i.e in 2017) are used rather than those of 2016 (e.g. Jan 2017 rather than Jan 2016)
if(length(most_recent_week)>0){
  most_recent_week = max(which(duplicated(year_data$week)))
  rows_to_use = c((most_recent_week-51):most_recent_week)
  rows_to_use = tail(rows_to_use,length(weeks_to_use))
  pred_X <- as.numeric(year_data$x.weighted.ili[rows_to_use])
}else{
  pred_X <- as.numeric(year_data$x.weighted.ili[year_data$week %in% weeks_to_use])
}

peak_week_prediction <- predict(r_fit_peak, newx = t(diff(pred_X,1))  , s=best_lambda_pea)
start_week_prediction <- predict(r_fit_start, newx = t(diff(pred_X,1)) , s=best_lambda_sta)
intensity_prediction <- predict(r_fit_intensity, newx = t(diff(pred_X,1)) , s=best_lambda_int)

# rounding
mean_peak <-  round(peak_week_prediction)
mean_start <- round(start_week_prediction)
#mean_intensity <- intensity_prediction

# The mean predicted intensity is currently exactly the same as the mean of all observations:
#mean_intensity
#mean(intensity) # This is because the model cannot use the information of the slope of the season

# The same is true for the sd 
#sd(intensity)
#sd(datfit$resintensity)

# Try using a weighted mean with more weights on last three years
y = 1
w.sd = c(rep(1,length(datfit$respeak)-y),rep(20,y))

## SD prediction
#sd_peak = sqrt(wtd.var(datfit$respeak,weights=w.sd))
#sd_start = sqrt(wtd.var(datfit$resstart,weights=w.sd))
sd_peak = sd(datfit$respeak)
sd_start = sd(datfit$resstart)

#sd_intensity = sqrt(wtd.var(datfit$resintensity,weights=w.sd))

mean_intensity = wtd.mean(intensity,weights=w.sd)
sd_intensity = sqrt(wtd.var(intensity,weights=w.sd))/2 # Divided by two is just arbitrarily done to create more certainty

########################################
#### save
savename <- paste0("./Data/", script_name, ".Rda")
save(mean_peak,mean_start,mean_intensity,
     sd_peak,sd_start,sd_intensity, file = savename)

#ggplot(DF, aes(x=week,y=x.weighted.ili))+geom_line()+facet_wrap(~season,ncol=3)

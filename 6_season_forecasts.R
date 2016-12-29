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
start_week <- c(17,19,21,26,22,16,22,21,21,23,27,5,21,21,20,21,19,27) # Not sure what these numbers are based on, visual inspection?
# the weeks we have in 2016
#DF_this_season <- subset(DF, year==2016)
DF_this_season <- subset(DF, season==20)
#most_recent_obs <- length(DF_this_season)
weeks_to_use <- DF_this_season$week

X_data <- array(dim = c(length(2:20), length(weeks_to_use))) # Take seasons 2 until 19 (i.e. 1998/1999 until 2015/2016 as 1997/1998 has missing values)
# WE SHOULD USE SEASONS AND NOT YEARS!
for(seasons in 2:19){
  season_data <- subset(DF, season==seasons)
  peak_week[seasons-1] <- season_data$week[which.max(season_data$x.weighted.ili)]
  intensity[seasons-1] <- season_data$x.weighted.ili[which.max(season_data$x.weighted.ili)]
  #take the last 5 weeks as predictors
  X_data[seasons-1,1:sum(season_data$week %in% weeks_to_use)] <- as.numeric(season_data$x.weighted.ili[season_data$week %in% weeks_to_use])
}

# remove years
seasons_to_remove <- c(1:5,12,19) # So remove season 1998/1999, 1999/2000, 2000/2001, 2001/2002, 2002/2003, 2009/2010 and 2015/2016
X_predict <- X_data[-seasons_to_remove,]
peak_week <- peak_week[-seasons_to_remove]
start_week <- start_week[-seasons_to_remove]
intensity <- intensity[-seasons_to_remove]

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
#years <- 2016
season = 20
season_data <- subset(DF, season==seasons)
pred_X <- as.numeric(season_data$x.weighted.ili[season_data$week %in% weeks_to_use])


peak_week_prediction <- predict(r_fit_peak, newx = t(diff(pred_X,1))  , s=best_lambda_pea)
start_week_prediction <- predict(r_fit_start, newx = t(diff(pred_X,1)) , s=best_lambda_sta)
intensity_prediction <- predict(r_fit_intensity, newx = t(diff(pred_X,1)) , s=best_lambda_int)

# rounding
mean_peak <-  round(peak_week_prediction)
mean_start <- round(start_week_prediction)
mean_intensity <- intensity_prediction


## SD prediction
sd_peak = sd(datfit$respeak)
sd_start = sd(datfit$resstart)
sd_intensity = sd(datfit$resintensity)


########################################
#### save
savename <- paste0("./Data/", script_name, ".Rda")
save(mean_peak,mean_start,mean_intensity,
     sd_peak,sd_start,sd_intensity, file = savename)

###################### ########### ###################


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
# the week we have in 2016
DF_this_season <- subset(DF, year==2016)
most_recent_20 <- 20
weeks_to_use <- tail(DF_this_season$week,most_recent_20)

X_data <- array(dim = c(length(1998:2015), length(1:20)))
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

## 2009 is a massive outlier so removed it
r_fit_peak <- glmnet(y=peak_week, x= t(apply(X = X_predict , 1, diff)))
r_fit_start <- glmnet(y=start_week, x= t(apply(X = X_predict, 1, diff)))
r_fit_intensity <- glmnet(y=intensity, x= t(apply(X = X_predict, 1, diff)))


plot(r_fit_intensity, xvar='lambda', label=T)
plot(r_fit_start, xvar='lambda', label=T)
plot(r_fit_peak, xvar='lambda', label=T)

par(mfrow=c(2,2))
plot(predict(r_fit_intensity, t(apply(X = X_predict, 1, diff)), s = .5), intensity,
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
plot(predict(r_fit_start, t(apply(X = X_predict, 1, diff)), s = .5), start_week,
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
plot(predict(r_fit_peak, t(apply(X = X_predict, 1, diff)), s = .5), peak_week,
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);

# Store fitted vs data
datfit = data.frame(obspeak = peak_week, fitpeak = predict(r_fit_peak, t(apply(X = X_predict, 1, diff)), s = .5),
                    obsstart = start_week, fitstart = predict(r_fit_start, t(apply(X = X_predict, 1, diff)), s = .5),
                    obsinstensity = intensity, fitintensity = predict(r_fit_intensity, t(apply(X = X_predict, 1, diff)), s = .5))
names(datfit) = c("obspeak" ,      "fitpeak"  ,          "obsstart"    ,  "fitstart"    ,      "obsinstensity" ,"fitintensity")
datfit$respeak = datfit$obspeak - datfit$fitpeak 
datfit$resstart = datfit$obsstart - datfit$fitstart 
datfit$resintensity = datfit$obsinstensity - datfit$fitintensity

## Mean prediction
# predict from this season
years <- 2016
year_data <- subset(DF, year==years)
pred_X <- as.numeric(year_data$x.weighted.ili[year_data$week %in% weeks_to_use])



peak_week_prediction <- predict(r_fit_peak, newx = t(diff(pred_X,1))  , s=.5)
start_week_prediction <- predict(r_fit_start, newx = t(diff(pred_X,1)) , s=.5)
intensity_prediction <- predict(r_fit_intensity, newx = t(diff(pred_X,1)) , s=.5)

# rounding
mean_peak <-  round(peak_week_prediction)
mean_start <- round(start_week_prediction)
mean_intensity <- round(intensity_prediction)


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

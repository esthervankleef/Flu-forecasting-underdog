###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "flu_chall_template"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(glmnet)
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
# add holidays to the dataframe
DF2 <- dplyr::full_join(DF1,holiday_perweek,by = "weekname")
DF3 <- dplyr::left_join(DF2,seas_times,by = "weekname")
DF4 <- dplyr::left_join(DF3,clim, by="weekname")
DF5 <- dplyr::left_join(DF4,google, by="weekname")
no_dates <- is.na(DF5$year)
DF5$year[no_dates] <- DF5$hyear[no_dates]
DF5$week[no_dates] <- DF5$hweek[no_dates]

DF1 <- DF5

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

end.timeline <- dim(DF1)[1]
### mutate predictive variables: derivatives
DF1 <-  DF1 %>% 
  mutate(
    cases = cases, # lag 4
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

# decide the time points from where to make first.prediction and last.prediction
first.prediction <- "2015-35" # week from where to make the first prediction
# last.prediction; to validate own predictions we need observed data: then minus shortest lag
last.prediction <- "2016-34"

# where to put the start given the last prediction
last.prediction_df <- which(DF2$weekname == last.prediction)
DF2$weekname[last.prediction_df - 200]

# make numeric for final training point = prediction point
prstart <- which(DF2$weekname == first.prediction)
prstop <- which(DF2$weekname == last.prediction)
# all prediction points
pred_vector <- prstart:prstop

# start training points
train_start_vector <- pred_vector - (190 - 10)

### initiate outputfile
num.of.pred <- (prstop - prstart) + 1
# forcast 
FAO <- data.frame(timepoint_reference = prstart:prstop) # rf
FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

##### call functions
source("functions.R")
#######################################################
#### start the prediction loop
DF <- DF2 # use DF in the loop
i <- 0
for (pred.tpoint in pred_vector){
  i = i + 1
  print(i) # print where you are in the loop
  
  # manage the training position
  df_point <- pred.tpoint # the data.frame row
  train_start <- train_start_vector[i] # moving window
  # which weeks are those
  first_week_you_see <- DF$weekname[train_start]
  last_week_you_see <- DF$weekname[df_point]
  tchoice_v <- train_start:df_point
  
  # lag: 4
  wks_ahead <- 4
  ###############################################
  # make train data
  choose_predictors <- c("week","cases","dcases")
  choose_lags <- c(0,4,5)
  # make predictor matrix and outcome
  X <- my_predictors_lag(choose_predictors,choose_lags,name_predictors,DF,tchoice_v)
  Y <- DF$cases[tchoice_v]
  
  ###############################################
  # train RANDOM FOREST
  Fit1 <- train(x = X,
                y = Y, 
                method = "rf", 
                trControl = myfit_control(wks_ahead),
                verbose = TRUE,
                tuneGrid = NULL,
                tuneLength = 3,
                importance = TRUE)
  # obtain feature rank
  varImp(Fit1)
  ### forecast: no longer than the shortest lag!
  tchoice_forc_v <- df_point + 1:wks_ahead
  covars_for_forecast <- my_predictors_lag(choose_predictors,choose_lags,name_predictors,DF,tchoice_forc_v)
  rf_predictions <- predict(Fit1, covars_for_forecast) # point predictions
  # observed values 
  observed <- exp(DF$cases[tchoice_forc_v])-1
  
  ##################################################
  # SARIMA
  # fit model
  Fit2 <- Arima(DF$cases[tchoice_v], order=c(1,0,0),
                seasonal=list(order=c(1,0,0),period=52))
  ### forecast
  wks_ahead_arim <- 4
  ar_predictions <- forecast.Arima(Fit2,4)
  
  #####################
  ### output  
  ### save random forest
  final_predict <- rf_predictions
  # save 1 weeks forecast and observed
  FAO$f1w[i] <- exp(final_predict[1])-1
  FAO$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO$f2w[i] <- exp(final_predict[2])-1
  FAO$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO$f3w[i] <- exp(final_predict[3])-1
  FAO$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO$f4w[i] <- exp(final_predict[4])-1
  FAO$o4w[i] <- observed[4]
  
  ### save ARIMA
  final_predict <- as.numeric(ar_predictions$mean)
  # save 1 weeks forecast and observed
  FAOa$f1w[i] <- exp(final_predict[1])-1
  FAOa$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAOa$f2w[i] <- exp(final_predict[2])-1
  FAOa$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAOa$f3w[i] <- exp(final_predict[3])-1
  FAOa$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAOa$f4w[i] <- exp(final_predict[4])-1
  FAOa$o4w[i] <- observed[4]
  
} ####### end of loop
#####################################################
# evaluate
mse_rf_4w <- mean((FAO$o4w - FAO$f4w)^2)
mse_AR_4w <- mean((FAOa$o4w - FAOa$f4w)^2)
mse_ref_4w <- mean((FAO$o4w - lag(FAO$o4w,n = 4))^2,na.rm = TRUE)
eval <- data.frame(mse_rf_4w=mse_rf_4w,
                   mse_AR_4w=mse_AR_4w,
                   mse_ref_4w=mse_ref_4w)

########################################
#### save & load
savename <- paste0("./Data/", "experimental_Rene", ".Rda")
save(FAO,FAOa,eval,file = savename)
# loading (from here can be run without re-running the loop)
load(savename)

#####################################################
# plot
par(mfrow=c(3,1)) 
# reference (naive)
my_title <- paste("Ref 4-weeks, MSE =", as.character(round(mse_ref_4w,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,c(lag(FAO$o4w,n = 4)),pch=20,col="darkred")
# rf
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse_rf_4w,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
# SARIMA
my_title <- paste("SARIMA 4-weeks, MSE =", as.character(round(mse_AR_4w,digits = 4)))
plot(FAOa$timepoint_reference,FAOa$o4w,pch=19, col="black"); title(my_title)
points(FAOa$timepoint_reference,FAOa$f4w,pch=20,col="darkred")
par(mfrow=c(1,1)) 

#write.csv(outputfile,file="./Data/DARIMA_forecastsRF_11.csv",row.names = FALSE)
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

load("./Data/data_manip_2.Rda")

############################################
### make timepoints
# decide the time points from where to make first.prediction and last.prediction
first.prediction <- "2015-35" # week from where to make the first prediction
# last.prediction; to validate own predictions we need observed data: then minus shortest lag
last.prediction <- "2016-34"
# where to put the start given the last prediction
last.prediction_df <- which(DF$weekname == last.prediction)
DF$weekname[last.prediction_df - 200]

# make numeric for final training point = prediction point
prstart <- which(DF$weekname == first.prediction)
prstop <- which(DF$weekname == last.prediction)
# all prediction points
pred_vector <- prstart:prstop

# start training points
train_start_vector <- pred_vector - (100)

### initiate outputfile
num.of.pred <- (prstop - prstart) + 1
# forcast 
FAO <- data.frame(timepoint_reference = prstart:prstop) # rf
FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

##### call functions
source("functions.R")
#######################################################
#### start the prediction loop
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
  
  ################## RANDOM FOREST
  # lag: 4
  wks_ahead <- 4
  choose_predictors <- c("sin_week","week","cases","cases","dcases",
                         "kids_cuddle",
                         "temp_av"
  )
  #
  choose_lags <- c(0,0,4,5,4,
                   1,
                   4
  )
  #
  rf_w4_pred <- my_randomforest(wks_ahead,choose_predictors,choose_lags)
  
  
  # lag: 3
  wks_ahead <- 3
  # 
  choose_predictors <- c("sin_week","week","cases","dcases",
                         "temp_av","temp_av"
  )
  choose_lags <- c(0,0,3,3,
                   3,4
  )
  # predict
  rf_w3_pred <- my_randomforest(wks_ahead,choose_predictors,choose_lags)
  
  
  # lag: 2
  wks_ahead <- 2
  # 
  choose_predictors <- c("sin_week","week","cases","cases",
                         "temp_av","temp_av"
  )
  choose_lags <- c(0,0,2,3,
                   2,3
  )
  #  
  rf_w2_pred <- my_randomforest(wks_ahead,choose_predictors,choose_lags)
  
  
  # lag: 1
  wks_ahead <- 1
  # 
  choose_predictors <- c("sin_week","week","cases","cases","cases","dcases",
                         "temp_av","temp_av"
  )
  choose_lags <- c(0,0,1,2,3,1,
                   1,2
  )
  #
  rf_w1_pred <- my_randomforest(wks_ahead,choose_predictors,choose_lags)
  
  
  ##################################################
  # SARIMA
  # fit model
  Fit2 <- Arima(DF$cases[tchoice_v], order=c(1,0,0),
                seasonal=list(order=c(1,0,0),period=52))
  ### forecast
  wks_ahead_arim <- 4
  ar_predictions <- forecast.Arima(Fit2,4)
  
  ##################################################
  # observed values 
  tchoice_forc_v <- df_point + 1:4
  observed <- exp(DF$cases[tchoice_forc_v])-1
  
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
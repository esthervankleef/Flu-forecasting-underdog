###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "17 October 2016"

# empty workspace
rm(list = ls())
script_name <- "flu_chall_template"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)

########################################
#### laod the data
load("./Data/data_manip.Rda")
# select input variables
DF <- usflu
DF <- DF %>% 
        dplyr::select(-region,-region.type)
# only keep DF
rm(usflu)

#######################################################
##### set timepoints, create evalution and output files
# timepoints
burn.in <- 8
first.predict <- burn.in + 1
last.prediction <- dim(DF)[1] 
# initiate outputfile
num.of.pred <- (last.prediction - first.predict) + 1
model.evaluation <- matrix(0, nrow = 2,ncol = num.of.pred)
outputfile <- data.frame(season = as.character("Season"),
                         week = 1:(num.of.pred*2),
                         target = as.character("Season"),
                         mean = 1,
                         se = 1)
outputfile$season <- as.character(outputfile$season)
outputfile$target <- as.character(outputfile$target)

#######################################################
# log transform data
# DO WE NEED TO LOG-TRANSFORM?

#######################################################
#### start the prediction loop
i <- 0
for (pred.tpoint in first.predict:last.prediction){
        i = i + 1
        print(i) # show where you are in the loop
        
        ###############################################
        ################# model fitting
        
        # example: ARIMA
        
        # fit the model on log transformed data
        wks_ahead_arim <- 8
        model <- arima(DF$cases[1:pred.tpoint], order=c(1,0,0),
                       seasonal=list(order=c(1,0,0),period=52))
        
        # forecast with ARIMA
        # WARNING - do not forecast longer than your lag
        forecast.wks.ahead <- 4
        forecast <- predict(model,n.ahead = wks_ahead_arim)
        # calculate mean prediction and se
        forecast <- as.data.frame(forecast) %>%
                mutate(
                        t.idx = pred.tpoint + 1:wks_ahead_arim,
                        point.pred = exp(pred) - 1,
                        lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
                        upr95.pred = exp(pred + qnorm(0.975) * se) - 1
                )
        # arimas for temp and rain
        model_temp <- arima(DF$air_temperature[1:pred.tpoint], order=c(1,0,0),
                            seasonal=list(order=c(1,0,0),period=52))
        model_rain <- arima(DF$rain_amount[1:pred.tpoint], order=c(1,0,0),
                            seasonal=list(order=c(1,0,0),period=52))
        
        # example: random forest
        
        ### combine forecaste
        # set weight of model to combine
        #
        # make 4 weeks forecast
        #
        # Save into output 
        #
        
        ####evaluate your models (diff metrices)
        # compare with actual value
        #
} ####### end of loop

#####################################################
# calculate the mean prediction error over all predictiona
mean.eval <- vector("numeric",2)
for (i in 1:2){
        mean.eval[i] <-  mean(model.evaluation[i,],na.rm = TRUE)  
}

########################################
#### saving
savename <- paste0("./Data/", filename, ".Rda")
save(mean.eval,file = savename)
write.csv(outputfile,file="./Data/DARIMA_forecastsRF_11.csv",row.names = FALSE)

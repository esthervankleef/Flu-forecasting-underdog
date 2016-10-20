###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "experimental_Rene"

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

### get rid of missing data by truncating
missing.vals <- which(is.na(DF$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(DF)[1] 
DF1 <- DF[(last.miss.val + 1):last.prediction,]

### mutate variables: derivatives and lags
# make derivatives
DF1$dcases <- c(NA,diff(DF1$cases))
#DF1$ddcases <- c(NA,diff(DF1$dcases)) # don't need now
# create different lags
end.timeline <- dim(DF1)[1]
# the predictor with SHORTEST lag gives the timepoint_reference
#
biggest_lag <- 5 # a lag of 5 produces 5 NAs in beginning (in lagged data), and in end (in outcome)

# extend the data.frame to fit in some lagged data
short_lag <- 4
# make NA data frame
add_df <- DF1[1:short_lag,]
add_df[!is.na(add_df)] <- NA 
# add at the botton
DF1 <- rbind(DF1,add_df)

# lag of 4
my_lag <- 4
DF1$timepoint_reference <- lag(seq_along(DF1$cases), n = my_lag)[1 : (end.timeline+short_lag)] 
DF1$cases_l4 <- lag(DF1$cases, n = my_lag)[1 : (end.timeline+short_lag)]
DF1$dcases_l4 <- lag(DF1$dcases, n = my_lag)[1 : (end.timeline+short_lag)]
# lag of 5
my_lag <- 5
DF1$cases_l5 <- lag(DF1$cases,n = my_lag)[1 : (end.timeline+short_lag)]
DF1$dcases_l5 <- lag(DF1$dcases,n = my_lag)[1 : (end.timeline+short_lag)]
# truncate the NA-tail
highest_d <- 1 # a second derivative produces 2 NAs
DF2 <- DF1[(biggest_lag + 1 + highest_d):(end.timeline+short_lag),] 

# decide the points from where to make first.prediction and last.prediction
first.prediction <- 675 # start of prediction window
last.prediction <- DF2$timepoint_reference[dim(DF2)[1]]
# in order to validate own predictions we need observed data: then minus shortest lag
prediction.ws.ahead <- 4
last.prediction <- last.prediction - short_lag - prediction.ws.ahead

### initiate outputfile
num.of.pred <- (last.prediction - first.prediction) + 1
model.evaluation <- matrix(0, nrow = 2,ncol = num.of.pred)
# forcast 
FAO <- data.frame(timepoint_reference = first.prediction:last.prediction) # rf
FAOa <- data.frame(timepoint_reference = first.prediction:last.prediction) # SARIMA

#######################################################
# log transform data
# DO WE NEED TO LOG-TRANSFORM?

#######################################################
#### start the prediction loop
DF <- DF2 # use DF in the loop
i <- 0
for (pred.tpoint in first.prediction:last.prediction){
  i = i + 1
  print(i) # print where you are in the loop
  
  ###############################################
  ################# model fitting
  # example: RF
  
  # make train data
  df_point <- which(DF$timepoint_reference == pred.tpoint)
  trainDF <- DF[1:df_point,]
  # create fit control 
  # timeslices for timeseries, from 2 seasons
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 104,
                             horizon = 4,
                             fixedWindow = TRUE)
  # decide on the input for the forest
  my_input <- c("week","cases_l4","dcases_l4",
                "cases_l5","dcases_l5")
  # train model
  Fit1 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = NULL, 
                tuneLength = 3)
  ####
  ### forecast: no longer than the shortest lag!
  rf_predictions <- predict(Fit1,DF[df_point+1:4,my_input]) # point predictions
  
  ##################################################
  # example: ARIMA

  # fit model
  Fit2 <- Arima(trainDF$cases, order=c(1,0,0),
                seasonal=list(order=c(1,0,0),period=52), lambda = 1)
  ####
  ### forecast
  wks_ahead_arim <- 4
  ar_predictions <- forecast.Arima(Fit2,4)

  # observed values
  observed <- DF$cases[df_point+1:4]
  #####################
  ### output
  
  ### save random forest
  final_predict <- rf_predictions
  # save 1 weeks forecast and observed
  FAO$f1w[i] <- final_predict[1]
  FAO$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO$f2w[i] <- final_predict[2]
  FAO$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO$f3w[i] <- final_predict[3]
  FAO$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO$f4w[i] <- final_predict[4]
  FAO$o4w[i] <- observed[4]
  
  ### save ARIMA
  final_predict <- as.numeric(ar_predictions$mean)
  # save 1 weeks forecast and observed
  FAOa$f1w[i] <- final_predict[1]
  FAOa$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAOa$f2w[i] <- final_predict[2]
  FAOa$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAOa$f3w[i] <- final_predict[3]
  FAOa$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAOa$f4w[i] <- final_predict[4]
  FAOa$o4w[i] <- observed[4]
} ####### end of loop
#####################################################
# evaluate
mse_rf_4w <- mean((FAO$o4w - FAO$f4w)^2)

#####################################################
# plot
# rf
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse_rf_4w,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")

########################################
#### saving
savename <- paste0("./Data/", script_name, ".Rda")
save(FAO,FAOa,file = savename)
#write.csv(outputfile,file="./Data/DARIMA_forecastsRF_11.csv",row.names = FALSE)

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
#### load the data
load("./Data/data_manip.Rda")

DF <- usflu

#######################################################
# log transform data
# DO WE NEED TO LOG-TRANSFORM?
# For ARIMA and LASSO yes. Is done in the below
DF <- DF %>% 
  dplyr::select(-region,-region.type) %>% mutate(cases = as.numeric(as.character(x.weighted.ili)),
                                                 cases = log(cases+1)) # NA is one missing value which was coded as X

# only keep DF
rm(usflu)

########################################
### get rid of missing data by truncating
missing.vals <- which(is.na(DF$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(DF)[1] 
DF1 <- DF[(last.miss.val + 1):last.prediction,]

### mutate variables: Add total number of cases from previous season as variable
# identify where there are 53 weeks 
week53 = DF1$year[which(DF1$week == 53)]
# give each timepoint the season name
season=NULL
for(i in 1:(length(unique(DF1$year))-1)){
  if(!unique(DF1$year)[i] %in% week53){
    s = rep(i,52)
  }
  else{
    s = rep(i,53)
  }
  season = c(season,s)
}
DF1$season = season

# create a sum over seasons 
seas = DF1%>%group_by(season) %>% summarise(seas_total = sum(cases)) %>% mutate(seas_total_l1=lag(seas_total,1))
# add as covariate
DF1 = left_join(DF1, seas, by="season")

### mutate predictive variables: derivatives and lags
# get length
end.timeline <- dim(DF1)[1]
# make first derivative
DF1$dcases <- c(NA,diff(DF1$cases)) 
# makes second derivative
#DF1$ddcases <- c(NA,diff(DF1$dcases)) # don't need now

# extend the data.frame to fit in lagged variables
short_lag <- 4
# make NA data frame
add_df <- DF1[1:short_lag,]
add_df[!is.na(add_df)] <- NA 
# add at the bottom
DF1 <- rbind(DF1,add_df)

# the predictor with shortest lag gives the timepoint_reference
# lag of 4
my_lag <- 4

DF1 = DF1[1 : (end.timeline+short_lag),] %>% mutate(timepoint_reference = lag(seq_along(cases), n = 4), # This is created so we can keep track of what data was available at the time from which the predictions are made
                                        cases_l4 = lag(cases, n = 4),
                                        dcases_l4 = lag(dcases, n = 4), 
                                        cases_l5 = lag(cases,n = 5),
                                        dcases_l5 = lag(dcases,n = 5))

# truncate the NA-tail
#biggest_lag <- 5 # will introduce NA 
biggest_lag <- 53 # the season lag produces 53 NAs
#highest_d <- 2 # a second derivative produces 2 NAs --> I think we only have first derivative which produces one lag; but I suppose this was meant for the variable you said earlier that is 'not needed for now' 
DF2 <- DF1[(biggest_lag):end.timeline,] 

# decide the time points from where to make first.prediction and last.prediction
first.prediction <- DF2$timepoint_reference[which(DF2$weekname=="2015-32")] # week from where to make the first prediction
# last.prediction; to validate own predictions we need observed data: then minus shortest lag
prediction.ws.ahead <- 4
last.prediction <- DF2$timepoint_reference[dim(DF2)[1] - short_lag - prediction.ws.ahead]
# make numeric timepoints
prstart <- which(DF2$timepoint_reference == first.prediction)
prstop <- which(DF2$timepoint_reference == last.prediction)

### initiate outputfile
num.of.pred <- (prstop - prstart) + 1
# forcast 
FAO <- data.frame(timepoint_reference = prstart:prstop) # rf
FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

#######################################################
#### start the prediction loop
DF <- DF2 # use DF in the loop
i <- 0
for (pred.tpoint in prstart:prstop){
  i = i + 1
  print(i) # print where you are in the loop
  
  ###############################################
  ################# model fitting
  # example: RF
  
  # make train data
  df_point <- pred.tpoint # the data.frame row
  trainDF <- DF[1:df_point,]
  # create fit control 
  # timeslices for timeseries, from 2 seasons, horizon 4 weeks
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 104,
                             horizon = 4,
                             fixedWindow = TRUE)
  # decide on the input for the forest
  my_input <- c("week","cases_l4","dcases_l4",
                "cases_l5")
  # train model
  Fit1 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = NULL, 
                tuneLength = 3,
                importance = FALSE)
  ####
  ### forecast: no longer than the shortest lag!
  rf_predictions <- predict(Fit1,DF[df_point+1:4,my_input]) # point predictions
  # obtain feature rank
  #varImp(Fit1)
  
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
  observed <- DF$x.weighted.ili[df_point+1:4]
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

########################################
#### save & load
savename <- paste0("./Data/", "experimental_Rene", ".Rda")
save(FAO,FAOa,mse_rf_4w,mse_AR_4w,mse_ref_4w,file = savename)
# loading (from here can be run without re-running the loop)
load(savename)

#####################################################
# plot
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

#write.csv(outputfile,file="./Data/DARIMA_forecastsRF_11.csv",row.names = FALSE)
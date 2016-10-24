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


### get rid of missing data by truncating
missing.vals <- which(is.na(DF$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(DF)[1] 
DF1 <- DF[(last.miss.val + 1):last.prediction,]


### mutate variables: derivatives and lags
# Add total number of cases from previous season as variable
week53 = DF1$year[which(DF1$week == 53)]

season=NULL
for(i in 1:(length(unique(DF1$year))-1)){
  if(!unique(DF1$year)[i] %in% week53){
    s = rep(i,52)
  }
  else{
    print(i)
    s = rep(i,53)
  }
  season = c(season,s)
}

DF1$season = season

seas = DF1%>%group_by(season) %>% summarise(seas_total = sum(log(cases+1))) %>% mutate(seas_total_l1=lag(seas_total,1))

DF1 = left_join(DF1, seas)

# make derivatives
DF1$dcases <- c(NA,diff(DF1$cases)) 
#DF1$ddcases <- c(NA,diff(DF1$dcases)) # don't need now
# create different lags
end.timeline <- dim(DF1)[1]


# the predictor with shortest lag gives the timepoint_reference
# lag of 4
DF1 = DF1[1 : end.timeline,] %>% mutate(timepoint_reference = lag(seq_along(cases), n = 4), # This is created so we can keep track of what data was available at the time from which the predictions are made
                                        cases_l4 = lag(cases, n = 4),
                                        dcases_l4 = lag(dcases, n = 4), 
                                        cases_l5 = lag(cases,n = 5),
                                        dcases_l5 = lag(dcases,n = 5))

# truncate the NA-tail
biggest_lag <- 53 # the season lag produces 53 NAs
#highest_d <- 2 # a second derivative produces 2 NAs --> I think we only have first derivative which produces one lag; but I suppose this was meant for the variable you said earlier that is 'not needed for now' 
DF2 <- DF1[(biggest_lag):end.timeline,] 

# decide the points from where to make first.prediction and last.prediction
first.prediction <- 675 # predict in a window of
last.prediction <- DF2$timepoint_reference[dim(DF2)[1]]

### initiate outputfile
num.of.pred <- (last.prediction - first.prediction) + 1
model.evaluation <- matrix(0, nrow = 2,ncol = num.of.pred)
#
FAO <- data.frame(timepoint_reference = first.prediction:last.prediction)

#######################################################
#### start the prediction loop
DF <- DF2 # use DF in the loop
i <- 0
for (pred.tpoint in first.prediction:last.prediction){
        i = i + 1
        print(i) # show where you are in the loop
        
        ###############################################
        ################# model fitting
        # example: RF
        
        # make train data
        df_point <- which(DF$timepoint_reference == pred.tpoint)
        trainDF <- DF[1:df_point,]
        # create fit control 
        # timeslices for timeseries
        fitControl <- trainControl(method = "timeslice",
                                   initialWindow = 104,
                                   horizon = 10,
                                   fixedWindow = TRUE)
        # input for the forest
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
        rf_predictions <- predict(Fit1,DF[df_point+1:4,]) # point predictions

        
        
        ##################################################
        # example: ARIMA
        
        # fit model
        Fit2 <- Arima(trainDF$cases, order=c(1,0,0),
                       seasonal=list(order=c(1,0,0),period=52), lambda = 1)
        ####
        ### forecast
        wks_ahead_arim <- 4
        ar_predictions <- forecast.Arima(Fit2,4)
        
        
        ### combine forecaste
        # set weight of model to combine
        weightfor_rf <- 1
        final_predict <- weightfor_rf*rf_predictions + 
          (1-weightfor_rf)*as.numeric(ar_predictions$mean)
        # actual values
        observed <- DF$x.weighted.ili[df_point+1:4]
        
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
        
        ####evaluate models
        ###
} ####### end of loop

#####################################################
# plot
#plot(FAO)

########################################
#### saving
savename <- paste0("./Data/", script_name, ".Rda")
save(FAO,file = savename)
#write.csv(outputfile,file="./Data/DARIMA_forecastsRF_11.csv",row.names = FALSE)

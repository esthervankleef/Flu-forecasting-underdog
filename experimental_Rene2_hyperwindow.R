###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "experimental_Rene2_hyperwindow"

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
DF <- usflu # already truncated and without NA (otherwise use usflu_allyears)

#######################################################
# log transform data
# DO WE NEED TO LOG-TRANSFORM?
# For ARIMA and LASSO yes. Is done in the below
DF1 <- DF %>% 
  dplyr::select(-region,-region.type) %>% 
  mutate(cases = as.numeric(as.character(x.weighted.ili)),
         cases = log(cases+1)) # NA is one missing value which was coded as X
# only keep DF
rm(usflu, usflu_allyears)

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
train_set_start <- DF2$timepoint_reference[which(DF2$weekname=="2011-20")] # first week included in the training set
first.prediction <- DF2$timepoint_reference[which(DF2$weekname=="2012-30")] # week from where to make the first prediction
# last.prediction; to validate own predictions we need observed data: then minus shortest lag
prediction.ws.ahead <- 4
last.prediction <- DF2$timepoint_reference[dim(DF2)[1] - short_lag - prediction.ws.ahead]
# make numeric timepoints
trstart <- which(DF2$timepoint_reference == train_set_start) # star of training
prstart <- which(DF2$timepoint_reference == first.prediction)
prstop <- which(DF2$timepoint_reference == last.prediction)

### initiate outputfile
num.of.pred <- (prstop - prstart) + 1
# forcast 
FAO1 <- data.frame(timepoint_reference = prstart:prstop) # rf
FAO2 <- data.frame(timepoint_reference = prstart:prstop) # SARIMA
FAO3 <- data.frame(timepoint_reference = prstart:prstop) # SARIMA
FAO4 <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

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
  trainDF <- DF[trstart:df_point,]
  # create fit control 
  ####### window = 20
  # timeslices for timeseries, from 2 seasons, horizon 4 weeks
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 20,
                             horizon = 4,
                             fixedWindow = TRUE)
  # decide on the input for the forest
  my_input <- c("week","cases_l4","dcases_l4")
  # train model
  Fit1 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = NULL, 
                tuneLength = 3,
                importance = FALSE)
  ### forecast: no longer than the shortest lag!
  rf_predictions1 <- predict(Fit1,DF[df_point+1:4,my_input]) # point predictions
  
  ####### window = 30
  # timeslices for timeseries, from 2 seasons, horizon 4 weeks
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 30,
                             horizon = 4,
                             fixedWindow = TRUE)
  # decide on the input for the forest
  my_input <- c("week","cases_l4","dcases_l4")
  # train model
  Fit2 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = NULL, 
                tuneLength = 3,
                importance = FALSE)
  ### forecast: no longer than the shortest lag!
  rf_predictions2 <- predict(Fit2,DF[df_point+1:4,my_input]) # point predictions
  
  ####### window = 40
  # timeslices for timeseries, from 2 seasons, horizon 4 weeks
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 30,
                             horizon = 4,
                             fixedWindow = TRUE)
  # decide on the input for the forest
  my_input <- c("week","cases_l4","dcases_l4")
  # train model
  Fit3 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = NULL, 
                tuneLength = 3,
                importance = FALSE)
  ### forecast: no longer than the shortest lag!
  rf_predictions3 <- predict(Fit3,DF[df_point+1:4,my_input]) # point predictions
  
  ####### window = 52
  # timeslices for timeseries, from 2 seasons, horizon 4 weeks
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 52,
                             horizon = 4,
                             fixedWindow = TRUE)
  # decide on the input for the forest
  my_input <- c("week","cases_l4","dcases_l4")
  # train model
  Fit4 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = NULL, 
                tuneLength = 3,
                importance = FALSE)
  ### forecast: no longer than the shortest lag!
  rf_predictions4 <- predict(Fit4,DF[df_point+1:4,my_input]) # point predictions
  
  # observed values
  observed <- DF$x.weighted.ili[df_point+1:4]
  #####################
  ### output
  
  ### save random forest
  final_predict <- rf_predictions1
  # save 1 weeks forecast and observed
  FAO1$f1w[i] <- exp(final_predict[1])-1
  FAO1$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO1$f2w[i] <- exp(final_predict[2])-1
  FAO1$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO1$f3w[i] <- exp(final_predict[3])-1
  FAO1$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO1$f4w[i] <- exp(final_predict[4])-1
  FAO1$o4w[i] <- observed[4]
  
  ### save random forest
  final_predict <- rf_predictions2
  # save 1 weeks forecast and observed
  FAO2$f1w[i] <- exp(final_predict[1])-1
  FAO2$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO2$f2w[i] <- exp(final_predict[2])-1
  FAO2$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO2$f3w[i] <- exp(final_predict[3])-1
  FAO2$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO2$f4w[i] <- exp(final_predict[4])-1
  FAO2$o4w[i] <- observed[4]
  
  ### save random forest
  final_predict <- rf_predictions3
  # save 1 weeks forecast and observed
  FAO3$f1w[i] <- exp(final_predict[1])-1
  FAO3$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO3$f2w[i] <- exp(final_predict[2])-1
  FAO3$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO3$f3w[i] <- exp(final_predict[3])-1
  FAO3$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO3$f4w[i] <- exp(final_predict[4])-1
  FAO3$o4w[i] <- observed[4]
  
  ### save random forest
  final_predict <- rf_predictions4
  # save 1 weeks forecast and observed
  FAO4$f1w[i] <- exp(final_predict[1])-1
  FAO4$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO4$f2w[i] <- exp(final_predict[2])-1
  FAO4$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO4$f3w[i] <- exp(final_predict[3])-1
  FAO4$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO4$f4w[i] <- exp(final_predict[4])-1
  FAO4$o4w[i] <- observed[4]
} ####### end of loop
#####################################################
# evaluate
mse_rf1_4w <- mean((FAO1$o4w - FAO1$f4w)^2)
mse_rf2_4w <- mean((FAO2$o4w - FAO2$f4w)^2)
mse_rf3_4w <- mean((FAO3$o4w - FAO3$f4w)^2)
mse_rf4_4w <- mean((FAO4$o4w - FAO4$f4w)^2)
mse_ref_4w <- mean((FAO$o4w - lag(FAO$o4w,n = 4))^2,na.rm = TRUE)


eval <- data.frame(mse_rf1_4w=mse_rf1_4w,
                   mse_rf2_4w=mse_rf2_4w,
                   mse_rf3_4w=mse_rf3_4w,
                   mse_rf4_4w=mse_rf4_4w)

########################################
#### save & load
savename <- paste0("./Data/", "experimental_Rene", ".Rda")
save(FAO1,FAO2,FAO3,FAO4,eval,file = savename)
# loading (from here can be run without re-running the loop)
load(savename)

#####################################################
# plot
par(mfrow=c(1,1))
#
# residuals reference
my_residuals_r <- FAO1$o4w - lag(FAO1$o4w,n = 4) # observed minus model
my_residuals_r <- abs(my_residuals_r)/FAO1$o4w
fitsp0=smooth.spline (FAO1$timepoint_reference[-(1:4)] ,my_residuals_r[-(1:4)] ,df =4)
#
FAO <- FAO1
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO$o4w
fitsp1=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
#
FAO <- FAO2
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO$o4w
fitsp2=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
#
FAO <- FAO3
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO$o4w
fitsp3=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
#
FAO <- FAO4
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO$o4w
fitsp4=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
#
plot(fitsp0,col =" red " ,lwd =2, ylim=c(0,1))
lines(fitsp1,col =" blue " ,lwd =2)
lines(fitsp2,col =" black " ,lwd =2)
lines(fitsp3,col =" darkblue " ,lwd =2)
lines(fitsp4,col =" darkgreen " ,lwd =2)


# reference (naive)
my_title <- paste("Ref 4-weeks, MSE =", as.character(round(mse_ref_4w,digits = 4)))
plot(FAO1$timepoint_reference,FAO1$o4w,pch=19, col="black"); title(my_title)
points(FAO1$timepoint_reference,c(lag(FAO1$o4w,n = 4)),pch=20,col="darkred")
# residuals reference
my_residuals_r <- FAO1$o4w - lag(FAO1$o4w,n = 4) # observed minus model
my_residuals_r <- abs(my_residuals_r)/FAO1$o4w
my_title <- "Absolute Residuals"
fitsp1=smooth.spline (FAO1$timepoint_reference[-(1:4)] ,my_residuals_r[-(1:4)] ,df =4)
plot(FAO1$timepoint_reference,my_residuals_r,pch=19, col="darkblue",ylim=c(0,1)); title(my_title)
lines(fitsp1,col =" red " ,lwd =2)

# rf1
mse <- mse_rf1_4w
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
# residuals
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO1$o4w
my_title <- "Absolute Residuals"
fitsp2=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
plot(FAO$timepoint_reference,my_residuals,pch=19, col="darkblue",ylim=c(0,1)); title(my_title)
lines(fitsp2,col =" red " ,lwd =2)

# rf2
FAO <- FAO2
mse <- mse_rf2_4w
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
# residuals
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO1$o4w
my_title <- "Absolute Residuals"
fitsp2=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
plot(FAO$timepoint_reference,my_residuals,pch=19, col="darkblue",ylim=c(0,1)); title(my_title)
lines(fitsp2,col =" red " ,lwd =2)

# rf3
FAO <- FAO3
mse <- mse_rf3_4w
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
# residuals
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO1$o4w
my_title <- "Absolute Residuals"
fitsp2=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
plot(FAO$timepoint_reference,my_residuals,pch=19, col="darkblue",ylim=c(0,1)); title(my_title)
lines(fitsp2,col =" red " ,lwd =2)

# rf4
FAO <- FAO4
mse <- mse_rf4_4w
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
# residuals
my_residuals <-  FAO$o4w - FAO$f4w  # observed minus model
my_residuals <- abs(my_residuals)/FAO1$o4w
my_title <- "Absolute Residuals"
fitsp2=smooth.spline (FAO$timepoint_reference ,my_residuals ,df =4)
plot(FAO$timepoint_reference,my_residuals,pch=19, col="darkblue",ylim=c(0,1)); title(my_title)
lines(fitsp2,col =" red " ,lwd =2)

#write.csv(outputfile,file="./Data/DARIMA_forecastsRF_11.csv",row.names = FALSE)


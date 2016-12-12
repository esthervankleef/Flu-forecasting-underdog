###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "caret_lasso_ridge_template"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(glmnet)
require(RCurl)
require(prettyR)
library(e1071)

########################################
#### load data
# flu data
load("./Data/data_manip_2.Rda")

############################################
### make timepoints
# decide the time points from where to make first.prediction and last.prediction
first.prediction <- "2015-35" # week from where to make the first prediction
# last.prediction, is made from the most_current_week
last.prediction <- most_current_week
# where to put the start given the last prediction
last.prediction_df <- which(DF$weekname == last.prediction)
DF$weekname[last.prediction_df - 200]

# make numeric for final training point = prediction point
prstart <- which(DF$weekname == first.prediction)
prstop <- which(DF$weekname == last.prediction)
# all prediction points
pred_vector <- prstart:prstop

# start training points
#train_start_vector <- pred_vector - (190-10)
train_start_vector <- pred_vector - (100)

### initiate outputfile
num.of.pred <- (prstop - prstart) + 1

### Remove NAs from google data
googleNA = which(is.na(DF$gdoctor))
googleNA = googleNA[googleNA<prstop]
#
g_words = c("gfever","gheadache","gdoctor","gshivering","gcough")
DF[googleNA,g_words] = DF[googleNA-1,g_words]

# initiate forecast 
FAO <- data.frame(timepoint_reference = prstart:prstop) # LASSO
#
FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

##### call functions
source("functions.R")

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
  
  ###############################################
  # make train data
  # Choose predictors
  choose_predictors1 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "big_holidays","inschool","seas_total","temp_av","temp_av")
  choose_predictors2 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "big_holidays","inschool","seas_total","temp_av","temp_av")
  choose_predictors3 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "big_holidays","inschool","seas_total","temp_av","temp_av")
  choose_predictors4 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "big_holidays","inschool","seas_total","temp_av")
  
  preddats = list(choose_predictors1,choose_predictors2,choose_predictors3,choose_predictors4)
  
  # Choose lags
  choose_lags1 <- c(0,0,0,1,2,2,0,0,52,1,4)
  choose_lags2 <- c(0,0,0,2,3,3,0,0,52,2,4)
  choose_lags3 <- c(0,0,0,3,4,4,0,0,52,3,4)
  choose_lags4 <- c(0,0,0,4,5,5,0,0,52,4)
  
  lagdats = list(choose_lags1,choose_lags2,choose_lags3,choose_lags4)
  
  # make predictor matrix 
  X1 <- as.matrix(my_predictors_lag(choose_predictors1,choose_lags1,name_predictors,DF,tchoice_v))
  X2 <- as.matrix(my_predictors_lag(choose_predictors2,choose_lags2,name_predictors,DF,tchoice_v))
  X3 <- as.matrix(my_predictors_lag(choose_predictors3,choose_lags3,name_predictors,DF,tchoice_v))
  X4 <- as.matrix(my_predictors_lag(choose_predictors4,choose_lags4,name_predictors,DF,tchoice_v))
  
  # Outcome
  cases <- DF$cases[tchoice_v]
  Y = DF$cases[tchoice_v]
  data1 = cbind(cases, X1)
  data2 = cbind(cases, X2)
  data3 = cbind(cases, X3)
  data4 = cbind(cases, X4)
  
  Xdats = list(data1,data2,data3,data4)
  
  ## set up validation process
  seed = 201
  set.seed(seed)

  glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), # alpha = 1 is LASSO alpha = 0 is ridge
                             lambda = seq(0, 1, length = 100))
  glmnet_ctrl <- trainControl(method = "timeslice",
                            initialWindow = 10,
                            horizon = 10,
                            fixedWindow = TRUE)
  
  h_weights = c(1*52) # Put higher weights on last 1 year observations
  weight_increase <- 1
  
  # fit LASSO regression
  
  # 1-week prediction
  Fit0.1 <- suppressWarnings(train(cases ~ ., data = data1,
                    method = "glmnet",
                    weights = c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights)),
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl))
  
  # 2-week prediction
  Fit0.2 <- suppressWarnings(train(cases ~ ., data = data2,
                  method = "glmnet",
                  weights = c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights)),
                  preProcess = c("center", "scale"),
                  tuneGrid = glmnet_grid,
                  trControl = glmnet_ctrl))
  
  # 3-week prediction
  Fit0.3 <- suppressWarnings(train(cases ~ ., data = data3,
                  method = "glmnet",
                  weights = c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights)),
                  preProcess = c("center", "scale"),
                  tuneGrid = glmnet_grid,
                  trControl = glmnet_ctrl))
  
  
  # 4-week prediction
  Fit0.4 <- suppressWarnings(train(cases ~ ., data = data4,
                  method = "glmnet",
                  weights = c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights)),
                  preProcess = c("center", "scale"),
                  tuneGrid = glmnet_grid,
                  trControl = glmnet_ctrl))
  
  models = list(Fit0.1,Fit0.2,Fit0.3,Fit0.4)
  
  # Forecast
  la.predictions = list()
  ### forecast: no longer than the shortest lag!
  for(w in c(1:4)){
    # weeks ahead
    wks_ahead = w
    # make vector
    tchoice_forc_v <- df_point + 1:wks_ahead
    #
    covars_for_forecast <- as.matrix(my_predictors_lag(preddats[[w]],lagdats[[w]],name_predictors,DF,tchoice_forc_v))
    predictions <- predict(models[[w]], covars_for_forecast) # point predictions
    # save prediction
    la.predictions[[w]] = predictions[wks_ahead]
  }
  
  names(la.predictions) = c("fw1","fw2","fw3","fw4")
  # observed values 
  observed <- DF$cases[tchoice_forc_v]
  
  if(i == max(pred_vector))
  par(mfrow=c(2,2))
  plot(Fit0.1)
  plot(Fit0.2)
  plot(Fit0.3)
  plot(Fit0.4)
  
  #####################
  ### output  
  ### save LASSO
  # save 1 weeks forecast and observed
  FAO$f1w[i] <- la.predictions$fw1
  FAO$o1w[i] <- observed[1]
  # save 2 weeks forecast and observed
  FAO$f2w[i] <- la.predictions$fw2
  FAO$o2w[i] <- observed[2]
  # save 3 weeks forecast and observed
  FAO$f3w[i] <- la.predictions$fw3
  FAO$o3w[i] <- observed[3]
  # save 4 weeks forecast and observed
  FAO$f4w[i] <- la.predictions$fw4
  FAO$o4w[i] <- observed[4]
  
} ####### end of loop
# cut away last 4 weeks, where you have nothing to compare to
its_length <- dim(FAO)[1]
FAO <- FAO[1:(its_length-4),]

# evaluate
#####################################################

# LASSO MSE
mse_LR = data.frame(cbind(model = rep("Lasso_Ridge",1), mse1w = rep(NA,1),
                          mse2w = rep(NA,1),mse3w = rep(NA,1),
                          mse4w = rep(NA,1)))
mse_LR[2] <- mean((FAO$o1w - FAO$f1w)^2)
mse_LR[3] <- mean((FAO$o2w - FAO$f2w)^2)
mse_LR[4] <- mean((FAO$o3w - FAO$f3w)^2)
mse_LR[5] <- mean((FAO$o4w - FAO$f4w)^2)

# Reference model MSE
mse_ref = data.frame(cbind(model = rep("Model0",1), mse1w = rep(NA,1),
                           mse2w = rep(NA,1),mse3w = rep(NA,1),
                           mse4w = rep(NA,1)))
mse_ref[2] <- mean((FAO$o1w - lag(FAO$o1w,n = 1))^2,na.rm = TRUE)
mse_ref[3] <- mean((FAO$o1w - lag(FAO$o1w,n = 2))^2,na.rm = TRUE)
mse_ref[4] <- mean((FAO$o1w - lag(FAO$o1w,n = 3))^2,na.rm = TRUE)
mse_ref[5] <- mean((FAO$o1w - lag(FAO$o1w,n = 4))^2,na.rm = TRUE)
# bind together
eval <- rbind(mse_ref,mse_LR)
print(eval)
########################################
#### save & load
savename <- paste0("./Data/", script_name , ".Rda")
save(FAO,eval,file = savename)
# loading (from here can be run without re-running the loop)
load(savename)

#####################################################
# plot
par(mfrow=c(2,1)) 
# reference (naive)
my_title <- paste("Ref 4-weeks, MSE =", as.character(round(mse_ref$mse4w,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,c(lag(FAO$o4w,n = 4)),pch=20,col="darkred")
# rf
my_title <- paste("RF 4-weeks, MSE =", as.character(round(mse_RF$mse4w,digits = 4)))
plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
par(mfrow=c(1,1)) 


#####################################################
# Calculate SD of residuals
pred = data.frame(cbind(target = c("1week","2week","3week","4week"),mean = rep(0,4), sd = rep(0,4)))
my_sd = NULL
# fill in sd for each lag 
w <- 1; my_v <- (FAO[w*2+1] - FAO[w*2])[[1]]; my_sd <-  c(my_sd,sd(my_v))
w <- 2; my_v <- (FAO[w*2+1] - FAO[w*2])[[1]]; my_sd <-  c(my_sd,sd(my_v))
w <- 3; my_v <- (FAO[w*2+1] - FAO[w*2])[[1]]; my_sd <-  c(my_sd,sd(my_v))
w <- 4; my_v <- (FAO[w*2+1] - FAO[w*2])[[1]]; my_sd <-  c(my_sd,sd(my_v))
pred$sd = my_sd

#####################################################
# Generate predictions
df_point = which(DF$weekname == most_current_week)
my_mean=NULL
# loop through the 
for(w in c(1:4)){
  wks_ahead = w
  # make vector
  tchoice_forc_v <- df_point + 1:wks_ahead
  #
  covars_for_forecast <- as.matrix(my_predictors_lag(preddats[[w]],lagdats[[w]],name_predictors,DF,tchoice_forc_v))
  predictions <- predict(models[[w]], covars_for_forecast) # point predictions
  # save prediction
  la.predictions[[w]] = predictions[wks_ahead]
  my_mean = c(my_mean, la.prediction)
}
pred$mean = my_mean

#####################################################
# Calculate probability of bins
breaks.in = c(seq(0,13.0,0.1)) # Everything 13 and above will be put together in one bin

prob.forecast = data.frame(cbind(Bin_start_incl = breaks.in,w1 = rep(NA,length(breaks.in)),w2 = rep(NA,length(breaks.in)),
                                 w3 = rep(NA,length(breaks.in)),w4 = rep(NA,length(breaks.in))))

#
par(mfrow=c(2,2))
for(w in c(1:4)){
  prob.forecast[,w+1] = gen.prob.distr(mean=pred$mean[w], sd=pred$sd[w], log.scale=T, breaks.in=breaks.in)
  plot(breaks.in,prob.forecast[,w+1], type="l", ylab="density", main=paste0(w,"-weeks prediction density"), xlab="breaks")
  
}
par(mfrow=c(1,1))

#####################################################
# Store forecasts
results.lr = read.csv("./Forecasts/Submission_template.csv")
results.lr$Value = NA

targets = c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead")
for(w in c(1:4)){
  nat_week = which(results.lr$Target==targets[w] & results.lr$Location=="US National")
  point = exp(pred$mean[w])-1
  results.lr$Value[nat_week] = c(point,prob.forecast[,w+1]) 
  # check that all on right scale
  print(breaks.in[which.max(prob.forecast[,w+1])])
  print(point)
}

#####################################################
# Save file
savename <- paste0("./Forecasts/", script_name, ".csv")
write.csv(results.la,file = savename)

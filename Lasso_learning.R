###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "lasso_chall_template"

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
one <- list(data.frame(timepoint_reference = prstart:prstop, f1w=NA,o1w=NA,f2w=NA,o2w=NA,f3w=NA,o3w=NA,f4w=NA,o4w=NA))
su = seq(0,1,0.001)
FAO=list()
for(i in 1:length(su)){
  FAO = c(FAO,one)
}
#
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
  
  ###############################################
  # make train data
  # Choose predictors
  choose_predictors1 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "gfever","gheadache","gdoctor","gshivering","gcough",
                          "big_holidays","inschool","seas_total","temp_av","temp_av")
  choose_predictors2 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "gfever","gheadache","gdoctor","gshivering","gcough",
                          "big_holidays","inschool","seas_total","temp_av","temp_av")
  choose_predictors3 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "gfever","gheadache","gdoctor","gshivering","gcough",
                          "big_holidays","inschool","seas_total","temp_av","temp_av")
  choose_predictors4 <- c("week","sin_week","cos_week", "cases","cases", "dcases",
                          "gfever","gheadache","gdoctor","gshivering","gcough",
                          "big_holidays","inschool","seas_total","temp_av")
  
  preddats = list(choose_predictors1,choose_predictors2,choose_predictors3,choose_predictors4)
  
  # Choose lags
  choose_lags1 <- c(0,0,0,1,2,2,1,1,1,1,1,0,0,1,52,4)
  choose_lags2 <- c(0,0,0,2,3,3,2,2,2,2,2,0,0,1,52,4)
  choose_lags3 <- c(0,0,0,3,4,4,3,3,3,3,3,0,0,1,52,4)
  choose_lags4 <- c(0,0,0,4,5,5,4,4,4,4,4,0,0,52,4)
  
  lagdats = list(choose_lags1,choose_lags2,choose_lags3,choose_lags4)
  
  # make predictor matrix 
  X1 <- as.matrix(my_predictors_lag(choose_predictors1,choose_lags1,name_predictors,DF,tchoice_v))
  X2 <- as.matrix(my_predictors_lag(choose_predictors2,choose_lags2,name_predictors,DF,tchoice_v))
  X3 <- as.matrix(my_predictors_lag(choose_predictors3,choose_lags3,name_predictors,DF,tchoice_v))
  X4 <- as.matrix(my_predictors_lag(choose_predictors4,choose_lags4,name_predictors,DF,tchoice_v))
  
  Xdats = list(X1,X2,X3,X4)
  
  # Outcome
  Y <- DF$cases[tchoice_v]
  
  ###############################################
  # train LASSO
  
  h_weights = c(1*52) # Put higher weights on last 1 year observations
  weight_increase <- 2
  # fit LASSO regression
  
  # 1-week prediction
  Fit0.1 = glmnet(y=Y,x=X1, family="gaussian",
                  weights=c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights))) # glmnet is fitting with alpha=1 by default, which means LASSO is used for parameter selection;
                                                                            
  # 2-week prediction
  Fit0.2 = glmnet(y=Y,x=X2, family="gaussian",
                  weights=c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights)))
  # 3-week prediction
  Fit0.3 = glmnet(y=Y,x=X3, family="gaussian",
                  weights=c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights))) 
  # 4-week prediction
  Fit0.4 = glmnet(y=Y,x=X4, family="gaussian",
                  weights=c(rep(1,length(Y)-h_weights), rep(weight_increase, h_weights))) 
  
  models = list(Fit0.1,Fit0.2,Fit0.3,Fit0.4)
  
  # Forecast
  la.predictions = list()
  ### forecast: no longer than the shortest lag!
  for(w in c(1:4)){
    wks_ahead = w
    tchoice_forc_v <- df_point + 1:wks_ahead
    covars_for_forecast <- as.matrix(my_predictors_lag(preddats[[w]],lagdats[[w]],name_predictors,DF,tchoice_forc_v))
    predictions <- predict.glmnet(models[[w]], n.ahead=wks_ahead,s=su, 
                                   newx=covars_for_forecast)
    la.predictions[[w]] = data.frame(predictions)
  }
  names(la.predictions) = c("fw1","fw2","fw3","fw4")
  # observed values 
  observed <- DF$cases[tchoice_forc_v]
  
  ##################################################
  # SARIMA
  # fit model
  #  Fit2 <- Arima(DF$cases[tchoice_v], order=c(1,0,0),
  #              seasonal=list(order=c(1,0,0),period=52))
  # ## forecast
  # wks_ahead_arim <- 4
  # ar_predictions <- forecast.Arima(Fit2,4)
  # 
  #####################
  ### output  
  ### save LASSO
  #final_predict = la_predictions #
  for(s in 1:length(su)){
    # save 1 weeks forecast and observed
    FAO[[s]]$f1w[i] <- la.predictions$fw1[[s]]
    FAO[[s]]$o1w[i] <- observed[1]
    # save 2 weeks forecast and observed
    FAO[[s]]$f2w[i] <- la.predictions$fw2[[s]][2]
    FAO[[s]]$o2w[i] <- observed[2]
    # save 3 weeks forecast and observed
    FAO[[s]]$f3w[i] <- la.predictions$fw3[[s]][3]
    FAO[[s]]$o3w[i] <- observed[3]
    # save 4 weeks forecast and observed
    FAO[[s]]$f4w[i] <- la.predictions$fw4[[s]][4]
    FAO[[s]]$o4w[i] <- observed[4]
  }
  # ### save ARIMA
  # final_predict <- as.numeric(ar_predictions$mean)
  # # save 1 weeks forecast and observed
  # FAOa$f1w[i] <- final_predict[1]
  # FAOa$o1w[i] <- observed[1]
  # # save 2 weeks forecast and observed
  # FAOa$f2w[i] <- final_predict[2]
  # FAOa$o2w[i] <- observed[2]
  # # save 3 weeks forecast and observed
  # FAOa$f3w[i] <- final_predict[3]
  # FAOa$o3w[i] <- observed[3]
  # # save 4 weeks forecast and observed
  # FAOa$f4w[i] <- final_predict[4]
  # FAOa$o4w[i] <- observed[4]
  # 
} ####### end of loop

#####################################################
# What is kept in final models (using internal build cross-validation of glmnet, not my s values
# I think these should be more or less similar)
tchoice_v <- train_start:df_point

dat_coefs = list()
par(mfrow=c(2,2))
for(w in c(1:4)){
  cv.lasso <- cv.glmnet(x=Xdats[[w]], y=Y,type.measure = "mse", nfolds = 10)
  coefs = data.frame(beta=rep(NA,length(rownames(coef(cv.lasso)))))
  
  plot(cv.lasso, main=paste0(w,"- weeks prediction"))  
  
  coefs[1] = as.numeric(round(coef(cv.lasso)[,1],3)) 
  rownames(coefs) = rownames(coef(cv.lasso))
  
  dat_coefs[[w]] = coefs
}
dat_coefs

#####################################################
# evaluate

# LASSO MSE - generates for each seperate value of s an mse
mse_LA = data.frame(cbind(s = unique(su), mse1w = rep(NA,length(su)),
                          mse2w = rep(NA,length(su)),mse3w = rep(NA,length(su)),
                          mse4w = rep(NA,length(su))))
for(i in 1:length(su)){
  mse_LA$mse1w[i] <- mean((FAO[[i]]$o1w - FAO[[i]]$f1w)^2)
  mse_LA$mse2w[i] <- mean((FAO[[i]]$o2w - FAO[[i]]$f2w)^2)
  mse_LA$mse3w[i] <- mean((FAO[[i]]$o3w - FAO[[i]]$f3w)^2)
  mse_LA$mse4w[i] <- mean((FAO[[i]]$o4w - FAO[[i]]$f4w)^2)
}

# Extract best fitting lambda for each target week
lambda_best = data.frame(target = c("1week","2week","3week","4week"), s=rep(NA,4),s.num=rep(NA,4))
for(i in 1:4){
  mse = mse_LA[,i+1]
  num.l = which(mse==min(mse))[length(which(mse==min(mse)))]
  best.l = mse_LA$s[num.l]
  lambda_best[i,c(2,3)] = c(best.l,num.l) 
}

# LASSO mse with best lambda
mse_LA_best = data.frame(cbind(model = rep("LASSO",1), mse1w = rep(NA,1),
                               mse2w = rep(NA,1),mse3w = rep(NA,1),
                               mse4w = rep(NA,1)))

mse_LA_best[2] <- mse_LA$mse1w[lambda_best$s.num[1]]
mse_LA_best[3] <- mse_LA$mse2w[lambda_best$s.num[2]]
mse_LA_best[4] <- mse_LA$mse3w[lambda_best$s.num[3]]
mse_LA_best[5] <- mse_LA$mse4w[lambda_best$s.num[4]]

# ARIMA MSE
mse_AR = data.frame(cbind(model = rep("Arima",1), mse1w = rep(NA,1),
                          mse2w = rep(NA,1),mse3w = rep(NA,1),
                          mse4w = rep(NA,1)))
mse_AR[2] <- mean((FAOa$o1w - FAOa$f1w)^2)
mse_AR[3] <- mean((FAOa$o2w - FAOa$f2w)^2)
mse_AR[4] <- mean((FAOa$o3w - FAOa$f3w)^2)
mse_AR[5] <- mean((FAOa$o4w - FAOa$f4w)^2)

# Reference model MSE
mse_ref = data.frame(cbind(model = rep("Model0",1), mse1w = rep(NA,1),
                          mse2w = rep(NA,1),mse3w = rep(NA,1),
                          mse4w = rep(NA,1)))
mse_ref[2] <- mean((FAO[[1]]$o1w - lag(FAO[[1]]$o1w,n = 1))^2,na.rm = TRUE)
mse_ref[3] <- mean((FAO[[1]]$o1w - lag(FAO[[1]]$o1w,n = 2))^2,na.rm = TRUE)
mse_ref[4] <- mean((FAO[[1]]$o1w - lag(FAO[[1]]$o1w,n = 3))^2,na.rm = TRUE)
mse_ref[5] <- mean((FAO[[1]]$o1w - lag(FAO[[1]]$o1w,n = 4))^2,na.rm = TRUE)



# All model mse together
eval <- rbind(mse_ref,mse_AR,mse_LA_best)
eval
########################################
#### save & load
savename <- paste0("./Data/", script_name , ".Rda")
save(FAO,eval,file = savename)
# loading (from here can be run without re-running the loop)
load(savename)


#####################################################
# Plot output

# Plot predictions of all values of s
png("~/Dropbox/Forecasting Flu Challenge/Figures/LASSO/Predictions_all_s.png", width=500,height=500)
par(mfrow=c(2,2))
for(w in c(1:4)){
  plot(FAO[[1]]$timepoint_reference, unlist(FAO[[1]][w*2+1]), 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main=paste0(w,"-week prediction"))

cols<-rainbow(length(su))
for(i in 1:length(su)){
  lines(FAO[[i]]$timepoint_reference, unlist(FAO[[i]][w*2]),
        col=adjustcolor(cols[i], 0.5), lwd=3)
  }
}

dev.off()

# Plot mse of all values of s
png("~/Dropbox/Forecasting Flu Challenge/Figures/LASSO/MSE_predictions_best_s.png", width=500,height=500)
par(mfrow=c(2,2))
for(w in c(1:4)){
  plot(mse_LA$s, mse_LA[,w+1], type="l", main=paste0("MSE ",w,"-week prediction"), xlab="value s", ylab="MSE")
  lines(rep(lambda_best$s[w],101),seq(0,1,0.01), lty=2,col="red")
}
dev.off()

# Plot fit with best fitting lambda
png("~/Dropbox/Forecasting Flu Challenge/Figures/LASSO/Prediction_best_s.png", width=500,height=500)
cols<-c("blue","green","red","orange","purple")
par(mfrow=c(2,2))
for(w in c(1:4)){
  num.l = lambda_best$s.num[w]
  best.l =lambda_best$s[w]
  plot(FAO[[1]]$timepoint_reference, unlist(FAO[[num.l]][w*2+1]), 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main=paste0(w,"-week prediction best s = ", best.l))
  lines(FAO[[num.l]]$timepoint_reference, unlist(FAO[[num.l]][w*2]),
      col=adjustcolor(cols[w], 0.5), lwd=3)
 lines(FAO[[1]]$timepoint_reference, unlist(FAO[[1]][w*2]),
    col=adjustcolor(cols[5], 0.5), lwd=3,lty=2)
}
dev.off()

# plot distribtion residuals to check normality
png("~/Dropbox/Forecasting Flu Challenge/Figures/LASSO/hist_residuals.png", width=500,height=500)
par(mfrow=c(2,2))
for(w in c(1:4)){
  num.l = lambda_best$s.num[w]
  best.l =lambda_best$s[w]
  hist(unlist(FAO[[num.l]][w*2+1]) - unlist(FAO[[num.l]][w*2]), main=paste0(w,"-weeks distribution residuals"),xlab="Residuals")
}
dev.off()

# Plot absolute errors
# par(mfrow=c(2,2))
# for(w in c(1:4)){
#   num.l = lambda_best$s.num[i]
#   best.l =lambda_best$s[i]
#   plot(FAO[[num.l]]$timepoint_reference, abs(unlist(FAO[[num.l]][w*2])-unlist(FAO[[num.l]][w*2+1]))/unlist(FAO[[num.l]][w*2+1]), 
#      pch=19, cex=0.25,
#      xlab="date", ylab="cases", main=paste0(i,"-week prediction best s = ", best.l))
# }
# Plot fit with best s
# plot(Fit0, label=T)



#####################################################
# Calculate SD of residuals

pred = data.frame(cbind(target = c("1week","2week","3week","4week"),mean = rep(0,4), sd = rep(0,4)))

sd = NULL
for(w in c(1:4)){
  num.l = lambda_best$s.num[w]
  best.l =lambda_best$s[w]
  sd = c(sd,sd(unlist(FAO[[num.l]][w*2+1]) - unlist(FAO[[num.l]][w*2])))
}
pred$sd = sd

#####################################################
# Generate predictions

df_point = which(DF$weekname == "2016-35")
mean=NULL
for(w in c(1:4)){
  wks_ahead = w
  tchoice_forc_v <- df_point + 1:wks_ahead
  covars_for_forecast <- as.matrix(my_predictors_lag(preddats[[w]],lagdats[[w]],name_predictors,DF,tchoice_forc_v))
  predictions <- predict.glmnet(models[[w]], n.ahead=wks_ahead,s=lambda_best$s[w], 
                                newx=covars_for_forecast)
  mean = c(mean, predictions[w])
}
pred$mean = mean 

#####################################################
# Calculate probability of bins

breaks.in = c(seq(0,13.0,0.1)) # Everything 13 and above will be put together in one bin

prob.forecast = data.frame(cbind(Bin_start_incl = breaks.in,w1 = rep(NA,length(breaks.in)),w2 = rep(NA,length(breaks.in)),
                                 w3 = rep(NA,length(breaks.in)),w4 = rep(NA,length(breaks.in))))


png("~/Dropbox/Forecasting Flu Challenge/Figures/LASSO/density_predictions.png", width=500,height=500)
 par(mfrow=c(2,2))
for(w in c(1:4)){
  prob.forecast[,w+1] = gen.prob.distr(mean=pred$mean[w], sd=pred$sd[w], log.scale=T, breaks.in=breaks.in)
  plot(breaks.in,prob.forecast[,w+1], type="l", ylab="density", main=paste0(w,"-weeks prediction density"), xlab="breaks")
}
dev.off()

#####################################################
# Store forecasts
results.la = read.csv("./Forecasts/Submission_template.csv")
results.la$Value = NA

targets = c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead")
for(w in c(1:4)){
  nat_week = which(results.la$Target==targets[w] & results.la$Location=="US National")
  point = exp(pred$mean[w])-1
  results.la$Value[nat_week] = c(point,prob.forecast[,w+1]) 
}

#####################################################
# Save file
savename <- paste0("./Forecasts/", script_name, ".csv")
write.csv(results.la,file = savename)

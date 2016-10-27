###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "lasso_learning"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(glmnet)
library(MASS); library(tsModel)

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
                                                 cases = log(cases+1)) # NA is one missing value which was coded as X
# only keep DF
rm(usflu, usflu_allyears, DF)
# add holidays to the dataframe
DF2 <- dplyr::full_join(DF1,holiday_perweek,by = "weekname")
DF3 <- dplyr::left_join(DF2,seas_times,by = "weekname")
DF4 <- dplyr::left_join(DF3,clim, by="weekname")
DF5 <- dplyr::left_join(DF4,google, by="weekname")

DF1 <- DF5

### mutate variables: Add total number of cases from previous season as variable
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

# the predictor with shortest lag gives lag for the week_name=the week from which we use all data
# predictors of which we know future values can have shorter lag 
# shortest lag of 4
my_shortest_lag <- 4
#
DF1 <-  DF1[1 : (end.timeline+short_lag),] %>% 
  mutate(
    data_weekname = lag(weekname, n = 4), # this is the week from where we use the data
    cases_l4 = lag(cases, n = 4),
    dcases_l4 = lag(dcases, n = 4), 
    cases_l5 = lag(cases,n = 5),
    dcases_l5 = lag(dcases,n = 5),
    kids_cuddle_l2 = lag(inschool,n = 2),
    big_hols_l1= lag(big_holidays,n = 1),
    sin_week = sin(2*pi*week/52),
    cos_week = cos(2*pi*week/52)
  ) %>% 
  mutate(
    weekname = lag(weekname, n = 4),
    cases = lag(cases, n = 4)
  )

# truncate the NA-beginning
# the season lag produces 53 NA
# but even more NA because holidays available form 2011-51
week_pos <- which(DF1$weekname == "2011-51") + 2 # plus because of lag
DF2 <- DF1[week_pos:end.timeline,] 

# decide the time points from where to make first.prediction and last.prediction
first.prediction <- "2015-35" # week from where to make the first prediction
# last.prediction; to validate own predictions we need observed data: then minus shortest lag
last.prediction <- "2016-35"

# where to put the start given the last prediction
last.prediction_df <- which(DF2$data_weekname == last.prediction)
DF2$data_weekname[last.prediction_df - 52]

# make numeric for final training point = prediction point
prstart <- which(DF2$data_weekname == first.prediction)
prstop <- which(DF2$data_weekname == last.prediction)
# all prediction points
pred_vector <- prstart:prstop

# start training points
train_start_vector <- pred_vector - 190

### initiate outputfile
num.of.pred <- (prstop - prstart) + 1

# forecast 
one <- list(data.frame(timepoint_reference = prstart:prstop, f1w=NA,o1w=NA,f2w=NA,o2w=NA,f3w=NA,o3w=NA,f4w=NA,o4w=NA))
su = seq(0,1,0.001)
FAO=list()
for(i in 1:length(su)){
  FAO = c(FAO,one)
}

FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

#######################################################
#### start the prediction loop
DF <- DF2 # use DF in the loop
i <- 0
for (pred.tpoint in pred_vector){
  i = i + 1
  print(i) # print where you are in the loop
  
  ###############################################
  ################# model fitting
  # Make train data
  df_point <- pred.tpoint # the data.frame row
  train_start <- train_start_vector[i] # moving window
  trainDF <- DF[train_start:df_point,]
  
  # input for the LASSO
  my_input <- c("week","sin_week","cos_week", "cases_l4","dcases_l4","cases_l5","seas_total_l1",
                "m_start_seas","m_end_seas","m_peak_seas")
  xreg = as.matrix(trainDF[,my_input])
  
  ######## 4 weeks-ahead ############
  wks_ahead <- 4
  # train model
  ltrain <- dim(trainDF)[1]
  
  
  
  # fit LASSO regression
  Fit0 = glmnet(y=trainDF$cases[(1 + wks_ahead):ltrain],x=xreg[1 : (ltrain - wks_ahead), my_input], family="gaussian") # glmnet is fitting with alpha=1 by default, which means LASSO is used for parameter selection;
                                                          # if alpha=1, ridge regression is used.

  # Forecast
  forecast.wks.ahead <- 4
  newx = xreg[(ltrain - wks_ahead + 1):ltrain, my_input]
  forecast <- predict.glmnet(Fit0, n.ahead=forecast.wks.ahead,s=su, 
                           newx=newx)

  ##################################################
  # example: ARIMA
  
  # fit model
  Fit2 <- Arima(trainDF$cases[(1 + wks_ahead):ltrain], order=c(1,0,0),
                seasonal=list(order=c(1,0,0),period=52), lambda = 1)
  ####
  ### forecast
  wks_ahead_arim <- 4
  ar_predictions <- forecast.Arima(Fit2,4)
  
  # observed values
  observed <- exp(trainDF$cases[(ltrain - wks_ahead + 1):ltrain])-1
  
  final_predict = forecast # Not extracting the se correct from forecasting
  for(s in 1:length(su)){
    # save 1 weeks forecast and observed
    FAO[[s]]$f1w[i] <- exp(final_predict[1,s])-1
    FAO[[s]]$o1w[i] <- observed[1]
    # save 2 weeks forecast and observed
    FAO[[s]]$f2w[i] <- exp(final_predict[2,s])-1
    FAO[[s]]$o2w[i] <- observed[2]
    # save 3 weeks forecast and observed
    FAO[[s]]$f3w[i] <- exp(final_predict[3,s])-1
    FAO[[s]]$o3w[i] <- observed[3]
    # save 4 weeks forecast and observed
    FAO[[s]]$f4w[i] <- exp(final_predict[4,s])-1
    FAO[[s]]$o4w[i] <- observed[4]
  }
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
}


#####################################################
# evaluate
mse_LA_4w = data.frame(cbind(s = unique(su), mse = rep(NA,length(su))))
for(i in 1:length(su)){
  mse_LA_4w$mse[i] <- mean((FAO[[i]]$o4w - FAO[[i]]$f4w)^2)
}
mse_AR_4w <- mean((FAOa$o4w - FAOa$f4w)^2)
mse_ref_4w <- mean((FAO[[1]]$o4w - lag(FAO[[1]]$o4w,n = 4))^2,na.rm = TRUE)

#####################################################
# Plot output
par(mfrow=c(1,3))
plot(FAO[[1]]$timepoint_reference, FAO[[1]]$o4w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main="4-week prediction")

cols<-rainbow(length(su))
for(i in 1:length(su)){
  lines(FAO[[i]]$timepoint_reference, FAO[[i]]$f4w,
        col=adjustcolor(cols[i], 0.5), lwd=3)
}

# Best fitting lambda
num.l = which(mse_LA_4w$mse==min(mse_LA_4w$mse))[length(which(mse_LA_4w$mse==min(mse_LA_4w$mse)))]
best.l = mse_LA_4w$s[num.l]

# Plot best fitting lambda
plot(mse_LA_4w$s, mse_LA_4w$mse, type="l", main="MSE using different s", xlab="value s", ylab="MSE")
lines(rep(best.l,101),seq(0,1,0.01), lty=2,col="red")

# Plot fit with best fitting lambda
plot(FAO[[num.l]]$timepoint_reference, FAO[[num.l]]$o4w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main=paste("4-week prediction best s =", best.l))
lines(FAO[[num.l]]$timepoint_reference, FAO[[num.l]]$f4w,
      col=adjustcolor(cols[num.l], 0.5), lwd=3)
lines(FAOa$timepoint_reference, FAOa$f4w,
      col=adjustcolor(cols[2], 0.5), lwd=3)

# Plot absolute errors
plot(FAO[[num.l]]$timepoint_reference, abs(FAO[[num.l]]$f4w-FAO[[num.l]]$o4w)/FAO[[num.l]]$o4w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main=paste("4-week prediction best s =", best.l))

# Plot fit with best s
plot(Fit0, label=T)
cv.lasso <- cv.glmnet(x=xreg, y=trainDF$cases)
plot(cv.lasso)  # Best fitting model has 3 parameters
exp(coef(cv.lasso)) # Season total does not seem to add anything
eval <- data.frame(mse_LA_4w=mse_LA_4w$mse[num.l],
                   mse_AR_4w=mse_AR_4w,
                   mse_ref_4w=mse_ref_4w)
eval

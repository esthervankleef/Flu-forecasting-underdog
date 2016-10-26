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
library(MASS)
library(spatstat)
########################################
#### load the data
load("./Data/data_manip.Rda")

DF <- usflu_allyears

#######################################################
# log transform data
# DO WE NEED TO LOG-TRANSFORM?
# For ARIMA and LASSO yes. Is done in the below
DF <- DF %>% 
  dplyr::select(-region,-region.type) %>% mutate(cases = as.numeric(as.character(x.weighted.ili)),
                                                 cases = log(cases+1)) # NA is one missing value which was coded as X

# only keep DF
rm(usflu); rm(usflu_allyears)

########################################
#### Explore data
########################################
names(DF)

plot(DF$cases,pch=20, ylab="Cases", xlab="Time")

# make time series
my.ys <- ts(DF$cases,frequency=52, start=c(1997,40))

# decompose time series
my.stl <- stl(my.ys, s.window="periodic",na.action = na.approx)

plot(my.stl)

########################################
### get rid of missing data by truncating
missing.vals <- which(is.na(DF$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(DF)[1] 
DF1 <- DF[(last.miss.val + 1):last.prediction,]

# Check distribution of weekly cases
par(mfrow=c(1,1))
ln = fitdistr(DF1$cases,"normal")
hist(DF1$cases, main = "Distribution cases", xlab="Weekly cases",freq=F);hist(DF1$cases, main = "Distribution log(cases)", xlab="log weekly cases",freq=F)
lines(density(rnorm(10000, mean=ln$estimate[1], sd=ln$estimate[2])), col="red") 



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
                                                    dcases_l5 = lag(dcases,n = 5),
                                                    magic_week = week^2)

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
one <- list(data.frame(timepoint_reference = prstart:prstop, f1w=NA,o1w=NA,f2w=NA,o2w=NA,f3w=NA,o3w=NA,f4w=NA,o4w=NA))
su = seq(0,1,0.001)
FAO=list()
for(i in 1:length(su)){
  FAO = c(FAO,one)
}
#FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA

#######################################################
#### start the prediction loop
DF <- DF2 # use DF in the loop
i <- 0

for (pred.tpoint in first.prediction:last.prediction){
    i = i + 1
    print(i) # show where you are in the loop
  
  ###############################################
  ################# model fitting
  # example: LASSO
  
  # make train data
  df_point <- which(DF$timepoint_reference == pred.tpoint)
  trainDF <- DF[1:df_point,]
  
  # input for the forest
  my_input <- c("week","magic_week","cases_l4","dcases_l4","cases_l5","seas_total_l1")
  xreg = as.matrix(trainDF[,my_input])
  
  # fit LASSO regression
  Fit0 = glmnet(y=trainDF$cases,x=xreg, family="gaussian") # glmnet is fitting with alpha=1 by default, which means LASSO is used for parameter selection;
                                                          # if alpha=1, ridge regression is used.

  #head(Fit0)
  #plot(Fit0, label=T)
  model.fit0 <- predict.glmnet(Fit0, s=su,newx=xreg, type="response") # Arbitrary choice of penalty term lambda
  
  # if(pred.tpoint == first.prediction){
  #   print("yes")
  # # capture model fit LASSO for plotting
  # model.fitp <- predict.glmnet(Fit0, s=seq(0,1,0.001),newx=xreg, type="response") # Arbitrary chocie of penalty term lambda; the higher s, the larger the penalty, the lower the number of co-variates accepted
  # model.fitp <- data_frame(
  #    pred=as.numeric(model.fitp), 
  #    se=rep(sd(model.fitp), length(model.fitp))) %>% # Not calculating se correct yet, need to find out how to extract residuals
  #    mutate(
  #      t.idx = trainDF$timepoint_reference,
  #      point.pred = exp(pred) - 1,
  #      lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
  #      upr95.pred = exp(pred + qnorm(0.975) * se) - 1
  #    )
  # }
  # Forecast
  forecast.wks.ahead <- 4
  newx = as.matrix(DF[df_point+1:forecast.wks.ahead,my_input])
  forecast <- predict.glmnet(Fit0, n.ahead=forecast.wks.ahead,s=su, 
                           newx=newx)

  # ### calculate mean predictions and 95% CIs
  # forecast <- as.data.frame(forecast) 
  # names(forecast) = "pred"
  # forecast <- forecast %>% mutate(se = sd(pred),
  #                               t.idx = df_point + 1:forecast.wks.ahead,
  #                               point.pred = exp(pred) - 1,
  #                               lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
  #                               upr95.pred = exp(pred + qnorm(0.975) * se) - 1
  # )
  observed <- DF$x.weighted.ili[df_point+1:4]  
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
}
#####################################################
# evaluate
mse_4w = data.frame(cbind(s = unique(su), mse = rep(NA,length(su))))
for(i in 1:length(su)){
  mse_4w$mse[i] <- mean((FAO[[i]]$o4w - FAO[[i]]$f4w)^2)
}
eval <- data.frame(mse_4w=mse_4w)
                   #,
                   #mse_AR_4w=mse_AR_4w,
                   #mse_ref_4w=mse_ref_4w)

# plot(FAO$timepoint_reference, FAO$o1w, 
#      pch=19, cex=0.25,
#      xlab="date", ylab="cases", main="1-week prediction")
# lines(FAO$timepoint_reference, FAO$f1w,
#       col=adjustcolor('darkblue', 0.5), lwd=3)
# plot(FAO$timepoint_reference+1, FAO$o2w, 
#      pch=19, cex=0.25,
#      xlab="date", ylab="cases", main="2-week prediction")
# lines(FAO$timepoint_reference+1, FAO$f2w,
#       col=adjustcolor('red', 0.5), lwd=3)
# plot(FAO$timepoint_reference+2, FAO$o3w, 
#      pch=19, cex=0.25,
#      xlab="date", ylab="cases", main="3-week prediction")
# lines(FAO$timepoint_reference+2, FAO$f3w,
#       col=adjustcolor('orange', 0.5), lwd=3)

    #legend(710,3.5, c("1-week","2-week","3-week","4-week"),lty=c(1,1,1,1),
    #   col=c(adjustcolor('darkblue', 0.5),adjustcolor('red', 0.5),
    #                                                       adjustcolor('orange', 0.5),adjustcolor('green', 0.5)))

par(mfrow=c(1,3))
plot(FAO[[1]]$timepoint_reference+3, FAO[[1]]$o4w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main="4-week prediction")

cols<-rainbow(length(su))
for(i in 1:length(su)){
  lines(FAO[[i]]$timepoint_reference+3, FAO[[i]]$f4w,
        col=adjustcolor(cols[i], 0.5), lwd=3)
}
# Best fitting lambda
num.l = which(eval$mse_4w.mse==min(eval$mse_4w.mse))
best.l = eval$mse_4w.s[num.l]

# Plot best fitting lambda
plot(eval$mse_4w.s, eval$mse_4w.mse, type="l", main="MSE using different s", xlab="value s", ylab="MSE")
lines(rep(best.l,101),seq(0,1,0.01), lty=2,col="red")

# Plot fit with best fitting lambda
plot(FAO[[num.l]]$timepoint_reference+3, FAO[[num.l]]$o4w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main=paste("4-week prediction best s =", best.l))
lines(FAO[[num.l]]$timepoint_reference+3, FAO[[num.l]]$f4w,
      col=adjustcolor(cols[num.l], 0.5), lwd=3)

# Plot absolute errors
plot(FAO[[num.l]]$timepoint_reference+3, abs(FAO[[num.l]]$f4w-FAO[[num.l]]$o4w), 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main=paste("4-week prediction best s =", best.l))

# Plot fit with best s
plot(Fit0, label=T)
cv.lasso <- cv.glmnet(x=xreg, y=trainDF$cases)
plot(cv.lasso)  # Best fitting model has 3 parameters
(coef(cv.lasso)) # Season total does not seem to add anything

###### Flu Forecasting Challenge ####### 

## title: "LASSO training"
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
library(MASS)
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
#### Explore data
########################################
names(DF)

plot(DF$cases,pch=20, ylab="Cases", xlab="Time")

# make time series
my.ys <- ts(DF$cases,frequency=52, start=c(1997,40))

# decompose time series
my.stl <- stl(my.ys, s.window="periodic",na.action = na.approx)

plot(my.stl)

### get rid of missing data by truncating
missing.vals <- which(is.na(DF$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(DF)[1] 
DF1 <- DF[(last.miss.val + 1):last.prediction,]

# attach dataframe
attach(DF1)

# Check distribution of weekly cases
par(mfrow=c(1,1))
ln = fitdistr(cases,"normal")
hist(cases, main = "Distribution cases", xlab="Weekly cases",freq=F);hist(cases, main = "Distribution log(cases)", xlab="log weekly cases",freq=F)
lines(density(rnorm(10000, mean=ln$estimate[1], sd=ln$estimate[2])), col="red") 
# Log normal seems to be reasonable assumption; could also try to fit negative binomial/poisson


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
cases = as.numeric(as.character(DF$x.weighted.ili))
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
  my_input <- c("week","cases_l4","dcases_l4","cases_l5","dcases_l5","seas_total_l1")
  xreg = as.matrix(trainDF[,my_input])
  
  # fit LASSO regression
  Fit0 = glmnet(y=trainDF$cases,x=xreg, family="gaussian")

  #head(Fit0)
  #plot(Fit0)
  model.fit0 <- predict.glmnet(Fit0, s=c(0.001),newx=xreg, type="response") # Arbitrary chocie of penalty term lambda
  
  if(pred.tpoint == first.prediction){
    print("yes")
  # capture model fit LASSO for plotting
  model.fitp <- predict.glmnet(Fit0, s=c(0.001),newx=xreg, type="response") # Arbitrary chocie of penalty term lambda
  model.fitp <- data_frame(
     pred=as.numeric(model.fitp), 
     se=rep(sd(model.fitp), length(model.fitp))) %>% # Not calculating se correct yet, need to find out how to extract residuals
     mutate(
       t.idx = trainDF$timepoint_reference,
       point.pred = exp(pred) - 1,
       lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
       upr95.pred = exp(pred + qnorm(0.975) * se) - 1
     )
  }
  # Forecast
  forecast.wks.ahead <- 4
  newx = as.matrix(DF[df_point+1:forecast.wks.ahead,my_input])
  forecast <- predict.glmnet(Fit0, n.ahead=forecast.wks.ahead,s=0.001, 
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
}

par(mfrow=c(2,2))
 
plot(FAO$timepoint_reference, FAO$o1w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main="1-week prediction")
lines(FAO$timepoint_reference, FAO$f1w,
      col=adjustcolor('darkblue', 0.5), lwd=3)
plot(FAO$timepoint_reference+1, FAO$o2w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main="2-week prediction")
lines(FAO$timepoint_reference+1, FAO$f2w,
      col=adjustcolor('red', 0.5), lwd=3)
plot(FAO$timepoint_reference+2, FAO$o3w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main="3-week prediction")
lines(FAO$timepoint_reference+2, FAO$f3w,
      col=adjustcolor('orange', 0.5), lwd=3)
plot(FAO$timepoint_reference+3, FAO$o4w, 
     pch=19, cex=0.25,
     xlab="date", ylab="cases", main="4-week prediction")
lines(FAO$timepoint_reference+3, FAO$f4w,
      col=adjustcolor('green', 0.5), lwd=3)
legend(710,3.5, c("1-week","2-week","3-week","4-week"),lty=c(1,1,1,1),
       col=c(adjustcolor('darkblue', 0.5),adjustcolor('red', 0.5),
                                                           adjustcolor('orange', 0.5),adjustcolor('green', 0.5)))

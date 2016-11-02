###### Flu Forecasting Challenge ####### 

## title: "Add peak week"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "peak_w_prediction"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(glmnet)

usflu <- get_flu_data("national", "ilinet", years=1997:2016)

## The data manipulation line
usflu$WEEK <- (usflu$WEEK - 31 ) %% 52



peak_week <- array(dim = length(1998:2015))
X_data <- array(dim = c(length(1998:2015), length(0:13)))
for(years in 1998:2015){
  year_data <- subset(usflu, YEAR==years)
  peak_week[years-1997] <- year_data$WEEK[which.max(year_data$X..WEIGHTED.ILI)]
  #take the last 5 weeks as predictors
  X_data[years-1997,] <- as.numeric(year_data$X..WEIGHTED.ILI[year_data$WEEK<14])
}
## 2009 is a massive outlier so removed it
r_fit <- cv.glmnet(y=peak_week[-c(1:5,12)], x= t(apply(X = X_data[-c(1:5,12),], 1, diff)))
plot(r_fit, xvar='lambda', label=T)
plot(predict(r_fit, t(apply(X = X_data[-c(1:5,12),], 1, diff)), s = .5), peak_week[-c(1:5,12)],
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
lines(23:30, 23:30, type='l', lty=3)

## Predictions
pred_X <- diff(as.numeric(usflu$X..WEIGHTED.ILI[979:992]))
peak_week_prediction <- predict(r_fit, newx = t(pred_X) , s=.1)
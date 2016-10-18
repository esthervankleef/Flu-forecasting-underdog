###### Flu Forecasting Challenge ####### 

## title: "RF testing"
## author: "Underdog"
## date: "18 October 2016"

# empty workspace
rm(list = ls())
script_name <- "experimental_Rene"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(ggplot2)

########################################
#### essential stuff
load("./Data/data_manip.Rda")
# select input variables
DF <- usflu
DF <- DF %>% 
  dplyr::select(-region,-region.type)
# only keep DF
rm(usflu)

########################################
#### look at the data
names(DF)
plot(cases,pch=20)
# make time series
my.ys <- ts(cases,frequency=52, start=c(1997,40))
# decompose the time series
my.stl <- stats::stl(my.ys, s.window="periodic",na.action = na.approx)
#my.stl <- decompose(my.ys) # i think stl() is better than decompose(), the latter does not give the bars to compare the compotents
plot(my.stl)


# timepoints
train.set <- 100
first.predict <- train.set + 1
last.prediction <- dim(DF)[1] 

#
missing.vals <- which(is.na(DF$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
DF1 <- DF[(last.miss.val + 1):last.prediction,]
# attach the Dataframe
attach(DF1)
#

### Visualisation with caret
dev.off()
#
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
featurePlot(x = DF1$week,
            y = DF1$cases,
            plot="scatter")


####
#
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1
)
#
rf_model <- caret::train(x=DF1$week, y=DF1$cases, method="rf",trcontrol=fitControl)


#######################################################
##### ML regression
# time slice cross validation
history <-  train.set
initial.window <-  80
train.control <-  trainControl(
  method="timeslice",
  initialWindow=initial.window,
  horizon=history-initial.window,
  fixedWindow=T)

X = data.frame(week=week[1:train.set],row.names = NULL)
rownames(X) <- NULL
#
fitControl <- trainControl(method="repeatedcv",number=10,repeats=10)
# train a random forest
Y = cases[1:train.set]
rf_model <- caret::train(x=X, y=Y, method="rf",na.action = na.pass)

# predict with random forest




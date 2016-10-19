###### Flu Forecasting Challenge ####### 

## title: "RF testing"
## author: "Underdog"
## date: "18 October 2016"

# empty workspace
rm(list = ls())
script_name <- "caret_learning"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(caret)

########################################
#### essential stuff
load("./Data/data_manip.Rda")
# select input variables
DF <- usflu
DF <- DF %>% 
  dplyr::select(-region,-region.type)
# only keep DF
rm(usflu)

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

### Visualisation with caret
dev.off()
#
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
# span: controls the smoothness of the smoother
featurePlot(x = DF1$week,
            y = DF1$cases,
            plot="scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(1,1))

# set training and test times
end.timeline <- dim(DF1)[1]
train.size <- 600

# make the changes
DF1$dcases <- c(NA,diff(DF1$cases))
#DF1$ddcases <- c(NA,diff(DF1$dcases)) # don't need now
# create different lags
# lag of 4
DF1$cases_l4 <- lag(DF1$cases,n = 4)[1 : end.timeline]
DF1$dcases_l4 <- lag(DF1$dcases,n = 4)[1 : end.timeline]
# lag of 5
DF1$cases_l5 <- lag(DF1$cases,n = 5)[1 : end.timeline]
DF1$dcases_l5 <- lag(DF1$dcases,n = 5)[1 : end.timeline]

# truncate the NA-tail
biggest_lag <- 5 # a lag of 5 produces 5 NAs
highest_d <- 2 # a second derivative produces 2 NAs
DF2 <- DF1[(biggest_lag + 1 + highest_d):end.timeline,] 
# select variables you want 
DF2 <- DF2 %>% 
  dplyr::select(cases,year,week,weekname,cases_l4,dcases_l4,cases_l5,dcases_l5)

# make train and test data.frame
end.timeline <- dim(DF2)[1]
trainDF <- DF2[1:train.size,]
testDF <- DF2[(train.size + 1):end.timeline,]

### Random forest
#
fitControl <- trainControl(method = "timeslice",
                           initialWindow = 104,
                           horizon = 10,
                           fixedWindow = TRUE)
#
my_input <- c("week","cases_l4","dcases_l4","cases_l5","dcases_l5")
Fit1 <- train(x = trainDF[,my_input], y = trainDF$cases, 
                 method = "rf", 
                 trControl = fitControl,
                 verbose = TRUE,
              tuneGrid = NULL, 
              tuneLength = 3)
# 
predictions <- predict(Fit1, testDF[,my_input]) # point predictions
plot(as.factor(testDF$weekname),testDF$cases,pch=20)
points(unname(predictions),pch=20,col="darkred")





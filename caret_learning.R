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
# attach the Dataframe
#attach(DF1)
#

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

# split into training and test data
end.timeline <- dim(DF1)[1]
train.size <- 600

my.lag <- 4
# add lag to cases
# and have change in cases
DF1$ lag(DF1$week,my.lag)
lag(DF1$cases,my.lag)
DF1$cases
# make train and test data.frame
trainDF <- DF1[1:train.size,]
testDF <- DF1[(train.size + 1):end.timeline,]

#


#
fitControl <- trainControl(method = "timeslice",
                           initialWindow = 104,
                           horizon = 10,
                           fixedWindow = TRUE)
#
Fit1 <- train(x = trainDF[,c("year","week")], y = trainDF$cases, 
                 method = "rf", 
                 trControl = fitControl,
                 verbose = TRUE,
              tuneGrid = NULL, 
              tuneLength = 3)






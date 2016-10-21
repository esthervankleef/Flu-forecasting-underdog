###### Flu Forecasting Challenge ####### 

## title: "RF testing"
## author: "Underdog"
## date: "18 October 2016"

# empty workspace
rm(list = ls())
script_name <- "visual_Rene"

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
attach(DF)
########################################
#### look at the entire timeline see missing areas
names(DF)
plot(as.factor(weekname),cases,pch=20)
where.na <- which(is.na(cases))
abline(v = where.na, col = "darkred")

########################################
#### Zoom in and look at missing areas
final.obs <- dim(DF)[1]
first.of2003 <- which(DF$year == "2003")[1]
DFcut <- DF[1:first.of2003,]
# plotting
plot(as.factor(DFcut$weekname),DFcut$cases,pch=20)
where.na <- which(is.na(DFcut$cases))
abline(v = where.na, col = "darkred")

########################################
#### Decompose timeseries
my.stl <- stats::stl(my.ys, s.window="periodic",na.action = na.approx)
#my.stl <- decompose(my.ys) # i think stl() is better than decompose(), the latter does not give the bars to compare the compotents
plot(my.stl)

########################################
#### Plot scatter with featurePlot
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
featurePlot(x = DF$week,
            y = DF$cases,
            plot="scatter")

# experimental_Rene
featurePlot(x=trainDF[,my_input],
            y=trainDF$x.weighted.ili,
            plot = "scatter")








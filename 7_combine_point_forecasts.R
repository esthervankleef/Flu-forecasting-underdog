###### Flu Forecasting Challenge ####### 

## title: "Combine point forecasts"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "combine_point_forecasts"

# libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(randomForest)
library(caret)
library(glmnet)

########################################
#### load data
results_la = read.csv("./Forecasts/lasso_chall_template.csv")
results_rf = read.csv("./Forecasts/experimental_Rene.csv")
#results_lr = read.csv("./Forecasts/caret_lasso_ridge_template.csv")

#
load("./Data/lasso_chall_template.Rda"); eval_la <- eval
load("./Data/experimental_Rene.Rda"); eval_rf <- eval
#load("./Data/caret_lasso_ridge_template.Rda"); eval_lr <- eval

# get the weights
###
weight_la <- numeric(4)
weight_la[1] <- 1/eval_la$mse1w[eval_la$model=="LASSO"]
weight_la[2] <- 1/eval_la$mse2w[eval_la$model=="LASSO"]
weight_la[3] <- 1/eval_la$mse3w[eval_la$model=="LASSO"]
weight_la[4] <- 1/eval_la$mse4w[eval_la$model=="LASSO"]
#
weight_rf <- numeric(4)
weight_rf[1] <- 1/eval_rf$mse1w[eval_rf$model=="Random Forest"]
weight_rf[2] <- 1/eval_rf$mse2w[eval_rf$model=="Random Forest"]
weight_rf[3] <- 1/eval_rf$mse3w[eval_rf$model=="Random Forest"]
weight_rf[4] <- 1/eval_rf$mse4w[eval_rf$model=="Random Forest"]
#

# put together
###
results_combined <- results_la
targets = c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead")
for(w in c(1:4)){
  # where to put
  nat_week = which(results_combined$Target==targets[w] & results_combined$Location=="US National")
  # combine
  weighted_combo <- weight_la[w]*results_la$Value[nat_week] + weight_rf[w]*results_rf$Value[nat_week]
  weighted_combo <- weighted_combo/(weight_la[w] + weight_rf[w])
  results_combined$Value[nat_week] <-  weighted_combo 
  # turn out, no need to normalise
  ###
  all_but_point <- nat_week[-1]
  sum_of_bins <- sum(results_combined$Value[all_but_point])
  print(weighted_combo[1])
  all_bins <- results_combined$Bin_start_incl[nat_week][-1]
  print(as.character(all_bins[which.max(weighted_combo[-1])]))
}

# Save
#####################################################
# Save file
savename <- paste0("./Forecasts/", script_name, ".csv")
write.csv(results_combined,file = savename)





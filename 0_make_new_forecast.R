###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "make_new_forecast"

###########################################################
# Section: Run scripts one by one
###########################################################
source('1_load_fludata.R')
source('2_data_manip.R')
source('3_data_manip_2.R')
source('4_Lasso_learning.R')
source('5_experimental_Rene.R')
source('6_season_forecasts.R')
source('7_combine_point_forecasts.R')
source('8_add_season_targets_to_points.R')
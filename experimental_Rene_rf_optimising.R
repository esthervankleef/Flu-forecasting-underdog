###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
for (initialWindow in c(26,40,52,104)){
  script_name <- paste0("experimental_Rene_rf_optimising_","iniW_",initialWindow)
  
  # libraries
  library(tidyverse)
  library(lubridate)
  library(forecast)
  library(randomForest)
  library(caret)
  library(glmnet)
  
  load("./Data/data_manip_2.Rda")
  
  ###
  ###
  ### set parameters
  # last.prediction; to validate own predictions we need observed data: then minus shortest lag
  # start training points
  first_start_train_point <- "2012-06"
  # decide the time points from where to make first.prediction and last.prediction
  first.prediction <- "2015-43" # week from where to make the first prediction
  last.prediction <- most_current_week # include the newest week
  
  # where to put the start given the last prediction
  last.prediction_df <- which(DF$weekname == first.prediction)
  DF$weekname[last.prediction_df - 180]
  
  # make numeric for final training point = prediction point
  prstart <- which(DF$weekname == first.prediction)
  prstop <- which(DF$weekname == last.prediction)
  # all prediction points
  pred_vector <- prstart:prstop
  
  # start training points
  train_start_vector <- pred_vector - (prstart - which(DF$weekname == first_start_train_point))
  DF$weekname[train_start_vector]
  
  
  ### initiate outputfile
  num.of.pred <- (prstop - prstart) + 1
  # forcast 
  FAO <- data.frame(timepoint_reference = prstart:prstop) # rf
  FAOa <- data.frame(timepoint_reference = prstart:prstop) # SARIMA
  ### Remove NAs from google data
  googleNA = which(is.na(DF$gdoctor))
  googleNA = googleNA[googleNA<prstop]
  
  g_words = c("gfever","gheadache","gdoctor","gshivering","gcough")
  
  DF[googleNA,g_words] = DF[googleNA-1,g_words]
  
  ##### call functions
  source("functions.R")
  
  #######################################################
  ### make decisions about RF
  choose_predictors <- list()
  choose_lags <- list()
  ###
  wks_ahead <- 4
  choose_predictors[[wks_ahead]] <- c("sin_week","cos_week","week","cases","dcases","dcases",
                                      "kids_cuddle","temp_av","temp_av"
  )
  #
  choose_lags[[wks_ahead]] <- c(0,0,0,4,4,5,
                                1,4,5
  )
  ##########
  wks_ahead <- 3
  # 
  choose_predictors[[wks_ahead]] <- c("sin_week","cos_week","week","cases","dcases","dcases",
                                      "kids_cuddle","temp_av","temp_av"
  )
  #
  choose_lags[[wks_ahead]] <- c(0,0,0,3,3,4,
                                1,3,4
  )
  ###########
  wks_ahead <- 2
  # 
  choose_predictors[[wks_ahead]] <- c("sin_week","cos_week","week","cases","dcases","dcases",
                                      "kids_cuddle","temp_av","temp_av"
  )
  #
  choose_lags[[wks_ahead]] <- c(0,0,0,2,2,3,
                                1,2,4
  )
  ##########
  wks_ahead <- 1
  # 
  choose_predictors[[wks_ahead]] <- c("sin_week","cos_week","week","cases","dcases","dcases",
                                      "kids_cuddle","temp_av","temp_av"
  )
  #
  choose_lags[[wks_ahead]] <- c(0,0,0,1,1,2,
                                1,1,4
  )
  ######################################
  ######### start loop
  ####################################
  i <- 0
  for (pred.tpoint in pred_vector){
    i = i + 1
    print(i) # print where you are in the loop
    
    # manage the training position
    df_point <- pred.tpoint # the data.frame row
    train_start <- train_start_vector[i] # moving window
    # which weeks are those
    first_week_you_see <- DF$weekname[train_start]
    last_week_you_see <- DF$weekname[df_point]
    tchoice_v <- train_start:df_point
    
    ################## RANDOM FOREST
    
    # fitting
    ####################################################################
    wks_ahead <- 4
    #
    my_X <- my_predictors_lag(choose_predictors[[wks_ahead]],choose_lags[[wks_ahead]],x,DF,tchoice_v)
    Fit0.4 <- my_rf_fit2(wks_ahead,initialWindow,my_X)
    
    ####################################################################
    wks_ahead <- 3
    # 
    my_X <- my_predictors_lag(choose_predictors[[wks_ahead]],choose_lags[[wks_ahead]],x,DF,tchoice_v)
    Fit0.3 <- my_rf_fit2(wks_ahead,initialWindow,my_X)
    
    ####################################################################  
    wks_ahead <- 2
    #
    my_X <- my_predictors_lag(choose_predictors[[wks_ahead]],choose_lags[[wks_ahead]],x,DF,tchoice_v)
    Fit0.2 <- my_rf_fit2(wks_ahead,initialWindow,my_X)
    
    ####################################################################  
    wks_ahead <- 1
    # 
    my_X <- my_predictors_lag(choose_predictors[[wks_ahead]],choose_lags[[wks_ahead]],x,DF,tchoice_v)
    Fit0.1 <- my_rf_fit2(wks_ahead,initialWindow,my_X)
    ###
    models = list(Fit0.1,Fit0.2,Fit0.3,Fit0.4)
    
    # forecasts
    ####################################################################  
    rf.predictions = list()
    ### forecast: no longer than the shortest lag!
    for(w in c(1:4)){
      # weeks ahead
      wks_ahead = w
      # make vector
      tchoice_forc_v <- df_point + 1:wks_ahead
      #
      covars_for_forecast <- my_predictors_lag(choose_predictors[[wks_ahead]],choose_lags[[wks_ahead]],name_predictors,DF,tchoice_forc_v)
      rf_predictions <- predict(models[[w]], covars_for_forecast) # point predictions
      # save prediction
      rf.predictions[[w]] = rf_predictions[wks_ahead]
    }
    names(rf.predictions) = c("fw1","fw2","fw3","fw4")
    
    # observed values 
    tchoice_forc_v <- df_point + 1:4
    observed <- DF$cases[tchoice_forc_v]
    
    #####################
    ### output  
    ### save random forest
    # save 1 weeks forecast and observed
    FAO$f1w[i] <- rf.predictions$fw1
    FAO$o1w[i] <- observed[1]
    # save 2 weeks forecast and observed
    FAO$f2w[i] <- rf.predictions$fw2
    FAO$o2w[i] <- observed[2]
    # save 3 weeks forecast and observed
    FAO$f3w[i] <- rf.predictions$fw3
    FAO$o3w[i] <- observed[3]
    # save 4 weeks forecast and observed
    FAO$f4w[i] <- rf.predictions$fw4
    FAO$o4w[i] <- observed[4]
    
  } ####### end of loop
  # cut away last 4 weeks, where you have nothing to compare to
  its_length <- dim(FAO)[1]
  FAO <- FAO[1:(its_length-4),]
  
  # evaluate
  #####################################################
  
  # RF MSE
  mse_RF = data.frame(cbind(model = rep("Random Forest",1), mse1w = rep(NA,1),
                            mse2w = rep(NA,1),mse3w = rep(NA,1),
                            mse4w = rep(NA,1)))
  mse_RF[2] <- mean((FAO$o1w - FAO$f1w)^2)
  mse_RF[3] <- mean((FAO$o2w - FAO$f2w)^2)
  mse_RF[4] <- mean((FAO$o3w - FAO$f3w)^2)
  mse_RF[5] <- mean((FAO$o4w - FAO$f4w)^2)
  
  # Reference model MSE
  mse_ref = data.frame(cbind(model = rep("Model0",1), mse1w = rep(NA,1),
                             mse2w = rep(NA,1),mse3w = rep(NA,1),
                             mse4w = rep(NA,1)))
  mse_ref[2] <- mean((FAO$o1w - lag(FAO$o1w,n = 1))^2,na.rm = TRUE)
  mse_ref[3] <- mean((FAO$o1w - lag(FAO$o1w,n = 2))^2,na.rm = TRUE)
  mse_ref[4] <- mean((FAO$o1w - lag(FAO$o1w,n = 3))^2,na.rm = TRUE)
  mse_ref[5] <- mean((FAO$o1w - lag(FAO$o1w,n = 4))^2,na.rm = TRUE)
  # bind together
  eval <- rbind(mse_ref,mse_RF)
  
  ########################################
  #### save & load
  savename <- paste0("./Data/", script_name , ".Rda")
  save(FAO,eval,file = savename)
  # loading (from here can be run without re-running the loop)
  load(savename)
  
  #####################################################
  # plot
  par(mfrow=c(2,1)) 
  # reference (naive)
  my_title <- paste(script_name,"Ref 4-weeks, MSE =", as.character(round(mse_ref$mse4w,digits = 4)))
  plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
  points(FAO$timepoint_reference,c(lag(FAO$o4w,n = 4)),pch=20,col="darkred")
  # rf
  my_title <- paste(script_name,"RF 4-weeks, MSE =", as.character(round(mse_RF$mse4w,digits = 4)))
  plot(FAO$timepoint_reference,FAO$o4w,pch=19, col="black"); title(my_title)
  points(FAO$timepoint_reference,FAO$f4w,pch=20,col="darkred")
  par(mfrow=c(1,1)) 
  
}


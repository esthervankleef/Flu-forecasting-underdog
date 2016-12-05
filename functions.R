######################
### my functions

####################################
# Topic: Statistical learning
# function to return rf fit
my_rf_fit2 <- function(wks_ahead,initialWindow,my_X){
  #
  # make predictor matrix and outcome
  X <- my_X
  Y <- DF$cases[tchoice_v]
  # train RANDOM FOREST
  Fit0.4 <- train(x = X,
                  y = Y, 
                  method = "rf", 
                  trControl = myfit_control2(wks_ahead,initialWindow),
                  verbose = FALSE,
                  tuneGrid = NULL,
                  tuneLength = 20,
                  importance = FALSE)
}

# function to return rf fit
my_rf_fit <- function(wks_ahead,my_X){
  #
  # make predictor matrix and outcome
  X <- my_X
  Y <- DF$cases[tchoice_v]
  # train RANDOM FOREST
  Fit0.4 <- train(x = X,
                  y = Y, 
                  method = "rf", 
                  trControl = myfit_control(wks_ahead),
                  verbose = FALSE,
                  tuneGrid = NULL,
                  tuneLength = 20,
                  importance = FALSE)
}

# funtion for random forest
my_randomforest <- function(wks_ahead,choose_predictors,choose_lags){
  # lag: 
  wks_ahead <- wks_ahead
  ###############################################
  # make train data
  
  # make predictor matrix and outcome
  X <- my_predictors_lag(choose_predictors,choose_lags,name_predictors,DF,tchoice_v)
  Y <- DF$cases[tchoice_v]
  ###############################################
  # train RANDOM FOREST
  Fit1 <- train(x = X,
                y = Y, 
                method = "rf", 
                trControl = myfit_control(wks_ahead),
                verbose = FALSE,
                tuneGrid = NULL,
                tuneLength = 10,
                importance = FALSE)
  # obtain feature rank
  #varImp(Fit1)
  ### forecast: no longer than the shortest lag!
  tchoice_forc_v <- df_point + 1:wks_ahead
  covars_for_forecast <- my_predictors_lag(choose_predictors,choose_lags,name_predictors,DF,tchoice_forc_v)
  rf_predictions <- predict(Fit1, covars_for_forecast) # point predictions
  return(rf_predictions[wks_ahead])
}


# function that makes regression function and plots
regplot <- function(x,y,...){
  # fit model
  fit <- lm(y~x)
  # plot
  par(mfrow=c(1,1)); 
  plot(x,y,...)
  #
  abline(fit,col="darkred")
}
# exp: regplot(Price,Sales,xlab="Price",ylab="Sales",col="darkblue",pch=20)
# my own fit control function
myfit_control2 <- function(horizon,initialWindow){
  # fit control timeslices for timeseries, from 2 seasons, horizon 4 weeks
  if (horizon == 1){horizon <- 2}
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = initialWindow,
                             horizon = horizon,
                             fixedWindow = TRUE)
  return(fitControl)
}
# exp: regplot(Price,Sales,xlab="Price",ylab="Sales",col="darkblue",pch=20)
# my own fit control function
myfit_control <- function(horizon){
  # fit control timeslices for timeseries, from 2 seasons, horizon 4 weeks
  if (horizon == 1){horizon <- 2}
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 104,
                             horizon = horizon,
                             fixedWindow = TRUE)
  return(fitControl)
}
# my predictor function
my_predictors_lag <- function(choose_predictors,choose_lags,name_predictors,DF,tchoice_v){
  name_predictors <- paste(choose_predictors,choose_lags,sep="_")
  # number of predictors
  num_pred <- length(choose_predictors)
  # initiate date frame with first predictor
  X <- as.data.frame(DF[tchoice_v - choose_lags[1],choose_predictors[1]])
  names(X)[1] <- name_predictors[1]
  # loop through the rest
  for(i in 2:num_pred){
    X[name_predictors[i]] <- as.data.frame(DF[tchoice_v - choose_lags[i],choose_predictors[i]])
    X[name_predictors[i]] <- as.data.frame(DF[tchoice_v - choose_lags[i],choose_predictors[i]])
  }
  return(X)
}
####################################

# Generate bins based on normal distribution
gen.prob.distr <- function(mean, sd, log.scale, breaks.in) {
  breaks <- breaks.in
  if (log.scale) breaks <- log(breaks.in + 1)
  prob.distr <- numeric(length(breaks))
  for (i in 1:length(prob.distr)) {
    prob.distr[i] <- pnorm(breaks[i+1], mean, sd) - pnorm(breaks[i], mean, sd)
  }
  prob.distr[length(prob.distr)] <- 1 - pnorm(breaks[length(breaks)], mean, sd)
  prob.distr <- prob.distr/sum(prob.distr)
  return(prob.distr)
}


# this function takes in a character data frame and returns the unique non-number elements
UniqueNonNumbs <- function(a.data.frame){
  out <- unique(a.data.frame[(grepl("^([[:digit:].,]+)$", a.data.frame))==FALSE])
  return(out)
}

# True if a columncontains at least one number (for deciding what to change to as.numeric)
AreThereNums <- function(a.data.frame){
  out <- any(grepl("^([[:digit:].,]+)$", a.data.frame))
  return(out)
}

#
what_UniqueNonNumbs <- function(a.data.frame){
  out <- unique(a.data.frame[(grepl("^([[:digit:].,]+)$", a.data.frame))==FALSE])
  return(out)
}

#
N_UniqueNonNumbs <- function(a.data.frame){
  out <- unique(a.data.frame[(grepl("^([[:digit:].,]+)$", a.data.frame))==FALSE])
  out <- length(out)
  return(out)
}

#### old functions
# give number of NAs per column of a dataframe
give.NAs <- function(dataframe){
  num.of.vars <- dim(dataframe)[2]
  num.NAs <- dataframe[1,]
  num.NAs <- num.NAs %>% mutate_each(funs(as.numeric))
  for (i in 1:num.of.vars){
    num.of.na <- sum(is.na(dataframe[,i]))
    num.NAs[1,i] <- num.of.na
  }
  return(num.NAs)
}
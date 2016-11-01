######################
### my functions

####################################
# Topic: Statistical learning
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
myfit_control <- function(horizon){
  # fit control timeslices for timeseries, from 2 seasons, horizon 4 weeks
  fitControl <- trainControl(method = "timeslice",
                             initialWindow = 60,
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
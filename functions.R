######################
### my functions
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
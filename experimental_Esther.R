###### Flu Forecasting Challenge ####### 

## title: "LASSO testing"
## author: "Underdog"
## date: "18 October 2016"

# empty workspace
rm(list = ls())
script_name <- "experimental_Esther"

# libraries
library(tidyverse);library(lubridate);library(forecast)
library(xts);library(glmnet);library(MASS)
########################################
### preamble
########################################

load("./Data/data_manip.Rda")
# select input variables
DF <- usflu
DF <- DF %>% 
  dplyr::select(-region,-region.type)
# only keep DF
rm(usflu)

########################################
#### Explore data
########################################
names(DF)
plot(DF$cases,pch=20, ylab="Cases", xlab="Time")
# make time series
my.ys <- ts(DF$cases,frequency=52, start=c(1997,40))

# decompose time series
my.stl <- stl(my.ys, s.window="periodic",na.action = na.approx)
plot(my.stl)

# timepoints
train.set <- 52*5+2 # 5 years of training data
first.predict <- train.set + 1
last.prediction <- dim(DF)[1] 

# Remove years with missing values
missing.vals <- which(is.na(DF$cases))
last.miss.val <- missing.vals[length(missing.vals)] 

DF1 <- DF[(last.miss.val + 1):last.prediction,]

# attach dataframe
attach(DF1)

# Check distribution of weekly cases
par(mfrow=c(1,2))
ln = fitdistr(log(cases+1),"lognormal")
hist(cases, main = "Distribution cases", xlab="Weekly cases",freq=F);hist(log(cases+1), main = "Distribution log(cases)", xlab="log weekly cases",freq=F)
lines(density(rlnorm(10000, meanlog=ln$estimate[1], sdlog=ln$estimate[2])), col="red") 
# Log normal seems to be reasonable assumption; could also try to fit negative binomial/poisson

########################################
# Train model
########################################
# log transform data
log.cases <- log(cases + 1)
ts.cases = ts(cases,frequency=52, start=c(1997,40))
# limit data to data available at train.time
log.cases <- log.cases[1:train.set]
train.data <- DF1[1:train.set,]

my.lag = 4

# Fit simple model with lag 4 and week as factor
xreg = as.matrix(cbind(lag1=lag(log.cases,my.lag)[(1 + my.lag):train.set],week=factor(week[(1 + my.lag):train.set])))

# # fit simple linear regression
# lm0 = lm(log.cases[(1 + my.lag):train.set]~xreg[,1]+xreg[,2])

# fit LASSO regression
fit0 = glmnet(y=log.cases[(1 + my.lag):train.set],x=xreg, family="gaussian")

# capture model fit LASSO for plotting
model.fit0 <- predict.glmnet(fit0, s=0.01,newx=xreg[1:(train.set - my.lag),], type="response")

model.fit0 <- data_frame(
  pred=as.numeric(model.fit0), 
  se=rep(sd(model.fit0), length(model.fit0))) %>%
  mutate(
    t.idx = (1 + my.lag):train.set,
    point.pred = exp(pred) - 1,
    lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
    upr95.pred = exp(pred + qnorm(0.975) * se) - 1
  )


par(mfrow=c(1,1))
plot(model.fit0$t.idx[1:max(model.fit0$t.idx)], cases[1:max(model.fit0$t.idx)], 
     pch=19, cex=0.25,
     xlab="Time", ylab="cases", main="Fit LASSO lag 4 weeks + Seasonality (no forecast)")
polygon(x=c(model.fit0$t.idx, rev(model.fit0$t.idx)),
        y=c(model.fit0$upr95.pred, rev(model.fit0$lwr95.pred)), 
        col=adjustcolor('darkblue', 0.25), border=F)
lines(model.fit0$t.idx, model.fit0$point.pred, 
      col=adjustcolor('darkblue', 0.5), lwd=3)


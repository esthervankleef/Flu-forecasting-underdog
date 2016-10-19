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
### Preamble
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
t.idx <- 1:(52*10+2)
train.set <- 52*5+2 # 5 years of training data
first.predict <- train.set + 1
last.prediction <- dim(DF)[1] 

# Remove years with missing values
missing.vals <- which(is.na(DF$cases))
last.miss.val <- missing.vals[length(missing.vals)] 

DF1 <- DF[(last.miss.val + 1):last.prediction,]

# Check distribution of weekly cases
par(mfrow=c(1,2))
ln = fitdistr(log(cases+1),"lognormal")
hist(cases, main = "Distribution cases", xlab="Weekly cases",freq=F);hist(log(cases+1), main = "Distribution log(cases)", xlab="log weekly cases",freq=F)
lines(density(rlnorm(10000, meanlog=ln$estimate[1], sdlog=ln$estimate[2])), col="red") 
# Log normal seems to be reasonable assumption; could also try to fit negative binomial/poisson


# DF1$log.cases <- log(DF1$cases + 1)
ts.cases = ts(DF1$cases,frequency=52, start=c(1997,40))

# attach dataframe
attach(DF1)
########################################
# Train model
########################################
# log transform data
log.cases <- log(cases + 1)

my.lag = 2

# limit data to data available at train.time
log.cases <- log.cases[1:train.set]

# Fit simple model with lag 4 and week as factor
xreg <- as.matrix(cbind(lag4=log.cases[1:train.set],week=factor(week+my.lag)[1:train.set])) # Add lag to week, to make sure current week is taken, as this is known data and for seasonality no lag needed

# # fit simple linear regression
# lm0 = lm(log.cases[(1 + my.lag):train.set]~xreg[,1]+xreg[,2])

# fit LASSO regression
fit0 = glmnet(y=log.cases[(1 + my.lag):train.set],x=xreg[1:(train.set - my.lag),], family="gaussian")

# capture model fit LASSO for plotting
model.fit0 <- predict.glmnet(fit0, s=0.01,newx=xreg[1:(train.set - my.lag),], type="response") # Arbitrary chocie of penalty term lambda

model.fit0 <- data_frame(
  pred=as.numeric(model.fit0), 
  se=rep(sd(model.fit0), length(model.fit0))) %>% # Not calculating se correct yet, need to find out how to extract residuals
  mutate(
    t.idx = (1 + my.lag):train.set,
    point.pred = exp(pred) - 1,
    lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
    upr95.pred = exp(pred + qnorm(0.975) * se) - 1
  )

# Forecast
forecast.wks.ahead <- my.lag
forecast <- predict.glmnet(fit0, n.ahead=forecast.wks.ahead,s=0.01, 
                    newx=xreg[train.set - my.lag + 1:forecast.wks.ahead,])

### calculate mean predictions and 95% CIs
forecast <- as.data.frame(forecast) 
names(forecast) = "pred"
forecast <- forecast %>% mutate(se = sd(pred),
    t.idx = train.set + 1:forecast.wks.ahead,
    point.pred = exp(pred) - 1,
    lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
    upr95.pred = exp(pred + qnorm(0.975) * se) - 1
  )
forecast # Not extracting the se correct from forecasting

par(mfrow=c(1,1))

plot(t.idx[1:max(forecast$t.idx)], cases[1:max(forecast$t.idx)], 
     pch=19, cex=0.25,
     xlab="date", ylab="cases")
polygon(x=c(model.fit0$t.idx, rev(model.fit0$t.idx)),
        y=c(model.fit0$upr95.pred, rev(model.fit0$lwr95.pred)), 
        col=adjustcolor('darkblue', 0.25), border=F)
lines(model.fit0$t.idx, model.fit0$point.pred, 
      col=adjustcolor('darkblue', 0.5), lwd=3)
polygon(x=c(forecast$t.idx, rev(forecast$t.idx)),
        y=c(forecast$upr95.pred, rev(forecast$lwr95.pred)), 
        col=adjustcolor('darkred', 0.25), border=F)
lines(forecast$t.idx, forecast$point.pred, 
      col=adjustcolor('darkred', 0.5), lwd=3)


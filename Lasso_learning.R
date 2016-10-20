###### Flu Forecasting Challenge ####### 

## title: "LASSO training"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "lasso_learning"

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
  dplyr::select(-region,-region.type) %>% mutate(cases = as.numeric(as.character(x.weighted.ili))) # NA is one missing value which was coded as X
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
t.idx <- 1:(52*11+2) # 10 years of training and testing
train.set <- 52*7+2 # 7 years of training data
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


# DF1$log.cases <- log(DF1$cases + 1)
ts.cases = ts(DF1$cases,frequency=52, start=c(1997,40))

########################################
# Train model
########################################

# Adjust data
week53 = DF1$year[which(DF1$week == 53)]

season=NULL
for(i in 1:(length(unique(DF1$year))-1)){
  if(!unique(DF1$year)[i] %in% week53){
    s = rep(i,52)
  }
  else{
    print(i)
    s = rep(i,53)
  }
    season = c(season,s)
}

DF1$season = season

seas = DF1%>%group_by(season) %>% summarise(seas.total = sum(log(cases+1))) %>% mutate(lag.seas.total=lag(seas.total,1))

DF1 = left_join(DF1, seas)

# add lag terms
DF1 = DF1 %>% mutate(lag4 = lag(log(cases+1), 4), 
                     lag5 = lag(log(cases+1), 5), 
                     lag6 = lag(log(cases+1), 6),
                     lag7 = lag(log(cases+1), 7),
                     lag8 = lag(log(cases+1), 8),
                     lag9 = lag(log(cases+1), 9),
                     lag10 = lag(log(cases+1), 10),
                     diff = cases - lag(cases, default=first(cases)))

# remove the NAs from data
DF1 = DF1 %>% filter(!is.na(lag4)&!is.na(lag5)&!is.na(lag6)&!is.na(lag7)&!is.na(lag8)&!is.na(lag9),!is.na(lag10)&!is.na(lag.seas.total)) %>% 
  mutate(age.25.49 = ifelse(age.25.49=="X", NA, age.25.49),age.50.64 = ifelse(age.50.64=="X", NA, age.50.64))

# log transform data
log.cases <- log(cases + 1) 

# limit data to data available at train.time+test.time
log.cases <- log.cases[1:max(t.idx)]

#  as.matrix(cbind(lag4=log.cases[1:train.set],week=factor(week+my.lag)[1:train.set])) # Add lag to week, to make sure current week is taken, as this is known data and for seasonality no lag needed
#trainDF <- DF1[1:train.set,]
#testDF <- DF1[(train.set + 1):max(t.idx),]

# Fit model with all lag terms and week as factor and total cases in previous season and difference term
xreg = DF1 %>% dplyr::select(week,lag.seas.total,lag4,lag5,lag6,lag7,lag8,lag9,lag10,diff)
xreg = as.matrix(xreg[1:max(t.idx),])


# # fit simple linear regression
# lm0 = lm(log.cases[(1 + my.lag):train.set]~xreg[,1]+xreg[,2])

# fit LASSO regression
fit0 = glmnet(y=log.cases[1:train.set],x=xreg[1:train.set,], family="gaussian")

head(fit0)
plot(fit0)

# capture model fit LASSO for plotting
model.fit0 <- predict.glmnet(fit0, s=c(0.005),newx=xreg[1:train.set,], type="response") # Arbitrary chocie of penalty term lambda

model.fit0 <- data_frame(
  pred=as.numeric(model.fit0), 
  se=rep(sd(model.fit0), length(model.fit0))) %>% # Not calculating se correct yet, need to find out how to extract residuals
  mutate(
    t.idx = c(1:train.set),
    point.pred = exp(pred) - 1,
    lwr95.pred = exp(pred - qnorm(0.975) * se) - 1,
    upr95.pred = exp(pred + qnorm(0.975) * se) - 1
  )

# Forecast
forecast.wks.ahead <- 4
forecast <- predict.glmnet(fit0, n.ahead=forecast.wks.ahead,s=0.005, 
                           newx=xreg[train.set + 1:forecast.wks.ahead,])

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


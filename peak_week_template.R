###### Flu Forecasting Challenge ####### 

## title: "Holidays"
## author: "Underdog"
## date: "2 November 2016"

######################
# Predict peak weeks

rm(list=ls())

script_name <- "peak_week_forecast"

# libraries
library(glmnet)
library(cdcfluview)

# load data
load("./Data/data_manip.Rda")
source("./functions.R")

# select input variables
DF <- usflu_allyears

rm(usflu, usflu_allyears)

## The data manipulation line
DF$week <- (DF$week - 31 ) %% 52

peak_week <- array(dim = length(1998:2015))
X_data <- array(dim = c(length(1998:2015), length(0:13)))
for(years in 1998:2015){
  year_data <- subset(DF, year==years)
  peak_week[years-1997] <- year_data$week[which.max(year_data$x.weighted.ili)]
  #take the last 5 weeks as predictors
  X_data[years-1997,] <- as.numeric(year_data$x.weighted.ili[year_data$week<14])
}
## 2009 is a massive outlier so removed it
r_fit <- glmnet(y=peak_week[-c(1:5,12)], x= t(apply(X = X_data[-c(1:5,12),], 1, diff)))
plot(r_fit, xvar='lambda', label=T)
plot(predict(r_fit, t(apply(X = X_data[-c(1:5,12),], 1, diff)), s = .5), peak_week[-c(1:5,12)],
     xlab='predicted', ylab = 'true', bty='n', cex.lab = 1.5);
lines(23:30, 23:30, type='l', lty=3)

# Store fitted vs data
datfit = data.frame(obs = peak_week[-c(1:5,12)],fit = predict(r_fit, t(apply(X = X_data[-c(1:5,12),], 1, diff)), s = .5))
names(datfit) = c("obs", "fit")
datfit$res = datfit$obs - datfit$fit 

## Mean prediction
pred_X <- diff(as.numeric(DF$x.weighted.ili[979:992]))
peak_week_prediction <- predict(r_fit, newx = t(pred_X) , s=.1)

mean_peak = round(peak_week_prediction+31)

## SD prediction
sd_peak = sd(datfit$res)

# Load prediction file
results = read.csv("./Forecasts/Submission_template.csv")
results$Value = NA

targets = c("Season onset")
nat_peakweek = which(results$Target==targets & results$Location=="US National")

# Calculate bins mean peak prediction
check  = (results[nat_peakweek,])
bins = unique(check$Bin_start_incl)[!unique(check$Bin_start_incl)%in%c("none")]
bins = as.numeric(as.character(bins[2:length(bins)])) # remove NA
bins = c(bins,21) #add 21, which will be probability for no peak happening

breaks.in = as.numeric(as.character(bins))
prob.forecast = data.frame(cbind(Bin_start_incl = breaks.in,prob = rep(NA,length(breaks.in))))

prob.forecast$prob = gen.prob.distr(mean=mean_peak, sd=sd_peak, log.scale=F, breaks.in=breaks.in)


#####################################################
# Store forecasts

point = mean_peak
results$Value[nat_peakweek] = c(point,prob.forecast$prob) 

########################################
#### save & load
savename <- paste0("./Forecasts/", script_name, ".csv")
write.csv(results,file = savename)


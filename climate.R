#####################################
# GET CLIMATE DATA

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "19 October 2016"
rm(list=ls())
script_name <- "clim_data"

library(data.table); library(dplyr)

# Load data
clim <- data.frame(fread('https://www.ncdc.noaa.gov/temp-and-precip/us-weekly/wkly.csv'))

forecast_week = length(clim$weekEnding)-length(c((1:52),c(1:53),c(1:52)))

# Add week number, year 2014 has 53 weeks
clim$week = c(c(1:52),c(1:53),c(1:52),c(1:forecast_week)) 
clim$week = ifelse(clim$week %in%c(1:9), paste0("0",clim$week), clim$week) # adds 0 to single numbers
clim$year = c(rep("2013",52),rep("2014",53),rep("2015",52),rep("2016",forecast_week)) # idem

# Add week number, year 2014 has 53 weeks
clim$week = c(c(1:52),c(1:53),c(1:52),c(1:47)) # When loading in new data, change the last figure to the current week
clim$week = ifelse(clim$week %in%c(1:9), paste0("0",clim$week), clim$week)
clim$year = c(rep("2013",52),rep("2014",53),rep("2015",52),rep("2016",47)) # idem

# Create weekname variable
clim$weekname = paste0(clim$year, "-",clim$week)

# Plot temp
plot(clim$nat, main="Temperature (F)", ylim=c(0,100))
col=rainbow(length(c(2:10)))
for(i in c(2:10)){
  lines(clim[,i], col=col[i])  
}

# Temperature patterns seems quite similar across the regions

#  Plot Temperature anomalies
plot(clim$natAnom, main="Temperature anomaly", ylim=c(-15,15))
col=rainbow(length(c(11:20)))
for(i in c(11:20)){
  lines(clim[,i], col=col[i-10])  
}

clim = clim%>%dplyr::select(weekname,nat,natAnom) %>% mutate(nat = as.numeric(nat), 
                                                             natAnom=as.numeric(natAnom))
names(clim) = c("weekname","temp_av","temp_anom_av")


########################################
#### save & load
savename <- paste0("./Data/", script_name, ".Rda")
save(clim,file = savename)

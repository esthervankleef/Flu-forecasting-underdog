#####################################
# GET CLIMATE DATA

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "19 October 2016"


rm(list=ls())
script_name <- "clim_data"
# Downloaded from: http://www.ncdc.noaa.gov/temp-and-precip/us-weekly/
# Click on excel logo. Then I copy pasted the text file into excel, saved it as a text and then imported
# this text file in a new spread sheet and saved it as .csv

# In this new spread sheet, I have manually removed the first row (saying temp in F etc), and the " from
# first and last column. Then saved this as weekly_temp_raw.csv

# Load data
clim = read.csv("./Data/Weekly_temp_raw47.csv", colClasses="character")

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

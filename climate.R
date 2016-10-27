#####################################
# GET CLIMATE DATA

rm(list=ls())
script_name <- "clim_data"
# Downloaded from: http://www.ncdc.noaa.gov/temp-and-precip/us-weekly/
# Click on excel logo. Then I copy pasted the text file into excel, saved it as a text and then imported
# this text file in a new spread sheet and saved it as .csv

# In this new spread sheet, I have manually removed the first row (saying temp in F etc), then saved this 
# as weekly_temp_raw.csv

# Load data
clim = read.csv("./Data/Weekly_temp_raw.csv", colClasses="character")

# Add week number, year 2014 has 53 weeks
clim$weekno = c(c(1:52),c(1:53),c(1:52),c(1:42)) # When loading in new data, change the last figure to the current week
clim$year = c(rep("2013",52),rep("2014",53),rep("2015",52),rep("2016",42)) # idem

# Create weekname variable
clim$weekname = paste0(clim$year, "-",clim$weekno)

# Plot temp
plot(clim$nat, main="Temperature (F)", ylim=c(0,100))
col=rainbow(length(c(2:10)))
for(i in c(2:10)){
  lines(clim[,i], col=col[i])  
}

# Temperature patterns seems quite similar across the regions


#  Plot prec
plot(clim$natAnom, main="Precipitation anomaly", ylim=c(-15,15))
col=rainbow(length(c(11:20)))
for(i in c(11:20)){
  lines(clim[,i], col=col[i-10])  
}

clim = clim%>%dplyr::select(weekname,nat,natAnom)
names(clim) = c("weekname","temp_av","prec_anom_av")




########################################
#### save & load
savename <- paste0("./Data/", script_name, ".Rda")
save(clim,file = savename)
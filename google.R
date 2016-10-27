#####################################
# Google data

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "19 October 2016"

rm(list=ls())
script_name <- "google_data"

# libraries
library(tidyverse)
library(stringr)

g08 = read.csv("./Data/multiTimeline08.csv")
g09 = read.csv("./Data/multiTimeline09.csv")
g10 = read.csv("./Data/multiTimeline10.csv")
g11 = read.csv("./Data/multiTimeline11.csv")
g12 = read.csv("./Data/multiTimeline12.csv")
g13 = read.csv("./Data/multiTimeline13.csv")
g14 = read.csv("./Data/multiTimeline14.csv")
g15 = read.csv("./Data/multiTimeline15.csv")
g16 = read.csv("./Data/multiTimeline16.csv")


gdat = list(g08,g09,g10,g11,g12,g13,g14,g15,g16)

google = NULL
for(i in 1:length(gdat)){
  gdat[[i]]$weekno = c(1:length(gdat[[i]]$Week))
  gdat[[i]]$weekno = ifelse(gdat[[i]]$weekno %in%c(1:9), paste0("0",gdat[[i]]$weekno), gdat[[i]]$weekno)
  gdat[[i]]$year = 2000+i+7
  names(gdat[[i]]) <- c("week","gfever","gheadache","gdoctor","gshivering","gcough","weekno","year")
  gdat[[i]]$weekname = paste0(gdat[[i]]$year,"-",gdat[[i]]$weekno)
  google = rbind(google,gdat[[i]])
}

google = google%>%dplyr::select(weekname,gfever,gheadache,gdoctor,gshivering, gcough)

########################################
#### save & load
savename <- paste0("./Data/", script_name, ".Rda")
save(google,file = savename)

###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "20 October 2016"

# empty workspace
rm(list = ls())
script_name <- "seas_times"

# libraries
library(tidyverse)
########################################
#### load the data
load("./Data/data_manip.Rda")

DF <- usflu_allyears

# identify where there are 53 weeks 
week53 = usflu$year[which(usflu$week == 53)]
# give each timepoint the season name
season=NULL
for(i in 1:(length(unique(usflu$year))-1)){
  if(!unique(usflu$year)[i] %in% week53){
    s = rep(i,52)
  }
  else{
    s = rep(i,53)
  }
  season = c(season,s)
}
usflu$season = season

# Add co-variate 
epi.treshold = 2.2
usflu = usflu %>% group_by(season) %>% mutate(tresh_weeks = ifelse(x.weighted.ili > epi.treshold,1,0),
                                              start_seas = ifelse(duplicated(tresh_weeks), 0, tresh_weeks),
                                              end_seas = cumsum(tresh_weeks),
                                              end_seas = ifelse(tresh_weeks==1 & end_seas==max(end_seas, na.rm=T),1,0),
                                              peak_seas = ifelse(x.weighted.ili==max(x.weighted.ili, na.rm=T),1,0))
# Check whether one observation per season
# table(usflu$season,usflu$start_seas); table(usflu$season,usflu$end_seas);table(usflu$season,usflu$peak_seas)
# setwd("~/Dropbox/Forecasting Flu Challenge/Figures/")
# png(filename = "Distr_seas_start_end_peak.png",width=500, height=250)
# par(mfrow=c(1,3))
# hist(usflu$week[which(usflu$start_seas==1)], xlab = "Season", ylab="Week", main = "Start season", breaks=c(1:53))
# hist(usflu$week[which(usflu$peak_seas==1)], xlab = "Season", ylab="Week", main = "Peak season",breaks=c(1:53))
# hist(usflu$week[which(usflu$end_seas==1)], xlab = "Season", ylab="Week", main = "End season",breaks=c(1:53))
# dev.off()

m_start_seas = median(usflu$week[which(usflu$start_seas==1)])
m_end_seas = median(usflu$week[which(usflu$end_seas==1)])
m_peak_seas = median(usflu$week[which(usflu$peak_seas==1)])

usflu = usflu %>% mutate(m_start_seas = ifelse(week == m_start_seas,1,0),
                         m_end_seas = ifelse(week == m_end_seas,1,0),
                         m_peak_seas = ifelse(week == m_peak_seas,1,0))

seas_times =  usflu[,c("weekname","tresh_weeks","start_seas","end_seas",
                             "peak_seas","m_start_seas","m_end_seas","m_peak_seas")]

########################################
#### save & load
savename <- paste0("./Data/", script_name, ".Rda")
save(seas_times,file = savename)

###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "17 October 2016"

# empty workspace
rm(list = ls())
script_name <- "data_manip"
# libraries
library(tidyverse)
library(stringr)

########################################
# load raw data
load("./Data/flu_data.Rda")
# rename data file
usflu <- usflu_raw
rm(usflu_raw)

########################################
### modify columns names and cast columns
# change the names
names(usflu) <- tolower(names(usflu)) %>% 
        gsub("..",".",., fixed=TRUE)
# arrange by time
usflu <- usflu %>% 
        arrange(year,week)
# add variable for week name
usflu <- usflu %>% 
        mutate(weekname = paste(year,str_pad(week, 2, pad = "0"),sep="-"))

###
# check how many missing
source("./functions.R") # add all underdog functions
what_UniqueNonNumbs(usflu$total.patients)
what_UniqueNonNumbs(usflu$ilitotal)
#
# there are some missing data given through "X". Converting into numeric will introduce NAs

# convert into numeric
usflu <- usflu %>% 
        mutate(ilitotal = as.numeric(ilitotal),
               total.patients = as.numeric(total.patients),
               x.weighted.ili = as.numeric(x.weighted.ili))
# add output variable: ilitotal/total.patients
usflu <- usflu %>% 
  mutate(cases = 100*ilitotal/total.patients)

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

epi.treshold = 2.2
usflu = usflu %>% group_by(season) %>% mutate(start_seas = ifelse(x.weighted.ili > epi.treshold,1,0))


usflu_allyears <- usflu
# cut off the NA by truncating
missing.vals <- which(is.na(usflu_allyears$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(usflu_allyears)[1] 
usflu <- usflu_allyears[(last.miss.val + 1):last.prediction,]


###################################
#### save
savename <- paste0("./Data/", script_name,".Rda")
save(usflu,usflu_allyears, file = savename)

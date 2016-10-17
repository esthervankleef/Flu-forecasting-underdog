###### Flu Forecasting Challenge ####### 

## title: "Forecasting Flu Template"
## author: "Underdog"
## date: "17 October 2016"

# empty workspace
rm(list = ls())
script_name <- "data_manip"
# libraries
library(tidyverse)

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
               total.patients = as.numeric(total.patients))
# add output variable: ilitotal/total.patients
usflu <- usflu %>% 
  mutate(cases = ilitotal/total.patients)

###################################
#### save
savename <- paste0("./Data/", script_name,".Rda")
save(usflu, file = savename)

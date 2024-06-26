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
        gsub(".."," ",., fixed=TRUE) %>%
        gsub("."," ",., fixed=TRUE) %>%
        gsub("  "," ",., fixed=TRUE) %>%
        gsub(" ",".",., fixed=TRUE) %>%
        gsub("%","x",., fixed =TRUE)

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
# usflu <- usflu %>% 
#   mutate(cases = 100*ilitotal/total.patients) THIS SHOULD BE WEIGHTED ILI CASES!! 

usflu <- usflu %>% 
     mutate(cases = x.weighted.ili)

usflu_allyears <- usflu
# cut off the NA by truncating
missing.vals <- which(is.na(usflu_allyears$cases))
#
last.miss.val <- missing.vals[length(missing.vals)]
#
last.prediction <- dim(usflu_allyears)[1] 
most_current_week <- paste(usflu$year[last.prediction],str_pad(usflu$week[last.prediction], 2, pad = "0"),sep="-")
#most_current_week <- paste(usflu$year[last.prediction],usflu$week[last.prediction],sep="-")
most_current_week_int <- usflu$week[last.prediction]

usflu <- usflu_allyears[(last.miss.val + 1):last.prediction,]

#

###################################
#### save
savename <- paste0("./Data/", script_name,".Rda")
save(usflu,usflu_allyears,most_current_week,most_current_week_int, file = savename)


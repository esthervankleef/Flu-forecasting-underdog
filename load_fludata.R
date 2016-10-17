#### Looking at the flu data ####
# ...
# Author: Rene Niehus
# Date: 14 October 2016

# clear workspace
rm(list = ls())
script_name <- "flu_data"

# libraries
library(cdcfluview)
library(stringr)

# downlaod the flu data
usflu <- get_flu_data("national", "ilinet", years=1997:2015)
# change the names
names(usflu) <- tolower(names(usflu)) %>% 
        gsub("..",".",., fixed=TRUE)
# arrange by time
usflu <- usflu %>% 
        arrange(year,week)
# add variable for week name
usflu <- usflu %>% 
        mutate(weekname = paste(year,str_pad(week, 2, pad = "0"),sep="-"))
# convert into numeric
usflu <- 

###################################
#### save
savename <- paste0("../Data/", script_name,".Rda")
save(usflu, file = savename)

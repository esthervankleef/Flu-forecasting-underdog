#### Load and save flu data ####
# ...
# Author: Rene Niehus
# Date: 14 October 2016

# clear workspace
rm(list = ls())
script_name <- "flu_data"

# libraries
library(tidyverse)
library(cdcfluview)
library(stringr)

# downlaod the flu data
usflu <- get_flu_data("national", "ilinet", years=1997:2016)
usflu_raw <- usflu

###################################
#### save
savename <- paste0("./Data/", script_name,".Rda")
save(usflu_raw, file = savename)


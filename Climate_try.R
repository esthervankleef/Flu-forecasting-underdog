
library(weatherData)
library(rnoaa)
library(devtools)
devtools::install_github("adamhsparks/GSODR")

library(GSODR)
setwd("./Data/")

# Weatherdata package
stat = getStationCode("New York", region="US")

# rnoaa package
ghcnd_countries()
homr(headersOnly=TRUE, country='GHANA')

get_GSOD(years = 2010:2011, dsn = "~/", filename = "GSOD-agroclimatology",
         agroclimatology = TRUE, max_missing = 5, threads = 3)

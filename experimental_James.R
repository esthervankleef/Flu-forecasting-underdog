# Some experimental code to see how the k-nearest neighbours algorithm works on time-series data


library(class)
# load the flu data file
load(file = 'D:/Dropbox/Forecasting Flu Challenge/Data/flu_data.Rda')

# outcome of interest is percent of ILI?
usflu$ili_percent <- 100*as.numeric(usflu$ilitotal)/as.numeric(usflu$total.patients)
# how far to look for comparison in knn (two years is 104 weeks)
look_back <- 104


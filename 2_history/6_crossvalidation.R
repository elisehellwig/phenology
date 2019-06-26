

# Setup -------------------------------------------------------------------
library(dismo)
library(plyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(phenoclim)
source('functions/cvfunction.R')

options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

harv <- readRDS(file.path(historypath, 'harvestfit.RDS'))
tts <- readRDS(file.path(historypath, 'ThermalTimeSeries.RDS'))
fullLocVar <- read.csv(file.path(historypath, 'SeasonLengthParameters.csv'))


# Errors ------------------------------------------------------------------

locVar <- fullLocVar %>% 
    select(crop, cultivar, threshold) %>% 
    unique

datalist <- lapply(1:nrow(locVar), function(i) {
    filter(harv, crop==locVar[i,'crop'], cultivar==locVar[i, 'cultivar'])
})

locVar$RMSE <- sapply(1:nrow(locVar), function(i) {
    round(rmse(datalist[[i]]$fitlength, datalist[[i]]$length1),2)
})

locVar$RMSE30 <- sapply(1:nrow(locVar), function(i) {
    round(rmse(datalist[[i]]$fitlength30, datalist[[i]]$length1),2)
})

locVar$RMSEnull <- sapply(1:nrow(locVar), function(i) {
    round(rmse(datalist[[i]]$fitlengthnull, datalist[[i]]$length1),2)
})


# crossvalidation ---------------------------------------------------------

locVar$RMSEcv <- sapply(1:length(datalist), function(i) {
    round(CV(datalist[[i]], 'fitlength','length1', seed=248938),2)
})

locVar$RMSE30cv <- sapply(1:length(datalist), function(i) {
    round(CV(datalist[[i]], 'fitlength30','length1', seed=248938), 2)
})

locVar$RMSEnullcv <- sapply(1:length(datalist), function(i) {
    round(CV(datalist[[i]], '1','length1', seed=248938), 2)
})


write.csv(locVar, file.path(historypath, 'SeasonLengthErrors.csv'),
          row.names=FALSE)


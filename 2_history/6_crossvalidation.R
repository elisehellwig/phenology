

# Setup -------------------------------------------------------------------
library(plyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(phenoclim)
source('functions/extractlm.R')

options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

harv <- readRDS(file.path(historypath, 'harvestfit.RDS'))
tts <- readRDS(file.path(historypath, 'ThermalTimeSeries.RDS'))
locVar <- read.csv(file.path(historypath, 'SeasonLengthParameters.csv'))


# Errors ------------------------------------------------------------------

datalist <- lapply(1:nrow(locVar), function(i) {
    filter(harv, crop==locVar[i,'crop'], loc==locVar[i, 'loc'], 
           cultivar==locVar[i, 'cultivar'])
})

locVar$RMSE <- sapply(1:nrow(locVar), function(i) {
    round(rmse(datalist[[i]]$fitlength, datalist[[i]]$length1),2)
})

locVar$RMSE30 <- sapply(1:nrow(locVar), function(i) {
    round(rmse(datalist[[i]]$fitlength30, datalist[[i]]$length1),2)
})






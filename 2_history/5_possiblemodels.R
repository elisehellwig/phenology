
# Setup -------------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(reshape2)
library(plyr)
library(phenoclim)

options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

harv <- readRDS(file.path(historypath, 'harvest.RDS'))
tts <- readRDS(file.path(historypath, 'ThermalTimeSeries.RDS'))

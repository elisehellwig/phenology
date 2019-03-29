

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



# Setup -------------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(reshape2)
library(plyr)
library(phenoclim)

options(stringsAsFactors = FALSE)
source('functions/datetime.R')
source('functions/thermaltimesupport.R')

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

spring <- read.csv(file.path(historypath, 'spring.csv'), 
                    stringsAsFactors = FALSE)


temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")



# Average Event Days ------------------------------------------------------

factorlist <- list(harvest$crop, harvest$loc, harvest$cultivar)

avg1 <- tapply(harvest$event1, factorlist, mean, na.rm=TRUE)

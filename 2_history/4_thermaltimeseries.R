
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
locVar <- read.csv(file.path(historypath, 'SeasonLengthParameters.csv'), 
                   stringsAsFactors = FALSE)


temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")



# Almonds -----------------------------------------------------------------

aLocVar <- filter(locVar, crop=='almond')

a <- spring %>% 
    filter(crop=='almond', event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)

asl <- ldply(1:nrow(aLocVar), function(i) {
    calcThermalTime(a, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    aLocVar[i, 'threshold'], c('start','threshold'), 
                    location = aLocVar[i,'loc'], var=aLocVar[i,'cultivar'],
                    predictor='thermal')
    
})


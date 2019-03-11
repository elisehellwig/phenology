
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

a <- spring %>% 
    filter(crop=='almond', event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)

asl <- ldply(1:4, function(i) {
    calcThermalTime(a, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    locVar[i, 'threshold'], c('start','threshold'), 
                    location = locVar[i,'loc'], var=locVar[i,'cultivar'],
                    predictor='thermal')
    
})


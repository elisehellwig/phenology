
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

aLocVar <- filter(locVar, crop=='almond', cultivar != 'Sonora')

a <- spring %>% 
    filter(crop=='almond', event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)

asl <- ldply(1:nrow(aLocVar), function(i) {
    calcThermalTime(a, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    aLocVar[i, 'threshold'], c('start','threshold'), 
                    location = aLocVar[i,'loc'], var=aLocVar[i,'cultivar'],
                    predictor='thermal')
    
})


# Prune -------------------------------------------------------------------

p <- spring %>% 
    filter(crop=='prune',event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)

psl <- ldply(unique(p$loc), function(l) {
    calcThermalTime(p, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    locVar[5, 'threshold'], c('start','threshold'), 
                    location = l, predictor='thermal')
})


# Walnut ------------------------------------------------------------------

wLocVar <- filter(locVar, crop=='walnut')

w <- spring %>% 
    filter(crop=='walnut',event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)

ct <- list(c(0.4, 12.1), 11.1, c(4,25,36))

wsl <- ldply(1:nrow(wLocVar), function(i) {
    calcThermalTime(w, temp, 'harvest', wLocVar[i, 'modtype'], 
                    wLocVar[i,'form'], ct[[i]], 0, wLocVar[i, 'threshold'], 
                    c('start','threshold'), var=wLocVar[i,'cultivar'],
                    predictorName='thermal')
})



# Combine data and save ---------------------------------------------------

sl <- rbind(asl, psl, wsl)

saveRDS(sl, file.path(historypath, 'ThermalTimeSeries.RDS'))



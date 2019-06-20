
# Setup -------------------------------------------------------------------

library(plyr)
library(tidyverse)
library(lubridate)
library(reshape2)
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

aLocVar <- locVar %>% 
    filter(crop=='almond')

a <- spring %>% 
    filter(crop=='almond', event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)


# afill <- ldply(1:nrow(aLocVar), function(i) {
#     fillPhenology(a, 'event1', 1930, 2017, aLocVar[i,'loc'],
#                   aLocVar[i,'cultivar'])
# })


asl <- ldply(1:nrow(aLocVar), function(i) {
    print(aLocVar[i,])
    calcThermalTime(a, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    aLocVar[i, 'threshold'], c('start','threshold'), 
                    var=aLocVar[i,'cultivar'], location=aLocVar[i, 'loc'],
                    predictor='thermal')
    
})

asl$crop <- 'almond'
# Prune -------------------------------------------------------------------

p <- spring %>% 
    filter(crop=='prune',event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)

# pfill <- ldply(c('Chico','Parlier'), function(l) {
#     fillPhenology(p, 'event1', 1930, 2017, l, 'French')
# })


psl <- ldply(unique(p$loc), function(l) {
    calcThermalTime(p, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    locVar[5, 'threshold'], c('start','threshold'), 
                    location = l, predictor='thermal')
})

psl$crop <- 'prune'

# Walnut ------------------------------------------------------------------

wLocVar <- filter(locVar, crop=='walnut')

w <- spring %>% 
    filter(crop=='walnut',event=='event1') %>% 
    select(loc, cultivar, year, 'event1'=day)


# wfill <-  ldply(1:nrow(wLocVar), function(i) {
#     fillPhenology(w, 'event1', 1930, 2017, wLocVar[i,'loc'],
#                   wLocVar[i,'cultivar'])
# })


wsl <- ldply(1:nrow(wLocVar), function(i) {
    calcThermalTime(w, temp, 'harvest', 'DT', 'asymcur', c(4,25,36),
                    0, wLocVar[i, 'threshold'], c('start','threshold'), 
                    var=wLocVar[i,'cultivar'], predictorName='thermal')
})


wsl$crop <- 'walnut'
# Combine data and save ---------------------------------------------------

sl <- rbind(asl, psl, wsl)

saveRDS(sl, file.path(historypath, 'ThermalTimeSeries.RDS'))



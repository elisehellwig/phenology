
# This script 

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

a <- unique(read.csv(file.path(phenologypath,'almondclean.csv')))
p <- unique(read.csv(file.path(phenologypath,'pruneclean.csv')))
w <- read.csv(file.path(phenologypath,'walnutclean.csv'))
precip <- read.csv(file.path(historypath, 'precipitation.csv'))

amod <- readRDS(file.path(historypath, 'SLalmondDTanderson.RDS'))
pmod <- readRDS(file.path(historypath, 'SLpruneDTanderson.RDS'))
wmod <- readRDS(file.path(historypath, 'SLwalnutDTanderson.RDS'))


temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")


# Extract model thresholds ------------------------------------------------
locVar <- expand.grid('almond', c('Chico','Modesto'), 
                      c('Mission','Nonpareil','Sonora'))
names(locVar) <- c('crop','loc','cultivar')

locVar$threshold <- sapply(amod, function(m) round(unlist(threshold(m))))

pruneThreshold <- round(unlist(threshold(pmod)))

locVar <- rbind(locVar, data.frame(crop='prune',
                                   loc='Parlier',
                                   cultivar='French',
                                   threshold=pruneThreshold))


# Almond Harvest ----------------------------------------------------------
ah <- a %>% 
    filter(source=='RAVT', loc %in% c('Chico','Modesto'),
                    cultivar %in% c('Nonpareil','Mission','Sonora'))

aLocVar <- filter(locVar, crop=='almond')

aslopt <- ldply(1:nrow(aLocVar), function(i) {
    calcThermalTime(ah, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    aLocVar[i, 'threshold'], c('start','threshold'), 
                    location = aLocVar[i,'loc'], var=aLocVar[i,'cultivar'],
                    predictor='thermal')

})

asl30 <- ldply(1:nrow(aLocVar), function(i) {
    calcThermalTime(ah, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    30, c('start','threshold'), 
                    location = aLocVar[i,'loc'], var=aLocVar[i,'cultivar'],
                    predictor='thermal30')
})

asl <- merge(aslopt, asl30)

asl$length1 <- asl$event2 - asl$event1
asl$crop <- 'almond'

# Prune harvest -----------------------------------------------------------

pslopt <- calcThermalTime(p, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                       pruneThreshold, c('start','threshold'), 
                       location = 'Parlier', var='French', 
                       predictorName = 'thermal')


psl30 <- calcThermalTime(p, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                       30, c('start','threshold'), 
                       location = 'Parlier', var='French', 
                       predictorName = 'thermal30')

psl <- merge(pslopt, psl30)

psl$length1 <- psl$event2 - psl$event1

psl$crop <- 'prune'

# Walnut harvest ----------------------------------------------------------

cv <- c('Chandler','Payne','Franquette')
w$loc <- 'Davis'

locVar2 <- data.frame(crop='walnut',
                      loc='Davis',
                      cultivar=cv,
                      threshold=c(49,65,36)) 
locVar <- rbind(locVar, locVar2)
write.csv(locVar, file.path(historypath, 'SeasonLengthParameters.csv'),
          row.names=FALSE)

wLocVar <- filter(locVar, crop=='walnut')
wslopt <- ldply(1:nrow(wLocVar), function(i) {
    calcThermalTime(w, temp, 'harvest', 'DT', 
                    'asymcur', c(4,25,36), 0, wLocVar[i, 'threshold'], 
                    c('start','threshold'), var=wLocVar[i,'cultivar'],
                    predictorName='thermal')
    })


wsl30 <- ldply(1:nrow(wLocVar), function(i) {
    calcThermalTime(w, temp, 'harvest', 'DT', 
                    'asymcur', c(4,25,36), 0, wLocVar[i, 'threshold'], 
                    c('start','threshold'), var=wLocVar[i,'cultivar'],
                    predictorName='thermal30')
})


wsl <- merge(wslopt, wsl30)

wsl$length1 <- wsl$event2 - wsl$event1
wsl$crop <- 'walnut'

# Combine harvest ---------------------------------------------------------

harvest <- rbind(asl, psl, wsl)
harvestprecip <- merge(harvest, precip, by.x=c('loc','year'), 
                       by.y=c('location','year'))

saveRDS(harvestprecip, file.path(historypath, 'harvest.RDS'))

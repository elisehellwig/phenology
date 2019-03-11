
# This script 

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

a <- unique(read.csv(file.path(phenologypath,'almondclean.csv')))
p <- unique(read.csv(file.path(phenologypath,'pruneclean.csv')))
w <- read.csv(file.path(phenologypath,'walnutclean.csv'))
precip <- read.csv(file.path(historypath, 'precipitation.csv'))

amod <- readRDS(file.path(historypath, 'SLalmondDTanderson.RDS'))
pmod <- readRDS(file.path(historypath, 'SLpruneDTanderson.RDS'))


temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")


# Extract model thresholds ------------------------------------------------
locVar <- expand.grid(c('Chico','Modesto'), c('Mission','Nonpareil'))
names(locVar) <- c('loc','cultivar')

locVar$threshold <- sapply(amod, function(m) round(unlist(threshold(m))))

pruneThreshold <- round(unlist(threshold(pmod)))

# Almond Harvest ----------------------------------------------------------
ah <- a %>% 
    filter(source=='RAVT', loc %in% c('Chico','Modesto'),
                    cultivar %in% c('Nonpareil','Mission','Sonora'))

asl <- ldply(1:nrow(locVar), function(i) {
    calcThermalTime(ah, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                    locVar[i, 'threshold'], c('start','threshold'), 
                    location = locVar[i,'loc'], var=locVar[i,'cultivar'],
                    predictor='thermal')

})

asl$length1 <- asl$event2 - asl$event1
asl$crop <- 'almond'

# Prune harvest -----------------------------------------------------------

psl <- calcThermalTime(p, temp, 'harvest', 'DT', 'asymcur', c(4,25,36), 0, 
                       pruneThreshold, c('start','threshold'), 
                       location = 'Parlier', var='French', 
                       predictorName = 'thermal')


psl$length1 <- psl$event2 - psl$event1

psl$crop <- 'prune'

# Walnut harvest ----------------------------------------------------------

cv <- c('Chandler','Payne','Franquette')
w$loc <- 'Davis'
mt <- c('TTT', 'DT', 'DT')
frm <- c('flat','gdd','asymcur')
thresh <- c(50127.33,62,46)
ct <- list(c(0.4, 12.1), 11.1, c(4,25,36))

wsl <- ldply(seq_along(cv), function(i) {
    calcThermalTime(w, temp, 'harvest', mt[i], frm[i], ct[[i]], 0, thresh[i], 
                       c('start','threshold'), var=cv[i],
                    predictorName='thermal')
    })

wsl$length1 <- wsl$event2 - wsl$event1
wsl$crop <- 'walnut'

# Combine harvest ---------------------------------------------------------

harvest <- rbind(asl, psl, wsl)
harvestprecip <- merge(harvest, precip, by.x=c('loc','year'), 
                       by.y=c('location','year'))

saveRDS(harvestprecip, file.path(historypath, 'harvest.RDS'))

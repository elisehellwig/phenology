
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

temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")


# Almond Harvest ----------------------------------------------------------
ah <- a %>% 
    filter(source=='RAVT', loc %in% c('Chico','Modesto'),
                    cultivar %in% c('Nonpareil','Mission'))

LocVar <- expand.grid(unique(ah$loc), unique(ah$cultivar))
names(LocVar) <- c('loc','cultivar')

asl <- ldply(1:nrow(LocVar), function(i) {
    calcThermalTime(ah, temp, 'DT', 'asymcur', c(4,25,36), 0, 60, 
                    c('start','threshold'), location = LocVar[i,'loc'],
                    var=LocVar[i,'cultivar'])

})

asl$length1 <- asl$event2 - asl$event1
asl$crop <- 'almond'

# Prune harvest -----------------------------------------------------------

psl <- calcThermalTime(p, temp, 'DT', 'asymcur', c(4,25,36), 0, 60, 
                    c('start','threshold'), location = 'Parlier',
                    var='French')


psl$length1 <- psl$event2 - psl$event1

psl$crop <- 'prune'

# Walnut harvest ----------------------------------------------------------

cv <- c('Chandler','Payne','Franquette')
w$loc <- 'Davis'

wsl <- ldply(cv, function(v) {
    calcThermalTime(w, temp, 'DT', 'asymcur', c(4,25,36), 0, 40, 
                       c('start','threshold'), var=v)
    })

wsl$length1 <- wsl$event2 - wsl$event1
wsl$crop <- 'walnut'

# Combine harvest ---------------------------------------------------------

harvest <- rbind(asl, psl, wsl)
harvestprecip <- merge(harvest, precip, by.x=c('loc','year'), 
                       by.y=c('location','year'))

saveRDS(harvestprecip, file.path(historypath, 'harvest.RDS'))

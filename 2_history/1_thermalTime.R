
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

temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")


# Almond Harvest ----------------------------------------------------------
asl <- a %>% 
    filter(source=='RAVT', loc %in% c('Chico','Modesto'),
                    cultivar %in% c('Nonpareil','Mission'))

LocVar <- expand.grid(unique(asl$loc), unique(asl$cultivar))
names(LocVar) <- c('loc','cultivar')

ah <- ldply(1:nrow(LocVar), function(i) {
    calcThermalTime(asl, temp, 'DT', 'asymcur', c(4,25,36), 0, 60, 
                    c('start','threshold'), location = LocVar[i,'loc'],
                    var=LocVar[i,'cultivar'])

})

ah$length1 <- ah$event2 - ah$event1


# Prune harvest -----------------------------------------------------------

psl <- calcThermalTime(p, temp, 'DT', 'asymcur', c(4,25,36), 0, 60, 
                    c('start','threshold'), location = 'Parlier',
                    var='French')


psl$length1 <- psl$event2 - psl$event1



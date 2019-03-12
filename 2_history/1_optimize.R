# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(reshape2)
library(plyr)
library(phenoclim)

options(stringsAsFactors = FALSE)
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'
historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'


a <- unique(read.csv(file.path(phenologypath,'almondclean.csv')))
p <- unique(read.csv(file.path(phenologypath,'pruneclean.csv')))
w <- read.csv(file.path(phenologypath,'walnutclean.csv'))

temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")

# Prune -------------------------------------------------------------------

pc <- dcast(p, cultivar+year+loc ~ event, value.var='day')
pcc <- pc[complete.cases(pc),]

pruneDT <- list(parameterlist(1, 'DT', FALSE, 'asymcur', list(c(4,25,36)), 30, 0, 
                         c('start','threshold'), 'threshold', "PlantModel"))

ppmDT <- plantmodel(pcc, temp, pruneDT, 0, 220, cores=4L)


saveRDS(ppmDT, file.path(historypath, 'SLpruneDTanderson.RDS'))

# Almond ------------------------------------------------------------------

asub <- a %>% 
    filter(cultivar %in% c('Nonpareil','Mission','Sonora'),
           loc %in% c('Chico','Modesto'),
           source=='RAVT')

asub <- unique(asub)

ac <- dcast(asub, cultivar+year+loc ~ event, value.var='day',
            fun.aggregate = mean)
acc <- ac[complete.cases(ac),]

locVar <- expand.grid(c('Chico','Modesto'), c('Mission','Nonpareil','Sonora'))
names(locVar) <- c('location', 'variety')

almondData <- lapply(1:nrow(locVar), function(i) {
    filter(acc, cultivar==locVar[i, 'variety'], loc==locVar[i, 'location'])
})

almondDT <- list(parameterlist(1, 'DT', FALSE, 'asymcur', list(c(4,25,36)), 30, 
                               0, c('start','threshold'), 'threshold', 
                               "PlantModel"))

apmDT <- lapply(seq_along(almondData), function(i) {
    print(i)
    plantmodel(almondData[[i]], temp, almondDT, 0, 270, cores=4L)
})


saveRDS(apmDT, file.path(historypath, 'SLalmondDTanderson.RDS'))




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


# Walnut ------------------------------------------------------------------

wcults <- c('Chandler','Payne','Franquette')

wc <- dcast(w, cultivar+year ~ event, value.var='day')
wcc <- wc[complete.cases(wc),]

walnutData <- lapply(1:length(wcults), function(i) {
    filter(wcc, cultivar==wcults[i])
})

walnutDT <- list(parameterlist(1, 'DT', FALSE, 'asymcur', list(c(4,25,36)), 30, 
                              0, c('start','threshold'), 'threshold', 
                              "PlantModel"))


wpmDT <- lapply(seq_along(walnutData), function(i) {
    print(i)
    plantmodel(walnutData[[i]], temp, walnutDT, 0, 270, cores=4L)
})

saveRDS(wpmDT, file.path(historypath, 'SLwalnutDTanderson.RDS'))


# Prune -------------------------------------------------------------------

pc <- dcast(p, cultivar+year+loc ~ event, value.var='day')
pcc <- pc[complete.cases(pc),]

pruneDT <- list(parameterlist(1, 'DT', FALSE, 'asymcur', list(c(4,25,36)), 30, 
                              0, c('start','threshold'), 'threshold', 
                              "PlantModel"))

ppmDT <- plantmodel(pcc, temp, pruneDT, 0, 220, cores=4L)


saveRDS(ppmDT, file.path(historypath, 'SLpruneDTanderson.RDS'))

# Almond ------------------------------------------------------------------

asub <- a %>% 
    filter(cultivar %in% c('Nonpareil','Mission','Sonora'),
           source=='RAVT')

asub <- unique(asub)

ac <- dcast(asub, cultivar+year+loc ~ event, value.var='day',
            fun.aggregate = mean)
acc <- ac[complete.cases(ac),]


vars <- c('Mission','Nonpareil','Sonora')

almondData <- lapply(vars, function(cv) {
    select(filter(acc, cultivar==cv), c(cultivar, year, event1, event2))
})

almondDT <- lapply(c('asymcur'), function(form) {
    parameterlist(n=1,
                  mt='DT',
                  simple=FALSE,
                  ff=form,
                  ct=list(c(4,25,36)), 
                  modelthreshold=c(1000),
                  start=c(0),
                  varyingparameters=c('start','threshold'),
                  optimized=c('threshold'),
                  ModelClass='PlantModel')
})

apmDT <- lapply(seq_along(almondData), function(i) {
    print(i)
    plantmodel(almondData[[i]], temp, almondDT, 0, 270, cores=4L,
               iterations=25)
})


saveRDS(apmDT, file.path(historypath, 'SLalmondDTanderson.RDS'))




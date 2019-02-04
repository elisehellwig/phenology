library(lubridate)
library(phenoclim)
library(reshape2)
library(DEoptim)
source('functions/helperfunctions.R')

rpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/flowering/flowermodels'
dpath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/flowering'

w <- read.csv(file.path(dpath, 'walnutclean.csv'), stringsAsFactors = FALSE)
dav <- read.csv(file.path(dpath, 'davisdailyhourlytemp.csv'))
dav$dt <- as.POSIXct(dav$dt, format="%Y-%m-%d %H:%M:%OS")

wc <- w[w$cultivar=='Chandler',]
wc1 <- dcast(wc, year + cultivar~ event, value.var = 'day')
wc2 <- wc1[complete.cases(wc1), ]
wc2$event0 <- 300

cults <- sort(unique(w$cultivar))

simplefiles <- list.files(path=rpath,
                          pattern='CVsimple.RDS',
                          full.names=TRUE)

walfiles <- list.files(path=rpath,
                       pattern='CV.RDS',
                       full.names=TRUE)

simpleMods <- lapply(simplefiles, function(fn) readRDS(fn))
walMods <-  lapply(walfiles, function(fn) readRDS(fn))

names(simpleMods) <- cults
names(walMods) <- cults
#la <- extractResults(simplefiles, cults, 'chillbasic')

sm <- summarizeModel(simpleMods, cults)

minrmse(c(4,25,36), wc2, dav, 'TTT', 'anderson', 1, c(4,25,36), 49, 21100,
        TRUE, NA, 'FlowerModel', wc2$event0)



wm <- summarizeModel(walMods, cults)


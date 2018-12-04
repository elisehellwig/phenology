library(lubridate)
library(phenoclim)
library(DEoptim)
source('functions/helperfunctions.R')

rpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/flowering'
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
wm <- summarizeModel(walMods, cults)


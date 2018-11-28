library(lubridate)
library(phenoclim)
library(DEoptim)

rpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/flowering'
dpath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/flowering'

w <- read.csv(file.path(dpath, 'walnutclean.csv'), stringsAsFactors = FALSE)

cults <- sort(unique(w$cultivar))

simplefiles <- list.files(path=rpath,
                          pattern='CVsimple.RDS',
                          full.names=TRUE)

walfiles <- list.files(path=rpath,
                       pattern='CV.RDS',
                       full.names=TRUE)


simpleMods <- lapply(simplefiles, function(fn) readRDS(fn))
walMods <-  lapply(walfiles, function(fn) readRDS(fn))

la <- extractResults(simplefiles, cults, 'chillbasic')

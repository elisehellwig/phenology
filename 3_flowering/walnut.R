
# Setup -------------------------------------------------------------------


library(lubridate)
library(phenoclim)
library(DEoptim)
#library(ehelpr)

options(stringsAsFactors = FALSE)

datapath <- '/Users/echellwig/Drive/Phenology/data/walnutdata'

dav <- read.csv(file.path(datapath, 'davisdailyhourlytemp2.csv'))
wall <- read.csv(file.path(datapath, 'walnutclean.csv'))

dav$dt <- as.POSIXct(dav$dt)

vars <- 'Payne'

setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(raster)
library(plyr)
library(reshape2)
library(optimx)

source('functions/minrmse.R')


wk <- readRDS(file.path(drivepath, 'data/walnutdata/walnutkfold.RDS'))
htemps <- read.csv(file.path(drivepath,'data/walnutdata/davishourlytemp.csv'), 
                   stringsAsFactors = FALSE)
dtemps <- read.csv(file.path(drivepath,'data/walnutdata/davisdailytemp.csv'), 
                   stringsAsFactors = FALSE)

w <- read.csv(file.path(drivepath,'data/walnutdata/walnutclean.csv'),
              stringsAsFactors = FALSE)


vars <- sapply(wk, function(d) d[1,'cultivar'])
##############################################################################

#'backwards' way
ys <- yearsums(c(2), w, dtemps, cult='Payne', typ='gddsimple', sumlen=50)
mr <- minrmse(c(2, 35), w, htemps, cult="Payne", type='nocrit', sumlength=50,
              flowername='flower', harvestname='harvest')

# 'forwards' way
ygd <- yeargd(c(600, 2), w, dtemps, cult='Payne', type='gddsimple')
mrf <- minrmsefull(c(600, 2, 30), w, htemps, cult='Payne', type='nocrit')


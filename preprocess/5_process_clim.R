datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
library(lubridate)	
library(phenoclim)
library(plyr)
library(Interpol.T)

source('functions/tempInterpolation.R')
source('functions/datetime.R')
#source('R/functions/generalfunctions.R')
source('functions/diurnal_temperature2.R')
options(stringsAsFactors = FALSE)

#temp <- read.csv(file='data/clean/temp.csv')

nchico <- read.csv(file=file.path(datapath,'clean/noaachico.csv') )
ndavis <- read.csv(file=file.path(datapath, 'clean/noaadavis.csv'))
nmod <- read.csv(file=file.path(datapath, 'clean/noaamodesto.csv'))
nparlier <- read.csv(file=file.path(datapath, 'clean/noaaparlier.csv'))

cdavis <- read.csv(file.path(datapath, 'clean/cimisdavis.csv'))
cchico <- read.csv(file.path(datapath, 'clean/cimischicodurham.csv'))
cmod <- read.csv(file.path(datapath, 'clean/cimismodesto.csv'))
cparlier <- read.csv(file.path(datapath, 'clean/cimisparlier.csv'))
#this script readies the climate data for analysis.



# Calibrating temperature interpolation model -----------------------------


dv <- InterpTemp(ndavis, cdavis, 'Davis', 1930, 2017)
davisfinal <- mergeDailyHourly(ndavis, cdavis, dv)
davisfinal$loc <- 'davis'

chicoInterp <- InterpTemp(nchico, cchico, 'Chico', 1930, 2017)
chicofinal <- mergeDailyHourly(nchico, cchico, chicoInterp)
chicofinal$loc <- 'chico'

modestoInterp <- InterpTemp(nmod, cmod, 'Modesto', 1930, 2017)
modestofinal <- mergeDailyHourly(nmod, cmod, modestoInterp)
modestofinal$loc <- 'modesto'

parlierInterp <- InterpTemp(nparlier, cparlier, 'Parlier', 1930, 2017)
parlierfinal <- mergeDailyHourly(nparlier, cparlier, parlierInterp)
parlierfinal$loc <- 'parlier'


# Combine -----------------------------------------------------------------

dailyhourlytemps <- do.call(rbind, list(davisfinal, chicofinal, modestofinal,
                                        parlierfinal)) 

write.csv(dailyhourlytemps, file.path(datapath, 'clean/dailyhourlytemp.csv'),
          row.names = FALSE)
saveRDS(dailyhourlytemps, file.path(datapath, 'clean/dailyhourlytemp.RDS'))

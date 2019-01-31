datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
library(lubridate)	
library(phenoclim)
library(plyr)
library(Interpol.T)

source('functions/tempInterpolation.R')
source('functions/datetime.R')
options(stringsAsFactors = FALSE)


nchico <- read.csv(file=file.path(datapath,'clean/noaachico.csv') )
ndavis <- read.csv(file=file.path(datapath, 'clean/noaadavis.csv'))
nmod <- read.csv(file=file.path(datapath, 'clean/noaamodesto.csv'))
nparlier <- read.csv(file=file.path(datapath, 'clean/noaaparlier.csv'))

cdavis <- read.csv(file.path(datapath, 'clean/cimisdavis.csv'))
cchico <- read.csv(file.path(datapath, 'clean/cimischicodurham.csv'))
cmod <- read.csv(file.path(datapath, 'clean/cimismodesto.csv'))
cparlier <- read.csv(file.path(datapath, 'clean/cimisparlier.csv'))
#this script readies the climate data for analysis.



# Calibrating and running the Temp Interpolation -----------------------------

davisInterp <- InterpTemp(ndavis, cdavis, 'Davis', 1930, 2017)
chicoInterp <- InterpTemp(nchico, cchico, 'Chico', 1930, 2017)
modestoInterp <- InterpTemp(nmod, cmod, 'Modesto', 1930, 2017)
parlierInterp <- InterpTemp(nparlier, cparlier, 'Parlier', 1930, 2017)


# Merging daily and hourly temperatures -----------------------------------

davisfinal <- mergeDailyHourly(ndavis, cdavis, davisInterp)
chicofinal <- mergeDailyHourly(nchico, cchico, chicoInterp)
modestofinal <- mergeDailyHourly(nmod, cmod, modestoInterp)
parlierfinal <- mergeDailyHourly(nparlier, cparlier, parlierInterp)


 # Combine -----------------------------------------------------------------

davisfinal$loc <- 'Davis'
chicofinal$loc <- 'Chico'
modestofinal$loc <- 'Modesto'
parlierfinal$loc <- 'Parlier'


dailyhourlytemps <- do.call(rbind, list(davisfinal, chicofinal, modestofinal,
                                        parlierfinal)) 


# Save data ---------------------------------------------------------------

write.csv(davisfinal, file.path(datapath, 'clean/dailyhourlytempdavis.csv'),
          row.names = FALSE)
write.csv(chicofinal, file.path(datapath, 'clean/dailyhourlytempchico.csv'),
          row.names = FALSE)
write.csv(modestofinal, file.path(datapath, 'clean/dailyhourlytempmodesto.csv'),
          row.names = FALSE)
write.csv(parlierfinal, file.path(datapath, 'clean/dailyhourlytempparlier.csv'),
          row.names = FALSE)

write.csv(dailyhourlytemps, file.path(datapath, 'clean/dailyhourlytemp.csv'),
          row.names = FALSE)
saveRDS(dailyhourlytemps, file.path(datapath, 'clean/dailyhourlytemp.RDS'))

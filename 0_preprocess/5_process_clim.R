
#This script takes uses hourly temperature data to fit temporal interpolation 
    #models which it then uses to interpolate daily temperature data for a 
    #much longer time series. It then combines the daily hourly (original and
    #interpolated) into one data.frame that can be used to fit FlowerModels and
    #plant models with any functional form and model type.

# Setup -------------------------------------------------------------------


datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
library(lubridate)	
library(phenoclim)
library(plyr)
library(Interpol.T)

source('functions/tempInterpolation.R')
source('functions/datetime.R')
options(stringsAsFactors = FALSE)


# Import data -------------------------------------------------------------


#cleaned NOAA data
nchico <- read.csv(file=file.path(datapath,'clean/noaachico.csv') )
ndavis <- read.csv(file=file.path(datapath, 'clean/noaadavis.csv'))
nmanteca <- read.csv(file=file.path(datapath, 'clean/noaamanteca.csv'))
nparlier <- read.csv(file=file.path(datapath, 'clean/noaaparlier.csv'))
nshafter <- read.csv(file=file.path(datapath, 'clean/noaashafter.csv'))

#cleaned CIMIS data
cdavis <- read.csv(file.path(datapath, 'clean/cimisdavis.csv'))
cchico <- read.csv(file.path(datapath, 'clean/cimischicodurham.csv'))
cmanteca <- read.csv(file.path(datapath, 'clean/cimismanteca.csv'))
cparlier <- read.csv(file.path(datapath, 'clean/cimisparlier.csv'))
cshafter <- read.csv(file.path(datapath, 'clean/cimisshafter.csv'))

#this script readies the climate data for analysis.



# Calibrating and running the Temp Interpolation -----------------------------

davisInterp <- InterpTemp(ndavis, cdavis, 'Davis', 1930, 2017)
chicoInterp <- InterpTemp(nchico, cchico, 'Chico', 1930, 2017)
mantecaInterp <- InterpTemp(nmanteca, cmanteca, 'Manteca', 1930, 2017)
parlierInterp <- InterpTemp(nparlier, cparlier, 'Parlier', 1930, 2017)
shafterInterp <- InterpTemp(nshafter, cshafter, 'Shafter', 1940, 2017)


# Merging daily and hourly temperatures -----------------------------------

davisfinal <- mergeDailyHourly(ndavis, cdavis, davisInterp)
chicofinal <- mergeDailyHourly(nchico, cchico, chicoInterp)
mantecafinal <- mergeDailyHourly(nmanteca, cmanteca, mantecaInterp)
parlierfinal <- mergeDailyHourly(nparlier, cparlier, parlierInterp)
shafterfinal <- mergeDailyHourly(nshafter, cshafter, shafterInterp)


 # Combine -----------------------------------------------------------------

davisfinal$loc <- 'Davis'
chicofinal$loc <- 'Chico'
mantecafinal$loc <- 'Manteca'
parlierfinal$loc <- 'Parlier'
shafterfinal$loc <- 'Shafter'

# davisfinal <- read.csv(file.path(datapath, 'clean/dailyhourlytempdavis.csv'))
# chicofinal <- read.csv(file.path(datapath, 'clean/dailyhourlytempchico.csv'))
# mantecafinal <- read.csv(file.path(datapath, 'clean/dailyhourlytempmanteca.csv'))
# parlierfinal <- read.csv(file.path(datapath, 'clean/dailyhourlytempparlier.csv'))
# shafterfinal <- read.csv(file.path(datapath, 'clean/dailyhourlytempshafter.csv'))


dailyhourlytemps <- do.call(rbind, list(davisfinal, chicofinal, mantecafinal,
                                        parlierfinal, shafterfinal)) 


# Save data ---------------------------------------------------------------

write.csv(davisfinal, file.path(datapath, 'clean/dailyhourlytempdavis.csv'),
          row.names = FALSE)
write.csv(chicofinal, file.path(datapath, 'clean/dailyhourlytempchico.csv'),
          row.names = FALSE)
write.csv(mantecafinal, file.path(datapath, 'clean/dailyhourlytempmanteca.csv'),
          row.names = FALSE)
write.csv(parlierfinal, file.path(datapath, 'clean/dailyhourlytempparlier.csv'),
          row.names = FALSE)
write.csv(shafterfinal, file.path(datapath, 'clean/dailyhourlytempshafter.csv'),
          row.names = FALSE)

write.csv(dailyhourlytemps, file.path(datapath, 
            'phenology/dailyhourlytemp.csv'), row.names = FALSE)

saveRDS(dailyhourlytemps, file.path(datapath, 'phenology/dailyhourlytemp.RDS'))

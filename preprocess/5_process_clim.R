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

cdavis <- read.csv(file.path(datapath, 'clean/cimisdavis.csv'))
cchico <- read.csv(file.path(datapath, 'clean/cimischicodurham.csv'))

#this script readies the climate data for analysis.



# Calibrating temperature interpolation model -----------------------------


dv <- InterpTemp(ndavis, cdavis, 'Davis', 1925, 2017)
davisfinal <- mergeDailyHourly(ndavis, cdavis, dv)

chicoInterp <- InterpTemp(nchico, cchico, 'Chico', 1930, 2018)

la <- which(is.na(cim$temp))

calibdat <- data.frame(ID='Davis',
                       date=gsub('-', '/', as.character(date(cim$date))),
                       hour=hour(cim$date),
                       temp=cim$temp)


cpar <- par_calibration(calibdat, band_min = 3:9, band_max = 12:20, 
                        band_suns = 13:21)

cshape <- shape_calibration(calibdat, cal_times_list = cpar)

# Running the temp interpolation ------------------------------------------

davis$date <- as.Date(davis$date)

dtmin <- data.frame(year=davis$year,
                    month=month(davis$date),
                    day=davis$day,
                    Davis=davis$tmin)

dtmax <- data.frame(year=davis$year,
                    month=month(davis$date),
                    day=davis$day,
                    Davis=davis$tmax)

davhourly <- Th_int_series(cal_times=cpar, 
                           cal_shape = cshape,
                           TMIN=dtmin, 
                           TMAX=dtmax, 
                           start_year=1925, 
                           end_year=2017,
                           active_IDs = 'Davis')

dh <- davhourly$Date
dh$temp <- davhourly$Davis
#write.csv(dh, file.path(datapath, 'clean/interpolatedDavis.csv'),
         # row.names = FALSE)

dhdt <- convertToDT(dh, c('dt','temp'))
dhdt$date <- POSIXtoDate(dhdt$dt)
#locs <- c('Chico','Davis', 'Manteca','Parlier')


# Merge with daily temperatures -------------------------------------------

dt <- merge(dhdt, davis, by='date')
dtfinal <- dt[,c('dt','year','day','temp','tmin','tmax')]
dtfinal$hour <- hour(dtfinal$dt)
dtfinal <- dtfinal[order(dtfinal$dt), ]


cim$date <- toPOSIX(cim$date)

dtfinal$temp <- sapply(1:nrow(dtfinal), function(i) {
    dtdate <- dtfinal[i, 'dt']
    
    if (dtdate %in% cim$date) {
        if (!is.na(cim[which(cim$date==dtdate),'temp'])) {
            cim[which(cim$date==dtdate),'temp']
        } else {
            dtfinal[i, 'temp']
        }
        
    } else {
        dtfinal[i, 'temp']
        
    }
})



write.csv(dtfinal, file.path(datapath, 'clean/davisdailyhourlytemp.csv'), 
          row.names = FALSE)


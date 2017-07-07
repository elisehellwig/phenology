setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(lubridate)
library(reshape2)
library(rnoaa)
library(dplyr)
library(parallel)
source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')
source('functions/tempclean.R')
source('functions/preTclean.R')


#This script imports climate date from CIMIS and NOAA (NCDC) and cleans it up for further processing and analysis.
#This script focuses on min and max temperatures.
vars <- c('id','year','date','tmin','tmax')
yrs <- 1920:2017
# ghcndstns <- ghcnd_stations()
#saveRDS(ghcndstns, file.path(drivepath, 
#                             'data/historydata/ghcndstationmetadata.RDS'))
ghcndstns <- readRDS(file.path(drivepath, 
                               'data/historydata/ghcndstationmetadata.RDS'))

cll <- data.frame(id=c('USC00041715','USC00042294'),
                  latitude=c(39.699513, 38.538698),
                  longitude=c(-121.812008,-121.761336 ) )

APItoken='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN'
#############################################################
##############Data Import/Prep###############################

#find closest locations

nearby <- meteo_nearby_stations(cll, radius=100)
nearbychico <- as.data.frame(nearby[[1]])

cs <- lapply( nearbychico$id[1:35], function(stn) {
    ncdc_stations(stationid = paste0('GHCND:',stn), limit=50, token=APItoken)
})

#####
##NOte: write a function that runs linear models for a whole bunch of stations 
    #and then extracts the adjusted R squared and use that to calculate a
    #weighted mean.



## Davis
davstations <- c('USC00042294','USC00049742','USC00049781','USW00023232',
                 'USC00049200')

dav <- ghncd_download(davstations)
dateNA <- which(is.na(dav$date))
dav2 <- dav[-dateNA, vars]


Davprime <- davstations[1]
Davsecnd<- davstations[-1]


davisdaily <- tempclean(data=dav2, 
                        primary=prime, 
                        secondary=secnd,
                        years=yrs, 
                        dateform="%Y-%m-%d")


###Chico

#chico, orland
chicostations <- c('USC00041715', 'USC00046506')

chico <- ghncd_download(chicostations)
chicoNA <- which(is.na(chico$date))
chico2 <- chico[-chicoNA, vars]

chicoprime <- chicostations[1]
chicosecond <- chicostations[-1]

chicodaily <- tempclean(chico2, chicoprime, chicosecond, yrs, 
                        dateform="%Y-%m-%d")

#####
#CIMIS

cd <- read.csv(file.path(drivepath,'data/raw/climate/cimis/davis.csv'),
			   			 stringsAsFactors = FALSE)
cd <- cd[, c('Stn.Name', 'Date', 'Max.Air.Temp..C.', 'Min.Air.Temp..C.')]
names(cd) <- c('loc','date', 'tmax', 'tmin')

cd$date <- as.Date(cd$date, format='%m/%d/%Y')
cd <- cd[-which(is.na(cd$tmin) | is.na(cd$tmax)),]

#############################################################
##############location conversion model######################

setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(lubridate)
library(reshape2)
library(rnoaa)
library(plyr)
library(dplyr)
library(parallel)
source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')
source('functions/tempclean.R')
source('functions/preTclean.R')
source('functions/modeltest.R')

parlierCIMIS <- read.csv(file.path(drivepath,
                                   'data/historydata/parlierdaily.csv'),
                         stringsAsFactors = FALSE)

#This script imports climate date from CIMIS and NOAA (NCDC) and cleans it up for further processing and analysis.
#This script focuses on min and max temperatures.
vars <- c('id','year','date','tmin','tmax')
yrs <- 1920:2017
# ghcndstns <- ghcnd_stations()
#saveRDS(ghcndstns, file.path(drivepath, 
#                             'data/historydata/ghcndstationmetadata.RDS'))
ghcndstns <- readRDS(file.path(drivepath, 
                               'data/historydata/ghcndstationmetadata.RDS'))
nearby <- readRDS(file.path(drivepath, 'data/historydata/nearbystations.RDS'))

cll <- data.frame(id=c('USC00041715','USC00042294', 'USC00046476'),
                  latitude=c(39.699513, 38.538698, 36.602002),
                  longitude=c(-121.812008,-121.761336,-119.50816) )

EH_APItoken='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN'
#############################################################
# #############Data Import/Prep###############################

#find closest locations
#nearby <- meteo_nearby_stations(cll, radius=100)

#saveRDS(nearby,file.path(drivepath, 'data/historydata/nearbystations.RDS'))

########chico####
nearbychico <- as.data.frame(nearby[[1]])
cres <- modeleval(nearbychico$id[1], nearbychico$id[2:35])

cs <- ldply( cres$id, function(stn) {
    ncdc_stations(stationid = paste0('GHCND:',stn), limit=50, 
                  token=EH_APItoken)$data[,c('id', 'name', 'mindate','maxdate')]
})

cs$id <- sub("GHCND:", '', cs$id)

chicoAuxInfo <- metatemptable(cs, cres)
chicoAuxInfo <- chicoAuxInfo[chicoAuxInfo$minR2>0.75, ]
chicoAuxIDs <- chicoAuxInfo$id

########Davis####

nearbydavis <- as.data.frame(nearby[[2]])
dres <- modeleval(nearbydavis$id[1], nearbydavis$id[1:41])

ds <- ldply( dres$id, function(stn) {
    ncdc_stations(stationid = paste0('GHCND:',stn), limit=50, 
                  token=EH_APItoken)$data[,c('id', 'name', 'mindate','maxdate')]
})

ds$id <- sapply(ds$id, function(ch) {
    strsplit(ch, ':')[[1]][2]  
})

davisAuxInfo <- metatemptable(ds, dres)
davisAuxInfo <- davisAuxInfo[davisAuxInfo$minR2>0.78, ]
davisAuxIDs <- davisAuxInfo$id

########Parlier####
parcim <- parlierCIMIS[,c(1, 6,8,4)]
names(parcim) <- c('id','tmin','tmax','date')
parcim$date <- as.Date(parcim$date, format='%m/%d/%Y')

nearbypar <- as.data.frame(nearby[[3]])

ps <- ldply( nearbypar$id, function(stn) {
    ncdc_stations(stationid = paste0('GHCND:',stn), limit=(dim(nearbypar)[1]+5), 
                  token=EH_APItoken)$data[,c('id', 'name', 'mindate','maxdate')]
})

ps$maxdate <- as.Date(ps$maxdate)
earlyrows <- which(ps$maxdate < as.Date('1985-01-01'))
pslate <- ps[-earlyrows,]
pslate$id <- sub("GHCND:", "", pslate$id)

pres <- modeleval(primaryid = NA, secondaryids = pslate$id[1:45],
                  focaldf=parcim)

parlierInfo <- metatemptable(pslate, pres)
parlierInfo <- parlierInfo[parlierInfo$minR2>0.78, ]
parlierIDs <- parlierInfo$id
################################################################
################################################################
################################################################
## Davis
davstations <- c(cll[2,'id'], davisAuxIDs)

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

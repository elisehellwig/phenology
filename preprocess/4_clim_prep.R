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


#This script imports climate date from CIMIS and NOAA (NCDC) and cleans it up for further processing and analysis.
#This script focuses on min and max temperatures.


#############################################################
##############Data Import/Prep###############################

## Davis
davstations <- c('USC00042294','USC00049742','USC00049781','USW00023232',
                 'USC00049200')

dav <- ghncd_download(davstations)
dateNA <- which(is.na(dav$date))
dav2 <- dav[-dateNA, c('id','year','date','tmin','tmax')]


Davprime <- davstations[1]
Davsecnd<- davstations[-1]

yrs <- 1920:2017

davisdaily <- tempclean(data=dav2, 
                        primary=prime, 
                        secondary=secnd,
                        years=yrs, 
                        dateform="%Y-%m-%d")


n <- ghcnd(stationid = stations[1])





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

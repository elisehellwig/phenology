datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
library(lubridate)	
library(phenoclim)
library(plyr)

#source('R/functions/generalfunctions.R')
source('functions/diurnal_temperature2.R')

#temp <- read.csv(file='data/clean/temp.csv')
davis <- read.csv(file=file.path(datapath, 'clean/noaadavis.csv'),
                 stringsAsFactors = FALSE)

cim <- read.csv(file.path(datapath, 'clean/cimisdavis.csv'),
                stringsAsFactors = FALSE)


#this script readies the climate data for analysis.
#load('data/clean/hrly3.Rdata')


###########################################

davis$date <- as.Date(davis$date)

#locs <- c('Chico','Davis', 'Manteca','Parlier')
#####################Create Hourly Temperatures####################


#davis <- temp[temp$nearest=='Davis',]
#davis$date <- as.Date(davis$date)

hrdav <- ldply(1:length(davis$date), function(i) {
    temp <- round(diTemp(38.544476, davis[i, 'date'], davis[i,'tmin'], 
                         davis[i, 'tmax']), 1)
    hour <- 1:24
    date <- rep(as.Date(davis$date[i]), 24)
    data.frame(date, hour, temp)
})

hrdav$day <- yday(hrdav$date)
hrdav$year <- year(hrdav$date)

dt <- merge(hrdav, davis)
dt$hourstring <-  ifelse(nchar(dt$hour-1)<2, paste0(0, dt$hour-1), 
                         as.character(dt$hour-1))
dt$dt <- as.POSIXct(paste(dt$date, 
                          paste0(dt$hourstring, ':00:00')),
                    format="%Y-%m-%d %H:%M:%OS")

missingDTs <- which(is.na(dt$dt))
dtMiss <- dt[missingDTs, ]
dtstring <- paste0(dtMiss$date, ' ', dtMiss$hourstring, ':00:00')
dt$dt[missingDTs] <- dtstring


mindt <- min(dt$dt)
maxdt <- max(dt$dt)
dtfinal <- dt[,c('dt','year','day','hour','temp','tmin','tmax')]

cim$date <- as.POSIXct(cim$date, format="%Y-%m-%d %H:%M:%OS")
 
tdfinal$temp <- sapply(1:nrow(dtfinal), function(i) {
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

write.csv(dtfinal, file.path(datapath, 'davisdailyhourlytemp.csv'), 
          row.names = FALSE)
######################################

testdf <- temp[temp$nearest=='Chico', ]

la <- diTemp(39.693182, testdf$date, testdf$tmin, testdf$tmax)

htemp$date <- as.Date(htemp$date)
htemp$day <- yday(htemp$date)
htemp$month <- month(htemp$date)
htemp$year <- year(htemp$date)


write.csv(htemp, file='data/clean/hourlytemp.csv')

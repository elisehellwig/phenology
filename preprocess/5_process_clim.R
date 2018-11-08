setwd('/Volumes/GoogleDrive/My Drive/Phenology')
library(lubridate)	
library(phenoclim)

#source('R/functions/generalfunctions.R')
source('R/functions/diurnal_temperature2.R')

temp <- read.csv(file='data/clean/temp.csv')


#this script readies the climate data for analysis.
#load('data/clean/hrly3.Rdata')


###########################################

temp$date <- as.Date(temp$date)

locs <- c('Chico','Davis', 'Manteca','Parlier')
#####################Create Hourly Temperatures####################


davis <- temp[temp$nearest=='Davis',]
davis$date <- as.Date(davis$date)

hrdav <- data.frame(date=0, hour=0, temp=0)

for (i in 1:length(davis$date)) {
    temp <- round(diTemp(38.544476, davis[i, 'date'], davis[i,'tmin'], davis[i, 'tmax']), 1)
    hour <- 1:24
    date <- rep(as.Date(davis$date[i]), 24)
    dat <- cbind(date, hour, temp)
    hrdav <- rbind(hrdav, dat)
}

hrdav$date <- as.Date(hrdav$date, origin='1970-01-01')

dht <- hrdav[-1,]

dht$day <- as.numeric(format(as.Date(dht$date), "%j"))
dht$year <- year(dht$date)
dht$month <- month(dht$date)





testdf <- temp[temp$nearest=='Chico', ]

la <- diTemp(39.693182, testdf$date, testdf$tmin, testdf$tmax)

htemp$date <- as.Date(htemp$date)
htemp$day <- yday(htemp$date)
htemp$month <- month(htemp$date)
htemp$year <- year(htemp$date)


write.csv(htemp, file='data/clean/hourlytemp.csv')

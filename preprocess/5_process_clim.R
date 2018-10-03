setwd('/Volumes/GoogleDrive/My Drive/Phenology')
library(lubridate)	

source('R/functions/generalfunctions.R')
source('R/functions/diurnal_temperature2.R')

#this script readies the climate data for analysis.
load('data/clean/hrly3.Rdata')


###########################################
temp <- merge(temp, ll[,1:2], by='nearest')

#creates columns that identify the month and the day that an observation took place
temp$day <- yday(temp$date)

#####################Create Hourly Temperatures####################
htemp <- as.data.frame(do.call(rbind, hrly))
names(htemp) <- c('nearest', 'date', 'hour','temp')


htemp$date <- as.Date(htemp$date)
htemp$day <- yday(htemp$date)
htemp$month <- month(htemp$date)
htemp$year <- year(htemp$date)


write.csv(htemp, file='data/clean/hourlytemp.csv')

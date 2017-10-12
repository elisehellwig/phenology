setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(plyr)
library(lubridate)
library(ScrapeClim)
#source('functions/generalfunctions.R')
#source('functions/preprocessfunctions.R')
#source('functions/diurnal_temperature2.R')
dav <- readRDS(file.path(drivepath, 'data/historydata/Davis1916.RDS'))
yrs <- unique(dav$year)

#temp$lat <- 38.556345
#temp$lon <-121.815597

###############################################################

dav$month <- month(dav$date)
dav$day <- yday(dav$date)
dav <- dav[, c('year','month','day','tmin','tmax')]

davyear <- data.frame(year=yrs, 
                      ymin=round(tapply(dav$tmin, dav$year, mean),2),
                      ymax=round(tapply(dav$tmax, dav$year, mean),2),
                      row.names = NULL)
saveRDS(davyear, file.path(drivepath, 'data/historydata/Davis1916year.RDS'))


dav$nmonth <- month.name[dav$month]
dav$nmy <- paste0( as.character(dav$nmonth), as.character(dav$year)) 

ym <- expand.grid(1:12, yrs)
names(ym) <- c('month','year')
mminv <- sapply(1:nrow(ym), function(i) {
    mean(dav[dav$year==ym[i,'year'] & dav$month==ym[i,'month'], 'tmin'])
})
mmaxv <- sapply(1:nrow(ym), function(i) {
    mean(dav[dav$year==ym[i,'year'] & dav$month==ym[i,'month'], 'tmax'])
})

davmonth <- data.frame(year=ym$year,
                       month=ym$month,
                       nmonth=month.name[ym$month],
                       mmin=round(mminv,2),
                       mmax=round(mmaxv,2))

saveRDS(davmonth, file.path(drivepath, 'data/historydata/Davis1916month.RDS'))





hrly <- ldply(1:length(temp$date), function(i) {
    dT <- round(diTemp(temp$lat[i], temp$date[i], temp$tmin[i], 
                       temp$tmax[i]), 1)
    hour <- 1:24
    dateC <- rep(temp$date[i], 24)
    cbind(dateC, hour, dT)
})

hrly$dateC <- as.Date(as.character(hrly$dateC))
hrly$hour <- as.numeric(as.character(hrly$hour))
hrly$dT <- as.numeric(as.character(hrly$dT))

hrly$year <- year(hrly$dateC)
hrly$month <- month(hrly$dateC)
hrly$day <- yday(hrly$dateC)

names(hrly)[1:3] <- c('date','hour','temp')

write.csv(hrly, 
          file=file.path(drivepath, 'data/walnutdata/davishourlytemp.csv'), 
          row.names = FALSE)



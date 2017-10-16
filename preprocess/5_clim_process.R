setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(plyr)
library(lubridate)
library(ScrapeClim)
library(phenoclim)
options(stringsAsFactors = FALSE)
#source('functions/generalfunctions.R')
#source('functions/preprocessfunctions.R')
#source('functions/diurnal_temperature2.R')
loc <- read.csv(file.path(drivepath, 'data/historydata/sitelocations.csv'))
dav <- readRDS(file.path(drivepath, 'data/historydata/Davis1916.RDS'))
yrs <- unique(dav$year)



#temp$lat <- 38.556345
#temp$lon <-121.815597

###############################################################

##hourly temps
davlat <- loc[loc$site=='Davis','lat']
davhrly <- ldply(1:length(dav$date), function(i) {
    dT <- round(diTemp(davlat, dav$date[i], dav$tmin[i], 
                       dav$tmax[i]), 1)
    hour <- 1:24
    dateC <- rep(dav$date[i], 24)
    cbind(dateC, hour, dT)
})
davhrly$dateC <- as.Date(davhrly$dateC, origin = '1970-01-01')
davhrly$year <- year(davhrly$dateC)
davhrly$month <- month(davhrly$dateC)
davhrly$day <- yday(davhrly$dateC)
names(davhrly)[1:3] <- c('date','hour','temp')
saveRDS(davhrly, file=file.path(drivepath, 'data/historydata/Davis1916hr.RDS'))




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







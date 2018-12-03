datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'

library(lubridate)
library(phenoclim)
source('functions/datetime.R')

dh <- read.csv(file.path(datapath, 'clean/interpolatedDavis.csv'),
          stringsAsFactors = FALSE)
dav <- read.csv(file.path(datapath, 'flowering/davisdailyhourlytemp.csv'))
di <- read.csv(file.path(datapath, 'badinterpolatedDavis.csv'), 
          stringsAsFactors=FALSE)
cim <- read.csv(file.path(datapath, 'clean/cimisdavis.csv'),
                stringsAsFactors = FALSE)

truevals <- c(365, 524, 178, 485, 431, 429)

dhnew <- dh[dh$year >= 2011, ]
dav2 <- dav[dav$year >= 2011, ]

dh2 <- convertToDT(dhnew, c('year','dt','temp'))
dav2$dt <- as.POSIXct(dav2$dt, format="%Y-%m-%d %H:%M:%OS")
cim$dt <- toPOSIX(cim$date)
di$dt <- toPOSIX(di$dt)

ts <- thermalsum(list(7.2), 2014:2018, dh2, 'DT', 'chillbasic',274,
                 305, varying=NA, 'FlowerModel', rep(244, 5))


tsmat <- t(sapply(245:365, function(d) {
    thermalsum(list(7.2), 2014:2018, dh2, 'DT', 'chillbasic',244,
               d, varying=NA, 'FlowerModel', rep(244, 5))
}))

tsdf <- as.data.frame(cbind(245:365, tsmat))


tsdav <- thermalsum(list(7.2), 2013:2017, dav2, 'DT', 'chillbasic', 274,
                    305, varying=NA, 'FlowerModel', rep(244, 5))


tsmatcim <- t(sapply(245:365, function(d) {
    thermalsum(list(7.2), 2014:2018, cim, 'DT', 'chillbasic',244,
               d, varying=NA, 'FlowerModel', rep(244, 5))
}))

tsdfcim <- cbind(245:365, tsmatcim)

tsmatdi <- t(sapply(245:365, function(d) {
    thermalsum(list(7.2), 2014:2018, di, 'DT', 'chillbasic',244,
               d, varying=NA, 'FlowerModel', rep(244, 5))
}))

tsdfdi <- cbind(245:365, tsmatdi)



la2 <- dav2[dav2$dt >= toPOSIX("2017-10-01 00:00:00"),]

modinterval <- interval(toPOSIX("2017-10-01 00:00:00"), 
                        toPOSIX("2017-11-01 00:00:00"))


cimdav <- cim[which(cim$date %within% modinterval), 'temp']
tempdav <- dav2[which(dav2$dt %within% modinterval), 'temp']
sumsdav <- cumsum(chillbasic(tempdav, 7.2, FALSE))

tempdh <- dh[which(dh2$dt %in% modinterval), 'temp']
sumsdh <- cumsum(chillbasic(cimdav, 7.2, FALSE))



thermalsums
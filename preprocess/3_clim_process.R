setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(plyr)

source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')
source('functions/diurnal_temperature2.R')

temp <- read.csv(file=file.path(drivepath, 'data/walnutdata/davisdailytemp.csv'), 
                 stringsAsFactors = FALSE)

temp$lat <- 38.556345
temp$lon <-121.815597


hrly <- ldply(1:length(temp$date), function(i) {
    dT <- round(diTemp(temp$lat[i], temp$date[i], temp$tmin[i], temp$tmax[i]), 1)
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



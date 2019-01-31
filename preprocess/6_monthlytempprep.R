datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'

library(lubridate)	
library(phenoclim)
library(plyr)
library(dplyr)
library(raster)
library(reshape2)
source('functions/cleanTemps.R')
options(stringsAsFactors = FALSE)

dht <- readRDS(file.path(datapath, 'clean/dailyhourlytemp.RDS'))


# Calculating Monthly Temps -----------------------------------------------

tminMonth <- AverageTemps(dht, 'tmin')
tmaxMonth <- AverageTemps(dht, 'tmax')

monthly <- merge(tminMonth, tmaxMonth, by=c('string','year','month','loc'))

monthly$tavg <- (monthly$tmin + monthly$tmax)/2

monthly$tmin <- round(monthly$tmin, 1)
monthly$tmax <- round(monthly$tmax, 1)
monthly$tavg <- round(monthly$tavg, 1)

monthly$loc <- recode(monthly$loc, 'modes'='modesto', 'parli'='parlier')
monthly$nmonth <- month.name[monthly$month]

monthly <- monthly[,c('loc','year','month','nmonth','tmin','tmax','tavg')]


# Calculating Annual Temps ------------------------------------------------

tminAnnual <- AverageTemps(dht, 'tmin', monthly=FALSE)
tmaxAnnual <- AverageTemps(dht, 'tmax', monthly=FALSE)

annual <- merge(tminAnnual, tmaxAnnual, by=c('string','year','loc'))

annual$tavg <- (annual$tmin + annual$tmax)/2

annual$tmin <- round(annual$tmin, 1)
annual$tmax <- round(annual$tmax, 1)
annual$tavg <- round(annual$tavg, 1)

annual$loc <- recode(annual$loc, 'modes'='modesto', 'parli'='parlier')

annual <- annual[,c('loc','year','tmin','tmax','tavg')]


# Calculate 5 year moving average -----------------------------------------


oldnames<-c('tmin', 'tmax','tavg')
newnames <-c('Minimum', 'Maximum','Average') #moving averages


for (i in unique(annual$loc)) {
    for (j in 1:3) {
        s <- which(annual$loc==i)
        annual[s,newnames[j]] <- round(movingFun(annual[s, oldnames[j]], 
                                           5, na.rm=TRUE),1) 
        
        
    }  
}

annualm <- melt(annual, id.vars=c('loc','year'), value.name = 'temp')

# Saving data -------------------------------------------------------------

write.csv(annual, file.path(datapath, 'clean/annualtemperatures.csv'), 
          row.names = FALSE)

write.csv(monthly, file.path(datapath, 'clean/monthlytemperatures.csv'),
          row.names = FALSE)




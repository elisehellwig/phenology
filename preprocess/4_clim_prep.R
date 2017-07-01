setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(lubridate)
library(reshape2)
source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')


#This script imports climate date from CIMIS and NOAA (NCDC) and cleans it up for further processing and analysis.
#This script focuses on min and max temperatures.


#############################################################
##############Data Import/Prep###############################


#NCDC
nums <- c('','2','3','4')
nfils <- file.path(drivepath, paste0('data/raw/climate/noaa',nums,'.csv'))
nlist <- lapply(nfils, function(f) read.csv(f, stringsAsFactors=FALSE))

#creates a vector of variables that we are interested in
ivars <- c('STATION_NAME','DATE','TMAX','TMIN')

#gets the variables wer are interested in from the various NOAA datasets
n <- rbind(nlist[[1]][,ivars],nlist[[2]][,ivars], nlist[[3]][,ivars],nlist[[4]][,ivars])


names(n) <- c('loc', 'date','tmax', 'tmin')
n$date <- as.Date(as.character(n$date), format='%Y%m%d')
n$year <- year(n$date)
n$month <- month(n$date)

n <- n[n$year>1950 & n$year < 2014,]
n[n$tmax==-9999,'tmax'] <- NA
n[n$tmin==-9999,'tmin'] <- NA

n$tmin <- n$tmin/10
n$tmax <- n$tmax/10

nd <- n[n$loc=="DAVIS 2 WSW EXPERIMENTAL FARM CA US",]
ndreduced <- nd[-which(is.na(nd$tmax) | is.na(nd$tmin)),]

ni <- n[n$loc=='WINTERS CA US',]
nireduced <- ni[-which(is.na(ni$tmax) | is.na(ni$tmin)),]

no <- n[n$loc=='WOODLAND 1 WNW CA US',]
noreduced <- no[-which(is.na(no$tmax) | is.na(no$tmin)),]

##missing dates
missingdates <- c(as.Date(305:334 - 1, origin = '1998-01-01'),
                  as.Date(1:31 - 1, origin = '2005-01-01'),
                  as.Date(260-1, origin = '1956-01-01'),
                  as.Date(c('1953-04-26', '2012-06-08', '2012-07-20',
                         '2012-08-23', '2013-06-22', '2013-11-27')))


missingdata <- data.frame(loc='DAVIS 2 WSW EXPERIMENTAL FARM CA US',
                           date=missingdates,
                           tmax=rep(NA, length(missingdates)),
                           tmin=rep(NA, length(missingdates)),
                           year=year(missingdates),
                           month=month(missingdates))

nd <-rbind(nd, missingdata)

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

sdateswinters <- intersect(ndreduced$date,nireduced$date)
sdateswoodland <- intersect(ndreduced$date,noreduced$date)

moddatawi <- data.frame(tmindavis=nd[nd$date %in% sdateswinters,'tmin'],
                      tmaxdavis=nd[nd$date %in% sdateswinters,'tmax'],
                      tminwinters=ni[ni$date %in% sdateswinters,'tmin'],
                      tmaxwinters=ni[ni$date %in% sdateswinters,'tmax'])

moddatawo <- data.frame(tmindavis=nd[nd$date %in% sdateswoodland,'tmin'],
                        tmaxdavis=nd[nd$date %in% sdateswoodland,'tmax'],
                        tminwoodland=no[no$date %in% sdateswoodland,'tmin'],
                        tmaxwoodland=no[no$date %in% sdateswoodland,'tmax'])

tminwin <- lm(tmindavis ~ tminwinters, data=moddatawi)
tmaxwin <- lm(tmaxdavis ~ tmaxwinters, data=moddatawi)

tminwood <- lm(tmindavis ~ tminwoodland, data=moddatawo)
tmaxwood <- lm(tmaxdavis ~ tmaxwoodland, data=moddatawo)
#############################################################
##############fill in gaps######################

ndates <- nd[which(is.na(nd$tmin)), 'date']
xdates <- nd[which(is.na(nd$tmax)), 'date']

#CIMIS Data
cdn <- cd[cd$date %in% ndates, c('date', 'tmin')] 
nd[nd$date %in% cdn$date, 'tmin'] <- cdn$tmin

cdx <- cd[cd$date %in% xdates, c('date', 'tmax')] 
nd[nd$date %in% cdx$date, 'tmax'] <- cdx$tmax

ndates <- nd[which(is.na(nd$tmin)), 'date']
xdates <- nd[which(is.na(nd$tmax)), 'date']


#NCDC Data

#min temps
nin <- ni[ni$date %in% ndates, c('date', 'tmin')] 
nin$tminD <- predict(tminwin, newdata=data.frame(tminwinters=nin$tmin))

non <- no[no$date %in% ndates, c('date', 'tmin')] 
non$tminD <- predict(tminwood, newdata=data.frame(tminwoodland=non$tmin))

nd[nd$date %in% ndates, 'tmin'] <- sapply(1:length(ndates), function(i) mean(c(nin[i,'tminD'], non[i,'tminD'])))

#max temps
nix <- ni[ni$date %in% xdates, c('date', 'tmax')] 
nix$tmaxD <- predict(tmaxwin, newdata=data.frame(tmaxwinters=nix$tmax))

nox <- no[no$date %in% xdates, c('date', 'tmax')] 
nox$tmaxD <- predict(tmaxwood, newdata=data.frame(tmaxwoodland=nox$tmax))

nd[nd$date %in% xdates, 'tmax'] <- sapply(1:length(xdates), function(i) mean(c(nix[i,'tmaxD'], nox[i,'tmaxD'])))


#switching min and max values
srows <- which(nd$tmin>nd$tmax)
sdat <- data.frame(tn=nd[srows, 'tmax'], tx=nd[srows,'tmin'])
nd[srows,'tmin'] <- sdat[,'tn']
nd[srows,'tmax'] <- sdat[,'tx']

nd$day <- as.numeric(format(as.Date(nd$date), "%j"))
nd[,'loc'] <- NULL

allyears <- unique(nd$years)

if (timeSeriesCheck(nd, allyears)) {
    saveRDS(nd, file=file.path(drivepath, 'data/walnutdata/davisdailytemp.RDS'))
    write.csv(nd, file=file.path(drivepath, 'data/walnutdata/davisdailytemp.csv'), 
              row.names = FALSE)
} else {
    print('There is missing data')
}
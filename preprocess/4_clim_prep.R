library(tidyverse)
library(lubridate)
library(reshape2)
#source('R/functions/generalfunctions.R')
datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
source('functions/cleanTemps.R')
options(stringsAsFactors = FALSE)

#This script imports climate date from CIMIS and NOAA (NCDC) and cleans it up for further processing and analysis.
#This script focuses on min and max temperatures.



# Davis-NOAA --------------------------------------------------------------


n <- read.csv(file.path(datapath, 'raw/climate/noaadavisnew.csv'))

#convert date number to date class
n$DATE <- as.Date(n$DATE)

n <- n %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

#gives them reasonable names
n$loc <- recode(n$loc, "WINTERS, CA US"='winters', 
                "WOODLAND 1 WNW, CA US"='woodland',
                "DAVIS 2 WSW EXPERIMENTAL FARM, CA US"='davis')



n$year <- year(n$date)
n <- n[n$year > 1924, ]
n[which(n$tmin > n$tmax), 'tmin'] <- NA
n[which(n$tmin > n$tmax), 'tmax'] <- NA
n[which(n$tmax<28), 'tmax'] <- NA
n[which(n$tmin>90 | n$tmin<10), 'tmin'] <- NA

ndmin <- fillinTemps(n, 'tmin', c('winters','woodland'), 'davis')
ndmax <- fillinTemps(n, 'tmax', c('winters','woodland'), 'davis')

nd <- merge(ndmin, ndmax)

#convert to celcius
nd$tmax <- round(FtoC(nd$tmax))
nd$tmin <- round(FtoC(nd$tmin))

nd$year <- year(nd$date)
nd$day <- yday(nd$date)

write.csv(nd, file=file.path(datapath, 'clean/noaadavis.csv'),
          row.names = FALSE)


# Davis-CIMIS -------------------------------------------------------------
filenames <- list.files(file.path(datapath, 'raw/climate/cimis'),
                        pattern='.csv',
                        full.names = TRUE)



cimis <- plyr::ldply(filenames, function(fn) {
    read.csv(fn)[,c('Stn.Name','Date','Hour..PST.','Jul','Air.Temp..C.','qc')]
    }) 

names(cimis) <- c('name','date','hour','day','temp','qc')
cimis[which(cimis$name=="Bryte (experimental)"), 'name'] <- 'Bryte'


cimis$hour <- cimis$hour/100
timestring <- paste(cimis$date, 
                    paste0(sprintf('%02d', cimis$hour-1), ":00:00"))
cimis$date <- as.POSIXct(timestring, format="%m/%d/%Y %H:%M:%OS")

extremerows <- which(cimis$temp>50 | cimis$temp<=-14 | cimis$qc=='R')
cimis[extremerows, 'temp'] <- NA

cd <- cimis[which(cimis$name=='Davis'),]
library(phenoclim)
timeSeriesCheck(cd, start="1982-07-17 23:00:00 PDT", 
                end="2018-11-13 23:00:00 PST", hours = TRUE,
                datename='date')


cim80 <- cimis[which(cimis$date<'1994-09-20 00:00:00'), ] 
cim80d <- fillinTemps(cim80, 'temp','Zamora','Davis','name')

row95 <- which(cimis$date>='1994-09-20 00:00:00' & 
                   cimis$date<'1998-12-10 00:00:00')
cim95 <- cimis[row95, ] 
cim95d <- fillinTemps(cim95, 'temp',c('Dixon','Zamora'),'Davis','name')

row00 <- which(cimis$date>='1998-12-10 00:00:00' & 
                   cimis$date<'2006-01-21 00:00:00')
cim00 <- unique(cimis[row00, ]) 
cim00d <- fillinTemps(cim00, 'temp',c('Dixon','Winters','Zamora','Bryte'),
                      'Davis','name')


row10 <- which(cimis$date>='2006-01-21 00:00:00' & cimis$date<'2011-05-12 00:00:00')
cim10 <- cimis[row10, ] 
cim10d <- fillinTemps(cim10, 'temp',c('Dixon','Winters','Bryte'),
                      'Davis','name')

row15 <- which(cimis$date>='2011-05-12 00:00:00')
cim15 <- cimis[row15, ] 
cim15d <- fillinTemps(cim15, 'temp',c('Dixon','Winters', 'Woodland','Bryte'),
                      'Davis','name')


cimdav <- do.call(rbind, list(cim80d, cim95d, cim00d, cim10d, cim15d))
timeSeriesCheck(cimdav, start="1982-07-17 23:00:00 PDT", 
                end="2018-11-13 23:00:00 PST", hours = TRUE,
                datename='date')

write.csv(cimdav, file.path(datapath, 'clean/cimisdavis.csv'),
          row.names=FALSE)

#############################################################
##############Winters##########################################
n <- rbind(nlist[[1]][,ivars],nlist[[2]][,ivars], nlist[[3]][,ivars],nlist[[4]][,ivars])
names(n) <- c('loc', 'date','tmax', 'tmin')

nwi <- n[n$loc=="WINTERS CA US", ]
nwi$date <- as.Date(as.character(nwi$date), format='%Y%m%d')
nwi$year <- year(nwi$date)
nwi <- nwi[nwi$year > 1987,]

nwi[nwi$tmin < -1000, 'tmin'] <- NA
nwi[nwi$tmax < -1000, 'tmax'] <- NA

nrows <- which(is.na(nwi$tmin))
xrows <- which(is.na(nwi$tmax))


for (i in nrows) {
	nwi$tmin[i] <- mean(nwi$tmin[i-1], nwi$tmin[i+1])
}

for (i in xrows) {
	nwi$tmax[i] <- mean(nwi$tmax[i-1], nwi$tmax[i+1])
}

nwi$tmin <- nwi$tmin/10
nwi$tmax <- nwi$tmax/10
nwi$jday <- yday(nwi$date)

nwi$loc <- 'Winters'

#############################################################
##############Chico##########################################
chico <- read.csv('data/raw/climate/Chico1930-2015.csv')

names(chico) <- c('loc', 'month', 'day', 'year','tmin','tmax')

chico$loc <- 'chico'

chico <- chico[which(chico$year < 2014),]
chico$date <- sapply(1:length(chico$year), function(i) {
	paste0(chico$year[i], '-', chico$month[i], '-', chico$day[i])
	})

chico$date <- as.Date(chico$date, format='%Y-%m-%d')
chico$jday <- as.numeric(format(as.Date(chico$date), "%j"))
chico <- subset(chico, select=-c(month,day))

chico[chico$tmin>1000 | chico$tmin < -1000,'tmin'] <- NA
chico[chico$tmax>1000 | chico$tmax < -1000,'tmax'] <- NA

nrows <- which(is.na(chico[, 'tmin']))
xrows <- which(is.na(chico[, 'tmax']))

ndates <- chico$date[nrows]
xdates <- chico$date[xrows]
#####################################
#get more data
cdat <- read.csv('data/raw/climate/cimis/durham.csv', stringsAsFactors=FALSE)

#selects only the variables we are interested in
c <- cdat[,c('Stn.Name','Date','Max.Air.Temp..C.','Min.Air.Temp..C.')]

names(c) <- c('loc','date','tmax','tmin')
cd <- c[c$loc=='Durham',]

#creates a vector of the names of variables that are numeric variables
numvars <- c('tmax','tmin')

#converts numvars to numeric variables
for (v in numvars) {
	cd[,v] <- as.numeric(cd[,v])
}

#converts the string dates to Date class
cd$date <- as.Date(as.character(cd$date), format='%m/%d/%Y')
cd$year <- year(cd$date)


cdn <- cd[cd$date %in% ndates, ]
crows <- which(is.na(cdn$tmin))
cndates <- cdn[-crows, 'date']

chico[chico$date %in% cndates, 'tmin'] <- cdn[cdn$date %in% cndates, 'tmin']*10

cdx <- cd[cd$date %in% xdates, ]
crows <- which(is.na(cdx$tmax))
cxdates <- cdx[-crows, 'date']

chico[chico$date %in% cxdates, 'tmax'] <- cdx[cdx$date %in% cxdates, 'tmax']*10

##############################
#get temps for when durham did not have data

nrows <- which(is.na(chico$tmin))
xrows  <- which(is.na(chico$tmax))

ndates <- chico$date[nrows]
xdates <- chico$date[xrows]

n <- rbind(nlist[[1]][,ivars],nlist[[2]][,ivars], nlist[[3]][,ivars],nlist[[4]][,ivars])
names(n) <- c('loc', 'date','tmax', 'tmin')

#vector of names of stations
station <- c("OROVILLE MUNICIPAL AIRPORT CA US", 'ORLAND CA US') 
n <- n[n$loc %in% station,]

n[n$tmin< -1000 | n$tmin>1000, 'tmin'] <- NA
n[n$tmax< -1000 | n$tmax>1000, 'tmax'] <- NA

n$date <- as.Date(as.character(n$date), format='%Y%m%d')
n$year <- year(n$date)


#rename stations
n$loc <- as.factor(n$loc)
levels(n$loc) <- c('orland', 'oroville')
n$loc <- as.character(n$loc)
nov <- n[n$loc=='oroville', ]
nol <- n[n$loc=='orland', ]

missingdates <- as.Date(c("1980-11-27", "1980-11-28", "1980-11-29", "1980-11-30"))

for (d in ndates[!(ndates %in% missingdates)]) {
	chico[chico$date==d, 'tmin'] <- nol[nol$date==d, 'tmin']
}

for (d in xdates[!(xdates %in% missingdates)]) {
	chico[chico$date==d, 'tmax'] <- nol[nol$date==d, 'tmax']
}

ntemp <- c(-12, -3, 6, 15)
xtemp <- c(147, 155, 163, 171)

chico[chico$date %in% missingdates, 'tmin'] <- ntemp
chico[chico$date %in% missingdates, 'tmax'] <- xtemp



chico$tmax <- chico$tmax/10
chico$tmin <- chico$tmin/10

chico <- chico[,c(1, 5,2, 3, 4, 6)]
#################################################
###################Parlier#######################
cp <- read.csv('data/raw/climate/cimis/parlier.csv', stringsAsFactors=FALSE)
cp <- cp[,c('Stn.Name','Date','Max.Air.Temp..C.','Min.Air.Temp..C.')]
names(cp) <- c('loc','date','tmax','tmin')

cp$date <- as.Date(cp$date, format='%m/%d/%Y')
cp$year <- year(cp$date)
cp<- cp[cp$year %in% 1987:2013,]

nrows <- which(is.na(cp$tmin))
xrows <- which(is.na(cp$tmax))

ndates <- cp[nrows, 'date']
xdates <- cp[xrows, 'date']


####################
#getting more data
n <- rbind(nlist[[1]][,ivars],nlist[[2]][,ivars], nlist[[3]][,ivars],nlist[[4]][,ivars])
names(n) <- c('loc', 'date','tmax', 'tmin')

n$date <- as.Date(as.character(n$date), format='%Y%m%d')
n$year <- year(n$date)

nlocs <- n[n$loc %in% c('VISALIA CA US', 'HANFORD 1 S CA US') &n$year %in% 1987:2013, ]
nlocs[nlocs$tmin==-9999,'tmin'] <- NA
nlocs[nlocs$tmax==-9999,'tmax'] <- NA

nlocs$tmin <- nlocs$tmin/10
nlocs$tmax <- nlocs$tmax/10

mins <- as.data.frame(ndates)
mins$vis <- nlocs[nlocs$date %in% ndates & nlocs$loc=='VISALIA CA US', 'tmin']
mins$han <- nlocs[nlocs$date %in% ndates & nlocs$loc=='HANFORD 1 S CA US', 'tmin']


mins$avg <- apply(mins[,2:3], 1, mean, na.rm=TRUE)
cp[nrows,'tmin'] <- mins$avg


#maximum vals
maxs <- as.data.frame(xdates)
maxs$vis <- nlocs[nlocs$date %in% xdates & nlocs$loc=='VISALIA CA US', 'tmax']
maxs$han <- nlocs[nlocs$date %in% xdates & nlocs$loc=='HANFORD 1 S CA US', 'tmax']

maxs$avg <- apply(maxs[,2:3], 1, mean, na.rm=TRUE)
cp[xrows,'tmax'] <- maxs$avg

cp$jday <- as.numeric(format(as.Date(cp$date), "%j"))

#############################################################
##############Manteca##########################################


nums <- c('','2','3','4')
nfils <- paste0('data/raw/climate/noaa',nums,'.csv')
nlist <- lapply(nfils, function(f) read.csv(f, stringsAsFactors=FALSE))

ivars <- c('STATION_NAME','DATE','TMAX','TMIN')

#gets the variables wer are interested in from the various NOAA datasets
n <- rbind(nlist[[1]][,ivars],nlist[[2]][,ivars], nlist[[3]][,ivars],nlist[[4]][,ivars])


names(n) <- c('loc', 'date','tmax', 'tmin')

n$date <- as.Date(as.character(n$date), format='%Y%m%d')
n$year <- year(n$date)

mstations <- c('MANTECA CA US')

nm <- n[n$loc %in% mstations,]



nm[nm$tmin==-9999, 'tmin'] <- NA
nm[nm$tmax==-9999, 'tmax'] <- NA

####replace missing data
nrows <- which(is.na(nm$tmin))

nm[nrows,'tmin'] <- sapply(nrows, function(i) {
	mean(nm[i+1,'tmin'],nm[i-1,'tmin'])
	})


xrows <- which(is.na(nm$tmax))
xdates <- nm[xrows, 'date']


#########################
#get more data

ns <- n[n$loc=="STOCKTON METROPOLITAN AIRPORT CA US" & n$date %in% xdates,]

nm[nm$date %in% xdates, 'tmax'] <- ns[, 'tmax']

nm$jday <- yday(nm$date)

######

nm$tmin <- nm$tmin/10
nm$tmax <- nm$tmax/10

# ########CIMIS#########################
cm <- read.csv('data/raw/climate/cimis/manteca.csv', stringsAsFactors=FALSE)
cm <- cm[,c('Stn.Name','Date','Max.Air.Temp..C.','Min.Air.Temp..C.')]
names(cm) <- c('loc','date','tmax','tmin')


numvars <- c('tmax','tmin')
#converts numvars to numeric variables
for (v in numvars) {
	cm[,v] <- as.numeric(cm[,v])
}

cm$date <- as.Date(cm$date, format='%m/%d/%Y')
cm$year <- year(cm$date)
cm<- cm[cm$year %in% 1995:2013,]

nrows <- which(is.na(cm$tmin))
cm <- cm[-nrows, ]

#xrows <- which(is.na(cm$tmax))
#cm <- cm[-xrows, ]





#############################################################
##############Schafter######################################
cs <- read.csv('data/raw/climate/cimis/shafter.csv', stringsAsFactors=FALSE)
cs <- cs[,c('Stn.Name','Date','Max.Air.Temp..C.','Min.Air.Temp..C.')]
names(cs) <- c('loc','date','tmax','tmin')

numvars <- c('tmax','tmin')
#converts numvars to numeric variables
for (v in numvars) {
	cm[,v] <- as.numeric(cm[,v])
}

cs$date <- as.Date(cs$date, format='%m/%d/%Y')
cs$year <- year(cs$date)

nrows <- which(is.na(cs$tmin))
xrows <- which(is.na(cs$tmax))

ndates <- cs$date[nrows]
xdates <- cs$date[xrows]

###########################
#get more data
n <- rbind(nlist[[1]][,ivars],nlist[[2]][,ivars], nlist[[3]][,ivars],nlist[[4]][,ivars])
names(n) <- c('loc', 'date','tmax', 'tmin')

n$date <- as.Date(as.character(n$date), format='%Y%m%d')
n$year <- year(n$date)

nwa <- n[n$loc== "WASCO CA US" & n$date %in% ndates,]
nwa$tmin <- nwa$tmin/10
nwa$tmax <- nwa$tmax/10

cs[cs$date %in% ndates, 'tmin'] <- nwa$tmin
cs[cs$date %in% xdates, 'tmax'] <- nwa[nwa$date %in% xdates, 'tmax']

cs$jday <- yday(cs$date)

######################################################################################
######################################################################################
######################################################################################
#put it all together

temp <- rbind(nd, nwi, chico, cs, nm, cp)

temp[temp$loc=='davis', 'loc'] <- 'Davis'
temp[temp$loc=='chico', 'loc'] <- 'Chico'
temp[temp$loc=='MANTECA CA US', 'loc'] <- 'Manteca'


names(temp)[1] <- 'nearest'

write.csv(temp, file='data/clean/temp.csv', row.names=FALSE)





############################
###old






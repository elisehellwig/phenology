library(tidyverse)
library(lubridate)
library(reshape2)
library(phenoclim)
#source('R/functions/generalfunctions.R')
datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
source('functions/cleanTemps.R')
options(stringsAsFactors = FALSE, na.rm=TRUE)

#This script imports climate date from CIMIS and NOAA (NCDC) and cleans it up for further processing and analysis.
#This script focuses on min and max temperatures.



# Davis-NOAA --------------------------------------------------------------

#note I think this might be in fahrenheit but I'm not sure)
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
filenames <- list.files(file.path(datapath, 'raw/climate/cimis/davis'),
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
tsc <- timeSeriesCheck(cimdav, start="1982-07-17 23:00:00 PDT", 
                end="2018-11-13 23:00:00 PST", hours = TRUE,
                datename='date')

write.csv(cimdav, file.path(datapath, 'clean/cimisdavis.csv'),
          row.names=FALSE)



# Chico-NOAA --------------------------------------------------------------
cn <- read.csv(file.path(datapath, 'raw/climate/noaachiconew.csv'))

cn$DATE <- as.Date(cn$DATE)

cn <- cn %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 
                          'tmin'='TMIN')

#gives them reasonable names
cn$loc <- recode(cn$loc, "ORLAND, CA US"='orland', 
                "OROVILLE MUNICIPAL AIRPORT, CA US"='orovilleAirport',
                "CHICO UNIVERSITY FARM, CA US"='chico',
                "OROVILLE 1 N, CA US"='oroville')


cn$year <- year(cn$date)
cn[which(cn$tmin > cn$tmax), 'tmin'] <- NA
cn[which(cn$tmin > cn$tmax), 'tmax'] <- NA
cn[which(cn$tmin>32 | cn$tmin<=-14), 'tmin'] <- NA

cnmin <- fillinTemps(cn, 'tmin', c('orland','oroville'), 'chico')
cnmax <- fillinTemps(cn, 'tmax', c('orland','oroville'), 'chico')

#note check issues with tmin on 1977-02-25 and 1980-10-25;
#                       tmax on 1980-10-27
#when NOAA is back up

cnd <- merge(cnmin, cnmax)
cnd$year <- year(cnd$date)
cnd$day <- yday(cnd$date)


write.csv(cnd, file.path(datapath,'clean/noaachico'))

# Chico-CIMIS --------------------------------------------------------------

cfilenames <- list.files(file.path(datapath, 'raw/climate/cimis/chico'),
                         pattern='.csv',
                         full.names = TRUE)

cchico <- plyr::ldply(cfilenames, function(fn) {
    read.csv(fn)[,c('Stn.Name','Date','Hour..PST.','Jul','Air.Temp..C.',
                    'qc')]
}) 


names(cchico) <- c('name','date','hour','day','temp','qc')

cchico$hour <- cchico$hour/100
chicotimestring <- paste(cchico$date, 
                    paste0(sprintf('%02d', cchico$hour-1), ":00:00"))
cchico$date <- as.POSIXct(chicotimestring, format="%m/%d/%Y %H:%M:%OS")

extremerows <- which(cchico$temp <= -12 | cchico$temp > 48 |
                     cchico$qc=='R' | (cchico$temp > 41 & cchico$day < 80))
cchico[extremerows, 'temp'] <- NA

cdu <- cchico[which(cchico$name=='Durham'),]

cc80 <- cchico[which(cchico$date<'1987-5-13 00:00:00'), ] 
cc80d <- fillinTemps(cc80, 'temp','Orland','Durham','name')

cc90 <- cchico[which(cchico$date>='1987-5-13 00:00:00' & 
                     cchico$date<'2015-6-18 00:00:00'), ] 
cc90d <- fillinTemps(cc90, 'temp','Orland','Durham','name')

cc15 <- cchico[which(cchico$date>='2015-6-18 00:00:00'), ] 
cc15d <- fillinTemps(cc15, 'temp','Biggs','Durham','name')

cimdur <- do.call(rbind, list(cc80d, cc90d, cc15d))

tsc <- timeSeriesCheck(cimdur, start="1982-10-30 23:00:00 PDT", 
                       end="2018-10-31 23:00:00 PST", hours = TRUE,
                       datename='date')

write.csv(cimdur, file.path(datapath, 'clean/cimischicodurham.csv'),
          row.names=FALSE)


# Parlier-NOAA ----------------------------------------------------------


# Parlier-Cimis -----------------------------------------------------------

pfilenames <- list.files(file.path(datapath, 'raw/climate/cimis/parlier'),
                         pattern='.csv',
                         full.names = TRUE)



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






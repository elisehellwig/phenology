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

tscd <- timeSeriesCheck(n[n$loc=='davis', ], 
                        start="1930-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

n$year <- year(n$date)
n <- n[n$year >= 1930, ]
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

cdavis <- importCIMIS(file.path(datapath, 'raw/climate/cimis/davis'))

cdavis[which(cdavis$name=="Bryte (experimental)"), 'name'] <- 'Bryte'

extremerows <- which(cdavis$temp>50 | cdavis$temp<=-14 | cdavis$qc=='R')
cimis[extremerows, 'temp'] <- NA

cd <- cdavis[which(cdavis$name=='Davis'),]


cim80 <- cdavis[which(cdavis$date<'1994-09-20 00:00:00'), ] 
cim80d <- fillinTemps(cim80, 'temp','Zamora','Davis','name')

row95 <- which(cdavis$date>='1994-09-20 00:00:00' & 
                   cdavis$date<'1998-12-10 00:00:00')
cim95 <- cimis[row95, ] 
cim95d <- fillinTemps(cim95, 'temp',c('Dixon','Zamora'),'Davis','name')

row00 <- which(cdavis$date>='1998-12-10 00:00:00' & 
                   cdavis$date<'2006-01-21 00:00:00')
cim00 <- unique(cdavis[row00, ]) 
cim00d <- fillinTemps(cim00, 'temp',c('Dixon','Winters','Zamora','Bryte'),
                      'Davis','name')


row10 <- which(cdavis$date>='2006-01-21 00:00:00' & cdavis$date<'2011-05-12 00:00:00')
cim10 <- cimis[row10, ] 
cim10d <- fillinTemps(cim10, 'temp',c('Dixon','Winters','Bryte'),
                      'Davis','name')

row15 <- which(cdavis$date>='2011-05-12 00:00:00')
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
cn1 <- read.csv(file.path(datapath, 'raw/climate/noaachiconew.csv'))
cn2 <- read.csv(file.path(datapath, 'raw/climate/noaachiconew2.csv'))

cn <- rbind(cn1, cn2[,c('STATION','NAME','DATE','TMAX','TMIN')])

cn$DATE <- as.Date(cn$DATE)

cn <- cn %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 
                          'tmin'='TMIN')

#gives them reasonable names
cn$loc <- recode(cn$loc, "ORLAND, CA US"='orland', 
                "OROVILLE MUNICIPAL AIRPORT, CA US"='orovilleAirport',
                "CHICO UNIVERSITY FARM, CA US"='chico',
                "OROVILLE 1 N, CA US"='oroville',
                "MARYSVILLE, CA US"='marysville',
                "COLUSA 2 SSW, CA US"='colusa')

missingDates <- timeSeriesCheck(cn[cn$loc=='chico', ], 
                                start="1925-01-01 23:00:00 PDT", 
                                end="2018-10-31 23:00:00 PST", hours = FALSE,
                                datename='date')

missingDF <- data.frame(loc='chico',
                        date=as.Date(missingDates),
                        tmax=NA,
                        tmin=NA)

cn <- rbind(cn, missingDF)
cn$year <- year(cn$date)
cn <- cn[cn$year >= 1930, ]
cn[which(cn$tmin > cn$tmax), 'tmin'] <- NA
cn[which(cn$tmin > cn$tmax), 'tmax'] <- NA
cn[which(cn$tmin>32 | cn$tmin<=-14), 'tmin'] <- NA

cnmin <- fillinTemps(cn, 'tmin', c('orland','oroville','marysville', 'colusa'), 
                     'chico')
cnmax <- fillinTemps(cn, 'tmax', c('orland','oroville', 'marysville', 'colusa'),
                     'chico')


cnd <- merge(cnmin, cnmax)
cnd$year <- year(cnd$date)
cnd$day <- yday(cnd$date)

tsc <- timeSeriesCheck(cnd, start="1930-01-01 23:00:00 PDT", 
                       end="2018-10-31 23:00:00 PST", hours = FALSE,
                       datename='date')

write.csv(cnd, file.path(datapath,'clean/noaachico.csv'),
          row.names=FALSE)

# Chico-CIMIS --------------------------------------------------------------


cchico <- importCIMIS(file.path(datapath, 'raw/climate/cimis/chico'))

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




# Parlier-CIMIS -----------------------------------------------------------

cparlier <- importCIMIS(file.path(datapath, 'raw/climate/cimis/parlier'))

cparlier$name <- recode(cparlier$name, "Fresno State"="FS",
                        "Fresno/F.S.U. USDA"="fresno",
                        "Orange Cove"="orange",
                        "Parlier"="parlier",
                        "Caruthers"="caruthers",
                        "Visalia"="visalia")

extremerows <- which(cparlier$qc=='R' | cparlier$temp<= -8 | 
                    cparlier$temp > 45)
cparlier[extremerows, 'temp'] <- NA

cp <- cparlier[which(cparlier$name=='parlier'),]

cp80 <- cparlier[which(cparlier$date<'1988-06-06 00:00:00'), ] 
cp80d <- fillinTemps(cp80, 'temp',c('caruthers','fresno', 'visalia'),
                     'parlier','name')

cp90 <- cparlier[which(cparlier$date>='1988-06-06 00:00:00' &
                       cparlier$date<'1999-01-01 00:00:00'), ] 
cp90d <- fillinTemps(cp90, 'temp',c('FS', 'visalia'), 'parlier','name')

cp00 <- cparlier[which(cparlier$date>='1999-01-01 00:00:00' &
                           cparlier$date<'2007-03-01 00:00:00'), ] 
cp00d <- fillinTemps(cp00, 'temp',c('FS', 'orange', 'visalia'),
                     'parlier','name')

cp10 <- cparlier[which(cparlier$date>='2007-03-01 00:00:00'), ] 
cp10d <- fillinTemps(cp10, 'temp',c('FS', 'orange'), 'parlier','name')

cimpar <- do.call(rbind, list(cp80d, cp90d, cp00d, cp10d))
tsc <- timeSeriesCheck(cimpar, start="1982-06-07 00:00:00 PDT", 
                       end="2018-10-31 23:00:00 PDT", hours = TRUE,
                       datename='date')

write.csv(cimpar, file.path(datapath, 'clean/cimisparlier.csv'),
          row.names=FALSE)


# Parlier-NOAA ----------------------------------------------------------
pn1 <- read.csv(file.path(datapath, 'raw/climate/noaaparlier.csv'))
cimpar <- read.csv(file.path(datapath, 'clean/cimisparlier.csv'))

pn1$DATE <- as.Date(pn1$DATE)
cimpar$date <- as.POSIXct(cimpar$date)
cimpar$dateOnly <- as.Date(cimpar$date)

pn2 <- data.frame(loc='parlier',
                  date=names(tapply(cimpar$temp, cimpar$dateOnly, min)),
                  tmin=unname(tapply(cimpar$temp, cimpar$dateOnly, min)),
                  tmax=unname(tapply(cimpar$temp, cimpar$dateOnly, max)))

pn1 <- pn1 %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

#gives them reasonable names
pn1$loc <- recode(pn1$loc, 
                "FRESNO YOSEMITE INTERNATIONAL, CA US"='fresno', 
                "HANFORD 1 S, CA US"='hanford',
                "ORANGE COVE, CA US"='orange',
                "VISALIA, CA US"='visalia')

pn <- rbind(pn1, pn2)

pn$year <- year(pn$date)
pn <- pn[pn$year >= 1930, ]
pn[which(pn$tmin > pn$tmax), 'tmin'] <- NA
pn[which(pn$tmin > pn$tmax), 'tmax'] <- NA
pn[which(pn$tmax>46.1 | pn$tmax<=0), 'tmax'] <- NA
pn[which(pn$tmin<=-8.4), 'tmin'] <- NA


pndmin <- fillinTemps(pn, 'tmin', c('fresno','hanford','visalia'), 'parlier')
pndmax <- fillinTemps(pn, 'tmax', c('fresno','hanford','visalia'), 'parlier')

pnd <- merge(pndmin, pndmax)

#update with lemon cove data when it becomes available
#until then the fix for missing values is below

pnd[pnd$date=='1930-11-09', 'tmax'] <- (pnd[pnd$date=='1930-11-08', 'tmax']+ 
                                        pnd[pnd$date=='1930-11-10', 'tmax'])/2

pnd[pnd$date=='1946-09-16', 'tmax'] <- pnd[pnd$date=='1946-09-17', 'tmax']

pnd[pnd$date=='1946-09-15', 'tmax'] <- (pnd[pnd$date=='1946-09-17', 'tmax']+
                                        pnd[pnd$date=='1946-09-14', 'tmax'])/2


tscd <- timeSeriesCheck(pnd, 
                        start="1930-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

pnd$year <- year(pnd$date)
pnd$day <- yday(pnd$date)

write.csv(pnd, file=file.path(datapath, 'clean/noaaparlier.csv'),
          row.names = FALSE)


# Modesto-NOAA ------------------------------------------------------------
mn1 <- read.csv(file.path(datapath, 'raw/climate/noaamodesto.csv'))
mn2 <- read.csv(file.path(datapath, 'raw/climate/noaamodesto2.csv'))

mn <- rbind(mn1, mn2)

mn$DATE <- as.Date(mn$DATE)

mn <- mn %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

mn$loc <- recode(mn$loc, "DENAIR 3 NNE, CA US"='denair', 
                "OAKDALE WOODWARD DAM, CA US"='oakdale',
                "TRACY CARBONA, CA US"='tracy',
                "STOCKTON METROPOLITAN AIRPORT, CA US"='stockton',
                "MODESTO CITY CO AIRPORT, CA US"='modesto',
                "TURLOCK NUMBER 2, CA US"='turlock')

tscd <- timeSeriesCheck(mn[mn$loc=='modesto', ], 
                        start="1926-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')


mn$year <- year(mn$date)
mn <- mn[mn$year >= 1930, ]
mn[which(mn$tmin > mn$tmax), 'tmin'] <- NA
mn[which(mn$tmin > mn$tmax), 'tmax'] <- NA
mn[which(mn$tmax>46.1 | mn$tmax<=-4), 'tmax'] <- NA
mn[which(mn$tmin<=-9.0), 'tmin'] <- NA

#note turlock should really start at 1985
#tapply(mn$date, mn$loc, range)

mn25 <- mn[which(mn$date>='1930-01-01' & mn$date<'1949-10-01'),]
mndmin25 <- fillinTemps(mn25,'tmin', c('denair','oakdale'), 'modesto')
mndmax25 <- fillinTemps(mn25,'tmax', c('denair','oakdale'), 'modesto')

mn49 <- mn[which(mn$date>='1949-10-01' & mn$date<'1968-01-01'),]
mndmin49 <- fillinTemps(mn49,'tmin', c('denair','oakdale','stockton'), 
                        'modesto')
mndmax49 <- fillinTemps(mn49,'tmax', c('denair','oakdale', 'stockton'), 
                        'modesto')

mn68 <-  mn[which(mn$date>='1968-01-01' & mn$date<'1984-07-01'),]
mndmin68 <- fillinTemps(mn68,'tmin', c('denair','stockton'), 
                        'modesto')
mndmax68 <- fillinTemps(mn68,'tmax', c('denair','stockton'), 
                        'modesto')

mn84 <-  mn[which(mn$date>='1984-07-01' & mn$date<='2018-10-31'),]
mndmin84 <- fillinTemps(mn84,'tmin', c('stockton','turlock'), 
                        'modesto')
mndmax84 <- fillinTemps(mn84,'tmax', c('stockton','turlock'), 
                        'modesto')

mndmin <- do.call(rbind, list(mndmin25,mndmin49,mndmin68,mndmin84))
mndmax <- do.call(rbind, list(mndmax25,mndmax49,mndmax68,mndmax84))

mnd <- merge(mndmin, mndmax)

tscd <- timeSeriesCheck(mnd, 
                        start="1930-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

mnd$year <- year(mnd$date)
mnd$day <- yday(mnd$date)

write.csv(mnd, file=file.path(datapath, 'clean/noaamodesto.csv'),
          row.names = FALSE)

# Modesto-CIMIS -----------------------------------------------------------


cmod <- importCIMIS(file.path(datapath, 'raw/climate/cimis/modesto'))

cmod[which(cmod$name=='Denair II'), 'name'] <- 'DenairII'

extremerows <- which(cmod$qc=='R' | cmod$temp<= -8 | cmod$temp > 45 |
                         (cmod$temp > 38 & (cmod$day > 285| cmod$day<100)))
cmod[extremerows, 'temp'] <- NA

cm <- cmod[which(cmod$name=='Modesto'),]

cm80 <- cmod[which(cmod$date<'1999-08-23 00:00:00'), ] 
cm80d <- fillinTemps(cm80, 'temp',c('Manteca'), 'Modesto','name')

cm00 <- cmod[which(cmod$date>='1999-08-23 00:00:00' &
                   cmod$date<'2004-11-02 00:00:00'), ] 
cm00d <- fillinTemps(cm00, 'temp',
                     c('Manteca', 'Tracy','Patterson','Denair'),
                     'Modesto','name')

cm05 <- cmod[which(cmod$date>='2004-11-02 00:00:00' &
                       cmod$date<'2009-04-09 00:00:00'), ] 
cm05d <- fillinTemps(cm05, 'temp',
                     c('Manteca', 'Tracy','Patterson','Denair','Oakdale'),
                     'Modesto','name')

cm10 <- cmod[which(cmod$date>='2009-04-09 00:00:00' &
                       cmod$date<'2017-5-31 00:00:00'), ] 
cm10d <- fillinTemps(cm10, 'temp',
                    c('Manteca', 'Tracy','Patterson','DenairII','Oakdale'),
                    'Modesto','name')

cm15 <- cmod[which(cmod$date>='2017-5-31 00:00:00'), ] 
cm15d <- fillinTemps(cm15, 'temp',
                     c('Manteca','DenairII','Oakdale'), 'Modesto','name')


cimmod <- do.call(rbind, list(cm80d, cm10d, cm00d, cm05d, cm15d))

tsc <- timeSeriesCheck(cimmod, start="1987-06-25 23:00:00 PDT", 
                       end="2018-10-31 23:00:00 PST", hours = TRUE,
                       datename='date')

write.csv(cimmod, file.path(datapath, 'clean/cimismodesto.csv'),
          row.names=FALSE)

# Combine -----------------------------------------------------------------


temp <- rbind(nd, nwi, chico, cs, nm, cp)

temp[temp$loc=='davis', 'loc'] <- 'Davis'
temp[temp$loc=='chico', 'loc'] <- 'Chico'
temp[temp$loc=='MANTECA CA US', 'loc'] <- 'Manteca'


names(temp)[1] <- 'nearest'

write.csv(temp, file='data/clean/temp.csv', row.names=FALSE)





############################







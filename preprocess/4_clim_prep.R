#This script imports climate date from CIMIS and NOAA Climate Data Center and
#cleans it up for further processing and analysis.

#NOAA:
# https://www.ncdc.noaa.gov/cdo-web/datasets
# Daily Summaries Data Set
# Measured variables: TMIN and TMAX

#CIMIS: 
# https://cimis.water.ca.gov/WSNReportCriteria.aspx
# Hourly CSV Report
# Measured variables: Air Temperatures
# Station List: Davis 


# Setup -------------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(reshape2)
library(phenoclim)
datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'
source('functions/cleanTemps.R')
options(stringsAsFactors = FALSE, na.rm=TRUE)


# Davis-NOAA --------------------------------------------------------------

#note this data is in F, it is converted to C later on
n <- read.csv(file.path(datapath, 'raw/climate/noaadavisnew.csv'))

#convert date number to date class
n$DATE <- as.Date(n$DATE)

#select and rename some of the columns
n <- n %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

#gives locations reasonable names
n$loc <- recode(n$loc, "WINTERS, CA US"='winters', 
                "WOODLAND 1 WNW, CA US"='woodland',
                "DAVIS 2 WSW EXPERIMENTAL FARM, CA US"='davis')

#check to see if we are missing dates in our time series
tscd <- timeSeriesCheck(n[n$loc=='davis', ], 
                        start="1930-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

#adding a year column
n$year <- year(n$date)

#subsetting to later than 1930 of too much missing data before then
n <- n[n$year >= 1930, ]

#setting temps to NA if tmax is less than tmin
n[which(n$tmin > n$tmax), 'tmin'] <- NA
n[which(n$tmin > n$tmax), 'tmax'] <- NA

#removing data that is outside the range of reasonable temperature expections
n[which(n$tmax<28), 'tmax'] <- NA
n[which(n$tmin>90 | n$tmin<10), 'tmin'] <- NA


#fill in missing temperatures 
ndmin <- fillinTemps(n, 'tmin', c('winters','woodland'), 'davis')
ndmax <- fillinTemps(n, 'tmax', c('winters','woodland'), 'davis')

#merge tmin and tmax filled in data frames
nd <- merge(ndmin, ndmax)

#convert to celcius
nd$tmax <- round(FtoC(nd$tmax))
nd$tmin <- round(FtoC(nd$tmin))

#add year and day columns
nd$year <- year(nd$date)
nd$day <- yday(nd$date)

write.csv(nd, file=file.path(datapath, 'clean/noaadavis.csv'),
          row.names = FALSE)


# Davis-CIMIS -------------------------------------------------------------

#import data
cdavis <- importCIMIS(file.path(datapath, 'raw/climate/cimis/davis'))

#rename a location so no spaces in it
cdavis[which(cdavis$name=="Bryte (experimental)"), 'name'] <- 'Bryte'

#remove observations outside of reasonable temp expectations
extremerows <- which(cdavis$temp>50 | cdavis$temp<=-14 | cdavis$qc=='R')
cimis[extremerows, 'temp'] <- NA

#select only observations in davis
cd <- cdavis[which(cdavis$name=='Davis'),]

#Fill in steps are broken up by year because you need to have data for most if
    #not all of the entire time series for the secondary stations for 
    #fillinTemps() to work well.

#filling in temps before 94
cim80 <- cdavis[which(cdavis$date<'1994-09-20 00:00:00'), ] 
cim80d <- fillinTemps(cim80, 'temp','Zamora','Davis','name')

#fillling in temps between 94 and 98
row95 <- which(cdavis$date>='1994-09-20 00:00:00' & 
                   cdavis$date<'1998-12-10 00:00:00')
cim95 <- cimis[row95, ] 
cim95d <- fillinTemps(cim95, 'temp',c('Dixon','Zamora'),'Davis','name')

#filling in temps between 98 and 06
row00 <- which(cdavis$date>='1998-12-10 00:00:00' & 
                   cdavis$date<'2006-01-21 00:00:00')
cim00 <- unique(cdavis[row00, ]) 
cim00d <- fillinTemps(cim00, 'temp',c('Dixon','Winters','Zamora','Bryte'),
                      'Davis','name')

#filling in temps between 06 and 11
row10 <- which(cdavis$date>='2006-01-21 00:00:00' & cdavis$date<'2011-05-12 00:00:00')
cim10 <- cimis[row10, ] 
cim10d <- fillinTemps(cim10, 'temp',c('Dixon','Winters','Bryte'),
                      'Davis','name')

#filling in temps between after 2011
row15 <- which(cdavis$date>='2011-05-12 00:00:00')
cim15 <- cimis[row15, ] 
cim15d <- fillinTemps(cim15, 'temp',c('Dixon','Winters', 'Woodland','Bryte'),
                      'Davis','name') 

#merging all the filled in temp data
cimdav <- do.call(rbind, list(cim80d, cim95d, cim00d, cim10d, cim15d))

#checking to see if we are missing dates from our time series
tsc <- timeSeriesCheck(cimdav, start="1982-07-17 23:00:00 PDT", 
                end="2018-11-13 23:00:00 PST", hours = TRUE,
                datename='date')

#save data
write.csv(cimdav, file.path(datapath, 'clean/cimisdavis.csv'),
          row.names=FALSE)



# Chico-NOAA --------------------------------------------------------------

#import data
cn1 <- read.csv(file.path(datapath, 'raw/climate/noaachiconew.csv'))
cn2 <- read.csv(file.path(datapath, 'raw/climate/noaachiconew2.csv'))

#merge two data sources together
cn <- rbind(cn1, cn2[,c('STATION','NAME','DATE','TMAX','TMIN')])

#converting date string to Date
cn$DATE <- as.Date(cn$DATE)

#selecting only the columns we want
cn <- cn %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 
                          'tmin'='TMIN')

#gives them reasonable names
cn$loc <- recode(cn$loc, "ORLAND, CA US"='orland', 
                "OROVILLE MUNICIPAL AIRPORT, CA US"='orovilleAirport',
                "CHICO UNIVERSITY FARM, CA US"='chico',
                "OROVILLE 1 N, CA US"='oroville',
                "MARYSVILLE, CA US"='marysville',
                "COLUSA 2 SSW, CA US"='colusa')

#checking to see which dates are missing
missingDates <- timeSeriesCheck(cn[cn$loc=='chico', ], 
                                start="1925-01-01 23:00:00 PDT", 
                                end="2018-10-31 23:00:00 PST", hours = FALSE,
                                datename='date')

#creating a data.frame with the missing dates and temps as NA
missingDF <- data.frame(loc='chico',
                        date=as.Date(missingDates),
                        tmax=NA,
                        tmin=NA)

#adding the df of the missing dates to the main data frame
cn <- rbind(cn, missingDF)

#adding year column
cn$year <- year(cn$date)

#subsetting to later than 1930 of too much missing data before then
cn <- cn[cn$year >= 1930, ]

#setting temps to NA if tmax is less than tmin
cn[which(cn$tmin > cn$tmax), 'tmin'] <- NA
cn[which(cn$tmin > cn$tmax), 'tmax'] <- NA

#removing temp data that is outside of reasonable expectations
cn[which(cn$tmin>32 | cn$tmin<=-14), 'tmin'] <- NA

#filling in missing temperatures
cnmin <- fillinTemps(cn, 'tmin', c('orland','oroville','marysville', 'colusa'), 
                     'chico')
cnmax <- fillinTemps(cn, 'tmax', c('orland','oroville', 'marysville', 'colusa'),
                     'chico')

#merging filled in temps
cnd <- merge(cnmin, cnmax)

#adding date information
cnd$year <- year(cnd$date)
cnd$day <- yday(cnd$date)

#checking to see if we have any missing  dates
tsc <- timeSeriesCheck(cnd, start="1930-01-01 23:00:00 PDT", 
                       end="2018-10-31 23:00:00 PST", hours = FALSE,
                       datename='date')

#saving data
write.csv(cnd, file.path(datapath,'clean/noaachico.csv'),
          row.names=FALSE)

# Chico-CIMIS --------------------------------------------------------------

#importing data
cchico <- importCIMIS(file.path(datapath, 'raw/climate/cimis/chico'))

#selecting and removing rows with temperatures outside reasonable range
extremerows <- which(cchico$temp <= -12 | cchico$temp > 48 |
                     cchico$qc=='R' | (cchico$temp > 41 & cchico$day < 80))
cchico[extremerows, 'temp'] <- NA

#selecting primary station data
cdu <- cchico[which(cchico$name=='Durham'),]

#filling in data before 1987
cc80 <- cchico[which(cchico$date<'1987-5-13 00:00:00'), ] 
cc80d <- fillinTemps(cc80, 'temp','Orland','Durham','name')

#filling in data from 87 to 2015
cc90 <- cchico[which(cchico$date>='1987-5-13 00:00:00' & 
                     cchico$date<'2015-6-18 00:00:00'), ] 
cc90d <- fillinTemps(cc90, 'temp','Orland','Durham','name')

#filling in data after 2015
cc15 <- cchico[which(cchico$date>='2015-6-18 00:00:00'), ] 
cc15d <- fillinTemps(cc15, 'temp','Biggs','Durham','name')

#merging all filled in data together
cimdur <- do.call(rbind, list(cc80d, cc90d, cc15d))

#checking to see if there are any dates missing
tsc <- timeSeriesCheck(cimdur, start="1982-10-30 23:00:00 PDT", 
                       end="2018-10-31 23:00:00 PST", hours = TRUE,
                       datename='date')

#saving data.
write.csv(cimdur, file.path(datapath, 'clean/cimischicodurham.csv'),
          row.names=FALSE)




# Parlier-CIMIS -----------------------------------------------------------

#loading data
cparlier <- importCIMIS(file.path(datapath, 'raw/climate/cimis/parlier'))

#giving locations reasonable names
cparlier$name <- recode(cparlier$name, "Fresno State"="FS",
                        "Fresno/F.S.U. USDA"="fresno",
                        "Orange Cove"="orange",
                        "Parlier"="parlier",
                        "Caruthers"="caruthers",
                        "Visalia"="visalia")

#removing data that are outside the possible range of temperatures
extremerows <- which(cparlier$qc=='R' | cparlier$temp<= -8 | 
                    cparlier$temp > 45)
cparlier[extremerows, 'temp'] <- NA

#selecting data from the primary weather station
cp <- cparlier[which(cparlier$name=='parlier'),]

#filling in temperatures from before 1988
cp80 <- cparlier[which(cparlier$date<'1988-06-06 00:00:00'), ] 
cp80d <- fillinTemps(cp80, 'temp',c('caruthers','fresno', 'visalia'),
                     'parlier','name')

#filling in temps between 88 and 99
cp90 <- cparlier[which(cparlier$date>='1988-06-06 00:00:00' &
                       cparlier$date<'1999-01-01 00:00:00'), ] 
cp90d <- fillinTemps(cp90, 'temp',c('FS', 'visalia'), 'parlier','name')

#filing in temps between 99 and 07
cp00 <- cparlier[which(cparlier$date>='1999-01-01 00:00:00' &
                           cparlier$date<'2007-03-01 00:00:00'), ] 
cp00d <- fillinTemps(cp00, 'temp',c('FS', 'orange', 'visalia'),
                     'parlier','name')

#filling in temps after 07
cp10 <- cparlier[which(cparlier$date>='2007-03-01 00:00:00'), ] 
cp10d <- fillinTemps(cp10, 'temp',c('FS', 'orange'), 'parlier','name')

#combining the filled in temperatures
cimpar <- do.call(rbind, list(cp80d, cp90d, cp00d, cp10d))

#checking for any missing dates
tsc <- timeSeriesCheck(cimpar, start="1982-06-07 00:00:00 PDT", 
                       end="2018-10-31 23:00:00 PDT", hours = TRUE,
                       datename='date')

#saving data
write.csv(cimpar, file.path(datapath, 'clean/cimisparlier.csv'),
          row.names=FALSE)


# Parlier-NOAA ----------------------------------------------------------
#importing data
pn1 <- read.csv(file.path(datapath, 'raw/climate/noaaparlier.csv'))
pn2 <- read.csv(file.path(datapath, 'raw/climate/noaaparlier2.csv'))
cimpar <- read.csv(file.path(datapath, 'clean/cimisparlier.csv'))

#converting date and datetime strings to their respective classes
pn1$DATE <- as.Date(pn1$DATE)
pn2$DATE <- as.Date(pn2$DATE)
cimpar$date <- as.POSIXct(cimpar$date)
cimpar$dateOnly <- as.Date(cimpar$date)

#creating a dataframe with with min and max daily temp values for parlier
#this is because there is no noaa station in parlier, only a cimis station 
pn3 <- data.frame(loc='parlier',
                  date=names(tapply(cimpar$temp, cimpar$dateOnly, min)),
                  tmin=unname(tapply(cimpar$temp, cimpar$dateOnly, min)),
                  tmax=unname(tapply(cimpar$temp, cimpar$dateOnly, max)))

#renaming and selecting columns from both NOAA data sources
pn1 <- pn1 %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')
pn2 <- pn2 %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

#gives locations reasonable names
pn1$loc <- recode(pn1$loc, 
                "FRESNO YOSEMITE INTERNATIONAL, CA US"='fresno', 
                "HANFORD 1 S, CA US"='hanford',
                "ORANGE COVE, CA US"='orange',
                "VISALIA, CA US"='visalia')

pn2$loc <- 'lemon'

#combine all three data.frames
pn <- rbind(pn1, pn2, pn3)

#create year variable
pn$year <- year(pn$date)

#select only data after 1930 because too many missing days before 1930
pn <- pn[pn$year >= 1930, ]

#setting temps to NA where min temp > max temp
pn[which(pn$tmin > pn$tmax), 'tmin'] <- NA
pn[which(pn$tmin > pn$tmax), 'tmax'] <- NA

#setting temps to NA if they are higher or lower than most extreme values
pn[which(pn$tmax>46.1 | pn$tmax<=0), 'tmax'] <- NA
pn[which(pn$tmin<=-8.4), 'tmin'] <- NA

#filling in data for the entire parlier time series
pndmin <- fillinTemps(pn, 'tmin', c('fresno','hanford','visalia','lemon'),
                      'parlier')
pndmax <- fillinTemps(pn, 'tmax', c('fresno','hanford','visalia','lemon'),
                      'parlier')

#merging the filled in data
pnd <- merge(pndmin, pndmax)

#checking for missing dates
tscd <- timeSeriesCheck(pnd, 
                        start="1930-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

#creating year and julian day variables
pnd$year <- year(pnd$date)
pnd$day <- yday(pnd$date)

#saving data
write.csv(pnd, file=file.path(datapath, 'clean/noaaparlier.csv'),
          row.names = FALSE)


# Modesto-NOAA ------------------------------------------------------------
#importing modesto noaa data
mn1 <- read.csv(file.path(datapath, 'raw/climate/noaamodesto.csv'))
mn2 <- read.csv(file.path(datapath, 'raw/climate/noaamodesto2.csv'))

#merging 2 data frames
mn <- rbind(mn1, mn2)

#converting date string to Date class
mn$DATE <- as.Date(mn$DATE)

#selecting and renaming columns
mn <- mn %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

#giving stations more reasonable names
mn$loc <- recode(mn$loc, "DENAIR 3 NNE, CA US"='denair', 
                "OAKDALE WOODWARD DAM, CA US"='oakdale',
                "TRACY CARBONA, CA US"='tracy',
                "STOCKTON METROPOLITAN AIRPORT, CA US"='stockton',
                "MODESTO CITY CO AIRPORT, CA US"='modesto',
                "TURLOCK NUMBER 2, CA US"='turlock')

#checking for missing dates
tscd <- timeSeriesCheck(mn[mn$loc=='modesto', ], 
                        start="1926-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

#creating year variable
mn$year <- year(mn$date)

#selecting only data after 1930 due to data missingness
mn <- mn[mn$year >= 1930, ]

#Setting temps to NA where min temp > max temp
mn[which(mn$tmin > mn$tmax), 'tmin'] <- NA
mn[which(mn$tmin > mn$tmax), 'tmax'] <- NA

#Setting temps to NA if they were outside of historical extrema
mn[which(mn$tmax>46.1 | mn$tmax<=-4), 'tmax'] <- NA
mn[which(mn$tmin<=-9.0), 'tmin'] <- NA

#note turlock should really start at 1985
#tapply(mn$date, mn$loc, range)

#filling in data between 1930 and 49
mn25 <- mn[which(mn$date>='1930-01-01' & mn$date<'1949-10-01'),]
mndmin25 <- fillinTemps(mn25,'tmin', c('denair','oakdale'), 'modesto')
mndmax25 <- fillinTemps(mn25,'tmax', c('denair','oakdale'), 'modesto')

#filling in data between 49 and 68
mn49 <- mn[which(mn$date>='1949-10-01' & mn$date<'1968-01-01'),]
mndmin49 <- fillinTemps(mn49,'tmin', c('denair','oakdale','stockton'), 
                        'modesto')
mndmax49 <- fillinTemps(mn49,'tmax', c('denair','oakdale', 'stockton'), 
                        'modesto')

#filling in data between 68 and 84
mn68 <-  mn[which(mn$date>='1968-01-01' & mn$date<'1984-07-01'),]
mndmin68 <- fillinTemps(mn68,'tmin', c('denair','stockton'), 
                        'modesto')
mndmax68 <- fillinTemps(mn68,'tmax', c('denair','stockton'), 
                        'modesto')

#filling in data after 1984
mn84 <-  mn[which(mn$date>='1984-07-01' & mn$date<='2018-10-31'),]
mndmin84 <- fillinTemps(mn84,'tmin', c('stockton','turlock'), 
                        'modesto')
mndmax84 <- fillinTemps(mn84,'tmax', c('stockton','turlock'), 
                        'modesto')

#combining all the filled in data
mndmin <- do.call(rbind, list(mndmin25,mndmin49,mndmin68,mndmin84))
mndmax <- do.call(rbind, list(mndmax25,mndmax49,mndmax68,mndmax84))

mnd <- merge(mndmin, mndmax)

#checking to see if there are any missing dates
tscd <- timeSeriesCheck(mnd, 
                        start="1930-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

#creating year and julian day variables
mnd$year <- year(mnd$date)
mnd$day <- yday(mnd$date)

#saving data
write.csv(mnd, file=file.path(datapath, 'clean/noaamodesto.csv'),
          row.names = FALSE)

# Modesto-CIMIS -----------------------------------------------------------

#importing CIMIS data for modesto
cmod <- importCIMIS(file.path(datapath, 'raw/climate/cimis/modesto'))

#renaming a station with a space in the name
cmod[which(cmod$name=='Denair II'), 'name'] <- 'DenairII'

#setting temps outside of historical extrema to NA
extremerows <- which(cmod$qc=='R' | cmod$temp<= -8 | cmod$temp > 45 |
                         (cmod$temp > 38 & (cmod$day > 285| cmod$day<100)))
cmod[extremerows, 'temp'] <- NA

#selecting only data from the primary station
cm <- cmod[which(cmod$name=='Modesto'),]

#fill in data from before 1999
cm80 <- cmod[which(cmod$date<'1999-08-23 00:00:00'), ] 
cm80d <- fillinTemps(cm80, 'temp',c('Manteca'), 'Modesto','name')

#filling in data between 99 and 04
cm00 <- cmod[which(cmod$date>='1999-08-23 00:00:00' &
                   cmod$date<'2004-11-02 00:00:00'), ] 
cm00d <- fillinTemps(cm00, 'temp',
                     c('Manteca', 'Tracy','Patterson','Denair'),
                     'Modesto','name')

#filling in data between 04 and 09
cm05 <- cmod[which(cmod$date>='2004-11-02 00:00:00' &
                       cmod$date<'2009-04-09 00:00:00'), ] 
cm05d <- fillinTemps(cm05, 'temp',
                     c('Manteca', 'Tracy','Patterson','Denair','Oakdale'),
                     'Modesto','name')

#filling in data between 09 and 17
cm10 <- cmod[which(cmod$date>='2009-04-09 00:00:00' &
                       cmod$date<'2017-5-31 00:00:00'), ] 
cm10d <- fillinTemps(cm10, 'temp',
                    c('Manteca', 'Tracy','Patterson','DenairII','Oakdale'),
                    'Modesto','name')

#filling in data after 2017
cm15 <- cmod[which(cmod$date>='2017-5-31 00:00:00'), ] 
cm15d <- fillinTemps(cm15, 'temp',
                     c('Manteca','DenairII','Oakdale'), 'Modesto','name')

#merging all of the filled in data
cimmod <- do.call(rbind, list(cm80d, cm10d, cm00d, cm05d, cm15d))

#checking to see if there are any dates missing
tsc <- timeSeriesCheck(cimmod, start="1987-06-25 23:00:00 PDT", 
                       end="2018-10-31 23:00:00 PST", hours = TRUE,
                       datename='date')

#saving data
write.csv(cimmod, file.path(datapath, 'clean/cimismodesto.csv'),
          row.names=FALSE)





# CIMIS - Shafter ---------------------------------------------------------

cshafter <- importCIMIS(file.path(datapath, 'raw/climate/cimis/shafter'))

cshafter$name <- recode(cshafter$name, "Arvin-Edison"="arvin",
                        "Blackwells Corner"="blackwell",
                        "McFarland/Kern Farms"="mcfarland",
                        "Shafter"="shafter",
                        "Lamont"="lamont",
                        "Bakersfield/Bonanza"='bonanza',
                        "Bakersfield/Greenlee"= 'greenlee',
                        "Lost Hills"='losthills')


extremerows <- which(cshafter$qc=='R' | cshafter$temp<= -11 | 
                         cshafter$temp > 44 |
                         month(cshafter$date)!=12 & cshafter$temp<=(-8) |
                         month(cshafter$date) %in% 5:9 & cshafter$temp<=0 )
cshafter[extremerows, 'temp'] <- NA


cs <- cshafter[which(cshafter$name=='shafter'),]

cs80 <- cshafter[which(cshafter$date<'1986-10-01 00:00:00'), ] 
cs80d <- fillinTemps(cs80, 'temp',c('lamont','losthills','greenlee','bonanza',
                                    'mcfarland'),
                     'shafter','name')


cs90 <- cshafter[which(cshafter$date>='1986-10-01 00:00:00' &
                       cshafter$date<'1994-10-03 00:00:00' ), ] 
cs90d <- fillinTemps(cs90, 'temp',c('blackwell','mcfarland', 'lamont'),
                     'shafter','name')

cs00 <- cshafter[which(cshafter$date>='1994-10-03 00:00:00'), ] 
cs00d <- fillinTemps(cs00, 'temp',c('arvin','blackwell'),
                     'shafter','name')

cimshaft <- do.call(rbind, list(cs80d, cs90d, cs00d))


tsc <- timeSeriesCheck(cimshaft, start="1982-06-02 00:00:00 PDT", 
                       end="2018-10-31 23:00:00 PDT", hours = TRUE,
                       datename='date')

cimshaftdaily <- redit

# NOAA - Shafter ----------------------------------------------------------

sn <- read.csv(file.path(datapath, 'raw/climate/noaashafter.csv'))
cimshaft <- read.csv(file.path(datapath, 'clean/cimisparlier.csv'))

sn$DATE <- as.Date(sn$DATE)
cimshaft$date <- as.POSIXct(cimshaft$date)
cimshaft$dateOnly <- as.Date(cimshaft$date)

#selecting and renaming columns
sn <- sn %>% select('loc'='NAME','date'='DATE', 'tmax'='TMAX', 'tmin'='TMIN')

sn$loc <- recode(sn$loc, "BUTTONWILLOW, CA US"='buttonwillow',
                 "BAKERSFIELD AIRPORT, CA US"='bakersfieldAirport',
                 "BAKERSFIELD 5 NW, CA US"='bakersfield5',
                 "BAKERSFIELD 6.1 WSW, CA US"='bakersfield6',
                 "WASCO, CA US"='wasco', "SHAFTER 6 E, CA US"='shafter')




#creating a dataframe with with min and max daily temp values for parlier
#this is because there is no noaa station in parlier, only a cimis station 
pn3 <- data.frame(loc='parlier',
                  date=names(tapply(cimpar$temp, cimpar$dateOnly, min)),
                  tmin=unname(tapply(cimpar$temp, cimpar$dateOnly, min)),
                  tmax=unname(tapply(cimpar$temp, cimpar$dateOnly, max)))




tscd <- timeSeriesCheck(sn[sn$loc=='shafter', ], 
                        start="1931-01-01 23:00:00 PDT", 
                        end="2018-10-31 23:00:00 PST", hours = FALSE,
                        datename='date')

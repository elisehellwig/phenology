
#This script cleans almond data for further processing and analysis.
#note I use first flower and flower 10% as flowering

# Setup -------------------------------------------------------------------


drivepath <- '/Volumes/GoogleDrive/My Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

library(lubridate)
library(reshape2)
library(tidyverse)
source('functions/generalfunctions.R')


options(stringsAsFactors = FALSE)


cl <- read.csv(file.path(drivepath,'data/clean/croploc.csv'))

voi <- c('cultivar','year','loc','source','event','day')


# Import ------------------------------------------------------------------


#importing data from 3 different sources
araw1 <- read.csv(file.path(importpath, 'AlmondChicoModestoBloom.csv'))
araw2 <- read.csv(file.path(importpath, 'NSVAlmond.csv'))
araw3 <-  read.csv(file.path(importpath, 'RAVTHullsplitandBloom.csv'))


# Formatting first data source ---------------------------------------------


a1 <- araw1 %>% 
    select(c(Cultivar, Location, Year, JD)) %>% 
    rename(cultivar=Cultivar, loc=Location, year=Year, day=JD) %>% 
    add_column(event='event1', source='FF')



# Formatting second data source --------------------------------------------


#giving the cultivars all reasonable names
cultivars2 <- c('Joranolo', 'Ne_Plus_Ultra', 'IXL', 'Peerless', 'Nonpareil', 'Drake', 'Mission', 'TardyNonpareil', 'Butte', 'General')


#transposing the data (except for the first column which is just the cultivar
    #names)

a2 <- reform(araw2, cultivars2)

#converting the date into a 'Date' class vector
a2$Date <- as.Date(paste(a2$date, a2$year, sep='/'), 
                      format = '%m/%d/%Y') 

a2$day <- yday(a2$Date) #converting day to julian day

a2 <- a2[complete.cases(a2),] #getting only complete observations

#adding ID columns
a2$loc <- 'Chico'
a2$source <-'NSV'

#select variable we want
a2 <- a2[a2$event=='First_Flower', voi]
a2$event <- 'event1'


# Formatting third data source ---------------------------------------------


voi3 <- c('Year','Location','X','Hull.Split.Start','X10..bloom')

#selecting only variables we want
adat3 <- araw3 %>% 
    select(voi3) %>% 
    rename('year'='Year', 'loc'='Location', 'cultivar'='X', 
           'event2'='Hull.Split.Start', 'event1'='X10..bloom') %>% 
    add_column(source='RAVT')

#renaming locations
adat3$loc <- recode(adat3$loc, `1`='Chico',`2`='Manteca', `3`='Shafter')

#converting data to long format
adat3 <- melt(adat3, id.vars = c('year','loc','cultivar','source'),
              measure.vars = c('event1','event2'),
              variable.name = 'event',value.name = 'date')

#removing rows with missing data
zerorows <- which(adat3$date==0)
a3 <- adat3[-zerorows, ]

#converting date string to julian day
a3$Date <- as.Date(paste(a3$date, a3$year, sep='-'), format='%d-%b-%Y')
a3$day <- yday(a3$Date)

a3 <- a3[,voi]



# Combining the data sources ----------------------------------------------


#combining all of the data and 

a <- rbind(a1, a2, a3)

a$year <- as.numeric(a$year)

#renaming locations
a$loc <- recode(a$loc, 'Manteca'='Modesto', 'Mission '='Mission')
a$cultivar <- recode(a$cultivar, 'Mission '='Mission')

#a <- unique(a)
#save file
write.csv(a, file.path(drivepath,'data/phenology/almondclean.csv'),
          row.names = FALSE)


drivepath <- '/Users/echellwig/Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

library(tidyverse)
source('functions/preprocessfunctions.R')


options(stringsAsFactors = FALSE)

#This script cleans almond data for further processing and analysis.


cl <- read.csv(file.path(drivepath,'data/clean/croploc.csv'))

voi <- c('cultivar','year','loc','event','day')
########################Almonds###################################

#importing data from 3 different sources
araw1 <- read.csv(file.path(importpath, 'AlmondChicoModestoBloom.csv'))
araw2 <- read.csv(file.path(importpath, 'NSVAlmond.csv'))
araw3 <-  read.csv(file.path(importpath, 'RAVTHullsplitandBloom.csv'))


#########extracting useful columns (araw1)#####################

adat1 <- araw1 %>% 
    select(c(Cultivar, Location, Year, JD)) %>% 
    rename(cultivar=Cultivar, loc=Location, year=Year, day=JD) %>% 
    add_column(event='flowering')


#########restucturing the NSV Almond data (araw2)#####################

#giving the cultivars all reasonable names
cultivars2 <- c('Joranolo', 'Ne_Plus_Ultra', 'IXL', 'Peerless', 'Nonpareil', 'Drake', 'Mission', 'TardyNonpareil', 'Butte', 'General')


#transposing the data (except for the first column which is just the cultivar
    #names)

adat2 <- reform(araw2, cultivars2)

#converting the date into a 'Date' class vector
adat2$Date <- as.Date(paste(adat2$date, adat2$year, sep='/'), 
                      format = '%m/%d/%Y') 

adat2$day <- yday(adat2$Date)

adat2 <- adat2[complete.cases(adat2),]

adat2$loc <- 'NSacValley'

adat2 <- adat2[, voi]

#########Extracting the useful columns (araw3)#####################

voi3 <- c('Year','Location','X','Hull.Split.Start','Hull.Split.End',
         'X10..bloom', 'X90..bloom')

adat3 <- araw3 %>% 
    select(voi3) %>% 
    rename('year'='Year', 'loc'='Location', 'cultivar'='X', 
           'hstart'='Hull.Split.Start', 'hend'='Hull.Split.End', 
           'flower10'='X10..bloom', 'flower90'='X90..bloom') 

adat3$loc <- recode(adat3$loc, 'Chico','Manteca', 'Shafter')

adat3 <- melt(adat3, id.vars = c('year','loc','cultivar'),
              measure.vars = c('hstart','hend','flower10','flower90'),
              variable.name = 'event',value.name = 'date')

zerorows <- which(adat3$date==0)
a3 <- adat3[-zerorows, ]

a3$Date <- as.Date(paste(a3$date, a3$year, sep='-'), format='%d-%b-%Y')
a3$day <- yday(a3$Date)

a3 <- a3[,voi]

##################Putting it all together#####################

a <- rbind(adat1, adat2, a3)

write.csv(a, file.path(drivepath,'data/historydata/almondclean.csv'),
          row.names = FALSE)



# Reformatting for flowering ----------------------------------------------

af <- adat1 %>% 
    select(-event) %>% 
    rename(event1=day)


write.csv(af, 
          file=file.path(drivepath, 'data/flowering/almondbloomclean.csv'), 
          row.names=FALSE)





drivepath <- '/Users/echellwig/Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')

pkgs <- c('reshape2','lubridate','dismo')
checkpacks(pkgs)

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

adat1 <- araw1[,c('Cultivar', 'Location', 'Year', 'JD')]
names(adat1) <- c('cultivar', 'loc','year','day')
adat1$event <- "flowering"

adat1 <- adat1[,voi]

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

adat3 <- araw3[,voi3]

names(adat3) <- c('year', 'loc', 'cultivar', 'hstart', 'hend', 'flower10',
               'flower90')

adat3[adat3$loc==1,'loc'] <- 'Chico'
adat3[adat3$loc==2,'loc'] <- 'Manteca'
adat3[adat3$loc==3,'loc'] <- 'Shafter'

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


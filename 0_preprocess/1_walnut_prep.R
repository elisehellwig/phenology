
#This script cleans up walnut data for further processing analysis. 


# Setup -------------------------------------------------------------------


#setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Volumes/GoogleDrive/My Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(phenoclim)


options(stringsAsFactors = FALSE)

#read in data specifying where each of the orchards are
cl <- read.csv(file.path(drivepath,
                         'data/raw/crop/WalnutBreedProgram_Locations.csv'))

names(cl)[1:2] <- c('loc','nearest')

# Import ------------------------------------------------------------------

#import all walnut files
wraw1 <- read.csv(file.path(importpath, 'walnut1954.csv'))
wraw2 <- read.csv(file.path(importpath,'walnutpope2.csv'))
wraw3 <- read.csv(file.path(importpath,'walnutbloompope.csv'))
wraw4 <- read.csv(file.path(importpath,'WalnutJarvisSheanData2018_10.csv'))



# Merge -------------------------------------------------------------------

#walnut columns we care about
wcols <- c('CULT', 'YR','LOCATE','LFDA','HARV')

#removes UC code columns
w1 <- wraw1[,wcols]
w2 <- wraw2[,wcols]
w3 <- wraw3[,wcols]
w4 <- wraw4[,wcols]

#we only neeed the most recen data from this file (rest of the data is in other
    #files)
w4 <- w4[w4$YR>=2014, ]

#merges the sources of walnut data together
w <- bind_rows(w1, w2, w3, w4)

#rename columns
names(w) <- c('cultivar', 'year', 'loc', 'lfda', 'harvest')

w$flower <- w$lfda

#convert data to long format
mw <- melt(w, id.vars=c('cultivar', 'year','loc'), value.name='date')


# Clean ------------------------------------------------------------------

#remove blanks and NAs
blank <- which(mw$cultivar=="" | mw$date %in% c('', NA))
mw <- mw[-blank,]

#remove duplicated entries
mw <- unique(mw)

#rename cultivar
mw[mw$cultivar=='Scharsch Franquette', 'cultivar'] <- 'Franquette'

mw$date <- as.Date(mw$date, format='%m/%d/%Y')

#select only davis data
mw2 <- merge(mw, cl[,c('loc','nearest')])
mw <- mw2[mw2$nearest=='Davis', c('cultivar','year','variable','date')]

#checking to see if there are duplicates
w <- mw
ycheck <- w[,c('year','cultivar','variable')]
uy <- unique(ycheck)


#averages the dates and the nut weights
for (i in 1:length(uy[,1])) {
	w[w$cultivar==uy[i,2] & w$year==uy[i,1] & w$variable==uy[i,3], 'date'] <- avgdate(w[w$cultivar==uy[i,2] & w$year==uy[i,1] & w$variable==uy[i,3], 'date'])
}



# Format and subset -------------------------------------------------------

#removing duplicates
w <- unique(w)
w$day <- yday(w$date)

#converting to wide data
wc <-dcast(w[,c('cultivar','year','variable','day')], 
           year + cultivar ~ variable, value.var = 'day')

wc$lfda <- NULL #removign unnecessary column

names(wc)[3:4] <- c('event2','event1') #standardizing phenological event names

#converting data to long format
wm <- melt(wc, id.vars=c('year','cultivar'), value.name = 'day',
           variable.name = 'event', measure.vars = c('event1','event2'))

#removing rows with missing event day values
nas <- which(is.na(wm$day))
wm2 <- wm[-nas, ]

#cultivars of interest
cults <- c('Chandler','Franquette','Hartley','Howard','Ivanhoe','Payne','Serr',
           'Solano','Tulare','Vina')
#cults <- names(which(table(wm2$cultivar)>=70))
wcults <- wm2[wm2$cultivar %in% cults, ]



# Save --------------------------------------------------------------------


write.csv(wcults, 
          file=file.path(drivepath, 'data/phenology/walnutclean.csv'), 
          row.names=FALSE)


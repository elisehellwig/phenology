
# Setup -------------------------------------------------------------------


#setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Volumes/GoogleDrive/My Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

library(lubridate)
library(reshape2)
library(plyr)

source('functions/general.R')
source('functions/preprocessfunctions.R')

pkgs <- c('reshape2','lubridate','dismo')


#This script cleans up walnut data for further processing and analysis.

options(stringsAsFactors = FALSE)

cl <- read.csv(file.path(drivepath,'data/clean/croploc.csv'))


# Import ------------------------------------------------------------------


wraw1 <- read.csv(file.path(importpath, 'walnut1954.csv'))
wraw2 <- read.csv(file.path(importpath,'walnutpope2.csv'))
wraw3 <- read.csv(file.path(importpath,'walnutbloompope.csv'))



# Merge -------------------------------------------------------------------


wcols <- c('CULT', 'YR','LOCATE','LFDA','HARV')

#removes UC code columns
w1 <- wraw1[,wcols]
w2 <- wraw2[,wcols]
w3 <- wraw3[,wcols]


#merges the two sources of walnut data
w <- merge(w1, w2, by=wcols, all=TRUE)
w <- merge(w, w3, by=wcols, all=TRUE)
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

w <- mw
ycheck <- w[,c('year','cultivar','variable')]
uy <- unique(ycheck)


#averages the dates and the nut weights
for (i in 1:length(uy[,1])) {
	w[w$cultivar==uy[i,2] & w$year==uy[i,1] & w$variable==uy[i,3], 'date'] <- avgdate(w[w$cultivar==uy[i,2] & w$year==uy[i,1] & w$variable==uy[i,3], 'date'])
}

w <- unique(w)
w$day <- yday(w$date)

wc <-dcast(w[,c('cultivar','year','variable','day')], year + cultivar ~ variable)


nas <- union(which(is.na(wc$flower)), which(is.na(wc$harvest)))
w <- wc[-nas,]
w$lfda <- NULL

names(w)[3:4] <- c('event2','event1')

cults <- names(which(table(w$cultivar)>=35))
wcults <- w[w$cultivar %in% cults, ]


write.csv(wcults, 
          file=file.path(drivepath, 'data/walnutdata/walnutclean.csv'), 
          row.names=FALSE)



# creating flowering structured data --------------------------------------



wf <- ldply(cults, function(var) {
    df <- wcults[wcults$cultivar==var,]
    df2 <- df[order(df$year), ]
    
    data.frame(year=df2$year[-1],
               cultivar=var,
               event1=df2$event1[-1],
               event0=df2$event2[-nrow(df2)])
})

write.csv(wf, 
          file=file.path(drivepath, 'data/flowering/walnutbloomclean.csv'), 
          row.names=FALSE)




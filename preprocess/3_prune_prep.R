
#This script cleans prune data for further processing and analysis.

# Setup -------------------------------------------------------------------


drivepath <- '/Users/echellwig/Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

library(reshape2)
library(lubridate)
source('functions/generalfunctions.R')


options(stringsAsFactors = FALSE)


# Import ------------------------------------------------------------------

#import orchard locations
cl <- read.csv(file.path(drivepath,'data/clean/croploc.csv'))

#import raw prune data
praw1 <- read.csv(file.path(importpath,'french1988.csv'))[,c(1:5,10)]
praw2 <- read.csv(file.path(importpath,'NSVPrune.csv'))


# Clean first data source -------------------------------------------------

#renaming columns
names(praw1) <- c('event1','bend','event2','cultivar','loc','year')

#converting to long format data
p1melt <- melt(praw1, id.vars=c('cultivar','loc','year'),
               measure.vars = c('event1','bend','event2'),
               variable.name = 'event',
               value.name = 'date')
                

#adding day/date information
p1melt$Date <- as.Date(p1melt$date, format="%m/%d/%y")
p1melt$day <- yday(p1melt$Date)
p1melt$event <- as.character(p1melt$event)

#removing data we don't need
p1 <- p1melt[which(p1melt$event !='bend' & p1melt$loc=='KAC'),]
p1$loc <- 'Parlier'


# Clean second data source ------------------------------------------------

#cultivars in dataset
vars <- c('French','Gerren','Imperial','General')

#reformat df to have columns (not rows) be the variables
p2 <- reform(praw2, 'French')

#adding day/date information
p2$Date <- as.Date(paste(p2$date, p2$year, sep='/'), format='%m/%d/%Y')
p2$day <- yday(p2$Date) # gives the day of the year (julian day)
p2$year <- as.integer(p2$year)
p2$cultivar <- as.character(p2$cultivar)
p2$loc <- 'Chico'

#selecting only the phenological event we want
p2 <- p2[p2$event=='Full_Bloom', ]

p2$event <- 'event1'


# Combine data ------------------------------------------------------------

#variables to save
voi <- c('cultivar','year','loc','event','day')

#merging the two data sources
p <- rbind(p1[,voi], p2[,voi])

#removing rows with missing data
p <- p[complete.cases(p), ]

#saving data
write.csv(p, file.path(drivepath,'data/phenology/pruneclean.csv'),
          row.names = FALSE)









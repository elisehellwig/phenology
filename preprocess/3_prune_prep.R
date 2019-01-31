
drivepath <- '/Users/echellwig/Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')

pkgs <- c('reshape2','lubridate','dismo')
checkpacks(pkgs)


#This script cleans prune data for further processing and analysis.
options(stringsAsFactors = FALSE)

cl <- read.csv(file.path(drivepath,'data/clean/croploc.csv'))

########################Prunes###################################
praw1 <- read.csv(file.path(importpath,'french1988.csv'))[,c(1:5,10)]
praw2 <- read.csv(file.path(importpath,'NSVPrune.csv'))

####################Praw1#############################
names(praw1) <- c('event1','bend','event2','cultivar','loc','year')

p1melt <- melt(praw1, id.vars=c('cultivar','loc','year'),
               measure.vars = c('event1','bend','event2'),
               variable.name = 'event',
               value.name = 'date')
                
p1melt$Date <- as.Date(p1melt$date, format="%m/%d/%y")
p1melt$day <- yday(p1melt$Date)
p1melt$event <- as.character(p1melt$event)

p1 <- p1melt[which(p1melt$event !='bend' & p1melt$loc=='KAC'),]
p1$loc <- 'Parlier'
####################Praw2#############################
vars <- c('French','Gerren','Imperial','General')

p2 <- reform(praw2, 'French')

p2$Date <- as.Date(paste(p2$date, p2$year, sep='/'), format='%m/%d/%Y')
p2$day <- yday(p2$Date) # gives the day of the year (julian day)
p2$year <- as.integer(p2$year)
p2$cultivar <- as.character(p2$cultivar)
p2$loc <- 'Chico'

p2 <- p2[p2$event=='First_Flower', ]

p2$event <- 'event1'
#####################################
#put it together
voi <- c('cultivar','year','loc','event','day')

p <- rbind(p1[,voi], p2[,voi])

write.csv(p, file.path(drivepath,'data/historydata/pruneclean.csv'),
          row.names = FALSE)









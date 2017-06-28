
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
names(praw1) <- c('bstart','bend','maturity','cultivar','loc','year')

p1melt <- melt(praw1, id.vars=c('cultivar','loc','year'),
               measure.vars = c('bstart','bend','maturity'),
               variable.name = 'event',
               value.name = 'date')
                
p1melt$Date <- as.Date(p1melt$date, format="%m/%d/%y")
p1melt$day <- yday(p1melt$Date)


####################Praw2#############################
vars <- c('French','Gerren','Imperial','General')

p2 <- reform(praw2, vars)

p2$Date <- as.Date(paste(p2$date, p2$year, sep='/'), format='%m/%d/%Y')
p2$day <- yday(p2$Date) # gives the day of the year (julian day)







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
praw1 <- read.csv(file.path(importpath,'french1988.csv'))
praw2 <- read.csv(file.path(importpath,'NSVPrune.csv'))

####################Praw2#############################





####################Praw2#############################
vars <- c('French','Gerren','Imperial','General')

p2 <- reform(praw2, vars)

p2$Date <- as.Date(paste(p2$date, p2$year, sep='/'), format='%m/%d/%Y')
p2$day <- yday(p2$Date)




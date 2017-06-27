
drivepath <- '/Users/echellwig/Drive/Phenology'
importpath <- file.path(drivepath, 'data/raw/crop/')

source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')

pkgs <- c('reshape2','lubridate','dismo')
checkpacks(pkgs)


#This script cleans prune data for further processing and analysis.


cl <- read.csv(file.path(drivepath,'data/clean/croploc.csv'), stringsAsFactors=FALSE)

########################Prunes###################################
praw1 <- read.csv(file.path(importpath,'french1988.csv'),
                  stringsAsFactors=FALSE)
pdat2 <- as.data.frame(t(read.csv(file.path(importpath,'NSVPrune.csv'),
                                            stringsAsFactors=FALSE)))

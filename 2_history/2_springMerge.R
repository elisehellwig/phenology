
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(reshape2)
library(plyr)
library(phenoclim)

source('functions/datetime.R')
source('functions/thermaltimesupport.R')
options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

inpath <- '/Volumes/GoogleDrive/My Drive/Phenology/CA_Results/data'
outpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/history'
#precippath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/historydata'

options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

a <- unique(read.csv(file.path(phenologypath,'almondclean.csv')))
p <- unique(read.csv(file.path(phenologypath,'pruneclean.csv')))
walnut <- read.csv(file.path(phenologypath,'walnutclean.csv'))
precip <- read.csv(file.path(historypath, 'precipitation.csv'))

temp <- readRDS(file.path(phenologypath, 'dailyhourlytemp.RDS'))
temp$dt <- as.POSIXct(temp$dt, format="%Y-%m-%d %H:%M:%OS")


almondK <- read.csv(file.path(inpath, 'Almond_CrCragHrJD.csv'))
pruneK <- read_csv(file.path(inpath, 'Prune_Cr38JD_Crag49JD_HrJD.csv')) 

#chandK <- read.csv(file.path(inpath, 'Chand_Cr42JDCrag53JDHrJD.csv'))
#franqK <- read.csv(file.path(inpath, 'Franq_Cr51JDCrag60JDHrJD.csv'))
#payneK <- read.csv(file.path(inpath, 'Payne_Cr33JDCrag46JDHrJD.csv'))

walnutK <- read_csv(file.path(outpath, 'walnutchill.csv'))


# Almonds -----------------------------------------------------------------
acults <- c('Nonpareil','Mission')

a <- a %>% filter(source=='FF', loc=='Chico', 
                  cultivar %in% acults)


af1 <- ldply(acults, function(cv) {
    calcThermalTime(a, temp, 'flowering', 'TTT', 'chillPortions', NA, 305,
                          22, NA, 'Chico', cv)
})

startheatA <- lapply(acults, function(cv) {
    aff <- filter(af1, cultivar==cv)
    startHeat(aff$TTTchillPortions, aff$year)
})


names(startheatA) <- acults

af2 <- ldply(acults, function(cv) {
    calcThermalTime(a, temp, 'flowering', 'TTT', 'linear', 4.5, 
                    startheatA[[cv]], 6000, NA, 'Chico', cv)
})

af <- merge(af1, af2)
af$crop <- 'almond'
af$source <- NULL

# Prunes ------------------------------------------------------------------
plocs <- unique(p$loc)

p <- filter(p, event=='event1')

pf1 <- ldply(plocs, function(l) {
    calcThermalTime(p, temp, 'flowering', 'TTT', 'chillPortions', NA, 305,
                    35, NA, l, 'French')
})

startheatP <- lapply(plocs, function(l) {
    pff <- filter(pf1, loc==l)
    startHeat(pff$TTTchillPortions, pff$year)
})

names(startheatP) <- plocs

pf2 <- ldply(plocs, function(l) {
    calcThermalTime(p, temp, 'flowering', 'TTT', 'linear', 4.5,
                    startheatP[[l]], 5000, NA, l, 'French')
})


pf <- merge(pf1, pf2)
pf$crop <- 'prune'


# Walnuts -----------------------------------------------------------------
wcults <- c('Chandler','Hartley','Ivanhoe','Payne','Franquette')
wchill <- c(70.4,68.4, 66.1, 66.1, 70.2)
wheat <- c(8573,8551,5139, 5139, 10705)

w <- walnut %>% 
    filter(cultivar %in% wcults, event=='event1') %>% 
    add_column(loc='Davis')

wf1 <- ldply(seq_along(wcults), function(i) {
    calcThermalTime(w, temp, 'flowering', 'TTT', 'chillPortions', NA, 305,
                    wchill[i], NA, 'Davis', wcults[i])
})


startheatW <- lapply(wcults, function(cv) {
    wff <- filter(wf1, cultivar==cv)
    startHeat(wff$TTTchillPortions, wff$year)
})

wf2 <- ldply(seq_along(wcults), function(i) {
    calcThermalTime(w, temp, 'flowering', 'TTT', 'linear', 4.5, 
                    startheatW[[i]], 10500, NA, 'Davis', wcults[i])
})

wf <- merge(wf1, wf2)
wf$crop <- 'walnut'


#####################Putting it Together##########################

fruits <- rbind(af, pf, wf)
spring <- inner_join(fruits, precip, by=c('year', 'loc'='location'))

write.csv(spring, file.path(historypath, 'spring.csv'), row.names = FALSE)



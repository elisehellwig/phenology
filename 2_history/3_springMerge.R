
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


locVar <- read.csv(file.path(historypath, 'SeasonLengthParameters.csv'), 
                   stringsAsFactors = FALSE)

#almondK <- read.csv(file.path(inpath, 'Almond_CrCragHrJD.csv'))
#pruneK <- read_csv(file.path(inpath, 'Prune_Cr38JD_Crag49JD_HrJD.csv')) 

#chandK <- read.csv(file.path(inpath, 'Chand_Cr42JDCrag53JDHrJD.csv'))
#franqK <- read.csv(file.path(inpath, 'Franq_Cr51JDCrag60JDHrJD.csv'))
#payneK <- read.csv(file.path(inpath, 'Payne_Cr33JDCrag46JDHrJD.csv'))

#walnutK <- read_csv(file.path(outpath, 'walnutchill.csv'))


# Almonds -----------------------------------------------------------------
#almond cultivars of interest
acults <- c('Nonpareil','Mission','Sonora')

#extracting only the almond data we want
a <- filter(a, cultivar %in% acults, 
            (loc %in% c('Chico','Manteca') & source=='FF') | loc=='Shafter',
            event=='event1') 
a$event1 <- a$day

#selecting only almond parameters
aLocVar <- filter(locVar, crop=='almond')
    

#calculating when we meet chill portion requirement starting at november 1
#accumulating 22 chill portions
af1 <- ldply(1:nrow(aLocVar), function(i) {
    #print(aLocVar[i, ])
    calcThermalTime(events=a, temperatures=temp, step='flowering', 
                    modtype='TTT', form='chillPortions', cardinal=NA, 
                    start=305, thresh=22, varying=NA, 
                    location=aLocVar[i, 'loc'], var=aLocVar[i, 'cultivar'])
})

#calculating when to start accumulating heat
startheatA <- lapply(1:nrow(aLocVar), function(i) {
    aff <- filter(af1, cultivar==aLocVar[i, 'cultivar'], 
                  loc==aLocVar[i, 'loc'])
    startHeat(aff$TTTchillPortions, aff$year)
})

#calculating amount of time it takes to accumulate 6000GDH (linear ff)
#after chill portions met
af2 <- ldply(1:nrow(aLocVar), function(i) {
    calcThermalTime(a, temp, 'flowering', 'TTT', 'linear', 4.5, 
                    startheatA[[i]], 6000, NA, aLocVar[i, 'loc'],
                    aLocVar[i, 'cultivar'])
})

af <- merge(af1, af2)
af$crop <- 'almond'
af$source <- NULL

# Prunes ------------------------------------------------------------------
plocs <- unique(p$loc)

p <- filter(p, event=='event1')

#calculating when we meet chill portion requirement starting at november 1
#35 chill portions
pf1 <- ldply(plocs, function(l) {
    calcThermalTime(p, temp, 'flowering', 'TTT', 'chillPortions', NA, 305,
                    35, NA, l, 'French')
})

#calculating when to start accumulating heat for prunes
startheatP <- lapply(plocs, function(l) {
    pff <- filter(pf1, loc==l)
    startHeat(pff$TTTchillPortions, pff$year)
})

names(startheatP) <- plocs

#calculating amount of time it takes to accumulate 5000GDH (linear ff)
#after chill portions met
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

#calculating how long it takes to meet chill portion requirement
wf1 <- ldply(seq_along(wcults), function(i) {
    calcThermalTime(w, temp, 'flowering', 'TTT', 'chillPortions', NA, 305,
                    wchill[i], NA, 'Davis', wcults[i])
})

#calculating when to start accumulating heat for walnuts
startheatW <- lapply(wcults, function(cv) {
    wff <- filter(wf1, cultivar==cv)
    startHeat(wff$TTTchillPortions, wff$year)
})

#calculating amount of time it takes to accumulate heat requirement
#after chill portions met
wf2 <- ldply(seq_along(wcults), function(i) {
    calcThermalTime(w, temp, 'flowering', 'TTT', 'linear', 4.5, 
                    startheatW[[i]], 10500, NA, 'Davis', wcults[i])
})

wf <- merge(wf1, wf2)
wf$crop <- 'walnut'



# Merge and write ---------------------------------------------------------

af$event1 <- NULL

fruits <- rbind(af, pf, wf)
#spring <- inner_join(fruits, precip, by=c('year', 'loc'='location'))

write.csv(fruits, file.path(historypath, 'spring.csv'), row.names = FALSE)



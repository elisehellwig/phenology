
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
w <- read.csv(file.path(phenologypath,'walnutclean.csv'))
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

a <- a %>% filter(source=='FF', loc=='Chico', 
                  cultivar %in% c('Nonpareil','Mission'))


af <- ldply(c('Nonpareil','Mission'), function(cv) {
    calcThermalTime(a, temp, 'flowering', 'TTT', 'chillbasic','')
})
######################################################################
##############################ALMONDS###############################


almondEH <- select(a, -contains('_')) %>% 
    rename(location=loc, bloom=day) %>% 
    add_column(crop='almond') %>% 
    select(-event)

almondKJS <- almondK %>% 
    spread(requ, jd) %>% 
    select(-`Agronomic Chill`, -Bloom)

almond <- inner_join(almondEH, almondKJS)

#afrain <- inner_join(afnew, precip)
#write.csv(afnew, file.path(outpath, 'almondspring.csv'), row.names = FALSE)


######################################################################
##############################PRUNES##################################

#note to remove the filtering to only parlier when I calculate chill for chico
#for prunes
pruneEH <- p %>% 
    filter(loc=='Parlier') %>%
    select(-contains('_'), -event) %>% 
    add_column(crop='prune') %>% 
    rename(location=loc, bloom=day)


pruneKJS <- pruneK %>% 
    spread(requ, jd) %>% 
    select(-`Agronomic Chill`, -Bloom)

prune <- inner_join(pruneEH, pruneKJS)

#write.csv(pfnew, file.path(outpath, 'prunespring.csv'), row.names = FALSE)

######################################################################
##############################Walnuts###############################

####merge cultivars#######

# chandK$cultivar <- 'Chandler'
# franqK$cultivar <- 'Franquette'
# payneK$cultivar <- 'Payne'
# 
# walnutK <- rbind(chandK, franqK, payneK)
# write.csv(walnutK, file.path(outpath,'walnutchill.csv'), row.names = FALSE)

##########################

walnutEH <- w %>% 
    select(-contains("_"), bloom=day, -event) %>% 
    add_column(crop='walnut', location='Davis')

walnutKJS <- walnutK %>% 
    spread(requ, jd) %>% 
    select(-`Agronomic Chill`, -`Leaf-Out`)

walnut <- inner_join(walnutEH, walnutKJS)
######################################################################
#########################Putting it Together##########################

fruits <- rbind(almond, prune, walnut)
spring <- inner_join(fruits, precip)

write.csv(spring, file.path(historypath, 'spring.csv'), row.names = FALSE)

#write.csv(wfnew, file.path(outpath, 'walnutspring.csv'), row.names = FALSE)



t2 <- filter(spring, cultivar=='Payne')

mod2 <- lm(bloom ~ Chill + Heat, data=t2)

t3 <- filter(spring, cultivar=='French')

mod3 <- lm(bloom ~ Chill + Heat, data=t2)


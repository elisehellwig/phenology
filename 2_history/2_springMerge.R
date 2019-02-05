library(tidyverse)
library(lubridate)

options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

inpath <- '/Volumes/GoogleDrive/My Drive/Phenology/CA_Results/data'
outpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/history'
#precippath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/historydata'

a <- read.csv(file.path(phenologypath, 'almondclean.csv')) %>% 
    filter(event=='event1')

p <- read.csv(file.path(phenologypath, 'pruneclean.csv')) %>% 
    filter(event=='event1')

w <- read.csv(file.path(phenologypath, 'walnutclean.csv')) %>% 
    filter(event=='event1')

almondK <- read.csv(file.path(inpath, 'Almond_CrCragHrJD.csv'))
pruneK <- read_csv(file.path(inpath, 'Prune_Cr38JD_Crag49JD_HrJD.csv')) 

#chandK <- read.csv(file.path(inpath, 'Chand_Cr42JDCrag53JDHrJD.csv'))
#franqK <- read.csv(file.path(inpath, 'Franq_Cr51JDCrag60JDHrJD.csv'))
#payneK <- read.csv(file.path(inpath, 'Payne_Cr33JDCrag46JDHrJD.csv'))

walnutK <- read_csv(file.path(outpath, 'walnutchill.csv'))

precip <- read.csv(file.path(historypath, 'precipitation.csv'))
    

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





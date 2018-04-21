library(tidyverse)
library(lubridate)

inpath <- '/Volumes/GoogleDrive/My Drive/Phenology/CA_Results/data'
outpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/history'
precippath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/historydata'

almondK <- read_csv(file.path(inpath, 'Almond_CrCragHrJD.csv'))
af <- read_csv(file.path(inpath, 'almondfloweringdata.csv'))

pruneK <- read_csv(file.path(inpath, 'Prune_Cr38JD_Crag49JD_HrJD.csv')) 

pf <- read_csv(file.path(inpath,'prunefloweringdata.csv'))

wf <- read_csv(file.path(inpath, 'walnutfloweringdata.csv'))

#chandK <- read.csv(file.path(inpath, 'Chand_Cr42JDCrag53JDHrJD.csv'))
#franqK <- read.csv(file.path(inpath, 'Franq_Cr51JDCrag60JDHrJD.csv'))
#payneK <- read.csv(file.path(inpath, 'Payne_Cr33JDCrag46JDHrJD.csv'))

walnutK <- read_csv(file.path(outpath, 'walnutchill.csv'))

precip <- read_csv(file.path(precippath, 'precipitation.csv'))
    


elisecols <- c('year','cultivar','nearest','fday')
katherinecols  <- c('location','cultivar','year','Chill', 'Heat')

######################################################################
##############################ALMONDS###############################


almondEH <- select(af, elisecols)
names(almondEH)[3:4] <- c('location','bloom') 

almondKJS <- almondK %>% 
    spread(requ, jd) %>% 
    select(katherinecols)

almond <- inner_join(almondEH, almondKJS)
almond$crop <- 'almond'

#afrain <- inner_join(afnew, precip)

#write.csv(afnew, file.path(outpath, 'almondspring.csv'), row.names = FALSE)


######################################################################
##############################PRUNES##################################

pruneEH <- pf %>% 
    filter(nearest=='Parlier') %>%
    select(elisecols[-2])

pruneEH$cultivar <- 'French'
names(pruneEH)[2:3] <- c('location', 'bloom')


pruneKJS <- pruneK %>% 
    spread(requ, jd) %>% 
    select(katherinecols[-(1:2)])

prune <- inner_join(pruneEH, pruneKJS)
prune$crop <- 'prune'

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

walnutEH <- wf %>% select(elisecols)
names(walnutEH)[3:4] <- c('location','bloom') 

walnutKJS <- walnutK %>% 
    spread(requ, jd) %>% 
    select(katherinecols[-1])

walnut <- inner_join(walnutEH, walnutKJS)
walnut$crop <- 'walnut'
######################################################################
#########################Putting it Together##########################

fruits <- rbind(almond, prune, walnut)
spring <- inner_join(fruits, precip)

write.csv(spring, file.path(outpath, 'spring.csv'), row.names = FALSE)

#write.csv(wfnew, file.path(outpath, 'walnutspring.csv'), row.names = FALSE)





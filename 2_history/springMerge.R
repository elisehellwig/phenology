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
    

######################################################################
##############################ALMONDS###############################


almondEH <- select(af, -contains('_')) %>% 
    rename(location=nearest, bloom=fday) %>% 
    add_column(crop='almond')

almondKJS <- almondK %>% 
    spread(requ, jd) %>% 
    select(-`Agronomic Chill`, -Bloom)

almond <- inner_join(almondEH, almondKJS)

#afrain <- inner_join(afnew, precip)

#write.csv(afnew, file.path(outpath, 'almondspring.csv'), row.names = FALSE)


######################################################################
##############################PRUNES##################################

pruneEH <- pf %>% 
    filter(nearest=='Parlier') %>%
    select(-contains('_')) %>% 
    add_column(cultivar='French', crop='prune') %>% 
    rename(location=nearest, bloom=fday)


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

walnutEH <- wf %>% 
    select(-contains("_"), location=nearest, bloom=fday) %>% 
    add_column(crop='walnut')

walnutKJS <- walnutK %>% 
    spread(requ, jd) %>% 
    select(-`Agronomic Chill`, -`Leaf-Out`)

walnut <- inner_join(walnutEH, walnutKJS)
######################################################################
#########################Putting it Together##########################

fruits <- rbind(almond, prune, walnut)
spring <- inner_join(fruits, precip)

write.csv(spring, file.path(outpath, 'spring.csv'), row.names = FALSE)

#write.csv(wfnew, file.path(outpath, 'walnutspring.csv'), row.names = FALSE)





source('functions/general.R')
loadtidyverse()

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


afr <- select(af, elisecols)
names(afr)[3:4] <- c('location','bloom') 

akc <- almondK %>% spread(requ, jd)
akcr <- akc %>% select(katherinecols)

afnew <- inner_join(afr, akcr)

afrain <- inner_join(afnew, precip)

write.csv(afnew, file.path(outpath, 'almondspring.csv'), row.names = FALSE)


######################################################################
##############################PRUNES##################################

pfr <- pf[pf$nearest=='Parlier',  elisecols[-(2:3)]]
names(pfr)[2] <- 'bloom'


pkc <- dcast(pruneK, year ~ requ, value.var = 'jd')
pkr <- pkc[, katherinecols[-(1:2)]]


pfnew <- merge(pfr, pkr)

write.csv(pfnew, file.path(outpath, 'prunespring.csv'), row.names = FALSE)

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

wfr <- wf[,elisecols[-3]] 
names(wfr)[3] <- c('bloom') 

wkc <- dcast(walnutK, cultivar + year ~ requ, value.var = 'jd')
wkr <- wkc[, katherinecols[-1]]

wfnew <- merge(wfr, wkr)

write.csv(wfnew, file.path(outpath, 'walnutspring.csv'), row.names = FALSE)





library(reshape2)

inpath <- '~/Drive/Phenology/CA_Results/data'
outpath <- '~/Drive/Phenology/Results/history'

almondK <- read.csv(file.path(inpath, 'Almond_CrCragHrJD.csv'),
                    stringsAsFactors = FALSE)
af <- read.csv(file.path(inpath, 'almondfloweringdata.csv'), 
               stringsAsFactors = FALSE)
pruneK <- read.csv(file.path(inpath, 'Prune_Cr38JD_Crag49JD_HrJD.csv'))
pf <- read.csv(file.path(inpath,'prunefloweringdata.csv'))

wf <- read.csv(file.path(inpath, 'walnutfloweringdata.csv'), 
               stringsAsFactors = FALSE)

#chandK <- read.csv(file.path(inpath, 'Chand_Cr42JDCrag53JDHrJD.csv'))
#franqK <- read.csv(file.path(inpath, 'Franq_Cr51JDCrag60JDHrJD.csv'))
#payneK <- read.csv(file.path(inpath, 'Payne_Cr33JDCrag46JDHrJD.csv'))

walnutK <- read.csv(file.path(outpath, 'walnutchill.csv'))

elisecols <- c('year','cultivar','nearest','fday')
katherinecols  <- c('location','cultivar','year','Chill', 'Heat')

######################################################################
##############################ALMONDS###############################


afr <- af[,elisecols] 
names(afr)[3:4] <- c('location','bloom') 

akc <- dcast(almondK, location + cultivar + year ~ requ, value.var = 'jd')
akcr <- akc[, katherinecols]

afnew <- merge(afr, akcr)

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





library(reshape2)

inpath <- '~/Drive/Phenology/CA_Results/data'
outpath <- '~/Drive/Phenology/Results/history'

almondK <- read.csv(file.path(inpath, 'Almond_CrCragHrJD.csv'),
                    stringsAsFactors = FALSE)
af <- read.csv(file.path(inpath, 'almondfloweringdata.csv'), 
               stringsAsFactors = FALSE)

######################################################################
afr <- af[,c('year','cultivar','nearest','fday')] 
names(afr)[3:4] <- c('location','bloom') 

akc <- dcast(almondK, location + cultivar + year ~ requ, value.var = 'jd')
akcr <- akc[,c('location','cultivar','year','Heat')]

afnew <- merge(afr, akcr)

write.csv(afnew, file.path(outpath, 'almondspring.csv'), row.names = FALSE)

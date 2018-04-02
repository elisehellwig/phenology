library(reshape2)

datapath <- '~/Drive/Phenology/CA_Results/data'

almondK <- read.csv(file.path(datapath, 'Almond_CrCragHrJD.csv'),
                    stringsAsFactors = FALSE)
af <- read.csv(file.path(datapath, 'almondfloweringdata.csv'), 
               stringsAsFactors = FALSE)

######################################################################
afr <- af[,c('year','cultivar','nearest','fday')] 

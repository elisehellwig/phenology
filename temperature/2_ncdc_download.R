#using this to debug scrapeclim package
datapath <- '/Users/echellwig/Drive/Phenology/data/ScrapeClim'
library(devtools)
library(rnoaa)
library(ScrapeClim)
library(plyr)
options(stringsAsFactors = FALSE)
#installReload()



source('../phenology/functions/packagetesting.R')
allstns <- readRDS(file.path(datapath, 'all_ghcnd_stations.RDS'))
soi <- read.csv(file.path(datapath, 'CAghncdstations.csv'))
nearbystns <- readRDS(file.path(datapath, 'nearbyCAstns.RDS'))

#########################################################################
#########################################################################
locs <- soi$id

#chico USC00041715, i=122
ci <- which(locs=='USC00041715')

chicodaily <- NOAADownload(locs[ci], 1915:2015, nearbystns[[ci]], r2=60,
                           APItoken = 'LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN')



la <- lapply(1:length(locs), function(i) {
    print(i)
    NOAADownload(locs[i], 1915:2015, nearbystns[[i]], r2=50,
                 APItoken = 'LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN')
})


#########################
#39.710565, -121.849848








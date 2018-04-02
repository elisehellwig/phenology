#using this to debug scrapeclim package
datapath <- '/Users/echellwig/Drive/Phenology/data/ScrapeClim'
library(devtools)
library(rnoaa)
library(ScrapeClim)
library(raster)
library(rgdal)
library(rgeos)

source('../phenology/functions/packagetesting.R')

#########################################################################

#spatial interlude
usa <- getData('GADM',country='USA', level=1)
ca <- usa[usa$NAME_1=='California', ]
writeOGR(ca, dsn=file.path(datapath, 'CA'), layer='GeoJSON', driver='GeoJSON')

########################################################
#takes a long time to run, so only run once,
#used to run meteo_nearby_stations()
allghcndmeta <- ghcnd_stations()
saveRDS(allghcndmeta, file.path(datapath, 'all_ghcnd_stations.RDS'))

#Selecting only the things we are interested in, namely things in california
    #with temperature data
voi <- c('id', 'latitude','longitude', 'elevation','state','first_year',
         'last_year')
allstns <- as.data.frame(allghcndmeta)
tempstns <- unique(allstns[allstns$element %in% c('TMIN','TMAX'), voi])

tempstnssp <- unique(tempstns)
coordinates(tempstnssp) <- tempstnssp[,c('longitude','latitude')]
crs(tempstnssp) <- crs(ca)
CAtempstns <- intersect(tempstnssp, ca)
catempstns <- CAtempstns@data[,voi]

write.csv(catempstns, file.path(datapath, 'CAghncdstations.csv'),
          row.names = FALSE)
#########################################################################
#########################################################################
#nearby stations for all stations in california
vars <- c('id','latitude','longitude')
nearbyCAstnstibble <- meteo_nearby_stations(catempstns[,vars],
                                            station_data = allghcndmeta,
                                            var=c('TMIN','TMAX'), radius=100)

nearbyCAstns <- lapply(nearbyCAstnstibble, function(tib) {
    as.data.frame(tib)
})
saveRDS(nearbyCAstns, file.path(datapath, 'nearbyCAstns.RDS'))




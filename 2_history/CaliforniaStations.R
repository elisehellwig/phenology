setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(lubridate)
library(reshape2)
library(rnoaa)
library(raster)
library(rgdal)
source('functions/generalfunctions.R')
source('functions/preprocessfunctions.R')
source('functions/tempclean.R')
source('functions/preTclean.R')
source('functions/modeltest.R')

ghcndstns <- readRDS(file.path(drivepath, 
                               'data/historydata/ghcndstationmetadata.RDS'))

#######################################################
gs0 <- data.frame(ghcndstns)
NArow <- which(is.na(gs0))
gs <- gs0[-NArow, ]

statetemp <- which(gs$state=='CA' & gs$element=='TMIN')
gsca <- gs[statetemp,]

gsca$range <- gsca$last_year - gsca$first_year
long <- which(gsca$range > 10)
gsca <- gsca[long,]

write.csv(gsca, file.path(drivepath,'data/historydata/CAstationmetadata.csv'),
          row.names = FALSE)


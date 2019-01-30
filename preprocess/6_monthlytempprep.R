datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'

library(lubridate)	
library(phenoclim)
library(plyr)


dht <- readRDS(file.path(datapath, 'clean/dailyhourlytemp.RDS'))

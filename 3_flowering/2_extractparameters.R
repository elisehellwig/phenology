library(lubridate)
library(phenoclim)
library(DEoptim)

datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/flowering'

payne <- readRDS(file.path(datapath, 'TTTPaynetest.RDS'))

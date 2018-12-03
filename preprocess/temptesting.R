datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'

library(phenoclim)


dh <- read.csv(file.path(datapath, 'clean/interpolatedDavis.csv'),
          stringsAsFactors = FALSE)

dt <- 
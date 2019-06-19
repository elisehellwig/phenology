
# Setup -------------------------------------------------------------------

library(ggplot2)
library(dplyr)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'

harvest <- readRDS(file.path(historypath, 'harvest.RDS'))
asl <- filter(harvest, crop=='almond')
tts <- readRDS(file.path(historypath, 'ThermalTimeSeries.RDS'))
atts <- filter(tts, crop=='almond', cultivar=='Nonpareil')


# plotting attempts -------------------------------------------------------


a <- merge(asl, atts, all=TRUE)
names(a)[c(5,9)] <- c('temperature','harvest')

pairs(a[,c('year','temperature','harvest')], col=as.factor(a$loc))

datapath <-  '/Users/echellwig/Drive/Phenology/data/historydata'
library(plyr)
library(lubridate)
library(ScrapeClim)

davd <- readRDS(file.path(datapath, 'Davis1916.RDS'))
davm <- readRDS(file.path(datapath, 'Davis1916month.RDS'))
davy <- readRDS(file.path(datapath, 'Davis1916year.RDS'))

###################################################

amod <- lm(ymin ~ year, data=davy)
a2mod <- arf('ymin', davy, 'year', mo=NA)
yresdf <- data.frame(yr=davy$year,
                     temp=davy$ymin,
                     res=residuals(amod))

aarf <- arf('res', yresdf)

ares<-residuals(amod)[-1]
a1res <- residuals(amod)[-101]

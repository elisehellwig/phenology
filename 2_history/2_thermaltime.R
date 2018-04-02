datapath <-  '/Users/echellwig/Drive/Phenology/data/historydata'
library(plyr)
library(lubridate)
library(ScrapeClim)
library(phenoclim)
source('functions/thermaltimesupport.R')

davd <- readRDS(file.path(datapath, 'Davis1916.RDS'))
davh <- readRDS(file.path(datapath, 'Davis1916hr.RDS'))

w <- read.csv(file.path(datapath, 'walnutclean.csv'))
yrs <- 1916:2016
    
############################################################

##payne
DT <- 62
Tb <- 11.1

#formatting the phenology data
wp <- w[w$cultivar=='Payne', c('year','event1','event2','length1')]
wpext <- extendphenology(wp, 1916, 2016)

#format temperature
davd$day <- yday(davd$date)
davtl <- extracttemplist(davd, yrs, 'gddsimple')[[1]]


#thermalsums
tsp <- thermalsum(pars=Tb, wpext, davtl, 'DT','gddsimple',DT, 1)

thermaldf <- data.frame(year=yrs,
                        DT=tsp)


Tbh <- 10.6
TTT <- 5344

wa <- w[w$cultivar=='Ashley', c('year','event1','event2','length1')]
waext <- extendphenology(wa, 1916, 2016)
davtl <- extracttemplist(davh, yrs, 'linear')[[2]]

tttashly <- data.frame(year=yrs,
                       TTT=thermalsum(pars=Tbh, waext, davtl, 'TTT',
                                     'linear', TTT, 1))

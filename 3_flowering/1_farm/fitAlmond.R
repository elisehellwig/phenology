.libPaths( c(file.path('~','R'), .libPaths()) )
#Will take ~3 days to run and 9G of ram

library(parallel)
library(tidyverse)
library(DEoptim)
library(phenoclim)
source('helperfunctions.R')
#source('functions/helperfunctions.R')
#setwd('/Volumes/GoogleDrive/My Drive/Phenology/data/phenology')

temps <- readRDS('dailyhourlytemp.RDS')
a <- read.csv('almondclean.csv', stringsAsFactors = FALSE)

temps$dt <- as.POSIXct(temps$dt)

cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
v <- as.integer(cv)

v <- 1

forms <- list(c('chillbasic','linear'), c('chillbasic','asymcur'))
initpars <- list(list(7.2, 4), list(7.2, c(4,25,36)))
###########################
locVar <- expand.grid(c('Chico','Modesto'), c('Mission','Nonpareil'))
names(locVar) <- c('location', 'variety')

location <- locVar[v, 'location']
variety <- locVar[v, 'variety']
av <- a %>% 
    filter(cultivar==variety, loc==location,source=='FF') %>% 
    mutate(event=recode(event, 'event2'='event0')) %>% 
    spread(event, day) %>% 
    mutate(event0=300)

av <- av[complete.cases(av),]

pl <- lapply(seq_along(forms), function(i) {
    parameterlist(1, 
                  mt='TTT',
                  simple=TRUE,
                  ff=forms[[i]],
                  ct=initpars[[i]],
                  varyingparameters = NA, 
                  modelthreshold=c(100, 10000), 
                  start=c(0,0),
                  optimized=c('threshold'),
                  ModelClass = 'FlowerModel')
})

fm <- flowermodel(phenology=wv, 
                  temps=temps, 
                  parlist=pl, 
                  lbounds=c(1,1), 
                  ubounds=c(365, 20000),
                  iterations=400, 
                  cores = 12L)

warnings()

saveRDS(fm, paste0('TTT', variety ,'.RDS'))


fmcv <- crossval(object=fm, 
                 temps=temps, 
                 k=5, 
                 fun='rmse',
                 seed=2928391, 
                 lbounds=c(1,1),
                 ubounds=c(365,20000),
                 cores=12L, 
                 iterations=400)

saveRDS(fmcv, paste0('TTT', variety ,'CV.RDS'))



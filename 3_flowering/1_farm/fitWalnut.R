.libPaths( c(file.path('~','R'), .libPaths()) )
#Will take ~3 days to run and 9G of ram

library(parallel)
library(tidyverse)
library(DEoptim)
library(phenoclim)
source('helperfunctions.R')
#source('functions/helperfunctions.R')
#setwd('/Volumes/GoogleDrive/My Drive/Phenology/data/flowering')

temps <- read.csv('davisdailyhourlytemp.csv', stringsAsFactors = FALSE)
w <- read.csv('walnutclean.csv', stringsAsFactors = FALSE)

temps$dt <- as.POSIXct(temps$dt)

cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
v <- as.integer(cv)

#v <- 1

forms <- c('chillbasic','linear','gdd','anderson')
initpars <- initialparlist(c(1,1,1,3))
initpars[[1]][[1]] <- 7.2
###########################
vars <- sort(unique(w$cultivar))
variety <- vars[v]
wv <- w %>% 
    filter(cultivar==variety) %>% 
    mutate(event=recode(event, 'event2'='event0')) %>% 
    spread(event, day) %>% 
    mutate(event0=300)

wv <- wv[complete.cases(wv),]

pl <- lapply(seq_along(forms), function(i) {
    parameterlist(1, 
                  mt='TTT',
                  simple=FALSE,
                  ff=forms[i],
                  ct=initpars[[i]],
                  varyingparameters = NA, 
                  modelthreshold=100, 
                  start=300, 
                  optimized=c('start','threshold'),
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



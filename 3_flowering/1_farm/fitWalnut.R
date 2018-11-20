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

#cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
#v <- as.integer(cv)

v <- 6
w <- na.omit(w)

forms <- c('chillbasic','linear','gdd','anderson')
initpars <- initialparlist(c(1,1,1,3))
initpars[[1]][[1]] <- 7.2
###########################
vars <- sort(unique(w$cultivar))
variety <- vars[v]
wv <- w %>% 
    filter(cultivar==variety & year<1990) %>% 
    mutate(event=recode(event, 'event2'='event0')) %>% 
    spread(event, day) %>% 
    mutate(event0=300)

dpl <- lapply(1:length(forms), function(i) {
    parameterlist(1, 'TTT', FALSE, forms[i], initpars[[i]],
                  varyingparameters = NA, modelthreshold=50, start=300, 
                  optimized=c('threshold', 'start'),
                  ModelClass = 'FlowerModel')
})

dpmlen <- flowermodel(wv, temps, dpl, c(0,0), c(365,5000),
                   iterations=400, cores = 8L)

saveRDS(dpmlen, paste0('TTT', variety ,'test.RDS'))




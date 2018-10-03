.libPaths( c(file.path('~','R'), .libPaths()) )
#Will take ~3 days to run and 9G of ram

library(parallel)
library(DEoptim)
library(phenoclim)
source('helperfunctions.R')
#source('functions/helperfunctions.R')
#setwd('~/Drive/Phenology/data/walnutdata/')

temps <- read.csv('davisdailyhourlytemp2.csv', stringsAsFactors = FALSE)
w <- read.csv('walnutclean.csv')

#cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
#v <- as.integer(cv)

forms <- c('gdd','linear','flat', 'triangle','asymcur')
initpars <- initialparlist(c(1,1,2,3,3))

###########################
vars <- levels(w$cultivar)
variety <- vars[v]
wv <- w[w$cultivar==variety, ]

dpl <- mclapply(gdhseq1, function(len) {
    lapply(1:length(forms), function(i) {
        parameterlist(1, 'TTT', FALSE, forms[i], initpars[[i]], len,
                    optimized=c('cardinaltemps'))
    })
})

dpmlen <- mclapply(1:length(dpl), function(i) {
        error(plantmodel(wv, temps, dpl[[i]], c(0,0,0), c(100, 100,100),
                   iterations=200, cores = 4L))
    }, mc.cores = 4L)

saveRDS(dpmlen, paste0('TTTlengthGDH1', variety ,'.RDS'))




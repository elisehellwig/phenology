.libPaths( c(file.path('~','R'), .libPaths()) )
#will take ~8 hrs to run, 12 cores and 4G of ram

library(parallel)
library(phenoclim)
source('helperfunctions.R')

#setwd('~/Drive/Phenology/data/walnutdata/')

temps <- read.csv('davisdailyhourlytemp.csv', stringsAsFactors = FALSE)
w <- read.csv('walnutclean.csv')

cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
v <- as.integer(cv)


forms <- c('gdd','gddsimple')
initpars <- initialparlist(c(1,1))

###########################
vars <- levels(w$cultivar)
variety <- vars[v]
wv <- w[w$cultivar==variety, ]

dayseq <- round(seq(1, 4001, length.out=220))


dpl <- mclapply(dayseq, function(len) {
    lapply(1:length(forms), function(i) {
        parameterlist(1, 'TTT', FALSE, forms[i], initpars[[i]], len,
                    optimized=c('cardinaltemps'))
    })
})

dpmlen <- mclapply(1:220, function(i) {
        unlist(cardinaltemps(plantmodel(wv, temps, dpl[[i]], c(0), c(100),
                   iterations=200, cores = 4L)))
    }, mc.cores = 4L)

saveRDS(dpmlen, paste0('TTTtempsGDD', variety ,'.RDS'))




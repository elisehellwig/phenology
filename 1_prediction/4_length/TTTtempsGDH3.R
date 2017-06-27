.libPaths( c(file.path('~','R'), .libPaths()) )
#Will take ~3 days to run and 9G of ram

library(parallel)
library(phenoclim)
source('helperfunctions.R')
#source('functions/helperfunctions.R')
#setwd('~/Drive/Phenology/data/walnutdata/')

temps <- read.csv('davisdailyhourlytemp.csv', stringsAsFactors = FALSE)
w <- read.csv('walnutclean.csv')

cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
v <- as.integer(cv)

slens <- c(65000, 65000, 21000, 74000, 27000, 21000, 41000, 62000, 60000, 47000,
           26000, 15000)

forms <- c('linear','flat', 'triangle','asymcur')
initpars <- initialparlist(c(1,2,3,3))

###########################
vars <- levels(w$cultivar)
variety <- vars[v]
wv <- w[w$cultivar==variety, ]

gdhseq1 <- round(seq(1, slens[v]/3, by=150))
gdhseq2 <- round(seq(slens[v]/3, (slens[v]/3)*2, by=150))
gdhseq3 <- round(seq((slens[v]/3)*2, slens[v], by=150))

dpl <- mclapply(gdhseq3, function(len) {
    lapply(1:length(forms), function(i) {
        parameterlist(1, 'TTT', FALSE, forms[i], initpars[[i]], len,
                    optimized=c('cardinaltemps'))
    })
})

dpmlen <- mclapply(1:length(dpl), function(i) {
        unlist(cardinaltemps(plantmodel(wv, temps, dpl[[i]], c(0,0,0), 
                                        c(100, 100,100),
                   iterations=200, cores = 4L)))
    }, mc.cores = 4L)

saveRDS(dpmlen, paste0('TTTtempsGDH3', variety ,'.RDS'))




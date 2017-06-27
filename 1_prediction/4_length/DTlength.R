.libPaths( c(file.path('~','R'), .libPaths()) )
#will take 5 days and 9G of ram to run probably


library(parallel)
library(phenoclim)
source('helperfunctions.R')
#setwd('~/Drive/Phenology/data/walnutdata/')

temps <- read.csv('davisdailyhourlytemp.csv', stringsAsFactors = FALSE)
w <- read.csv('walnutclean.csv')

cv <- Sys.getenv("SLURM_ARRAY_TASK_ID")
v <- as.integer(cv)


forms <- c('gdd','gddsimple','linear','flat','triangle', 'asymcur')
initpars <- initialparlist(c(1,1,1,2,3,3))

###########################
vars <- levels(w$cultivar)
variety <- vars[v]
wv <- w[w$cultivar==variety, ]

tpl <- mclapply(1:160, function(len) {
    lapply(1:length(forms), function(i) {
        parameterlist(1, 'thermal', FALSE, forms[i], initpars[[i]], len,
                    optimized=c('cardinaltemps'))
    })
})

tpmlen <- mclapply(1:160, function(i) {
        error(plantmodel(wv, temps, tpl[[i]], c(0,0,0), c(100, 100, 100),
                   iterations=200, cores=4L))
},  mc.cores = 4L)

saveRDS(tpmlen, paste0('DTlength', variety,'.RDS'))




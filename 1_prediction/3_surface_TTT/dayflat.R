drivepath <- '/Users/echellwig/Drive/Phenology'

library(parallel)
library(phenoclim)
options('mc.cores'=3L)

temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)

w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
vars <- levels(w$cultivar)

ms <- read.csv(file.path(drivepath, 
                         'Results/walnutpaper/modelsummary.csv'))

modform <- 'flat'

##########################################
tablengthrows <- which(ms$complexity=='full' & ms$type=='day' & ms$form=='flat')
tlen <- ms[tablengthrows,'length']

w$length1 <- w$event2 - w$event1 
tempseq <- seq(-20, 100)

pg <- expand.grid(tempseq, tempseq)

errors <- mclapply(1:length(vars), function(j) {
    print(vars[j])
    wv <- w[w$cultivar==vars[j], ]
    tlist <- extracttemplist(temps, wv$year, modform)
    data.frame(cultivar=vars[j],
               base=pg[,2],
               opt=pg[,1],
               rmse=sapply(1:dim(pg)[1], function(i) {
                   minrmse(c(tlen[j], pg[i,2], pg[i,1]), wv, tlist[[2]], 
                           'day', modform, 1, c(pg[i,2], pg[i,1]), tlen[j],
                           FALSE)
               }))
})

errorsdf <- do.call('rbind', errors)
errorsdf[errorsdf==Inf] <- NA

saveRDS(errorsdf, file.path(drivepath,
                            'Results/walnutpaper/surface/dayflat.RDS'))



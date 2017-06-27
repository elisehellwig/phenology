drivepath <- '/Users/echellwig/Drive/Phenology'

library(parallel)
library(phenoclim)
library(plyr)
options('mc.cores'=8L)

temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)

w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
vars <- levels(w$cultivar)
w$length1 <- w$event2 - w$event1 




##########################################

gdseq <- seq(1, 20000, by=212)
pg <- expand.grid(gdseq, (-10):30) 
 modform <- 'linear'

errors <- mclapply(vars, function(v) {
    wv <- w[w$cultivar==v, ]
    tlist <- extracttemplist(temps, wv$year, modform)
    data.frame(cultivar=v,
               length=pg[,1],
               base=pg[,2],
               rmse=sapply(1:dim(pg)[1], function(i) {
        minrmse(c( pg[i,1], pg[i,2]), wv, tlist[[2]], 'day', modform, 1,
                pg[i,2], pg[i,1], FALSE)
    }))
})

errorsdf <- do.call('rbind', errors)

errorsdf[errorsdf==Inf] <- NA

saveRDS(errorsdf, file.path(drivepath,
                            'Results/walnutpaper/surface/thermallinear.RDS'))



##########################################
modform <- 'gdd'
gdseq <- seq(1, 7500, by=45)
pg <- expand.grid(gdseq, (-10):25) 


errorsgdd <- mclapply(vars, function(v) {
    wv <- w[w$cultivar==v, ]
    tlist <- extracttemplist(temps, wv$year, modform)
    data.frame(cultivar=v,
               length=pg[,1],
               base=pg[,2],
               rmse=sapply(1:dim(pg)[1], function(i) {
                   minrmse(c(pg[i,1], pg[i,2]), wv, tlist[[1]], 'day',
                           modform, 1, pg[i,2], pg[i,1], FALSE)
               }))
})

errorsgdddf <- do.call('rbind', errorsgdd)
errorsgdddf[errorsgdddf==Inf] <- NA


saveRDS(errorsgdddf, file.path(drivepath,
                               'Results/walnutpaper/surface/daygdd.RDS'))




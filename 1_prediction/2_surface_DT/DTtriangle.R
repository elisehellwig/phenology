drivepath <- '/Users/echellwig/Drive/Phenology'

library(parallel)
library(phenoclim)
options('mc.cores'=8L)

temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)

w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
vars <- levels(w$cultivar)

ms <- read.csv(file.path(drivepath, 
                         'Results/walnutpaper/modelsummary.csv'))

modform <- 'triangle'
cv <- 'Payne'
##########################################
tablengthrows <- which(ms$complexity=='full' & ms$type=='thermal' & ms$form==modform & ms$cultivar==cv)
tlen <- ms[tablengthrows,'length']

w$length1 <- w$event2 - w$event1 
wv <- w[w$cultivar==cv, ]
tlist <- extracttemplist(temps, wv$year, modform)


tempseq <- seq(-20, 100)
critseq <- seq(20, 75, by=5)

pg <- expand.grid(tempseq, tempseq)


errors <- mclapply(1:length(critseq), function(j) {
    data.frame(cultivar=cv,
               base=pg[,2],
               opt=pg[,1],
               crit=critseq[j],
               rmse=sapply(1:dim(pg)[1], function(i) {
                   minrmse(c(tlen, pg[i,2], pg[i,1], critseq[j]), wv, 
                           tlist[[2]], 'thermal', modform, 1, 
                           c(pg[i,2], pg[i,1], critseq[j]), tlen, FALSE)
               }))
})

errorsdf <- do.call('rbind', errors)

errorsdf[errorsdf==Inf] <- NA

saveRDS(errorsdf, file.path(drivepath,
                            'Results/walnutpaper/surface/thermaltriangle.RDS'))



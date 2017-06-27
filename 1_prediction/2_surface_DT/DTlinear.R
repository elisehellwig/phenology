drivepath <- '/Users/echellwig/Drive/Phenology'

library(parallel)
library(phenoclim)
library(plyr)
options('mc.cores'=8L) #number of cores used in mclapply

#temperature data
temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)

#phenology data
w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
vars <- levels(w$cultivar) #vector of cultivar names

modform <- 'linear' #name of functional form for this script

##########################################
 w$length1 <- w$event2 - w$event1 #calculating season length

pg <- expand.grid(1:160, (-20):100)  #matrix of every possible combination of 
                                     #day threshold and base temperature

#calculating the error for every possible combination of day threshold and 
#base temperature
errors <- mclapply(vars, function(v) {
    wv <- w[w$cultivar==v, ] #extract data for focal cultivar
    tlist <- extracttemplist(temps, wv$year, modform) #extract temp data and put
                                                      #in form of a list
    #create data frame with the errors for each DT/base temp combo along with 
    #identifying variables
    data.frame(cultivar=v,
               length=pg[,1],
               base=pg[,2],
               rmse=sapply(1:dim(pg)[1], function(i) {
        minrmse(c( pg[i,1], pg[i,2]), wv, tlist[[2]], 'thermal', modform, 1,
                pg[i,2], pg[i,1], FALSE) #calculate error (rmse)
    }))
})

errorsdf <- do.call('rbind', errors) #convert list of dfs to one data frame

errorsdf[errorsdf==Inf] <- NA #convert infs to NAs

#save data frame
saveRDS(errorsdf, file.path(drivepath,
                            'Results/walnutpaper/surface/DTlinear.RDS'))



##########################################
modform <- 'gdd'

errorsgdd <- mclapply(vars, function(v) {
    wv <- w[w$cultivar==v, ]
    tlist <- extracttemplist(temps, wv$year, modform)
    data.frame(cultivar=v,
               length=pg[,1],
               base=pg[,2],
               rmse=sapply(1:dim(pg)[1], function(i) {
                   minrmse(c(pg[i,1], pg[i,2]), wv, tlist[[1]], 'thermal',
                           modform, 1, pg[i,2], pg[i,1], FALSE)
               }))
})

errorsgdddf <- do.call('rbind', errorsgdd)
errorsgdddf[errorsgdddf==Inf] <- NA


saveRDS(errorsgdddf, file.path(drivepath,
                               'Results/walnutpaper/surface/thermalgdd.RDS'))




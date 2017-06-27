drivepath <- '/Users/echellwig/Drive/Phenology'

library(parallel)
library(phenoclim)
options('mc.cores'=8L) #number of cores to be used in mclapply call

#temperature data from NCDC
temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)

#phenology dat from UC Davis
w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))

vars <- levels(w$cultivar) #vector of cultivar names

#smmary of parameter values from fitted models from the previous step.
ms <- read.csv(file.path(drivepath, 
                         'Results/walnutpaper/modelsummary.csv'))

modform <- 'flat' #name of functional forma of interest.

##########################################

tablengthrows <- which(ms$complexity=='full' & ms$type=='thermal' & ms$form=='flat') #which rows are in the variables of interest?
tlen <- ms[tablengthrows,'length'] #extracting those rows

w$length1 <- w$event2 - w$event1 #calculating season length
tempseq <- seq(-20, 100) #creating a vector of temperatures from -20 to 100C

#creating a matrix where rows are of all possible combinations of integers -20 
    # to 100, will be used for cardinal temperatures.
pg <- expand.grid(tempseq, tempseq)


#calculates errors for each pair of cardinal temperatures
errors <- mclapply(1:length(vars), function(j) {
    print(vars[j]) #prints the cultivar name
    wv <- w[w$cultivar==vars[j], ]#extracts the phenology data for that cultivar
    tlist <- extracttemplist(temps, wv$year, modform) #extracts relevent temp
                                                      #data & puts it in a list
    data.frame(cultivar=vars[j], #creates a data frame with the RMSE and 
               base=pg[,2],      #identifying variables
               opt=pg[,1],
               rmse=sapply(1:dim(pg)[1], function(i) {
                   minrmse(c(tlen[j], pg[i,2], pg[i,1]), wv, tlist[[2]], 
                           'thermal', modform, 1, c(pg[i,2], pg[i,1]), tlen[j],
                           FALSE) #calculates rmse
               }))
})

errorsdf <- do.call('rbind', errors) #converts list of dfs to 1 df
errorsdf[errorsdf==Inf] <- NA #replaces infs with NAs

#saves file
saveRDS(errorsdf, file.path(drivepath,
                            'Results/walnutpaper/surface/DTflat.RDS'))



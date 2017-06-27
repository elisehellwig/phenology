drivepath <- '/Users/echellwig/Drive/Phenology'
library(phenoclim)
library(reshape2)

source('functions/helperfunctions.R')

temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)

w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
lens <- read.csv(file.path(drivepath, 'Results/walnutpaper/lengthsummary.csv'))
vars <- levels(w$cultivar)

w$length1 <- w$event2 - w$event1

###########
#plant models

DTpm <- readRDS(file.path(drivepath, 
                           'Results/walnutpaper/DTplantmodel.RDS'))



dtpheno <- phenology(DTpm[[8]])

cdat <- w[w$cultivar=='Payne', ]

###################################################
#day threshold models

daythresh <- seq(6, 184, by=6)

dtpl <- lapply(daythresh, function(l) {
    parameterlist(1, 'DT', FALSE, 'linear', list(c(0)), l, 
                  "cardinaltemps") 
})

dtpm <- lapply(1:length(daythresh), function(i) {
    plantmodel(cdat, temps, list(dtpl[[i]]), 0, 100, 3, 200, FALSE)
})


dtgdhlist <- lapply(1:length(dtpm), function(i) {
    x <- as.data.frame(phenology(dtpm[[i]])[,'DTlinear1'])
    names(x) <- paste0('gdh', daythresh[i])
    x
})

dtgdh <- do.call(cbind, dtgdhlist)

dtfitlist <- lapply(1:length(dtpm), function(i) {
    x <- as.data.frame(phenology(dtpm[[i]])[,'fitDTlinear1'])
    names(x) <- paste0('fit', daythresh[i])
    x
})
dtfit <- do.call(cbind, dtfitlist) 
###################################################
#thermal time threshold models

thermalthresh <- seq(100, 86661, by=3000)

ttpl <- lapply(thermalthresh, function(l) {
    parameterlist(1, 'TTT', FALSE, 'linear', list(c(0)), l, 
                  "cardinaltemps") 
})

ttpm <- lapply(1:length(thermalthresh), function(i) {
    plantmodel(cdat, temps, list(ttpl[[i]]), 0, 100, 3, 200, FALSE)
})


ttdaylist <- lapply(1:length(ttpm), function(i) {
    x <- as.data.frame(phenology(ttpm[[i]])[,'TTTlinear1'])
    names(x) <- paste0('days', thermalthresh[i])
    x
})

ttday <- do.call(cbind, ttdaylist)

ttfitlist <- lapply(1:length(ttpm), function(i) {
    x <- as.data.frame(phenology(ttpm[[i]])[,'fitTTTlinear1'])
    names(x) <- paste0('fitday', thermalthresh[i])
    x
})
ttfit <- do.call(cbind, ttfitlist) 


###########################################
#variances

samplevar <- var(cdat$length1)

dtgdhvar <- apply(dtgdh, 2, var)
dtfitvar <- apply(dtfit, 2, var)

ttdayvar <- apply(ttday, 2, var)
ttfitvar <- apply(ttfit, 2, var)


vardt <- data.frame(model='DT',
                    threshold=daythresh,
                     x=dtgdhvar,
                     fit=dtfitvar)
vardt$threshold <- vardt$threshold/max(vardt$threshold)

varttt <- data.frame(model='TTT',
                     threshold=thermalthresh,
                     x=ttdayvar,
                     fit=ttfitvar)
varttt$threshold <- varttt$threshold/max(varttt$threshold)

modvar <- rbind(vardt, varttt)
modvarm <- melt(modvar, id.vars = c('model','threshold'), 
                value.name = 'Variance')
modvarm$model <- as.character(modvarm$model)

names(modvarm)[1:3] <- c('Model','Threshold','Variable')
modvarm[modvarm$Model=='DT', 'Model'] <- 'Day Threshold model'
modvarm[modvarm$Model=='TTT', 'Model'] <- 'Thermal Time Threshold model'



write.csv(modvarm, file=file.path(drivepath,'Results/walnutpaper/variances.csv'),
          row.names = FALSE)

#########################################################

#distributions.







########################

par(mfrow=c(2,1))
plot(gdh ~ threshold, data=vardt)
abline(v=48)
plot(fit ~ threshold, data=vardt)
abline(v=48)


par(mfrow=c(2,1))
plot(days ~ threshold, data=varttt)
abline(v=2221)
plot(fitday ~ threshold, data=varttt)
abline(v=2221)





# Setup -------------------------------------------------------------------

library(lubridate)
library(phenoclim)
library(DEoptim)
#library(ehelpr)

options(stringsAsFactors = FALSE)

datapath <- '/Users/echellwig/Drive/Phenology/data/walnutdata'
funpath <- '/Users/echellwig/Research/phenoclim/R'

dav <- read.csv(file.path(datapath, 'davisdailyhourlytemp2.csv'))
wall <- read.csv(file.path(datapath, 'walnutclean.csv'))

#la <- paste(dav$date, 'FlowerModel', paste0(dav$hour, ':00:00'))
#write.csv(dav, file.path(datapath, 'davisdailyhourlytemp2.csv'),
 #   row.names = FALSE)
#dav$dt <- dayToDate(dav$year, dav$day, 'FlowerModel', dav$hour-1)

dav$dt <- as.POSIXct(dav$dt)

subyrs <- 1981:1992

ww <- which(wall$year %in% c(subyrs) & wall$cultivar=='Payne' )
w <- wall[ww,]

wf <- data.frame(year=w$year[-1],
                 event1=w$event1[-1],
                 event0=w$event2[-nrow(w)])



# Troubleshooting thermal sums --------------------------------------------


 #flowerSums <- thermalsum(list(0), wf$year, dav, 'TTT', 'gdd', 5,                       2000, varying=c('start'), 'FlowerModel', wf$event0)
#
#
# # troubleshooting minrmse -------------------------------------------------
#
#
#
# error <- minrmseDT(6, wf, dav, 'linear', 20, 122, 1,
#                    varying = 'start', 'FlowerModel', wf$event0)
#
#
# error2 <- minrmseTTT(6, wf, dav, 'linear', 6, 10, 1,
#                    varying = NA, 'FlowerModel', wf$event0)
#
#
# error3 <- minrmseTTTsimplified(0, wf, dav, 'linear', 20, 2000, 1,
#                      varying = NA, 'FlowerModel', wf$event0)
#
 #errorTT <- minrmse(c(1, 12, 0), wf, dav, 'DT', 'gdd', 1, TRUE, TRUE,
                 #  TRUE, FALSE, NA, 'FlowerModel', 'event0')
#
#
# # troubleshooting flowermodel -----------------------------------------------

forms <- c('gdd', 'linear')

parlist <- lapply(forms, function(f) {
    parameterlist(n=1,
                  mt='DT',
                  simple=FALSE,
                  ff=f,
                  ct=list(0),
                  modelthreshold=12,
                  start=5,
                  varyingparameters=c('start','threshold'),
                  optimized=c('cardinaltemps'),
                  ModelClass='FlowerModel')
})

# phenology <- wf
# temps <- dav
# lbounds <- c(0)
# ubounds <- c(50)
# iterations <- 200
# cores <- 4
#

fm <- flowermodel(phenology=wf,
                  temps=dav,
                  parlist=parlist,
                  lbounds=c(1),
                  ubounds=c(50),
                  cores = 4L)


fmc <- crossval(fm, dav, 3, 2029239, 'rmse', 1, 50, 50, 4L)


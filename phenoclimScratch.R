
# Setup -------------------------------------------------------------------
library(reshape2)
library(lubridate)
library(phenoclim)
library(DEoptim)
#library(ehelpr)

options(stringsAsFactors = FALSE)

datapath <- '/Users/echellwig/Drive/Phenology/data/flowering'
funpath <- '/Users/echellwig/Research/phenoclim/R'

dav <- read.csv(file.path(datapath, 'davisdailyhourlytemp.csv'))
wall <- read.csv(file.path(datapath, 'walnutclean.csv'))

dav$dt <- as.POSIXct(dav$dt, format="%Y-%m-%d %H:%M:%OS")

ww <- which( wall$cultivar=='Chandler' & wall$year %in% 2013:2016)
w <- wall[ww,]

wc <- dcast(w, year + cultivar~ event, value.var = 'day')
wc$length1 <- wc$event2 - wc$event1
set.seed(2938)
wc$event3 <- wc$event2 + round(wc$length1/3) + 
    sample((-3):3, nrow(wc), replace=TRUE)
wc$length2 <- wc$event3 - wc$event2

# wf <- data.frame(year=wc$year,
#                  event1=wc$event1,
#                  event0=250)
#wf <- wf[complete.cases(wf),]


# Plant Model Stuff -------------------------------------------------------

parlist <- lapply(c('gdd', 'linear'), function(form) {
    parameterlist(n=2,
                  mt='DT',
                  simple=FALSE,
                  ff=form,
                  ct=list(4,0), 
                  modelthreshold=c(50,20),
                  start=c(0,0),
                  varyingparameters=c('start', 'threshold'),
                  optimized=c('threshold', 'cardinaltemps'),
                  ModelClass='PlantModel')
})


pmDT <- plantmodel(wc, dav, parlist, c(0,0), c(200, 40), cores=4L)

minrmse(c(124,13.47), wc, dav, 'DT', 'gdd', 2, TRUE, 0, TRUE, FALSE, 
        c('start','threshold'), 'PlantModel', firstevent = 'event2',
        startingevent=wc$event2)

# Troubleshooting thermal sums --------------------------------------------

ds <- dualsum(pars=list(7.2, 4),
                yrs=wf$year,
                tdat=dav,
                forms=c('chillbasic','linear'),
                start=1,
                thresh=c(100, 500),
                varying=NA,
                mclass='FlowerModel',
                startingevent=wf$event0)


 flowerSums <- thermalsum(list(4), wf$year, dav, 'TTT', 'linear', 50,                       300, varying=NA, 'FlowerModel', wf$event0)
#
#
# # troubleshooting minrmse -------------------------------------------------
#
#
 errordual <- minrmseDual(pars=list(7.2,4), 
                          fdat=wf, 
                          tdat=dav, 
                          c('chillbasic','linear'), 
                          start=1, 
                          thresh=c(200,10000),
                          stage=1,
                          varying=NA,
                          modclass='FlowerModel',
                          startingevent = wf$event0)  
 
 
errordual <- minrmse(pars=list(7.2,4),
                     fdat=wf,
                     tdat=dav, 
                     modtype='Dual', 
                     form=c('chillbasic','linear'), 
                     stage=1,
                     CT=list(7.2, 4), 
                     S=1, 
                     TH=c(100, 500),
                     simple=TRUE,
                     varying=NA, 
                     modclass='FlowerModel',
                     firstevent='event0',
                     startingevent=wf$event0) 
 
 
#
# error <- minrmseDT(6, wf, dav, 'linear', 20, 122, 1,
#                    varying = 'start', 'FlowerModel', wf$event0)
#
#
 error2 <- minrmseTTT(7.2, wf, dav, 'chillbasic', 275, 20, 1,
                    varying = NA, 'FlowerModel')
#
#
# error3 <- minrmseTTTsimplified(0, wf, dav, 'linear', 20, 2000, 1,
#                      varying = NA, 'FlowerModel', wf$event0)
#

errorTT <- minrmse(c(50, 300), wf, dav, 'TTT', 'chillbasic', 1, 7.2, 
                   50, 300, TRUE, NA, 'FlowerModel', 'event0')
#
#
# # troubleshooting flowermodel -----------------------------------------------

forms <- c('chillbasic', 'linear','gdd','anderson')
CTs <- list(7.2, 4)

parlist <- lapply(1, function(i) {
    parameterlist(n=1,
                  mt='Dual',
                  simple=TRUE,
                  ff=c('chillbasic','linear'),
                  ct=CTs,
                  modelthreshold=c(100,500),
                  start=1,
                  varyingparameters=NA,
                  optimized=c('start'),
                  ModelClass='FlowerModel')
})

# phenology <- wf
# temps <- dav
# lbounds <- c(0)
# ubounds <- c(50)
# iterations <- 200
# cores <- 4
#

fm2 <- flowermodel(phenology=wf,
                  temps=dav,
                  parlist=parlist,
                  lbounds=c(1),
                  ubounds=c(1000),
                  cores = 4L)



chandler <- readRDS('/Volumes/GoogleDrive/My Drive/Phenology/Results/flowering/TTTChandler.RDS')

fmc <- crossval(object=chandler,
                temps=dav, 
                k=3, 
                seed=2029239, 
                fun='rmse', 
                lbounds=c(1,1), 
                ubounds=c(365, 1000), 
                iterations=50,
                cores=4L)

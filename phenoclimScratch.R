
# Setup -------------------------------------------------------------------
library(dismo)
library(reshape2)
library(lubridate)
library(phenoclim)
library(DEoptim)
#library(ehelpr)

options(stringsAsFactors = FALSE)

datapath <- '/Users/echellwig/Drive/Phenology/data/flowering'
resultspath <- '/Users/echellwig/Drive/Phenology/Results/test'
funpath <- '/Users/echellwig/Research/phenoclim/R'

dav <- read.csv(file.path(datapath, 'davisdailyhourlytemp.csv'))
wall <- read.csv(file.path(datapath, 'walnutclean.csv'))

dav$dt <- as.POSIXct(dav$dt, format="%Y-%m-%d %H:%M:%OS")

ww <- which( wall$cultivar=='Chandler' & wall$year %in% 2003:2016)
w <- wall[ww,]

wc <- dcast(w, year + cultivar~ event, value.var = 'day')
wc$length1 <- wc$event2 - wc$event1
set.seed(2938)
wc$event3 <- wc$event2 + round(wc$length1/3) + 
    sample((-3):3, nrow(wc), replace=TRUE)
wc$length2 <- wc$event3 - wc$event2
wc$event0 <- 300

# wf <- data.frame(year=wc$year,
#                  event1=wc$event1,
#                  event0=250)
#wf <- wf[complete.cases(wf),]


# Plant Model Stuff -------------------------------------------------------

parlist <- lapply(c('gdd', 'linear'), function(form) {
    parameterlist(n=1,
                  mt='TTT',
                  simple=TRUE,
                  ff=form,
                  ct=list(4,0), 
                  modelthreshold=c(10000,20),
                  start=c(0,0),
                  varyingparameters=c('start'),
                  optimized=c('threshold', 'cardinaltemps'),
                  ModelClass='PlantModel')
})




minrmse(c(124,13.47), wc, dav, 'DT', 'gdd', 2, TRUE, 0, TRUE, FALSE, 
        c('start','threshold'), 'PlantModel', firstevent = 'event1',
        startingevent=wc$event1)


# Testing Utah --------------------------------------------------------

threshs <- seq(20, 110, length.out=5)

FMutah <- lapply(1:length(threshs), function(i) {
    print(i)
    parlist <- lapply(c('utah'), function(form) {
        parameterlist(n=1,
                      mt='TTT',
                      simple=FALSE,
                      ff=form,
                      ct=list(NA), 
                      modelthreshold=threshs[i],
                      start=c(300),
                      varyingparameters=c(NA),
                      optimized=c('threshold'),
                      ModelClass='FlowerModel')
    })
    
    flowermodel(wc, dav, parlist, 0, 500, 4L)
})


threshs <- seq(20, 110, length.out=5)

FMcp <- lapply(1:length(threshs), function(i) {
    print(i)
    parlist <- lapply(c('chillPortions'), function(form) {
        parameterlist(n=1,
                      mt='TTT',
                      simple=FALSE,
                      ff=form,
                      ct=list(NA), 
                      modelthreshold=threshs[i],
                      start=c(300),
                      varyingparameters=c(NA),
                      optimized=c('threshold'),
                      ModelClass='FlowerModel')
    })
    
    flowermodel(wc, dav, parlist, 0, 500, 4L)
})



parlist <- lapply(c('chillPortions'), function(form) {
    parameterlist(n=1,
                  mt='TTT',
                  simple=FALSE,
                  ff=form,
                  ct=list(0), 
                  modelthreshold=c(90),
                  start=c(300),
                  varyingparameters=c(NA),
                  optimized=c('threshold'),
                  ModelClass='FlowerModel')
})

CPfm <- flowermodel(wc, dav, parlist, 0, 500, 4L)





minrmse(c(10), wc, dav, 'TTT', 'chillPortions', 1, NA, 350, TRUE, FALSE, 
        c(NA), 'FlowerModel', firstevent = 'event0',
        startingevent=wc$event0)

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


 flowerSums <- thermalsum(list(7), wc$year, dav, 'TTT', "utahalt", 1,                       150, varying=NA, 'FlowerModel', wc$event0)
#
#
# # troubleshooting minrmse -------------------------------------------------
#
#
 errordual <- minrmseDual(pars=list(7.2,4), 
                          fdat=wc, 
                          tdat=dav, 
                          c('chillbasic','linear'), 
                          start=1, 
                          thresh=c(200,10000),
                          stage=1,
                          varying=NA,
                          modclass='FlowerModel',
                          startingevent = wc$event0)  
 
 
errordual <- minrmse(pars=list(7.2,4),
                     fdat=wc,
                     tdat=dav, 
                     modtype='Dual', 
                     form=c('chillbasic','linear'), 
                     stage=1,
                     CT=list(7.2, 4), 
                     S=1, 
                     TH=c(200, 500),
                     simple=TRUE,
                     varying=NA, 
                     modclass='FlowerModel',
                     firstevent='event0',
                     startingevent=wc$event0) 
 
 
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



# Old stuff ---------------------------------------------------------------

pmTTTs <- plantmodel(wc, dav, parlist, c(0,0), c(90000, 40), cores=4L)

#saveRDS(pmDT, file.path(resultspath, 'plantmodelDT.RDS'))
pmDT <- readRDS(file.path(resultspath, 'plantmodelDT.RDS'))
pmTTT <- readRDS(file.path(resultspath, 'plantmodelTTT.RDS'))

pmDTcv <- crossval(pmDT, dav, 3, 100, 'rmse', c(0,0), c(200, 40), 4L)
pmTTTcv <- crossval(pmTTT, dav, 3, 100, 'rmse', c(0,0), c(200, 40), 4L)
pmTTTscv <- crossval(pmTTTs, dav, 3, 100, 'rmse', c(0,0), c(200, 40), 4L)





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

wf <- data.frame(year=wc$year,
                 event1=wc$event1,
                 event0=300)
wf <- wf[complete.cases(wf),]


# Troubleshooting thermal sums --------------------------------------------


 flowerSums <- thermalsum(list(NA), wf$year, dav, 'TTT', 'utah', 50,                       300, varying=NA, 'FlowerModel', wf$event0)
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
CTs <- list(7.2, 4, 4, c(4,25,36))

parlist <- lapply(seq_along(forms), function(i) {
    parameterlist(n=1,
                  mt='TTT',
                  simple=FALSE,
                  ff=forms[i],
                  ct=CTs[i],
                  modelthreshold=100,
                  start=275,
                  varyingparameters=NA,
                  optimized=c('threshold'),
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
                  ubounds=c(2000),
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

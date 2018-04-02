library(plyr)
library(phenoclim)

datapath <- '../Results/walnutpaper/'
resultspath <- '../data/history/'
TTTpm <- readRDS(file.path(datapath, 'TTTplantmodelCV.RDS'))

cv <- names(TTTpm)

extractResults <- function(pm, cultivars, form) {
    require(plyr)
    require(phenoclim)
    
    p <- ldply(cultivars, function(v) {
        pv <- phenology(pm[[v]])
        pv$cultivar <- v
        pv
    })
    
    
    nms <- names(p) 
    basenames <- c('year','cultivar','event1','event2', 'length1')
    baseIDs <- which(nms %in% basenames)
    modIDs <- grep(form, nms)
    
    IDs <- c(baseIDs, modIDs)
    
    pfinal <- p[,IDs]
    
    return(pfinal)
}

xlin <- extractResults(TTTpm, cv, 'linear')
names(xlin)[6:7] <- c('daysAccum', 'length1fit')


write.csv(xlin, file.path(resultspath, 'walnutlinear.csv'), row.names = FALSE)

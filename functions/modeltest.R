source('functions/preTclean.R')

modeleval <- function(primaryid, secondaryids, weights=FALSE,
                      APItoken='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN', 
                      threshold=0.8, thresholdcolumn='minR2') {
    require(plyr)
    
    focal <- ghcnd(stationid=primaryid, token=APItoken)
    
    fdf <- ghcnd_reshape(focal)
    
    
    aux <- lapply(secondaryids, function(sid) {
        as.data.frame(ghcnd(stationid=sid, token=APItoken))
    })
    
    keepstation <- sapply(aux, function(tble) {
        #print(unique(tble$element))
        
        elements <- unique(tble[,'element'])
        if ( ('TMIN' %in% elements) & ('TMAX' %in% elements)) {
            TRUE
        } else {
            FALSE
        }
    })
    
   
    auxkeep <- aux[which(keepstation)]
    
    
    adf <- lapply(auxkeep, function(gdf) {
        ghcnd_reshape(gdf)
    })
    
    fdf <- switchMinMax(fdf)
    fdf <- formatDates(fdf)
    
    adf <- switchMinMax(adf)
    adf <- formatDates(adf)
    
    moddf <- lapply(adf, function(d) {
        overlap(fdf, d)
    })
    
    
    MinMods <- lapply(moddf, function(d) {
        lm(tmin ~ Stmin, data=d)
    })

    MaxMods <- lapply(moddf, function(d) {
        lm(tmax ~ Stmax, data=d)
    })
    
    result <- ldply(seq_along(MinMods), function(i) {
        data.frame(id=adf[[i]]$id[1],
                   obs=length(MinMods[[i]]$residuals),
                   minR2=summary(MinMods[[i]])$adj.r.squared,
                   maxR2=summary(MaxMods[[i]])$adj.r.squared)
    })   
   
    if (weights) {
        
        qualitymods <- which(result[,thresholdcolumn] > threshold)
        result <- result[qualitymods,]
        
        result$minWeights <- result$minR2/sum(result$minR2)
        result$maxWeights <- result$maxR2/sum(result$maxR2)
        
    }
    
    return(result)
    
}
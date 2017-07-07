source('functions/preTclean.R')

modeleval <- function(primaryid, secondaryids, 
                      APItoken='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN') {
    require(plyr)
    
    focal <- ghcnd(stationid=primaryid, token=APItoken)
    
    fdf <- ghcnd_reshape(focal)
    
    
    aux <- lapply(secondaryids, function(sid) {
        ghcnd(stationid=sid, token=APItoken)
    })
    
    keepstation <- sapply(aux, function(tble) {
        if ( ('TMIN' %in% tble$element) & ('TMAX' %in% tble$element)) {
            TRUE
        } else {
            FALSE
        }
    })
    
    auxkeep <- aux[keepstation]
    
    
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
        data.frame(id=secondaryids[i],
                   obs=length(MinMods[[i]]$residuals),
                   minR2=summary(MinMods[[i]])$adj.r.squared,
                   maxR2=summary(MaxMods[[i]])$adj.r.squared)
    })   
   
    return(result)
    
}
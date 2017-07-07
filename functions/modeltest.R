source('functions/preTclean.R')

modeltest <- function(primaryid, secondaryids, type, 
                      APItoken='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN') {
    require(plyr)
    
    focal <- ghcnd(stationid=primaryid, token=EH_APItoken)
    
    fdf <- ghcnd_reshape(focal)
    
    
    aux <- lapply(secondaryids, function(sid) {
        ghcnd(stationid=sid, token=EH_APItoken)
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
    adf <- switchMinMax(adf)
    
    
    moddf <- lapply(adf, function(d) {
        overlap(fdf, d)
    })
    
    if (type=='tmin') {
        mods <- lapply(moddf, function(d) {
            lm(tmin ~ Stmin, data=d)
        })
    } else if (type=='tmax') {
        mods <- lapply(moddf, function(d) {
            lm(tmax ~ Stmax, data=d)
        })
    } else {
        stop('type must be tmin or tmax')
    }
    
    result <- ldply(seq_along(mods), function(i) {
        m <- mods[[i]]
        data.frame(id=secondaryids[i],
                   obs=length(m$residuals),
                   r2=summary(m)$adj.r.squared)
    })   
   
    return(result)
    
}
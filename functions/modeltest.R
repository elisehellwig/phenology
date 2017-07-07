source('functions/preTclean.R')

modeltest <- function(primaryid, secondaryids, APItoken=) {
    
    focal <- ghcnd(stationid=primaryid, token=APItoken)
    
    fdf <- ghncd_reshape(focal)
    
    
    aux <- lapply(secondaryids, function(sid) {
        ghcnd(stationid=sid, token=APItoken)
    })
    
    adf <- lapply(aux, function(gdf) {
        ghcnd_reshape(gdf)
    })
    
    switchf <- which(fdf$tmax < fdf$tmin)
    switchfdf <- data.frame(tmin=fdf[switchf,'tmax'],
                           tmax=fdf[switchf, 'tmin'])
    fdf[switchf, 'tmin'] <- switchfdf$tmin
    fdf[switchf, 'tmax'] <- switchfdf$tmax
    
    adf
    
    
}
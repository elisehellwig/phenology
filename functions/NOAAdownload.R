locationDownload <- function(loc, yrs, nearby=NA, limit=50, r2=0.85, 
                             stations=40, radius=100, cores=4L,
                         ncdc_token='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN', 
                         stationvars=c('id','name','latitude',
                                       'longitude','distance'),
                         datavars=c('id','name','mindate','maxdate'),
                         tempvars=c('id','year','date','tmin','tmax')) {
    require(rnoaa)
    require(parallel)
    source('functions/modeltest.R')
    source('functions/tempclean.R')
    
    
    if (is.na(nearby)) {
        nearbylist <- meteo_nearby_stations(location, radius=radius)
        nearbynames <- names(nearbylist[[1]])
        nearby <- data.frame(nearbylist[[1]])
        names(nearby) <- nearbynames
        
        
    } else if (is.list(nearby) & class(nearby[[1]])[1]=='tbl_df') {
        nearbynames <- names(nearby[[1]])
        nearby <- data.frame(nearby[[1]])
        names(nearby) <- nearbynames
        
    } else if (is.data.frame(nearby)) {
        
        nearbynames <- names(nearby)
        missingvars <- setdiff(stationvars, nearbynames)
        
        if (length(missingvars)!=0) {
            stop(paste0('You are missing the following variable(s) from your nearby station data: ', paste(missingvars, collapse=', '), '.') )
        }
        
    } else {
        stop('nearby station data must be a dataframe or in the format from meteo_nearby_stations().')
    }
    
    nearbyevals <- modeleval(nearby$id[1], nearby$id[2:stations])
    
    stns <- ldply( nearbyevals$id, function(stn) {
        ncdc_stations(stationid = paste0('GHCND:',stn), limit=limit, 
                      token=ncdc_token)$data[,datavars]
    })
    
    stns$id <- sub("GHCND:", '', cs$id)
    
    AuxInfo <- metatemptable(stns, nearbyevals)
    AuxInfo <- AuxInfo[AuxInfo$minR2>0.75, ]
    AuxIDs <- AuxInfo$id
    
    
    stnIDs <- c(loc[1,'id'], AuxIDs)
    temp <- ghncd_download(stnIDs)
    
    dateNA <- which(is.na(temp$date))
    temp2 <- temp[-dateNA, tempvars]
    
    prime <- stnIDs[1]
    secnd<- stnIDs[-1]
    
    
    davisdaily <- tempclean(data=temp2, primary=prime, secondary=secnd,
                            years=yrs, dateform="%Y-%m-%d")
    
    
}
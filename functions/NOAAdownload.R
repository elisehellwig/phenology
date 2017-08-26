locationDownload <- function(loc, nearby=NA, limit=50, r2=0.85, stations=40,
                         radius=100, cores=4L,
                         token='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN', 
                         variables=c('id','name','latitude','longitude',
                                     'distance')) {
    require(rnoaa)
    require(parallel)
    source('functions/modeltest.R')
    
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
        missingvars <- setdiff(variables, nearbynames)
        
        if (length(missingvars)!=0) {
            stop(paste0('You are missing the following variable(s) from your nearby station data: ', paste(missingvars, collapse=', '), '.') )
        }
        
    } else {
        stop('nearby station data must be a dataframe or in the format from meteo_nearby_stations().')
    }
    
    nearbyevals <- 
    
    
    
    
}
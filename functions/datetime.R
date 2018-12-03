toPOSIX <- function(datetime) {
    
    dt <- as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%OS")
    return(dt)
}

convertToDT <- function(df, returnedVars=c('year','dt')) {
    
    
    varnames <- names(df) 
    reqnames <- c('year','day','hour')
    
    missingnames <- ifelse(reqnames %in% varnames, FALSE, TRUE)
    
    if (any(missingnames)) {
        stop(paste('The following columns are missing from your data frame:',
                   varnames[missingnames]))
    }
    
    
    if ('month' %in% varnames) {
        
        dtstring <- paste0(df$year, '-',
                           df$month, '-',
                           df$day, ' ',
                           sprintf("%02d", df$hour), ':00:00')
        
        df$dt <- toPOSIX(dtstring)
        
    } else {
        
        originDates <- as.Date(paste0(df$year, '-01-01'))
        df$date  <- as.Date(df$day, format="%j", origin=originDates)
        
        dtstring <- paste0(df$date, ' ', sprintf("%02d", df$hour), ':00:00')
        
        df$dt <- toPOSIX(dtstring)
    }
    
    
    return(df[,returnedVars])
    
}



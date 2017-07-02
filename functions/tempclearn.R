tempclean <- function(years, data, primary, secondary, hourly=FALSE,
                      dateform ='%Y%m%d', na_val=-9999) {
    #columns should be in the order location, date, temp/tmin, (tmax)
    require(lubridate)
    
    
    if (hourly) {
        names(data) <- c('loc', 'date', 'temp')
        data[data$temp==na_val, 'temp'] <- NA
        
    } else {
        names(data) <- c('loc', 'date','tmax', 'tmin')
        data[data$tmin==na_val, 'tmin'] <- NA
        data[data$tmax==na_val, 'tmax'] <- NA
        
    }
    
    
    data <- data[data$year %in% years, ]
    
    
    if (!is.Date(data$date)) {
        data$date <- as.Date(data$date, format=dateform)
    }
    
    data$year <- year(data$date)
    data$month <- month(data$date)
    
    pri <- data[data$loc==primary, ]
    
    
    if (length(secondary<1)) {
        stop('You must have at least one secondary location.')
    } else if (length(secondary)==1) {
        sec <- data[data$loc==secondary, ]
        
    } else {
        sec <- lapply(secondary, function(sloc) {
            data[data$loc==sname, ]
        })
    }
    
    
    
    
    
    
    
}
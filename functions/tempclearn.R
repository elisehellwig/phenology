tempclean <- function(years, data, primary, secondary, hourly=FALSE) {
    #columns should be in the order location, date, temp/tmin, (tmax)
    
    if (hourly) {
        names(data) <- c('loc', 'date', 'temp')
    } else {
        names(data) <- c('loc', 'date','tmax', 'tmin')
    }
    
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
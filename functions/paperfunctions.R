rumlgdd <- function(fdat, tdat, ttal, slen=TRUE) {
    
    years <- fdat[,'year']
    
    start <- fdat[,'flower']
    end <- start+ttal
    
    gddmin <- sapply(1:length(years), function(i) {
        temps <- tdat[tdat$year==years[i] & tdat$day %in% start[i]:end[i], 'tmin'] 
        mean(temps)
    })
    
    gddmax <- sapply(1:length(years), function(i) {
        temps <- tdat[tdat$year==years[i] & tdat$day %in% start[i]:end[i], 'tmax'] 
        mean(temps)
    })
    
    gddavg <- sapply(1:length(years), function(i) {
        temps <- tdat[tdat$year==years[i] & tdat$day %in% start[i]:end[i], 'tavg'] 
        mean(temps)
    })
    
    if (slen) {
        rgdd <- data.frame(tmin=gddmin, tmax=gddmax, tavg=gddavg, slen=fdat[,'slen'])
    } else {
        rgdd <- data.frame(tmin=gddmin, tmax=gddmax, tavg=gddavg)
    }
    
    
    return(rgdd)
    
}

mae <- function(fitted, observed) {
    return(mean(abs(fitted-observed)))
}

ia <- function(fitted, observed) {
    fmu <- mean(fitted)
    omu <- mean(observed)
    
    num <- sum((fitted-observed)^2)
    denom <- sum( (abs(fitted-fmu) + abs(observed-omu))^2 )
    
    ind <- 1 - num/denom
    
    return(ind)
}





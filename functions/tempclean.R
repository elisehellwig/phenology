
ghcnd_reshape <- function(df, vars=c('TMAX','TMIN'), valuename='VALUE') {
    
    valcols <- grep('VALUE', names(df))
    
    dfr <- cbind(df[,c('id', 'year','month','element')], df[,valcols]) 
    
    dfr <- dfr[dfr$element %in% vars, ]
    
    dfmelt <- melt(dfr, id.vars = c('id', 'year','month','element'), 
                   variable.name = 'day', value.name = 'temp')
    dfmelt$day <- as.character(dfmelt$day)
    
    dfmelt$day <- gsub(valuename, '', dfmelt$day)
    dfmelt$day <- as.integer(dfmelt$day)
    
    
    dfcast <- dcast(dfmelt, id + year + month + day ~ element,
                    value.var = 'temp')
    
    names(dfcast)[5:6] <- c('tmin','tmax')
    
    return(dfcast)
    
}


ghncd_download <- function(stations, 
                           APItoken='LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN') {
    require(plyr)
    require(lubridate)
    
    stnids <- paste0('GHCND:', stations)
    
    meta <- ldply(stnids, function(id) {
        voi <- c('longitude','latitude','elevation')
        ncdc_stations(stationid=id, token=APItoken)$data[,voi]
        })
    
    meta$id <- stations
    
    
    dailyT <- lapply(stations, function(id) {
        ghcnd(stationid=id, token=APItoken)
    })
    
    dailyt <- ldply(dailyT, function(dt) {
        ghcnd_reshape(dt)
    })
    
    
    dt <- merge(dailyt, meta)
    
    dt$date <- as.Date(paste(dt$year, dt$month, dt$day, sep='-'))
    dt$jd <- yday(dt$date)
    
    dt$tmax <- dt$tmax/10
    dt$tmin <- dt$tmin/10
    
    return(dt)
}







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
    
    primaryNAs <- union(which(is.na(pri$tmin)), which(is.na(pri$tmax)))
    
    secondaryNAs <- lapply()
    
    
    
    
    
}
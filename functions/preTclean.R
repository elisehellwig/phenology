
ghcnd_reshape <- function(df, vars=c('TMAX','TMIN'), valuename='VALUE') {
    require(reshape2)
    
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
    require(rnoaa)
    
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
    
    dt$tmax <- dt$tmax/10
    dt$tmin <- dt$tmin/10
    
    return(dt)
}



overlap <- function(prd, srd) {
    odate <- as.Date(intersect(srd[,'date'], prd[,'date']), origin="1970-01-01")
    
    x <- cbind(prd[prd$date %in% odate, ], 
               srd[srd$date %in% odate, c('tmin','tmax')])
    
    names(x) <- c(names(prd), paste0("S", c('tmin','tmax')))
    
    return(x)
}



modelMissing <- function(pr, pmiss, sr, smiss) {
    
    olp <- overlap(pr, sr)
    
    tminmod <- lm(tmin ~ Stmin, data=olp)
    tmaxmod <- lm(tmax ~ Stmax, data=olp)
    
    sfound <- as.Date(setdiff(pmiss, smiss), origin='1970-01-01')
    
    ndat <- sr[sr$date %in% sfound, c('date','tmin','tmax')]
    names(ndat)[2:3] <- c('Stmin','Stmax')
    
    
    ndat$Ptmin <- predict(tminmod, newdata=ndat)
    ndat$Ptmax <- predict(tmaxmod, newdata=ndat)
    
    return(ndat)
    
}

splitcols <- function(df, name, extracols='date') {
    
    colnums <- grep(name, names(df))
    colvars <- c(extracols, names(df)[colnums])
    
    splitdf <- df[, colvars]
    
    return(splitdf)
    
}



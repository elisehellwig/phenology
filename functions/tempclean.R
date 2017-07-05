
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






tempclean <- function(data, primary, secondary, years=NA, hourly=FALSE,
                      dateform ='%Y%m%d', na_val=-9999) {
    #columns should be in the order location, date, temp/tmin, (tmax)
    require(lubridate)
    #requires you to load preprocess functions before running this specifically
        #for the missingDates function
    
    if (hourly) {
        names(data) <- c('loc', 'date', 'temp')
        data[data$temp==na_val, 'temp'] <- NA
        
    } else {
        names(data) <- c('loc', 'year' ,'date','tmin', 'tmax')
        #data[data$tmin==na_val, 'tmin'] <- NA
        #data[data$tmax==na_val, 'tmax'] <- NA
        
    }
    
    
    if (!is.na(years[1])) {
        data <- data[data$year %in% years, ]
    }
    
    
    if (!is.Date(data$date)) {
        data$date <- as.Date(data$date, format=dateform)
    }
    
    #data$year <- year(data$date)
    #data$month <- month(data$date)
    
    pri <- data[data$loc==primary, ]
    
    
    if (length(secondary<1)) {
        stop('You must have at least one secondary location.')
        
    }  else {
        sec <- lapply(secondary, function(sname) {
            data[data$loc==sname, ]
        })
    }
    
    primaryNAs <- union(which(is.na(pri$tmin)), which(is.na(pri$tmax)))
    prir <- pri[-primaryNAs, ]
    
    primissing <- as.Date(missingDates(prir), origin="1970-01-01")
    
    secr <- lapply(sec, function(d) {
        nas <- union(which(is.na(d[,'tmin'])), which(is.na(d[,'tmax'])))
        d[-nas,]
    })

    
    secmissing <- lapply(secr, function(dr) {
        as.Date(missingDates(dr), origin="1970-01-01")
    })

    
    mm <- lapply(seq_along(secr), function(i) {
        modelMissing(prir, primissing, secr[[i]], secmissing[[i]])
    })
    
    for (i in seq_along(mm)) {
        names(mm[[i]])[-1] <- paste0(names(mm[[i]])[-1], i)
        
    }
    
    mdat <- mm[[1]]
    for (i in 1:(length(mm)-1) ) {
        print(i+1)
        mdat <- merge(mdat,mm[[i+1]], by='date', all=TRUE)
    }
    
    Stmincols <- c(1, grep('Stmin', names(mdat)))
    Stmaxcols <- c(1, grep('Stmax', names(mdat)))
    
    
    
}
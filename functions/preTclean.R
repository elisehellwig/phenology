 
switchMinMax <- function(x, vars=c('tmin','tmax')) {
    #note the first one in the vars vector must be the smaller one
    
    if (is.data.frame(x)) {
        
        switchrows <- which(x[,vars[1]] > x[,vars[2]])
        switchdf <- data.frame(a=x[switchrows, vars[2]],
                              b=x[switchrows, vars[1]])
        x[switchrows, vars[1]] <- switchdf$a
        x[switchrows, vars[2]] <- switchdf$b
        
    } else if (is.list(x)) {
        
        xclass <- sapply(x, function(v) class(v))
        testclass <- rep('data.frame', length(x))
        
        
        if (identical(xclass, testclass)) {
            
            for (i in seq_along(x)) {
                xdf <- x[[i]]
                switchrows <- which(xdf[,vars[1]] > xdf[,vars[2]])
                switchdf <- data.frame(a=xdf[switchrows, vars[2]],
                                      b=xdf[switchrows, vars[1]])
                xdf[switchrows, vars[1]] <- switchdf$a
                xdf[switchrows, vars[2]] <- switchdf$b
                
                x[[i]] <- xdf
            } 
            
            
        } else {
            stop('x must be a list of data.frames')
        }
        
    } else {
        stop('x must be a list or a data.frame')
    }
    
    return(x)
    
}


formatDates <- function(x, yearname='year', monthname='month', dayname='day') {
    
    
    
    if (is.data.frame(x)) {
        
        x$date <- as.Date(paste(x[,yearname], x[,monthname], x[,dayname],
                                sep='-'))
        
        NArows <- which(is.na(x$date))
        x <- x[-NArows,]
        
        
        
    } else if (is.list(x)) {
        
        xclass <- sapply(x, function(v) class(v))
        testclass <- rep('data.frame', length(x))
        
        
        if (identical(xclass, testclass)) {
            
            x <- lapply(x, function(d) {
                d$date <- as.Date(paste(d[,yearname], d[,monthname], 
                                        d[,dayname], sep='-'))
                NArows <- which(is.na(d$date))
                d <- d[-NArows,]
                
                d
            })
            
        } else {
            stop('x must be a list of data.frames')
        }
        
    } else {
        stop('x must be a list or a data.frame')
    }
    
    return(x)
    
}


ghcnd_reshape <- function(df, vars=c('TMIN','TMAX'), valuename='VALUE') {
    require(reshape2)
    
    #print(head(df))
    
    valcols <- grep('VALUE', names(df))
    
    dfr <- cbind(df[,c('id', 'year','month','element')], df[,valcols]) 
    
    dfr <- dfr[dfr$element %in% vars, ]
    
    dfmelt <- melt(dfr, id.vars = c('id', 'year','month','element'), 
                   variable.name = 'day', value.name = 'temp')
    dfmelt$day <- as.character(dfmelt$day)
    
    dfmelt$day <- gsub(valuename, '', dfmelt$day)
    dfmelt$day <- as.integer(dfmelt$day)
    
   # print(head(dfmelt))
    dfcast <- dcast(dfmelt, id + year + month + day ~ element,
                    value.var = 'temp')
    
    names(dfcast)[5:6] <- c('tmax','tmin')
    
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
    
    dailyt <- switchMinMax(dailyt)
    
    
    
    dt <- merge(dailyt, meta)
    
    dt$date <- as.Date(paste(dt$year, dt$month, dt$day, sep='-'))
    
    dt$tmax <- dt$tmax/10
    dt$tmin <- dt$tmin/10
    
    
    
    return(dt)
}



overlap <- function(prd, srd) {
    
    vnames <- names(prd) 
    if ('id' %in% vnames) {
        voi <- c('id','tmin','tmax')
    } else if ('loc' %in% vnames) {
        voi <- c('loc','tmin','tmax')
    } else {
        stop('The location variable name must be id or loc.')
    }

    odate <- as.Date(intersect(srd[,'date'], prd[,'date']), origin="1970-01-01")
    
    x <- cbind(prd[prd$date %in% odate, ], 
               srd[srd$date %in% odate, voi])
    
    names(x) <- c(names(prd), paste0("S", voi))
    
    return(x)
}



modelMissing <- function(pr, pmiss, sr, smiss) {
    
    #print(str(pr))
    #print(str(sr))
    
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


metatemptable <- function(x1, x2, mergecol='id', roundcols=c('minR2','maxR2'), 
                          nmecol='name', finalcols=c('id','name','mindate',
                                                     'minR2','maxR2','range')) {
    
    x <- merge(x1, x2, by=mergecol)
    datecols <- grep('date', names(x), ignore.case = TRUE)
    
    for (i in datecols) {
        x[,i] <- as.Date(x[,i])
    }
    
    for (rc in roundcols) {
        x[,rc] <- round(x[,rc], 3)
    }
    
    x[,nmecol] <- sapply( x[,nmecol], function(str) {
        strsplit(str, ',')[[1]][1]
    })
    
    x$range <- as.integer(x[,'maxdate'] - x[,'mindate'])
    x$range <- ceiling(x$range/365)
    
    return(x[,finalcols])
}
 










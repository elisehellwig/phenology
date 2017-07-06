source('functions/preTclean.R')
source('functions/preprocessfunctions.R')



tempclean <- function(data, primary, secondary, years=NA, hourly=FALSE,
                      dateform ='%Y%m%d', na_val=-9999) {
    #columns should be in the order location, date, temp/tmin, (tmax)
    require(lubridate)
    require(plyr)
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
        data$date <- as.Date(data$date, format=dateform, origin='1970-01-01')
    }
    
    #data$year <- year(data$date)
    #data$month <- month(data$date)
    
    pri <- data[data$loc==primary, ]

    if (length(secondary)<1) {
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

    
    mm0 <- lapply(seq_along(secr), function(i) {
        modelMissing(prir, primissing, secr[[i]], secmissing[[i]])
    })
    
    
    mm <- lapply(seq_along(mm0), function(i) {
        names(mm0[[i]])[-1] <- paste0(names(mm0[[i]])[-1], i)
        mm0[[i]]
    })
    
    mdat <- mm[[1]]
    if (length(mm)>1) {
        
        for (i in 1:(length(mm)-1) ) {
            mdat <- merge(mdat,mm[[i+1]], by='date', all=TRUE)
        } 
        
    } 
    
    
    stmin <- splitcols(mdat, 'Stmin', 'date')
    stmin$avg <- apply(stmin[,-1], 1, mean, na.rm=TRUE)
    
    stmax <- splitcols(mdat, 'Stmax','date')
    stmax$avg <- apply(stmax[,-1], 1, mean, na.rm=TRUE)
    
    rdata <- data.frame(loc=primary,
                        date=as.Date(stmin[,'date'], origin='1970-01-01'),
                        year=year(as.Date(stmin[,'date'], origin='1970-01-01')),
                        tmin=stmin$avg,
                        tmax=stmax$avg)
    
    
    prir2 <- rbind(prir, rdata)
    
    primissing2 <- missingDates(prir2)
    
    if (isTRUE(primissing2)) {
        return(prir2)
        
    } else {
        
        warning('There is still missing data. More secondary locations are required to fill in the missing temperatures.')
        
        misslist <- list(prir2, as.Date(primissing2, origin='1970-01-01'))
        
        return(misslist)
        
    }
}
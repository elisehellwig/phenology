

avgdate <- function(datev) {
    #averages 2 dates together
    require(lubridate)
    
    d <- as.integer(datev)
    ad <- ceiling(sum(d, na.rm=TRUE)/sum(!is.na(datev)))
    aD <- as.Date(ad, origin="1970-01-01")
    return(aD)
}



missingDays <- function(x, days, limits) {
    
    year <- unique(x$year)
    
    if(length(year)>1) {
        stop('Only one year can be checked at a time.')
    }
    
    if (is.na(limits)) {
        
        if (is.leapyear(year)) {
            days <- c(days, 366)
        }
        
        observations <- x[,'day']
        
    } else {
        observations <- x[x$day %in% limits[1]:limits[2], 'day' ]
        
    }
    
    return(setdiff(days, observations))
    
}


timeSeriesCheck <- function(x, years, limits=NA, hours=FALSE) {
    require(lubridate)
    
    if (is.na(limits)) {
        alldays <- 1:365
        
    } else {
        alldays <- limits[1]:limits[2]
    }
    
    missing <- lapply(years, function(y) {
        missingDays(x[x$year==y,], alldays, limits)
    })
    
    names(missing) <- years
    
    noneMissing <- sapply(missing, function(v) (length(v)==0))
    
    if (all(noneMissing)) {
        return(TRUE)
    } else {
        return(missing)
    }
    
    
}

missingDates <- function(df, limits=NA, hourly=FALSE) {
    require(dplyr)
    
    
    if (!('date' %in% names(df))) {
        stop("There must be a column called 'date' in your data.frame. ")
        
    } else if (class(df[1,'date'])!='Date'){
            stop("The input data frame must have a column called date that has the class 'Date'. ")
        
         }
    
    
    if (is.na(limits[1])) {
        start <- min(df[,'date'], na.rm = TRUE)
        end <- max(df[,'date'], na.rm = TRUE)
    } else if (class(limits[1])=='Date' & length(limits)==2) {
        start <- limits[1]
        end <- limits[2]
        
    } else {
        stop("The argument limits must be NA or a vector of length 2 and the class Date")
    }
    
    
    alldates <- seq(start, end, by='days')
    
    if (hourly) {
        
        alldatetimes <- as.data.frame(expand.grid(alldates, 1:24))
        missing <- anti_join(alldatetimes, df[,c('date','hour')])
        
        
    } else {
        missing <- setdiff(alldates, df[,'date'])
        
    }
    
    if (hourly) {
        if (dim(missing)[1]==0) {
            return(TRUE)
        } else {
            return(missing)
        }

    } else {
        
        if (length(missing)==0) {
         return(TRUE) 
        
        } else {
            return(missing)
        }
    }

}





daylength <- function(jul, lat=38.5, ret='') {
    #from extractTemp.py: Cesaraccio et al (2001) "An improved model for determining degree-day values..."
    #jul is the julian date
    #lat is the latitude of the location
    #all calculations are in radians
    #dayangle is the day angle
    #dec is the sun's declination
    #hourangle is the hour angle at sunset
    
    dayangle <- 2*pi*(jul-1)/365
    dec <- 0.006918 - 0.399912*cos(dayangle) + 0.070257*sin(dayangle) - 0.006758*cos(2*dayangle) + 0.000907*sin(2*dayangle) - 0.002697*cos(3*dayangle) + 0.001480*sin(3*dayangle)
    hourangle <- acos(-tan(lat*pi/180)*tan(dec))
    
    daylen <- 24*hourangle/pi
    sunrise <- 12 - daylen/2 - dec
    sunset <- 12 + daylen/2 - dec
    
    ss <- c(daylen, sunrise, sunset)
    return(ss)
    
}


reform <- function(df, cultivars) {
    
    dft <- as.data.frame(t(df))[-1,]
    names(dft) <- c('year', cultivars)
    
    vnames <- names(df)[-1]
    
    namesplit <- strsplit(vnames, '[.]') 
    
    dft$event <- sapply(seq_along(namesplit), function(i) namesplit[[i]][1])
    
    dfmelt <- melt(dft, id.vars=c('year','event'), measure.vars=cultivars,
                   variable.name = 'cultivar', value.name = 'date')
    
    return(dfmelt)
    
} 







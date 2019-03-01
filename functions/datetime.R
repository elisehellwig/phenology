toPOSIX <- function(datetime) {
    #converting a date time string to a POSIXct class object
    # datetime - character, a vector of date time characters to be converted
    
    dt <- as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%OS")
    return(dt)
}

convertToDT <- function(df, returnedVars=c('year','dt')) {
    #create a POSIXct data out of year, day, and hour data.frame
    #df - data.frame, where the date/time data is stored, requires the
        # following variables: year, day, hour
    #returnedVars - character, the variables you want in the returned data.frame
    
    
    varnames <- names(df) #variables in the data.frame
    reqnames <- c('year','day','hour') #variables required for the function
    
    #are any of the required variables missing?
    missingnames <- ifelse(reqnames %in% varnames, FALSE, TRUE)
    
    #if they are return an error message specifying which ones
    if (any(missingnames)) {
        stop(paste('The following columns are missing from your data frame:',
                   varnames[missingnames]))
    }
    
    
    if ('month' %in% varnames) { #if you have year-month-day data:
        
        #create a string that can be converted to the POSIXct class
        dtstring <- paste0(df$year, '-',
                           df$month, '-',
                           df$day, ' ',
                           sprintf("%02d", df$hour), ':00:00')
        
        #convert string vector to POSIXct
        df$dt <- toPOSIX(dtstring)
        
    } else {
        
        #create a vector of first of the year dates
        originDates <- as.Date(paste0(df$year, '-01-01'))
        
        #create a Date using the (julian) day variable 
        df$date  <- as.Date(df$day, format="%j", origin=originDates)
        
        #convert that Date object into a string with time tacked on the end
        dtstring <- paste0(df$date, ' ', sprintf("%02d", df$hour), ':00:00')
        
        #convert datetime string to POSIXct object
        df$dt <- toPOSIX(dtstring)
    }
    
    #return dataframe with selected variable
    return(df[,returnedVars])
    
}

POSIXtoDate <- function(vec) {
    #convert a POSIXct object to a date
    # vec - POSIXct, date time vector
    
    chrvec <- as.character(vec) #convert to character
    subvec <- substr(chrvec, 1, 10) #select only the first 10 characters
    
    #convert the 10 characters to a date
    datevec <- as.Date(subvec, format="%Y-%m-%d")
    
    return(datevec)
}





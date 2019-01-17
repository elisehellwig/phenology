calibrateInterp <- function(data, name) {
    require(Interpol.T)
    
    df <- data.frame(ID=name,
                     date=gsub('-', '/', as.character(date(data$date))),
                     hour=hour(data$date),
                     temp=data$temp)
    
    
    cpar <- par_calibration(df, band_min = 3:9, band_max = 12:20, 
                            band_suns = 13:21)
    
    shape <- shape_calibration(df, cal_times_list = cpar)
    
    return(list(cpar, shape))
    
}

InterpTemp <- function(daily, hourly, name, start, end) {
    
    cal <- calibrateInterp(hourly, name)
    
    print(1)
    cpar <- cal[[1]]
    cshape <- cal[[2]]
    
    print(2)
    dmin <- data.frame(year = year(daily$date),
                       month = month(daily$date),
                       day = day(daily$date),
                       temp = daily$tmin)
    
    dmax <- data.frame(year = year(daily$date),
                       month = month(daily$date),
                       day = day(daily$date),
                       temp = daily$tmax)
    
    
    names(dmin)[4] <- name
    names(dmax)[4] <- name
    
    hourlyInterp<- Th_int_series(cal_times=cpar, 
                                 cal_shape = cshape,
                                 TMIN=dmin, 
                                 TMAX=dmax, 
                                 start_year=start, 
                                 end_year=end,
                                 active_IDs = name)
    print(4)
    
    tempTS <- hourlyInterp$Date
    tempTS$temp <- hourlyInterp[[name]]
    
    print(5)
    tempdt <- convertToDT(tempTS, c('dt','temp'))
    tempdt$date <- POSIXtoDate(tempdt$dt)
    
    return(tempdt)
}


mergeDailyHourly <- function(daily, hourly, interpolated) {
    
    daily$date <- as.Date(daily$date)
    
    dt <- merge(interpolated, daily, by='date')
    dtfinal <- dt[,c('dt','year','day','temp','tmin','tmax')]
    dtfinal$hour <- hour(dtfinal$dt)
    dtfinal <- dtfinal[order(dtfinal$dt), ]
    
    
    hourly$date <- toPOSIX(hourly$date)
    
    #print(length(which(is.na(dtfinal$dt))))
    dtfinal$temp <- sapply(1:nrow(dtfinal), function(i) {
        dtdate <- dtfinal[i, 'dt']
        
        #print(dtdate)
        #print(head(hourly$date))
        if (dtdate %in% hourly$date) {
            if (!is.na(hourly[which(hourly$date==dtdate),'temp'])) {
                hourly[which(hourly$date==dtdate),'temp']
            } else {
                dtfinal[i, 'temp']
            }
            
        } else {
            dtfinal[i, 'temp']
            
        }
    })
    
    return(dtfinal)
    
}


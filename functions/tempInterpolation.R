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
    
    cpar <- cal[[1]]
    cshape <- cal[[2]]
    
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
                                 TMIN=dtmin, 
                                 TMAX=dtmax, 
                                 start_year=start, 
                                 end_year=end,
                                 active_IDs = name)
    
    
    tempTS <- hourlyInterp$Date
    tempTS$temp <- hourlyInterp$Davis
    #write.csv(dh, file.path(datapath, 'clean/interpolatedDavis.csv'),
    # row.names = FALSE)
    
    tempdt <- convertToDT(tempTS, c('dt','temp'))
    tempdt$date <- POSIXtoDate(tempdt$dt)
    
    return(tempdt)
}


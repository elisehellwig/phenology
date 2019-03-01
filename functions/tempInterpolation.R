calibrateInterp <- function(data, name) {
    #this function calibrates the shape of the hourly temperature interpolation
        #it is called by InterpTemp()
    # data - data.frame, temperature data should have columns: date, temp
    # name - character, name of location of the temperature interpolation
    require(Interpol.T)
    
    #creating input data.frame for calibration functions
    df <- data.frame(ID=name,
                     date=gsub('-', '/', as.character(date(data$date))),
                     hour=hour(data$date),
                     temp=data$temp)
    
    #calibrating parameters, min temp 3-9, max temp 12-20, sunset 13-21
    cpar <- par_calibration(df, band_min = 3:9, band_max = 12:20, 
                            band_suns = 13:21)
    
    #calibrating the shape of the interploation
    shape <- shape_calibration(df, cal_times_list = cpar)
    
    return(list(cpar, shape))
    
}


InterpTemp <- function(daily, hourly, name, start, end) {
    #Interpolates hourly temperatures from daily temperatures using a model 
        #calibrated based on hourly temperatures for the given location. You
        #must source functions/datetime.R before running this function.
    # daily - data.frame, contains daily temperature data with columns: date,
        # tmin, tmax. date column should be of a Date or Datetime class
    # hourly - data.frame, contains hourly temperature data
    # name - character, name of location
    # start - numeric, year to start the interpolation
    # end - numeric, year to end the interpolation
    
    #calibrating the shape of the interpolation
    cal <- calibrateInterp(hourly, name)
    
    print(1)
    cpar <- cal[[1]] #calibration parameters
    cshape <- cal[[2]] #calibration shape
    
    print(2)
    #daily minimum temperature data frame
    dmin <- data.frame(year = year(daily$date),
                       month = month(daily$date),
                       day = day(daily$date),
                       temp = daily$tmin)
    #daily maximum temperature data frame
    dmax <- data.frame(year = year(daily$date),
                       month = month(daily$date),
                       day = day(daily$date),
                       temp = daily$tmax)
    
    
    #renaming the temp variable with the name of the location
    names(dmin)[4] <- name
    names(dmax)[4] <- name
    
    #running the hourly interpolation
    hourlyInterp<- Th_int_series(cal_times=cpar, 
                                 cal_shape = cshape,
                                 TMIN=dmin, 
                                 TMAX=dmax, 
                                 start_year=start, 
                                 end_year=end,
                                 active_IDs = name)
    print(4)
    
    #creating a new 
    tempTS <- hourlyInterp$Date #extracting the date
    tempTS$temp <- hourlyInterp[[name]] #extracting interpolated temperatures
    
    print(5)
    tempdt <- convertToDT(tempTS, c('dt','temp')) #converting date to POSIXct
    tempdt$date <- POSIXtoDate(tempdt$dt) #converting POSIXct to date
    
    return(tempdt)
}


mergeDailyHourly <- function(daily, hourly, interpolated) {
    #Merges daily and hourly data.frames so that there is only one dataframe to
        #load when running plantmodel() or flowermodel()
    # daily - data.frame, contains daily temp data
    
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


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
        #load when running plantmodel() or flowermodel(). Note: dt is a POSIXct
        #column
    # daily - data.frame, contains daily temp data with columns: dt, date, year,
        # day, tmin, tmax
    # hourly - data.frame, contains original hourly temp data with columns: dt,
        # date, year, day, temp
    # interpolated - data.frame, contains interpolated hourly temp data with 
        # columns: dt, date, year, day, temp
    
    #converting date (string) column to Date column
    daily$date <- as.Date(daily$date)
    
    #merging interpolated data with daily data
    dt <- merge(interpolated, daily, by='date')
    
    #selecting only the columns we want
    dtfinal <- dt[,c('dt','year','day','temp','tmin','tmax')]
    dtfinal$hour <- hour(dtfinal$dt) #creating hour variable
    dtfinal <- dtfinal[order(dtfinal$dt), ] #ordering rows by dt
    
    
    hourly$date <- toPOSIX(hourly$date) #creating POSIXct out of date column
    
    #print(length(which(is.na(dtfinal$dt))))
    #inserting original hourly data back into the data frame where we have it
    dtfinal$temp <- sapply(1:nrow(dtfinal), function(i) {
        dtdate <- dtfinal[i, 'dt'] #select a single row of data.frame
        
        #print(dtdate)
        #print(head(hourly$date))
        if (dtdate %in% hourly$date) { #if it exists in hourly
            if (!is.na(hourly[which(hourly$date==dtdate),'temp'])) {#and is not
                                                                    #NA
                hourly[which(hourly$date==dtdate),'temp'] #insert that data
            } else {
                dtfinal[i, 'temp'] #otherwise use the interpolated data
            }
            
        } else {
            dtfinal[i, 'temp']#otherwise use the interpolated data
            
        }
    })
    
    return(dtfinal)
    
}


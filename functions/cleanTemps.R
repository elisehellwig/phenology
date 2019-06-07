importCIMIS <- function(path) {
    #imports hourly temperature data downloaded from the CIMIS website
    #variables downloaded should include:
        # Station name, date, hour, julian day, Air temp, and quality control
    #path is the file path of the folder that contains all the cimis files
    #important this folder should contain ONLY cimis temperature data.
    
    
    #get a list of all the files to be imported
    filenames <- list.files(path, pattern='.csv', full.names = TRUE)
    
    
    #import the data from each of the files and select only vars of interest
    cimis <- plyr::ldply(filenames, function(fn) {
        read.csv(fn, skipNul = TRUE)[,c('Stn.Name','Date','Hour..PST.',
                                        'Jul','Air.Temp..C.','qc')]
    }) 
    
    #rename variables
    names(cimis) <- c('name','date','hour','day','temp','qc')
    
    cimis$hour <- cimis$hour/100 #convert hour to single digits
    
    #create string for POSIX conversion
    timestring <- paste(cimis$date, 
                        paste0(sprintf('%02d', cimis$hour-1), ":00:00"))
    
    #convert string to POSIX date-time class
    cimis$date <- as.POSIXct(timestring, format="%m/%d/%Y %H:%M:%OS")
    
    return(cimis)
    
}



fillinTemps <- function(df, variable, predictors, response, predname='loc',
                        printR2=TRUE) {
    #This function takes temperature data from secondary stations and uses it 
        #to fill in missing data from primary stations. A linear regression is
        #used to convert temperature from a secondary station to temperature at
        #a primary station.
    # df - the dataframe with all the temperature data
    # variable - the variable to fill in (ex. tmin, tmax, temp)
    # predictors - the names of the stations used to fill in data, secondary
        # stations
    # response - the name of the station that needs data filled in, primary
        # station
    # predname - the name of the column which contains the station identifers
    # printR2 - should the r2 value for each model be printed?
    
   require(reshape2)
    
    #print(1) 
    df <- unique(df) #removing any duplicate rows
    
    #renaming the variable with the station identifiers
    names(df)[which(names(df)==predname)] <- 'loc'
    
    #selecting only the stations necessary for the model
    df <- df[df$loc %in% c(predictors, response), ]
    
    #creating a formula to use for converting the data to wide data 
    fmla <- as.formula('date ~ loc') 
    
    #converting the data to wide data so each station has its own column
    wide <- dcast(df, fmla, value.var = variable)
    #print(head(wide))
    
    
    
    #print(1.1)
    wc <- wide[complete.cases(wide),] #removing rows with missing data


   # print(head(wc))
    #creating equations for the linear regressions
    eqns <- sapply(predictors, function(v) {
        paste(response, '~', v)
    })
    
   # print(1.2)
    
    #running the linear regressions
    modlist <- lapply(eqns, function(equation) {
        #print(equation)
        lm(as.formula(equation), data=wc)
    })    
    
    #printing R2 values if specified
    if (printR2) {
        lapply(modlist, function(mod) print(summary(mod)$adj.r.squared))
    }
    
    #print(2)
    #assessing which rows have missing values for the primary station
    missingResponse <- which(is.na(wide[,response]))
    
    #creating a data frame with all of the data points where the primary
        #station is missing data
    mr <- wide[missingResponse,] 
    
    #idenifying which of the missing data points from the primary station have 
        #have data in one of the secondary stations
    pres <- lapply(predictors, function(p) {
        which(!is.na(mr[,p]))
    })
    
    #print(3)
    #using the linear models to predict temperatures for the primary station
    predictions <- lapply(seq_along(predictors), function(i) {
        predict(modlist[[i]], mr[pres[[i]],])
    })
    
    #a matrix used to put the newly predicted data
    missingMat <- matrix(NA, nrow=nrow(mr), ncol=length(predictors))
    
    #putting the predicted temperatures in a matrix so we can average them
    for (i in 1:length(predictors)) {
        missingMat[pres[[i]], i] <- unname(predictions[[i]])
    }

    #print(4)
    #averaging the temperature predictions for each date
    meanVals <- apply(missingMat, 1, mean, na.rm=TRUE)
    
    #print(5)
    #creating new data frame with just the primary station's data
    newdf <- wide[,c('date',response)]
    
    #Replacing missing data with the predicted temps for the primary station
    newdf[missingResponse, response] <- unname(round(meanVals))
    
    #print(6)
    #renaming the variable 
    names(newdf)[2] <- variable
    
    #removing anything that is not a number
    newdf[which(!is.numeric(newdf[,variable])), variable] <- NA
    
    return(newdf)
    
}


FtoC <- function(v) {
    #converts Fahrenheit to Celcius
    # v - vector of temps in Fahrenheit
    cv <- (v-32)*5/9
    return(cv)
}


AverageTemps <- function(temps, variable, monthly=TRUE, datename='dt', 
                         placename='loc', namelength=5) {
    #Calculates monthly and annual temp averages
    # temps - data.frame, the temperature data
    # variable - character, the variable to average (ex. tmin, tmax)
    # monthly - logical, should monthly averages be calculated (alternate is
        # annual)
    # datename - character, name of the column specifying the date
    # placename - character, name of the column specifying the location 
    # namelength - numeric, number of characters used to identify a station
    
    if (monthly) {
        #creating string ids unique to each location, year, and month in the 
            #dataset
        stringID <- paste0(substr(temps[,placename], 1, namelength), 
                           substr(temps[,datename], 1, 7))
    } else {
        #creating strings unique to each location and year in the dataset
        stringID <- paste0(substr(temps[,placename], 1, namelength), 
                           substr(temps[,datename], 1, 4))
    }
    
    
    #calculating the monthly/annual averages
    avgs <- tapply(temps[,variable], stringID, mean)
    
    #storing those averages in a dataframe
    mtemps <- data.frame(string=names(avgs),
                         temp=unname(avgs))
    
    #extracting the year from the ID string
    mtemps$year <- substr(mtemps$string, namelength+1, namelength+4)
    
    if (monthly) {
        #extracting the month from the ID string
        mtemps$month <- as.numeric(substr(mtemps$string, namelength+6,
                                          namelength+7))
        
    }

    #extracting the location from the id string
    mtemps$loc <- substr(mtemps$string, 1, namelength)

    #renaming the temperature variable
    names(mtemps)[2] <- variable
    
    return(mtemps)
    
    
}

backfillTemps <- function(lat, dt, tmin, tmax) {
    #This function takes daily data and uses a simple interpolation method to 
        #create hourly data.
    #It is not super accurate but I am only using it to fill in hourly data
        #where I cannot find fillin data within 100 miles of the station.
    
    dateOnly <- as.Date(dt)
    
    hrlytemp <- round(diTemp(lat, dateOnly, tmin, tmax), 1)
    
    datevec <- seq(dt, by='hour', length.out = 24)
    
    tempdf <- data.frame(date=seq(dt, by='hour', length.out = 24),
                         temp=hrlytemp)
    
    return(tempdf)
    
}

seqTemp <- function(df, datetime, missinglength=1, varname='temp', 
                     dtname='date') {
    
    if (!is.POSIXct(datetime)) {
        datetime <- toPOSIX(datetime)
    }
    
    dateends <- c(datetime - hours(1), datetime + hours(missinglength))
    
    #print(dateends)
    
    if (missinglength==1) {
        
        avgtemp <- mean(df[which(df[,dtname] %in% dateends), 'temp'])
        missingID <- which(df[,dtname]==datetime)
        df[missingID, varname] <- avgtemp
        
        
    } else if (missinglength>1) {
        
        tempends <- c(df[which(df[,dtname]==dateends[1]), varname],
                      df[which(df[,dtname]==dateends[2]), varname])
        
        tempseq <- seq(tempends[1], tempends[2], length.out = missinglength)
        
        dateseq <- seq(datetime, datetime+hours(missinglength-1), by='hours')
        
        for (i in 1:missinglength) {
            
            df[which(df[,dtname]==dateseq[i]), 'temp'] <- tempseq[i]
            
        }
        
    } else {
        stop("missinglength must be >= 1.")
    }
   
    return(df)    

}

extractMinMax <- function(df, date, datename, tempname='temp', avg=TRUE) {
    
    if (length(date)==1) {
        temps <- df[which(df[,datename]==date), tempname]
        mmm <- t(as.matrix(c(min(temps, na.rm=TRUE), max(temps, na.rm = TRUE))))
        
    } else if (length(date)==2) {
        temps1 <- df[which(df[,datename]==date[1]), tempname]
        temps2 <- df[which(df[,datename]==date[2]), tempname]
        
        mm1 <- c(min(temps1, na.rm=TRUE), max(temps1, na.rm=TRUE))
        mm2 <- c(min(temps2, na.rm=TRUE), max(temps2, na.rm=TRUE))
        
        mm <- rbind(mm1, mm2)
        
        if (avg) {
            mmm <- t(as.matrix(apply(mm, 2, mean, na.rm=TRUE)))
        }
       
        
    } else {
        stop("Length of date must be 1 or 2.")
        
    }
    
    mmdf <- as.data.frame(mmm)
    mmdf$date <- mean(date)
    
    names(mmdf) <- c('tmin','tmax','date')
    
    return(mmdf)
    
}


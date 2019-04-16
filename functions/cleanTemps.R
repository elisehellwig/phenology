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



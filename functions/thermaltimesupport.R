extendphenology <- function(fdat, firstyear, lastyear=NA) {
    #fills in phenological events with the average day for years where it is 
    #missing not yet observed.
    fdat <- fdat[,c('year','event1','event2','length1')]
    
    avgevent1 <- round(mean(fdat[,'event1']))
    avgevent2 <- round(mean(fdat[,'event2']))
    avglength <- avgevent2 - avgevent1
    
    if (is.na(lastyear)) {
        lastyear <- max(fdat[,'year'])
    }
    
    missingyears <- setdiff(firstyear:lastyear, fdat[,'year'])
    
    fdatadded <- data.frame(year=missingyears,
                            event1=avgevent1,
                            event2=avgevent2,
                            length1=avglength)
    
    fdatextended <- rbind(fdat, fdatadded)
    
    return(fdatextended)
}


startHeat <- function(days, years) {
    #when should the heat accumulation/forcing start?
    # days - numeric, days from January 1 on the year of the starting event
        # that the heat should start
    # years - year of the flowering event
    
    #if day is in previous year add 365 to convert to julian day
    daymod <- ifelse(days>0, days, days + 365)
    
    return(daymod)
    
}

calcThermalTime <- function(events, temperatures, step, modtype, form, 
                            cardinal, start, thresh, varying, 
                            location=NA, var=NA, datename='dt',
                            predictorName=NA) {
    source('functions/datetime.R')
    #calculates thermal time for predicting flowering and harvest/season length
        #Note loc must be the name of the column that determines location
    # events - data.frame, contains phenology data with columns: cultivar, loc,
        # year, event, and day
    # temperatures - data.frame, contains temperature data, must have a date/
        # time column
    # step - character, either 'harvest' or 'flowering'
    # modtype - character, either 'DT', or 'TTT', 'dual' not currently 
        # implemented
    # form - character, functional form of the chill or heat accumulation 
    # cardinal - numeric, cardinal temperatures used for functional form, may
        # be NA for some forms
    # start - numeric, what (julian) day should the model start accumulating
        # thermal time
    # thresh - numeric, model threshold
    # varying - character, c('start','threshold'), should the julian day of the
        #start or threshold vary from year to year?
    # location - character, what location should be used (for both events and
        # temperature)
    # var - character, what cultivar should be used?
    # datename - character, what is the name of the column containing the 
        # date/time data
    
    
    # convert datetime column to POSIXct if it is not already
    if (!is.POSIXct(temperatures[,datename])) {
        temperatures[,datename] <- toPOSIX(temperatures[,datename])
    }
    
    #print(1)
    #if a location is specified, select data only from that location
    if (!is.na(location)) {
        events <- filter(events, loc==location)
        temperatures <- filter(temperatures, loc==location)
    }
    
    #if a cultivar is specified, select data only for that cultivar
    if (!is.na(var)) {
        events <- filter(events, cultivar==var)
    }
    
    #print(2)
    events <- unique(events) #make sure there are no repeat observations
    yrtable <- table(events[,'year']) # how many for events for each year
    
    #if there is more than one event/observation for any of the years
    if (any(ifelse(yrtable>1, TRUE, FALSE))) { 
        
        #convert data to wide data
        eventsC <- dcast(events, cultivar + loc+year ~ event, value.var = 'day')
        eventsC <- eventsC[complete.cases(eventsC), ] #complete observations
        
       # print(3)
    } else {
        eventsC <- events
    } 
    
    years <- eventsC[,'year'] #years for which we have data
    
    if (step=='harvest') { #for harvest step
        #print(4)
        
        startdays <- eventsC[,'event1'] #set the start day to be flowering
        
        #print(head(years))
        #print(head(startdays))
        #convert day of flowering to to date
        startdates <- dayToDate(years, startdays, 'PlantModel', varying)
        #print(3)
        if (modtype=='DT') { #convert thresh to date if necessary
            threshvalues <- startdates + days(thresh)
        } else {
            threshvalues <- thresh
        }
        
        #calculate the thermal sum
        result <- thermalsum(cardinal, years, temperatures, modtype, form,
                             startdates, threshvalues, varying, 'PlantModel',
                             startdays)
        
        
        
    } else if (step=='flowering') {
        
        #convert start day to dates
        startdates <- dayToDate(years, start, 'FlowerModel', varying)
        
        #add a day on the years with leap year
        startdates <- ifelse(leap_year(startdates) & start>59,
                             startdates + ddays(1),
                             startdates)
        
        #convert date string to POSIXct
        startdates <- as.POSIXct(startdates, origin = '1970-01-01 00:00.00 UTC')
        
        
        #print(startdates)
        #calculate thermal sums
        sums <- thermalsum(cardinal, years, temperatures, 'TTT', form,
                           startdates, thresh, varying, 'FlowerModel',
                           start)
        
        #add day sum to the start dates to get the flowering date
        enddates <- startdates + ddays(sums)
        enddays <- yday(enddates) #calculate julian day
        
        # calculate year lengths
        yearlengths <- ifelse(leap_year(years), 366, 365)
        
        #center day of the year around january first (i.e. days of the year 
            #after day 200 are in negative values)
        result <- ifelse(enddays>200, enddays-yearlengths, enddays)
        
    } else {
        
        stop('step must be either harvest or flowering.')
        
    }
    
    #print(5)
    
    #add results to the data.frame
    if (!is.na(predictorName)) {
        eventsC[,predictorName] <- result
    } else {
        eventsC[,paste0(modtype, form)] <- result
    }
    
    
    return(eventsC)
    
}

fillPhenology <- function(x, event, first, last=NA, location=NA, variety=NA) {
    
    voi <- c('year', event)
    
    if (!is.na(location)) {
        voi <- c(location, voi)
    }
    
    if (!is.na(variety)) {
        voi <- c(variety, voi)
    }
    
    fdat <- x %>% 
        filter(loc=location, cultivar=variety) %>% 
        select(voi)
    
    if (is.na(last)) {
        last <- max(fdat[,'year'])
    }
    
    
    avgday <- mean(fdat[,event])
    
    missingyears <- setdiff(first:last, fdat[,'year'])
    
    filleddata <- data.frame(year=missingyears)
    
    filleddata[,event] <- avgday
    
    if (!is.na(variety)) {
        filleddata <- cbind(data.frame(cultivar=variety), filleddata)
    }
    
    if (!is.na(location)) {
        filleddata <- cbind(data.frame(loc=location), filleddata)
    }
    
    
    alldata <- rbind(fdat, filleddata)
    
    return(alldata)
    
}





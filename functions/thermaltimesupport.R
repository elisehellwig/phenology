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


calcThermalTime <- function(events, temperatures, step, modtype, form, 
                            cardinal, start, thresh, varying, 
                            location=NA, var=NA, datename='dt') {
    source('functions/datetime.R')
    #this is just for harvest at this point
    #note loc must be the name of the column that determines location
    
    if (!is.POSIXct(temperatures[,datename])) {
        temperatures[,datename] <- toPOSIX(temperatures[,datename])
    }
    
    
    if (!is.na(location)) {
        events <- filter(events, loc==location)
        temperatures <- filter(temperatures, loc==location)
    }
    
    if (!is.na(var)) {
        events <- filter(events, cultivar==var)
    }
    

    events <- unique(events)
    yrtable <- table(events[,'year'])
    
    if (any(ifelse(yrtable>1, TRUE, FALSE))) {
        
        eventsC <- dcast(events, cultivar + loc+year ~ event, value.var = 'day')
        eventsC <- eventsC[complete.cases(eventsC), ]
        
    } else {
        eventsC <- events
    } 
    
    years <- eventsC[,'year']
    if (step=='harvest') {
        
        startdays <- eventsC[,'event1']
        
        startdates <- dayToDate(years, startdays, 'PlantModel', varying)
        
        if (modtype=='DT') {
            threshvalues <- startdates + days(thresh)
        } else {
            threshvalues <- thresh
        }
        
        
        result <- thermalsum(cardinal, years, temperatures, modtype, form,
                             startdates, threshvalues, varying, 'PlantModel',
                             startdays)
        
        
        
    } else if (step=='flowering') {
        
        startdates <- dayToDate(years, start, 'FlowerModel', varying)
        
        startdates <- ifelse(leap_year(startdates) & start>59,
                             startdates + ddays(1),
                             startdates)
        startdates <- as.POSIXct(startdates, origin = '1970-01-01 00:00.00 UTC')
        
        
        #print(startdates)
        sums <- thermalsum(cardinal, years, temperatures, 'TTT', form,
                           startdates, thresh, varying, 'FlowerModel',
                           start)
        
        
        enddates <- startdates + ddays(sums)
        enddays <- yday(enddates)
        
        yearlengths <- ifelse(leap_year(years), 366, 365)
        result <- ifelse(enddays>100, enddays-yearlengths, enddays)
    } 
    
    
    
  
    eventsC[,paste0(modtype, form)] <- result
    
    return(eventsC)
    
}


startHeat <- function(days, years) {
    
    daymod <- ifelse(days>0, days, days + 365)
    
    return(daymod)
    
}




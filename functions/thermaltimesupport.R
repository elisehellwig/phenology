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


calcThermalTime <- function(events, temperatures, modtype, form, 
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
    startdays <- eventsC[,'event1']
    
    startdates <- dayToDate(years, startdays, 'PlantModel', varying)
    
    if (modtype=='DT') {
        threshvalues <- startdates + days(thresh)
    } else {
        threshvalues <- thresh
    }
    
    
    sums <- thermalsum(cardinal, years, temperatures, modtype, form,
                       startdates, threshvalues, varying, 'PlantModel',
                       startdays)
    
    
  
    eventsC[,paste0(modtype, form)] <- sums
    
    return(eventsC)
    
}



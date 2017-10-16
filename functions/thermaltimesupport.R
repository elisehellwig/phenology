extendphenology <- function(fdat, firstyear, lastyear=NA) {
    
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
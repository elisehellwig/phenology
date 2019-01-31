importCIMIS <- function(path) {
    
    filenames <- list.files(path, pattern='.csv', full.names = TRUE)
    
    cimis <- plyr::ldply(filenames, function(fn) {
        read.csv(fn)[,c('Stn.Name','Date','Hour..PST.','Jul','Air.Temp..C.',
                        'qc')]
    }) 
    
    
    names(cimis) <- c('name','date','hour','day','temp','qc')
    
    cimis$hour <- cimis$hour/100
    timestring <- paste(cimis$date, 
                        paste0(sprintf('%02d', cimis$hour-1), ":00:00"))
    cimis$date <- as.POSIXct(timestring, format="%m/%d/%Y %H:%M:%OS")
    
    return(cimis)
    
}



fillinTemps <- function(df, variable, predictors, response, predname='loc',
                        printR2=TRUE) {
   require(reshape2)
    
    #print(1) 
    fmla <- as.formula('date ~ loc')
    df <- unique(df)
    
    names(df)[which(names(df)==predname)] <- 'loc'
    df <- df[df$loc %in% c(predictors, response), ]
    wide <- dcast(df, fmla, value.var = variable)
    #print(head(wide))
    
    
    
    #print(1.1)
    wc <- wide[complete.cases(wide),]

   # print(head(wc))
    eqns <- sapply(predictors, function(v) {
        paste(response, '~', v)
    })
    
   # print(1.2)
    modlist <- lapply(eqns, function(equation) {
        #print(equation)
        lm(as.formula(equation), data=wc)
    })    
    
    if (printR2) {
        lapply(modlist, function(mod) print(summary(mod)$adj.r.squared))
    }
    
    #print(2)
    missingResponse <- which(is.na(wide[,response]))
    mr <- wide[missingResponse,]
    
    pres <- lapply(predictors, function(p) {
        which(!is.na(mr[,p]))
    })
    
    #print(3)
    predictions <- lapply(seq_along(predictors), function(i) {
        predict(modlist[[i]], mr[pres[[i]],])
    })
    
    missingMat <- matrix(NA, nrow=nrow(mr), ncol=length(predictors))
    
    for (i in 1:length(predictors)) {
        missingMat[pres[[i]], i] <- unname(predictions[[i]])
    }
    
    #print(4)
    meanVals <- apply(missingMat, 1, mean, na.rm=TRUE)
    
    #print(5)
    newdf <- wide[,c('date',response)]
    newdf[missingResponse, response] <- unname(round(meanVals))
    
    #print(6)
    names(newdf)[2] <- variable
    
    newdf[which(!is.numeric(newdf[,variable])), variable] <- NA
    
    return(newdf)
    
}


FtoC <- function(v) {
    cv <- (v-32)*5/9
    return(cv)
}


AverageTemps <- function(temps, variable, monthly=TRUE, datename='dt', 
                         placename='loc', namelength=5) {
    
    
    if (monthly) {
        stringID <- paste0(substr(temps[,placename], 1, namelength), 
                           substr(temps[,datename], 1, 7))
    } else {
        stringID <- paste0(substr(temps[,placename], 1, namelength), 
                           substr(temps[,datename], 1, 4))
    }
    
    
    avgs <- tapply(temps[,variable], stringID, mean)
    
    mtemps <- data.frame(string=names(avgs),
                         temp=unname(avgs))
    
    mtemps$year <- substr(mtemps$string, namelength+1, namelength+4)
    
    if (monthly) {
        mtemps$month <- as.numeric(substr(mtemps$string, namelength+6,
                                          namelength+7))
        
    }

    mtemps$loc <- substr(mtemps$string, 1, namelength)

    
    names(mtemps)[2] <- variable
    
    return(mtemps)
    
    
}



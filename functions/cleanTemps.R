fillinTemps <- function(df, variable, predictors, response, predname='loc',
                        printR2=TRUE) {
   require(reshape2)
    
    #print(1) 
    fmla <- as.formula(paste('date ~', predname))
    wide <- dcast(df, fmla, value.var = variable)
        
    #print(1.1)
    wc <- wide[complete.cases(wide),]

    #print(head(wc))
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
    
   # print(2)
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
    
    return(newdf)
    
}


FtoC <- function(v) {
    cv <- (v-32)*5/9
    return(cv)
}

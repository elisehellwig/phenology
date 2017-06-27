mround <- function(x, base) {
    base*round(x/base)
}

accumulatedTemps <- function(tempdat, phenology, TTAL, startname='event1') {
    phenology$end <- phenology[,startname] + TTAL
    #print(flowering$year)
    
    d <- extracttemp(tempdat, phenology$year, phenology[,startname], 
                     phenology$end)
    
    return(unlist(d))
}

thermalEval <- function(temp, pars, method) {

   if (method=='linear') {
        TT <- linear(temp, pars[1], sum=FALSE)
        
    } else if (method=='flat') {
        TT <- flat(temp, pars[1], pars[2], sum=FALSE)
        
    } else if (method=='triangle') {
        TT <- triangle(temp, pars[1], pars[2], pars[3], sum=FALSE)
        
    } else if (method=='asymcur') {
        TT <- asymcur(temp, pars[1], pars[2], pars[3], sum=FALSE)
        
    } else if (method=='anderson') {
        TT <- asymcur(temp, 4, 25, 36, sum=FALSE)
    } else {
        stop('Method is not one of the recognized types')
    }
    
    return(TT)
}

collateTT <- function(tempvec, pardat, methods) {
    require(reshape2)

    tt <- sapply(1:length(methods), function(i) {
        thermalEval(tempvec, pardat[i,], methods[i])
    })
    
    df <- as.data.frame(cbind(tempvec, tt))
    names(df) <- c('temp', methods)
    dfm <- melt(df, id.vars='temp', measure.vars=methods, variable.name='form', value.name='TTA')
    
    return(dfm)
}


tempfreq <- function(phenodat, tempdat, maxlength, startname='event1',
                     binsize=5) {
    require(plyr)
    
    tm <- round(accumulatedTemps(tempdat, phenodat, maxlength, startname))
    dt <- as.data.frame(table(tm))
    names(dt)[1] <- 'temp'
    dt$temp <- as.numeric(as.character(dt$temp))
    dt$bin <- mround(dt$temp, binsize)
    
    bf <- ldply(sort(unique(dt$bin)), function(bt) {
        total <- sum(dt[dt$bin==bt, 'Freq'])
        c(bt, total)
    })
    
    names(bf) <- c('bin', 'binfreq')
    bf$binfreq <- bf$binfreq/sum(bf$binfreq)
    
    dtb <- merge(dt, bf, by='bin')
    names(dtb)[4] <- paste0('freq', binsize)
    
    return(dtb)
}

.thermalToTime <- function(pars, pheno, temp, form, length) {
    
    pvec <- na.omit(as.numeric(pars))
    attributes(pvec) <- NULL

    tds <- thermalsum(pvec, pheno, temp, 'day', form, length, 1)
    
    return(tds)
    
}

TTtoDay <- function(pardat, pheno, temp) {
    
    years <- sort(unique(temp$year))
    templist <- extracttemplist(temp, years, 'linear')[[2]]

    lengths <- sapply(1:dim(pardat)[1], function(i) {
        .thermalToTime(pardat[i, c('base','optimal','critical')], pheno,
                       templist, pardat[i, 'form'], pardat[i, 'length'])
    })
    
    return(lengths)
}


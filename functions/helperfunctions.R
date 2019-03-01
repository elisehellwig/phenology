initialpars <- function(n) {
    #creates a list of initial cardinal temperatures given the number of
        #parameters in the model
    # n - numeric, number of parameters in the model
    
    if (n==1) { 
        p <- list(4)
    } else if (n==2) {
        p <- list(c(4,25))
    } else if (n==3) {
        p <- list(c(4,25,36))
    } else if (n==4) {
        p <- list(c(4,25,36,40))
    }
}

initialparlist <- function(v) {
    ipl <- lapply(v, function(n) initialpars(n))
    
    return(ipl)
}

extractslots <- function(slot, object) {
    #extracts values from
    
    if (is.list(object[[1]])) {
        n <- length(object)
        m <- length(object[[1]])
        
        slots <- lapply(1:n, function(i) {
            sapply(1:m, function(j) {
                do.call(slot, list(object[[i]][[j]]))
            })
        })
    } else if (is.list(object)) {
        n <- length(object)
        
        slots <-sapply(1:n, function(i) {
            do.call(slot, list(object[[i]]))
        })
    } else {
        slots <- do.call(slot, list(object))
    }
    
    return(slots)
}


extractCT <- function(object, stage) {
    
    ctvec <- rep(NA, length(object))
    
    cts <- extractslots('cardinaltemps', object)
    
    for (i in 1:length(cts)) {
        
        if (stage <= length(cts[[i]])) {
            ctvec[i] <- cts[[i]][stage]
        }
    }

    return(ctvec)
}


summarizeModel <- function(modlist, cultivars, CT=TRUE) {
    
    forms <- unlist(extractslots('form', modlist))
    starts <- round(as.vector(extractslots('startday', modlist)))
    threshs <- round(unlist(extractslots('threshold', modlist)), 1)
    errors <- round(as.vector(extractslots('error', modlist)), 2)
    simples <- as.vector(extractslots('simplified', modlist))
    
    n <- length(unique(forms))
    
    if (CT) {
        cts <- extractCT(modlist, 1)
        baseCT <- sapply(cts, function(v) v[1])
        optCT <- sapply(cts, function(v) v[2])
        critCT <- sapply(cts, function(v) v[3])
        
        df <- data.frame(cultivar=rep(cultivars, each=n),
                         simplified=simples,
                         form=forms,
                         error=errors,
                         start=starts,
                         threshold=threshs,
                         base=baseCT,
                         optimal=optCT,
                         critical=critCT)
        
        
    } else {
        
        df <- data.frame(cultivar=rep(cultivars, each=n),
                         simplified=simples,
                         form=forms,
                         error=errors,
                         start=starts,
                         threshold=threshs)
    }
    
    return(df)
    
}



importfiles <- function(path, label, varieties, ext) {
    
    fv <- sapply(varieties, function(v) {
        file.path(path, paste0(label, v, '.', ext))
    })
    
    if (grep(ext, 'RDS', ignore.case=TRUE)==1) {
        
        dv <- lapply(fv, function(f) readRDS(f))
        
    } else if (grep(ext, 'csv', ignore.case=TRUE)==1) {
        
        dv <- read.csv(f)
    } else {
        stop('ext must be rds and or csv.')
    }
    
    return(dv)
}


seasonfrac <- function(len, type, maxgd) {
    
    if (type=='DT') {
        fraction <- len/160
    } else if (type=='TTT') {
        fraction <- len/maxgd
    } else {
        stop('type must be DT or TTT')
    }
    
    return(fraction)
}




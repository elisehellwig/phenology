initialpars <- function(n) {
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


extractCT <- function(object, ct) {
    
    ctvec <- rep(NA, length(object))
    
    cts <- extractslots('cardinaltemps', object)
    
    for (i in 1:length(cts)) {
        
        if (ct <= length(cts[[i]])) {
            ctvec[i] <- cts[[i]][ct]
        }
    }

    return(ctvec)
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




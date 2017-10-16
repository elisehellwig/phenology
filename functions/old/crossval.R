source('functions/generalfunctions.R')
source('functions/yearsums.R')

#extractpars

importpars <- function(path, varieties, absolutepath=NA) {
    
    if (is.na(absolutepath)) {
        files <- paste0(path, varieties, '.RDS')
        
    } else if (class(absolutepath)=='character') {
        files <- file.path(absolutepath, paste0(path, varieties, '.RDS'))
    }
    
    pars <- lapply(files, function(f) readRDS(f))
}

extractpars <- function(x, value=FALSE, names=TRUE) {
    require(plyr)
    
        
    if (class(x)=='list' & class(x)[[1]]=='numeric') {
        
        if (value) {
            
            if (names) {
                pars <- c(x$bestmem, x$bestval)
                parnames <- c(paste0('p', 1:length(x$bestmem)),'value')
                names(pars) <- parnames
                
            } else {
                pars <- unname(as.matrix(c(x$bestmem, x$bestval)))
            }
            
        } else {
            
            if (names) {
                pars <- x$bestmem
                parnames <- paste0('p', 1:length(x$bestmem))
                names(pars) <- parnames
                
            } else {
                pars <- unname(as.matrix(x$bestmem))
            }
        }
        
         
    } else if (class(x)=='list' & class(x)[[1]]=='list') {

        if (class(x[[1]][[1]])=='list') {
            stop('The function extractpars() does not work on nested lists')
        }
        

        if (value) {
            
           pars <- ldply(x, function(l) c(l$bestmem, l$bestval))
           names(pars) <- c(paste0('p', 1:length(x[[1]]$bestmem)), 'value')
           
        } else {
            
            pars <- ldply(x, function(l) l$bestmem)
            names(pars) <- paste0('p', 1:length(x[[1]]$bestmem))
            
            
        }
        
    } else {
        stop('The class of x must be a list.')
    }
    
    return(pars)
    
}


kcrossval <- function(dat, k, xname, yname, metric, mean=TRUE, ensemble=FALSE) {
    #print(str(dat))
    
    if (class(dat)=='data.frame') {
        len <- 1
        dlist <- list(dat)
        
    } else if (class(dat)=='list') {
        
        if (class(dat[[1]])!= 'data.frame') {
            stop('dat must be a data.frame or list of data.frames')
        }
        len <- length(dat)
        dlist <- dat
        
        
    } else {
        stop('dat must be a data.frame or a list of data.frames.')
    }
    
    measure <- matrix(rep(NA, len*k), nrow=k)
    
    
    for (i in 1:len) {
        
        if (is.na(xname[1])) {
            d <- dlist[[i]][,c('fold',yname)]
            
        } else {

            d <- dlist[[i]][,c('fold',xname,yname)]
        }
        
        
        for (j in 1:k) {
            train <- d[d$fold!=j, ]
            test <- d[d$fold==j, ]
            
            if (is.na(xname[1])) {
                fmla <- as.formula(paste(yname, '~ 1'))
                mod <- lm(fmla, data=train)
                
                
                fit <- predict(mod, newdata=data.frame(x=test[,yname]))
            } else {
                
                
                if (ensemble) {
                    fmlas <- lapply(xname, function(xn) {
                        as.formula(paste(yname, '~', xn))
                    })
                    
                    mods <- lapply(fmlas, function(f) {
                        lm(f, data=train)
                    })
                    
                    fits <- sapply(1:length(xname), function(i) {
                        df <- data.frame(x=test[, xname[i]])
                        names(df) <- xname[i]
                        predict(mods[[i]], newdata=df)
                        
                    }) 
                    
                    fit <- apply(fits, 1, mean)
                    
                } else {
                    
                    fmla <- as.formula(paste(yname, '~', xname))
                    mod <- lm(fmla, data=train)
                    
                    df <- data.frame(x=test[,xname])
                    names(df) <- xname
                    fit <- predict(mod, newdata=df)
                    
                }
                
                
                
                
            }
            
            
            measure[j,i] <- do.call(metric, list(fit, test[,yname]))
            
        }
        
    }
    
    if (mean) {
        measure<- apply(measure, 2, mean)
    }
    
    return(measure)
}



cardinalcrossval <- function(parlist, wdat, tempdat, type, algorithm, smooth=FALSE) {
    
    folds <- max(wdat$k)
    
    pl <- lapply(1:folds, function(i) {
        ep <- extractpars(parlist[[i]], algorithm=algorithm, value=TRUE, 
                          names=TRUE)
        ep$fold <- i
        ep
    })
    

    pnum <- which(names(pl[[1]])=='value') - 1
    
    
    if (smooth) {
        require(raster)
        
        for (i in 1:folds) {
            for (p in 1:pnum) {
                pl[[i]][,p] <- round(movingFun(pl[[i]][,p], 5, na.rm=TRUE),3)
            }
        }
        
    }
    
    minslen <- 1:(dim(pl[[1]])[1])
    
    ystrain <- lapply(1:folds, function(i) {
        pdat <- pl[[i]]
        wdatk <- wdat[wdat$k!=i,]
        ydat <- sapply(minslen, function(j) {
            yearsums(pdat[j,1:pnum],
                     fdat=wdatk,
                     tdat=tempdat,
                     type=type,
                     sumlen=pdat$days[j],
                     fn='flower',
                     hn='harvest')
        })
        ydat2 <- cbind(wdatk[,c('year','slen')], ydat)
        ydat2
    })
    

    ystest <- lapply(1:folds, function(i) {
        pdat <- pl[[i]]
        wdatk <- wdat[wdat$k==i,]
        ydat <- sapply(minslen, function(j) {
            yearsums(pdat[j,1:pnum],
                     fdat=wdatk,
                     tdat=tempdat,
                     typ=type,
                     sumlen=pdat$days[j],
                     fn='flower',
                     hn='harvest')
        })
        ydat2 <- cbind(wdatk[,c('year','slen')], ydat)
        ydat2
    })
    
    ynames <- paste0('day', minslen) 
    
    
    for (i in 1:folds) {
        names(ystrain[[i]])[-(1:2)] <- ynames
        names(ystest[[i]])[-(1:2)] <- ynames
    }
    
    mods <- lapply(1:folds, function(i) {
        lapply(minslen, function(j) {
            lm(as.formula(paste('slen ~', ynames[j])), data=ystrain[[i]])
        })
    })
    
    testfit <- lapply(1:folds, function(i) {
        sapply(minslen, function(j) {
            newdat <- as.data.frame(ystest[[i]][,j+2])
            names(newdat) <- ynames[j]
            predict(mods[[i]][[j]], newdata=newdat)
        })
    })
    
    testrmse <- sapply(1:folds, function(i) {
        sapply(minslen, function(j) rmsd(testfit[[i]][,j], ystest[[i]][,'slen']))
    })
    
    meantestrmse <- apply(testrmse, 1, mean)
    return(meantestrmse)
}


cardinalcrossvalfull <- function(pl, wdat, tempdat, type, avgrmse=TRUE) {
    
    folds <- max(wdat$k)

    
    pd <- extractpars(pl)
    
    #print(pd)
    #print(pl[[1]])
    
    #print(pl[[1]])
    pnum <- length(pl[[1]]$bestmem)
    
    
    templist <- extracttemp(tempdat, wdat$year, wdat$flower, 
                            rep(336, length(wdat$year)))
    
    test <- lapply(1:folds, function(i) {
        #print(i)
        wdatk <- wdat[wdat$k==i,]
        ydat <- yeargd(pd[i,], fdat=wdatk, tdat=templist, type=type, 
               fn='flower', hn='harvest')
        ydat2 <- cbind(wdatk[,'slen'], ydat)
        ydat2
    })
    
   testrmse <- sapply(1:folds, function(i) {
       d <- test[[i]][complete.cases(test[[i]]),]
       rmsd(d[,2], d[,1])
   }) 
    
   if (avgrmse) {
        return(mean(testrmse))
    } else {
        return(testrmse)
    }
}


cardinalcrossvalcomb <- function(pl, wdat, tempdat, type, avgrmse=TRUE) {
    
    folds <- max(wdat$k)
    
    pd <- extractpars(pl)
    
    templist <- extracttemp(tempdat, wdat$year, wdat$flower, 
                            rep(336, length(wdat$year)))

    
    ytrain <- lapply(1:folds, function(i) {
        wdatk <- wdat[wdat$k!=i,]
        daymet <- yeargd(pd[i,],
                     fdat=wdatk,
                     tdat=templist,
                     type=type,
                     fn='flower',
                     hn='harvest',
                     replaceinf=336)
        ydat <- cbind(wdatk[,c('year','slen')], daymet)
        ydat
    })
    
    
    ytest <- lapply(1:folds, function(i) {
        wdatk <- wdat[wdat$k==i,]
        daymet <- yeargd(pd[i,],
                     fdat=wdatk,
                     tdat=templist,
                     typ=type,
                     fn='flower',
                     hn='harvest',
                     replaceinf=336)
        ydat2 <- cbind(wdatk[,c('year','slen')], daymet)
        ydat2
    })
    
    #print(ytest)
    
    mods <- lapply(1:folds, function(i) {
        lm(slen ~ daymet, data=ytrain[[i]])
    })
    
    testfit <- lapply(1:folds, function(i) {
            predict(mods[[i]], newdata=ytest[[i]])
    })
    
    testrmse <- sapply(1:folds, function(i) {
        rmsd(testfit[[i]], ytest[[i]][,'slen'])
    })
    
    if (avgrmse) {
        return(mean(testrmse))
    } else {
        return(testrmse)
    }

}


comparecv <- function(rmselist, types) {
    require(plyr)
    la <- ldply(1:length(rmselist), function(i) {
        data.frame(cultivar=names(rmselist[[i]]),
                   length=apply(rmselist[[i]], 2, which.min),
                   rmse=apply(rmselist[[i]], 2, min),
                   type=types[i],
                   row.names = NULL,
                   stringsAsFactors = FALSE)
        })
    
    return(la)
}





cardinalassess <- function(parlist, wdat, tempdat, type, method, smooth=FALSE) {
    require(plyr)
    source('functions/yearsums.R')
    
    vars <- sort(unique(wdat[,'cultivar']))
 
    pl <- lapply(parlist, function(par) {
        extractpars(par, method=method, value=TRUE)
    })
    
    pnum <- which(names(pl[[1]])=='value') - 1
    
    
    if (smooth) {
        require(raster)
        
        for (i in 1:length(pl)) {
            for (p in 1:pnum) {
                pl[[i]][,p] <- round(movingFun(pl[[i]][,p], 5, na.rm=TRUE),3)
            }
        }
        
    }
    
    minslen <- lapply(1:length(pl), function(i) {
        1:(dim(pl[[i]])[1])
    })
    
    dat <- lapply(1:length(pl), function(i) {
        wc <- wdat[wdat$cultivar==vars[i],]
        pdat <- pl[[i]]
        ydat <- sapply(minslen[[i]], function(j) {
            yearsums(pdat[j,1:pnum],
                     fdat=wc,
                     tdat=tempdat,
                     typ=type,
                     sumlen=pdat$days[j],
                     fn='flower',
                     hn='harvest')
        })
        ydat2 <- cbind(wc[,c('year','slen')], ydat)
    })
    
    
    ynames <- lapply(minslen, function(ml) paste0('day', ml) )
    
    for (i in 1:length(pl)) {
        names(dat[[i]])[-(1:2)] <- ynames[[i]]
    }
    
    mods <- lapply(1:length(pl), function(i) {
        lapply(minslen[[i]], function(j) {
            lm(as.formula(paste('slen ~', ynames[[i]][[j]])), data=dat[[i]])
        })
    })
    
    fullrmse <- lapply(1:length(pl), function(i) {
        sapply(1:160, function(j) {
            fit <- predict(mods[[i]][[j]])
            rmsd(fit, dat[[i]][,'slen'])
        })
    })
    
    fullrmsedat <- as.data.frame(matrix(unlist(fullrmse), nrow=160, byrow=TRUE))
    
    names(fullrmsedat) <- vars
    
    return(fullrmsedat)   
}



gdensemble <- function(cardinaldat, fdat, templist, cult) {
    forms <- unique(cardinaldat$form)
    cdf <- cardinaldat[cardinaldat$cultivar==cult,]
    
    #print(cdf)
    #print(cult)
    pars <- lapply(forms, function(f) {
        parind <- which(!is.na(cdf[cdf$form==f, c('p1','p2','p3')]))+2
        #print(parind)
        parind <- c(6, parind)
        unname(cdf[cdf$form==f, parind])
    })
    
    #print(pars)
    
    extractedtemps <- lapply(1:length(forms), function(i) {
        extracttemp(templist[[i]], fdat$year, fdat$flower, 336)
    })
    
    slenfit <- sapply(1:length(forms), function(i) {
        #print(forms[i])
        yeargd(pars[[i]], fdat, extractedtemps[[i]], forms[i])
    })
    
    
    ensembleslen <- apply(slenfit, 1, mean, na.rm=TRUE)
    
    rmse <- rmsd(ensembleslen, fdat[, 'slen'])
    return(rmse)
}

gdensemblecombined <- function(cardinaldat, fdat, templist, cult) {
    forms <- unique(cardinaldat$form)
    cdf <- cardinaldat[cardinaldat$cultivar==cult,]
    
    pars <- lapply(forms, function(f) {
        parind <- which(!is.na(cdf[cdf$form==f, c('p1','p2','p3')]))+2
        parind <- c(6, parind)
        unname(cdf[cdf$form==f, parind])
    })
    
    extractedtemps <- lapply(1:length(forms), function(i) {
        extracttemp(templist[[i]], fdat$year, fdat$flower, 336)
    })
    
    
    daymet <- lapply(1:length(forms), function(i) {
        yeargd(pars[[i]], fdat, extractedtemps[[i]], forms[i], replaceinf=336)
    })
    
    #print(daymet)
    
    fits <- sapply(1:length(forms), function(i) {
        mod <- lm(fdat$slen ~ daymet[[i]])
        fitted(mod)
    })
    
    
    ensemblefits <- apply(fits, 1, mean, na.rm=TRUE)
    
    rmse <- rmsd(ensemblefits, fdat$slen)
    return(rmse)
}


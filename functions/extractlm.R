extractLM <- function(mod, col.names=c('coef','pval'), intname='intercept',
                      loc=NA, cultivar=NA) {
    
    if (!grepl('summary', class(mod))) {
        mod <- summary(mod)
    } 
    
    coefs <- as.data.frame(coef(mod))
    names(coefs) <- c('coef','stderr','tval','pval')
   
    varnames <- c(intname, row.names(coefs)[-1])
    coefs$var <- varnames 
    row.names(coefs) <- NULL
    
    coefs$r2 <- mod$adj.r.squared
    
    if (!is.na(loc)) {
        coefs$loc <- loc
    }
    
    if (!is.na(cultivar)) {
        coefs$cultivar <- cultivar
    }
    
    return(coefs)
    
}

formatLM <- function(df, lmlist, cols=c('coef', 'pval','r2'), crop=NA,
                     dropIntercept=TRUE) {
    
    if (class(lmlist)=='lm') {
        moddf <- extractLM(lmlist, loc=df[1,'loc'], cultivar=df[1,'cultivar'])
        
    } else {
        moddf <- ldply(1:nrow(df), function(i) {
            extractLM(lmlist[[i]], loc=df[i,'loc'], cultivar=df[i,'cultivar'])
        })
        
    }
    
    if (dropIntercept) {
        moddf <- filter(moddf, var!='intercept')
    } else {
        cols <- c('var', cols)
    }
   
    
    coi <- c('loc','cultivar', cols)
    
    newdf <- moddf[,coi]
    
    if (!is.na(crop)) {
        newdf[,'crop'] <- crop
    }
    
    
    
    return(newdf)
}



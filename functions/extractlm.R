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
        coefs$location <- loc
    }
    
    if (!is.na(cultivar)) {
        coefs$cultivar <- cultivar
    }
    
    return(coefs)
    
}
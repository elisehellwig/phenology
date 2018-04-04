extractLM <- function(mod, col.names=c('coef','pval'), intname='intercept') {
    coefs <- as.data.frame(coef(mod))
    names(coefs) <- c('coef','stderr','tval','pval')
    coefs$var <- row.names(coefs)
}
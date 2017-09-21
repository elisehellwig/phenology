arf <- function(response, predictor, mo, data) {
    #note this only works for AR(1)
    df <- data
    d <- df[df$month==mo, ]
    n <- length(d[,1])
    y <- d[-1,response]
    x1 <- d[-1, predictor]
    x2 <- d[-n, response]
    
    mod <- lm(y ~ x1 + x2)
    
    pval <- summary(mod)$coefficients[2,c(1,4)]
    attributes(pval) <- NULL
    return(pval)
}


sig <- function(v, p1=0.05, p2=0.01, p3=0.001) {
    v[v>p1] <- 'NS'
    v[v<p3] <- paste0('p<', p3)
    v[v<p2] <- paste0('p<',p2)
    v[v<p1] <- paste0('p<',p1)
    return(v)
}

sig2 <- function(v, p1=0.05, sf=2, snn=5) {
    v <- signif(v, digits=sf)
    sv <- which(v<p1)
    
    n <- nchar(v)
    nw <- which(n>snn)
    v[nw] <- format(v[nw], scientific=TRUE)
    
    v[-sv] <- paste0("", v[-sv])
    v[sv] <- paste0("**",v[sv], "**")
    return(v)
}
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

lmtext <- function(response, predictors, output="pval", sigfig=3, bvar="b", xnames=NA) {
  #response and predictor are the response and predictor data frames

  response <- as.data.frame(response)
  predictors <-as.data.frame(predictors)

  pnum <- dim(predictors)[2]

  d <- cbind(response, predictors)


  names(d)[1] <- "y"

  if (!is.na(xnames)) {
    names(d)[-1] <- xnames
  } 

  mod <- summary(lm(y ~ ., data=d))

  coefs <- signif(mod$coefficients, digits=sigfig)


  if (output=="pval") {

    pvals <- coefs[2:(pnum+1), 4]

    ptext <- paste("p-value =", pvals)

    return(ptext)

  } else if (output=='beta') {

    b <-coefs[2:(pnum+1),1]
    attributes(b) <- NULL
  
    bs <- paste(bvar, "=", b)

    return(bs)

  } else if (output=="equation") {


    n <- names(d)[-1]

    b <- coefs[2:(pnum+1),1]
    attributes(b) <- NULL

    rightside <- paste(paste0(b, "*", n), collapse=" + ")

    eq <- paste0(names(d)[1], " = ", rightside, " + ", coefs[1:1])
    return(eq)

  } else if (output=="r2") {

    r2 <- paste("R^2 = ", signif(mod$adj.r.squared, digits=sigfig))

    return(r2)

  } else if (output=="se") {
  
    return(coefs[2:(pnum+1),2])

  } else {
    stop("The only options for output are 'pval', 'beta', 'equation', 'r2' and 'se'. Check your spelling and try again.")
  }


}
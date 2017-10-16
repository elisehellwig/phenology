pasteplus <- function(vector) {
	paste(vector, collapse="+")
}

multiregression <- function(data, predictors, response, index, order=NA, plotcolor='Black', cultivar=NA, nearest=NA, respname='Start of Leaf-Out (Julian Day)', plot=TRUE) {
	require(ggplot2)
	require(reshape2)

	allvars <- c(index, response, predictors, 'nearest')
	mnames <- substr(predictors, 1, nchar(predictors)-5)

	data2 <-data

	#print(cultivar)

	if (is.na(cultivar)) {
		d <- data2[,allvars]
	} else {
		d <- data2[data2$cultivar==cultivar, allvars]
	}

	#print(unique(d$cultivar))
	#print(str(d))

	if (!is.na(nearest)) {
		d <- d[d$nearest==nearest,]
	}

	#print(str(d))
	if (length(predictors)>1) {
		d[, predictors] <- sapply(predictors, function(v) {
			ds <- scale(d[,v], scale=FALSE)
			attributes(ds) <- NULL
			ds
			})

		predictorname <- expression("Deviation from Mean Temperature  "(~degree*C))
	} else {
		predictorname <- expression("Temperature "(~degree*C))
	}

	#print(str(d))

	formulastr <- paste(response, '~', pasteplus(predictors))

	mod <- lm(as.formula(formulastr), data=d)

	rng <- range(sapply(predictors, function(v) range(d[,v], na.rm=TRUE)))

	if (length(predictors)>1) {
		rngmax <- ceiling(max(abs(rng)))
		rngsym <- (-rngmax):rngmax

		} else {
			rngsym <- floor(rng[1]):ceiling(rng[2])
		}

	

	dflist <- lapply(1:length(predictors), function(i) {
		m <- matrix(0, length(rngsym), length(predictors))
		m[,i] <- rngsym
		df <- as.data.frame(m)
		names(df) <- predictors
		df
		})

	fit <- as.data.frame(sapply(1:length(predictors), function(i) {
		predict(mod, dflist[[i]])
		}))

	
	names(fit) <- mnames
	fit$temp <- rngsym

	#print(str(fit))

	fitm <- melt(fit, id.vars='temp', measure.vars=mnames, value.name='fit')

	if (!is.na(order[1])) {
		levels(fitm$variable) <- mnames[order]
	}

	#print(str(d))

	dlong <- melt(d, id.vars=c(index, response), measure.vars=c(predictors), value.name='temp')
	
	if (is.na(order[1])) {
		levels(dlong$variable) <- mnames

		} else {
			levels(dlong$variable) <- mnames[order]
		}
		

	#print(predictors)
	#print(str(dlong))

	mplot <- ggplot(data=dlong) + geom_point(aes(x=temp, y=get(response)), color=plotcolor, size=1.5) + facet_wrap(~variable) + geom_line(aes(x=temp, y=fit), data=fitm) 
	mplot <- mplot + theme_bw(10) + labs(x=predictorname, y=respname)

	if (plot) {
		return(mplot)

		} else {
			return(mod)
		}
	

}


arf <- function(response, predictor, mo, data) {
  #note this only works for AR(1)
  df <- data
 
  if (is.na(mo)) {
    d <- df
  } else {
    d <- df[df$month==mo, ]
  }
  
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

sig2 <- function(v, p1=0.05, sf=2, snn=5, v2=NA) {
  #v is the vector of the numbers
  #p1 is the p-value that indicates significance
  #sf is the number of significant figures to use
  #snn is the number of digits to allow before converting to scientific notation
  #v2 is another vector to bold using the significance given by the p values in v
  v <- signif(v, digits=sf)
  sv <- which(v<p1)
  
  n <- nchar(v)
  nw <- which(n>snn)
  v[nw] <- format(v[nw], scientific=TRUE)
  
  v[-sv] <- paste0("", v[-sv])
  v[sv] <- paste0("**",v[sv], "**")
  
  if (length(v2)>1) {
    v2 <- signif(v2, digits=sf)

    v2[-sv] <- paste0("", v2[-sv])
    v2[sv] <- paste0("**",v2[sv], "**")
    return(v2)
    
  } else {
    return(v)
  }
  
}



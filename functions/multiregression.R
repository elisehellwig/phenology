pasteplus <- function(vector) {
	paste(vector, collapse="+")
}

multiregression <- function(data, predictors, response, index, order=NA, plotcolor='Black', cultivar=NA, nearest=NA, respname='Start of Leaf-Out (Julian Day)') {
	require(ggplot2)
	require(reshape2)

	allvars <- c(index, response, predictors)
	mnames <- substr(predictors, 1, nchar(predictors)-5)

	if (is.na(cultivar)) {
		d <- data[,allvars]
	} else {
		d <- data[data$cultivar==cultivar, allvars]
	}


	if (!is.na(nearest)) {
		d <- d[d$nearest==nearest,]
	}

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


	formulastr <- paste(response, '~', pasteplus(predictors))

	mod <- lm(as.formula(formulastr), data=d)

	rng <- range(sapply(predictors, function(v) range(d[,v], na.rm=TRUE)))
	rngmax <- ceiling(max(abs(rng)))
	rngsym <- (-rngmax):rngmax

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


	fitm <- melt(fit, id.vars='temp', measure.vars=mnames, value.name='fit')

	if (!is.na(order[1])) {
		levels(fitm$variable) <- mnames[order]
	}

	dlong <- melt(d, id.vars=c(index, response), measure.vars=predictors, value.name='temp')
	levels(dlong$variable) <- mnames[order]

	mplot <- ggplot(data=dlong) + geom_point(aes(x=temp, y=get(response)), color=plotcolor, size=1.5) + facet_wrap(~variable) + geom_line(aes(x=temp, y=fit), data=fitm) 
	mplot <- mplot + theme_bw(10) + labs(x=expression("Deviation from Mean Temperature  "(~degree*C)), y=respname)


	return(mplot)

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





library(lubridate)

fit <- function(a, b, dat) {
	f <- a + b*dat
	return(f)
} 


extracttemp <- function(tdat, years, starts, ends, tempname=NA, 
                        yearname='year', dayname='day') {
	
    #print(1)
    
    if (length(ends)==1) {
        ends <- rep(ends, length(starts))
    }
    
    #print(years)
    
    if (is.na(tempname)) {
        
        if ('temp' %in% names(tdat)) {
            tnames <- 'temp'
            
        } else if ('tmin' %in% names(tdat) & 'tmax' %in% names(tdat)) {
            tnames <- c('tmin','tmax')
            
        } else {
            stop('The names of the variables with temperature data must be 
                    temp, tmin and tmax, or it must be specified with the
                    tempname argument.')
        }
    } else {
        tnames <- tempname
    }
    
    tlist <- lapply(1:length(years), function(i) {
        rows <- which(tdat[,yearname]==years[i] & tdat[,dayname]>=starts[i] & tdat[,dayname]<=ends[i])
        tdat[rows,tnames]
    })

    names(tlist) <- years
	return(tlist)
}

is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


head.list <- function(obj, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    origN <- n
    n <- if (n < 0L)
        max(length(obj) + n, 0L)
    else min(n, length(obj))
    lapply(obj[seq_len(n)], head, origN, ...)
}


rmsd <- function(fit, dat, na.rm=FALSE) {
	if (na.rm) {
		frows <- which(is.na(fit))
		fit1 <- fit[-frows]

		drows <- which(is.na(dat))
		dat1 <- dat[-drows]	
	} else {
		fit1 <- fit
		dat1 <- dat
	}

	rmsd <- sqrt(sum((fit1-dat1)^2)/length(fit1))	
	return(rmsd)
}

r2 <- function(fit, dat, n, p, adj=TRUE, na.rm=FALSE) {

	if (na.rm) {
		frows <- which(is.na(fit))
		fit1 <- fit[-frows]

		drows <- which(is.na(dat))
		dat1 <- dat[-drows]	
	} else {
		fit1 <- fit
		dat1 <- dat
	}

	m <-mean(dat)

	tot <- sum((dat-m)^2)
	res <- sum((dat-fit)^2)

	rs <- 1-((res )/ tot)
	
	if (adj) {

		return( rs - (1-rs)*(p/(n-p-1)) )

	} else {

		return(rs)
		
		}
	 
}


#have been installed and installs them if they have not.
checkpacks <- function(packnames, load=TRUE, quiet=TRUE) {
    #packnames is a character vector
    
    #gets vector of installed packages
    installed_pkgs <- row.names(installed.packages())
    
    #sees which packages from packnames have not been installed
    installtf <- ifelse(packnames %in% installed_pkgs, FALSE, TRUE)
    
    if (sum(installtf) > 0) { #checks to see if any have not been installed
        
        #if some have not been installed, installs them
        installpacks <- packnames[installtf]
        install.packages(installpacks, dependencies = TRUE)
        
    }
    
    if (load) {
        for (pack in packnames) { 
            require(pack, quietly=quiet, warn.conflicts = (!quiet), 
                    character.only = TRUE)
            
        }
    }
    
}



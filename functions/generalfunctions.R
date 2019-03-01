#requires the lubridate package to use some of the functions

reform <- function(df, cultivars) {
    #converts data where the 'variables' are rows and observations are 
        #columns to variables being the the columns and observations the rows
    # df - data.frame containing the data
    # cultivars - character, the names of the cultivars in the data (in the
        # order they are in the data)
    
    #transpose dataframe and remove first row (cultivar names)
    dft <- as.data.frame(t(df))[-1,]
    names(dft) <- c('year', cultivars) #give rows appropriate names
    
    vnames <- names(df)[-1] #get the names of the columns
    
    namesplit <- strsplit(vnames, '[.]') #split names by '.'
    
    #select the first element in each column name which is the phenological
        #event
    dft$event <- sapply(seq_along(namesplit), function(i) namesplit[[i]][1])
    
    #convert data to long format
    dfmelt <- melt(dft, id.vars=c('year','event'), measure.vars=cultivars,
                   variable.name = 'cultivar', value.name = 'date')
    
    return(dfmelt)
    
} 



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



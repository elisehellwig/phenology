source('functions/generalfunctions.R')
source('functions/thermalfunctions.R')

parlist <- function(temps, pars, sum=FALSE, full=FALSE) {

    if (length(pars)==1) {
        pl <- list(temps, pars, sum)
        
    } else if (length(pars)==2) {
        pl <- list(temps, pars[1], pars[2], sum)
        
    } else if (length(pars)==3) {
        pl <- list(temps, pars[1], pars[2], pars[3], sum)
        
    } else if (length(pars)==4) {
        pl <- list(temps, pars[1], pars[2], pars[3], pars[4], sum)
        
    } else {
        stop('There are no models with more than 4 parameters')
    }
    
    return(pl)
}


yearsums <- function(pars, fdat, tdat, type, tempname='temp', sumlen=NA, 
                     fn='flower', hn='harvest') {

	# for walnut
	#fdat is data for the 'fruit'
	pars <- unname(pars)
	
    cd <- fdat

	start <- cd[, fn]

	if (is.na(sumlen)) {
	    end <- cd[,hn]
	    
	} else {
	    end <- start+sumlen
	}

	if (type %in% c('gdd','gddsimple', 'linear','nocrit','triangle',
	                'trapezoid')) {
	    #print(pars)
	    tsums <- sapply(cd[,'year'], function(y) {
	        plist <- parlist(tdat[[as.character(y)]], pars, sum=TRUE)
	        do.call(type, plist)
	    }) 
	    
	} else if (type=='anderson') {
	    tsums <- sapply(cd[,'year'], function(y) {
	        plist <- parlist(tdat[[as.character(y)]], c(4,25,36), sum=TRUE)
	        do.call(type, plist)
	    })
	    
	} else {
        stop('type must be linear, nocrit triangle, anderson, gdd, gddsimple 
             or trapezoid')
	}

	return(tsums)	
}




yeargd <- function(pars, fdat, tdat, type, fn='flower', 
                   hn='harvest', end=336, replaceinf=NA) {
    
    pars <- unlist(pars)
    #print(str(pars))
    
    start <- fdat[,fn]
    #print(1)
    
    if (type %in% c('gdd', 'gddsimple')) {
    
        gd <- lapply(fdat[,'year'], function(y) {
            plist <- parlist(tdat[[as.character(y)]], 
                             pars[[2:length(pars)]])
            #print(plist)
            tt <- do.call(type, plist)
            cumsum(tt)
        })
        
        predslen <- sapply(1:length(fdat[,'year']), function(i) {
            #print(pars[1])
            if (is.infinite(suppressWarnings(min(which(gd[[i]]>pars[1]))))) {
                # print(5)
                replaceinf
            } else {
                #print(4)
                min(which(gd[[i]]>pars[1]))
            }
        })
        
       # print(2)
    } else if (type %in% c('linear', 'nocrit', 'triangle')) {

        gd <- lapply(fdat[,'year'], function(y) {
            plist <- parlist(tdat[[as.character(y)]], pars[-1])
            tt <- do.call(type, plist)
            cumsum(tt)
        })
        
        predslen <- sapply(1:length(fdat[,1]), function(i) {
            #print(pars[1])
            if (is.infinite(suppressWarnings(min(which(gd[[i]]>pars[1]))))) {
                # print(5)
                replaceinf
            } else {
                #print(4)
                round(min(which(gd[[i]]>pars[1]))/24)
            }
        })
        
    } else if (type=='anderson') {
        
        gd <- lapply(fdat[,'year'], function(y) {
            plist <- parlist(tdat[[as.character(y)]], c(4,25,36))
            tt <- do.call(type, plist)
            cumsum(tt)
        })
        
        predslen <- sapply(1:length(fdat[,1]), function(i) {
            #print(pars[1])
            if (is.infinite(suppressWarnings(min(which(gd[[i]]>pars[1]))))) {
                # print(5)
                replaceinf
            } else {
                #print(4)
                round(min(which(gd[[i]]>pars[1]))/24)
            }
        })
        
    } else {
        stop('type must be one of the following: gdd, gddsimple, linear
             nocrit, triangle, anderson')
    }
    
     
    return(predslen)
    
}





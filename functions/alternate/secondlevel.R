#second level function

yearsums <- function(pars, fdat, tdat, cult=NA, typ='nocrit', tempname='temp', sumlen=NA) {
	source('R/functions/generalfunctions.R')
	source('R/functions/thermalfunctions.R')

	# for walnut
	#fdat is data for the 'fruit'


	if (is.na(cult)) {
		cd <- fdat
	} else {
		inds <- which(fdat[,'cultivar'] %in% cult)
		cd <- fdat[inds,]
	}

	if (is.na(sumlen)) {
		end <- cd[,'hday']
	} else {
		end <- cd[, 'lday'] + sumlen
	}

	
	if (typ=='nocrit') {

		tsums <- sapply(1:length(cd[,1]), function(i) {
			temps <- extracttemp(tdat, cd[i,'year'], cd[i, 'lday'], end[i])[,tempname]
			nocrit(temps, pars[1], pars[2])

			} )

	} else if (typ=='anderson') {
		
		tsums <- sapply(1:length(cd[,1]), function(i) {
			temps <- extracttemp(tdat, cd[i,'year'], cd[i, 'lday'], end[i])[,tempname]
			anderson(temps, pars[1], pars[2], pars[3])
			
			})
				


	} else if (typ=='trapezoid') {
		
		tsums <- sapply(1:length(cd[,1]), function(i) {
			temps <- extracttemp(tdat, cd[i,'year'], cd[i, 'lday'], end[i])[,tempname]
			trapezoid(temps, pars[1], pars[2], Ts=pars[3], Tc=pars[4])
			
			})



		} else {
			stop('type must be anderson, trapezoid, or nocrit')
		}

	
return(tsums)	

}

mincv <- function(pars, fdat, tdat, cult=NA, type='nocrit', sum=FALSE) {

	if(pars[1] > pars[2]) {
		return(Inf)
	} else {
		ysums <- yearsums(pars, fdat, tdat, cult, typ=type)

		cv <- abs(sd(ysums)/mean(ysums))

		if(sum) {
			return(ysums)
		} else{
			return(cv)
		}
	}
}



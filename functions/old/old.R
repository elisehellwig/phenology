
mincv <- function(pars, fdat, tdat, cult=NA, type='nocrit', sum=FALSE, sumlength=NA, sday=NA, def.cv=1) {
	
	if (length(pars==1)) {
		ysums <- yearsums(pars, fdat, tdat, cult, typ=type, sumlen=sumlength, startday=sday)
		cv <- abs(sd(ysums)/mean(ysums))
	
	} else if (length(pars)==2) {
		
		if (pars[1]>pars[2]) {
			cv <- def.cv
		
		} else {
			ysums <- yearsums(pars, fdat, tdat, cult, typ=type, sumlen=sumlength, startday=sday)
			cv <- abs(sd(ysums)/mean(ysums))

		}

	} else if (length(pars)==3) {

		if (pars[1]>pars[2]| pars[2]>pars[3]|pars[1]>pars[3]) {
			cv <- def.cv
		} else {
			ysums <- yearsums(pars, fdat, tdat, cult, typ=type, sumlen=sumlength, startday=sday)
			cv <- abs(sd(ysums)/mean(ysums))

		}
	} else if (length(pars)==4) {
		
		if (pars[1]>pars[2]| pars[2]>pars[3]|pars[1]>pars[3]|pars[1]>pars[4]| pars[2]>pars[4]|pars[3]>pars[4]) {
			cv <- def.cv
		} else {
			ysums <- yearsums(pars, fdat, tdat, cult, typ=type, sumlen=sumlength, startday=sday)
			cv <- abs(sd(ysums)/mean(ysums))

		}
	} else {
		stop('There are no models implemented with more than 4 parameters.')
	}

	if (is.finite(cv)) {

		} else {
			cv <- def.cv
		}
	

	if(sum) {
		return(ysums)
	} else{
		return(cv)
	}

}


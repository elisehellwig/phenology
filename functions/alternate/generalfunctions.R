library(lubridate)

days <- function(date) {
	require(lubridate)
	#this function converts a Date object to a number that is the day of the year that the date is
	#EX. 02/02/1993 would return 33
	#Note: all days are returned as if it were a leap year
	#date must be in the Date format

	if (is.na(date)) { #returns NA if the entry is NA
		return(NA)

	} else { 
		if (leap_year(year(date))) { #decides if it is a leap year
			d <- yday(date)

		} else { #if not adds 1 day accordingly
			if (yday(date)>59) {
				d <- yday(date) + 1

			} else {
				d <- yday(date)

			}
		}
	return(d)
	}	
	
}

avgdate <- function(datev) {
	#averages 2 dates together

	d <- as.integer(datev)
	ad <- ceiling(sum(d, na.rm=TRUE)/sum(!is.na(datev)))
	aD <- as.Date(ad, origin="1970-01-01")
	return(aD)
}

nabv <- function(base, tvec) {
	#Calculates the number of entries in a vector (tvec) that are above some number (base)

	l <- length(which(tvec>base))
	return(l)
}


abovebase <- function(name, tdat, base=4, reg=TRUE) {
	#note tdat must have 3 columns in this order: location, year, temp

	names(tdat) <- c('loc', 'year','tmin')
	dat <- tdat[tdat$loc==name,]
	r <- range(dat$year)[1]:range(dat$year)[2]
	numd <- sapply(r, function(y) nabv(4,dat[dat$year==y,'tmin']))
	if (reg) {
		mod <- lm(numd ~ r)
		bp <- summary(mod)$coefficients[2,c(1,4)]
		return(bp)
	} else {
		return(numd)
	}
}

dd <- function(tmin, tmax, x) {
	#calculates the degree days accumulated with a fairly naive formula
	if (is.na(tmin)) {
		return(NA)
	} else if (is.na(tmax)) {
		return(NA)
	} else {
		tm <- (tmin + tmax)/2
		if (x>tmax) {
			D <- 0
		} else if (x<tmin) {
			D <- tm-x
		} else {
			D <- ((tmax - x)^2)/(2*(tmax-tmin))
		}
		return(D)
	}
}


tdd <- function(start, end, climdat) {
	#climdat must have the named columns day, and dd
	cd <- climdat
	days <- start:end
	cd <- cd[cd$day %in% days,]

	totdd <- sum(cd$dd)

	return(totdd)
}


daylength <- function(jul, lat=38.5, ret='') {
	#from extractTemp.py: Cesaraccio et al (2001) "An improved model for determining degree-day values..."
	#jul is the julian date
	#lat is the latitude of the location
	#all calculations are in radians
	#dayangle is the day angle
	#dec is the sun's declination
	#hourangle is the hour angle at sunset

	dayangle <- 2*pi*(jul-1)/365
	dec <- 0.006918 - 0.399912*cos(dayangle) + 0.070257*sin(dayangle) - 0.006758*cos(2*dayangle) + 0.000907*sin(2*dayangle) - 0.002697*cos(3*dayangle) + 0.001480*sin(3*dayangle)
	hourangle <- acos(-tan(lat*pi/180)*tan(dec))

	daylen <- 24*hourangle/pi
	sunrise <- 12 - daylen/2 - dec
	sunset <- 12 + daylen/2 - dec

	ss <- c(daylen, sunrise, sunset)
	return(ss)

}


hourlytemp <- function(hour, jul, tmin, tmax, tnext, latitude=38.5) {
	#tnext is temp minimum of the next day

	today <- daylength(jul, latitude)
	len <- today[1]
	rise <- today[2]
	set <- today[3]

	tom <- daylength(jul+1, latitude)
	lenn <- tom[1]
	risen <- tom[2]
	setn <- tom[3]

	sunmax <- set - 4
	tset <- tmax - 0.39*(tmax - tnext)
	a <- tmax - tmin
	r <- tmax - tset
	b <- (tnext - tset)

	bn <- 
	

	if (hour <= rise) {
		hrt <- tset + b*sqrt(24*hour - set)

	} else if (hour <= sunmax) {
		hrt <- tmin + a*sin( ((hour - rise) / (sunmax - rise))*(pi/2) )

	} else if (hour < set) {
		hrt <- tset + r*sin( (pi/2) - (((hour - sunmax)/4)*pi/2))

	} else if (hour > set) {
		hrt <- tset + b*sqrt(hour - set)
	} else {
		stop('WTF Mate?')
	}


	return(hrt)
}


extracttemp <- function(tdat, year, lday, hday, yearname='year', dayname='day') {
	yrows <- which(tdat[,yearname]==year)
	tdat <- tdat[yrows,]

	lhrows <- which(tdat[,dayname]>=lday & tdat[,dayname]<=hday)
	tdat <- tdat[lhrows,]

	return(tdat)
}


gdhsum <- function(Th, Tb, To, Ts=38, Tc=40, type='anderson') {	
	#Tb is the base temp
	#To is the optimum temp
	#Tc is the critical temp
	#Temp is the vector of hourly data
	#start is the beginning of the season (day)
	#end is the end of the season (day)
	#time/date in tdat must be called dt
	source('R/functions/thermalfunctions.R')
	

		if (type=='nocrit') {
			which(cumsum(nocrit(Th)) > accum)[1]

		} else if (type=='anderson') {
			which(cumsum(anderson(Th)) > accum)[1]

		} else if (type=='beta') {
			which(cumsum(beta(Th)) > accum)[1]
			
		} else if (type=='trapezoid') {
			which(cumsum(beta(Th)) > accum)[1]
			
		} else {
			stop('Type must be nocrit, anderson, beta or trapezoid')
		}











	if (is.na(start) | is.na(end)) {
		stop(paste('There is no bloom or harvest day for', year))
	} else {

		days <- start:end

		gdh <- lapply(days, function(d) {
			sapply(1:24, function(h) {

				Throw <- which(Tdat[,yearname]==year & Tdat[,dayname]==d & Tdat[,hourname]==h)
				Th <- Tdat[Throw , tempname]
			
				if (type=='nocrit') {
					nocrit(Tb, To, Th)
				
				} else if (type=='anderson') {
					anderson(Th, Tb, To, Tc)
			
				} else if (type=='beta') {
					beta(Th, Tb, To, Tc)

				} else if (type=='trapezoid') {

					trapezoid(Th, Tb, To, Ts, Tc)

					
				} else {
					stop("Type must be 'nocrit', 'beta', 'trapezoid', or 'anderson'")
				}
				
			})
		})
		
		gdhs <- sapply(gdh, function(l) sum(l))
	
		gdhss <- sum(gdhs)

		return(gdhss)
 	}
}


gdhsumOLD <- function(Tb, To, Tdat, start, end, year, Ts=38, Tc=40, type='anderson') {	
	require(lubridate)
	require(date)
	#Tb is the base temp
	#To is the optimum temp
	#Tc is the critical temp
	#Tdat holds the temp data
	#start is the beginning of the season (day)
	#end is the end of the season (day)
	#time/date in tdat must be called dt
	source('R/functions/thermalfunctions.R')

	days <- seq(start, end)

	ss <- as.data.frame(t(sapply(days, function(d) daylength(d)[2:3] )))
	names(ss) <- c('sunrise','sunset')
	ss$sunrise <- ceiling(ss$sunrise)
	ss$sunset <- floor(ss$sunset)



	hrs <- lapply(1:length(ss[,1]), function(i) seq(ss[i,1], ss[i,2]) )

	hours <- lapply(1:length(days), function(i) {
		sapply(1:length(hrs[[i]]), function(j) {
			
			if (nchar(hrs[[i]][j])==1) {
				paste0(0,hrs[[i]][j])

			} else if (nchar(hrs[[i]][j])==2) {
				hrs[[i]][j]	

			} else {
				stop('Incorrect number of characters for a time')
			}

			})
		})

		
	times <- lapply(1:length(days), function(i) {
			strptime(paste0(year, days[i], ' ',hours[[i]], ':00:00'),	format='%Y %j %H:%M:%S')
		})
	
	

	
	gdh <- lapply(1:length(days), function(i) {
		sapply(1:length(times[[i]]), function(j) {
			

			ind <- which(Tdat[,'dt']==times[[i]][j])
			
			Th <- Tdat[ind,'tair']
			
			
			if (type=='nocrit') {
				nocrit(Tb, To, Th)
				
			} else if (type=='anderson') {
				anderson(Th, Tb, To, Tc)
			
			} else if (type=='beta') {
				beta(Th, Tb, To, Tc)

			} else if (type=='trapezoid') {
				trapezoid(Th, Tb, To, Ts, Tc)

			} else {
				stop("Type must be 'nocrit', 'beta', or 'anderson'")
			}
				
		})
	})
		


	gdhs <- sapply(gdh, function(l) sum(l))
	
	gdhss <- sum(gdhs)

	return(gdhss)

}









gdd <- function(climdat, tbase, calctype='dd', seasontype='specify', start=100, end=250, ddpres=FALSE) {
	#climdat must have columns year, tmin, and tmax, it must also have the column day if calctype is tdd or sdd
	#calctype specifies what you want to calculate: degree days of a series of days ('dd'), total degree days in one
		#'dd' calculates the degree days of a series of days. This is the default
		#'tdd' calculates the total degree days in one season
		#season ('tdd'), or total degree days in a series of seasons ('sdd')
	
	#seasontype specifies the type of season you want to calculate your degree days for. 
		#'average' specifies that you want to take the average of the start and end dates provided in start and end and use that.
			#can only be used if calctype is sdd
		#'yearly' means that there are start and end days for each season, enter these as two vectors into start and end
			#can only be used if the calctype is sdd
		#'specify' means that you will specify a single start and end using start= and end= in the command, this is the default, the default start and end are 100 and 250

	#start and end: these can either be single numbers or vectors, they should be numbers for 'specify', and vectors for 'average' and 'yspec'
	
	#ddpres=TRUE indicates that the degree days have already been calculated and to not need to be calculated again
	if (calctype=='dd') {
		#calculates the growing degree days for a series of days
		if (all(c('tmin', 'tmax') %in% colnames(climdat))) {
			D <- sapply(1:length(climdat[,1]), function(i) dd(climdat[i,'tmin'],climdat[i,'tmax'],tbase))
			return(D)
		} else {
			stop("Climate data frame must contain the columns 'tmin' and 'tmax'. Please check your data and try again.")
		}
		



	} else if (calctype=='tdd') {
		#calculates the growing degree days total for all the days in one season

		if (all(c('tmin', 'tmax', 'day') %in% colnames(climdat))) {
			
			if (ddpres) {
				if (length(start)==1 & length(end)==1) {
				D2 <- data.frame(day=climdat[,'day'], dd=climdat[,'dd'])
				tD <- tdd(start, end, D2)
				return(tD)
			
				} else {
					stop("One start day and end day is needed for calctype 'tdd'. Please check your code and try again.")
				}
			
			} else {

				D <- sapply(1:length(climdat[,1]), function(i) dd(climdat[i,'tmin'],climdat[i,'tmax'],tbase))
		
				if (length(start)==1 & length(end)==1) {
					D2 <- data.frame(day=climdat[,'day'], dd=D)
					tD <- tdd(start, end, D2)
					return(tD)
			
				} else {
					stop("One start day and end day is needed for calctype 'tdd'. Please check your code and try again.")
				}
			}

		} else {
			stop("Climate data frame must contain the columns 'tmin', 'tmax', and 'day'. Please check your data and try again.")
		}




	} else if (calctype=='sdd') {
		#calculates the total number of growing degree days in a series of seasons
		#climdat must contain a column called year for calctype 'sdd'
		if (all(c('tmin','tmax','day','year') %in% colnames(climdat))) {
			q <- climdat
			yrs <- unique(q[,'year'])

			if (ddpres) {
				if (seasontype=='specify') {
					sD <- sapply(yrs, function(y) tdd(start,end,q[q$year==y, c('day','dd')]))
					return(sD)

				} else if (seasontype=='yearly') {
					sD <- sapply(1:length(yrs), function(i) tdd(start[i], end[i], q[q$year==yrs[i],c('day','dd')]))
					return(sD)

				} else if (seasontype=='average') {
					savg <- round(mean(start, na.rm=TRUE))
					eavg <- round(mean(end, na.rm=TRUE))

					sD <- sapply(yrs, function(y) tdd(savg, eavg, q[q$year==y,c('day','dd')]))
					return(sD)
				} else {
					stop("Did you make a typo? seasontype must be 'specify', 'yearly', or 'average'. Please check your code and try again.")
				}


			} else {


				D <- sapply(1:length(climdat[,1]), function(i) dd(climdat[i,'tmin'],climdat[i,'tmax'],tbase))
				q2 <- data.frame(year=q$year, day=q$day, dd=D)

				if (seasontype=='specify') {
					sD <- sapply(yrs, function(y) tdd(start,end,q2[q2$year==y,]))
					return(sD)

				} else if (seasontype=='yearly') {
					sD <- sapply(1:length(yrs), function(i) tdd(start[i], end[i], q2[q2$year==yrs[i],]))
					return(sD)

				} else if (seasontype=='average') {
					savg <- round(mean(start, na.rm=TRUE))
					eavg <- round(mean(end, na.rm=TRUE))

					sD <- sapply(yrs, function(y) tdd(savg, eavg, q2[q2$year==y,]))
					return(sD)

				} else {
					stop("Did you make a typo? seasontype must be 'specify', 'yearly', or 'average'. Please check your code and try again.")
				}
			}




		} else {
			stop("Climate data frame must contain the columns 'tmin', 'tmax', 'day', and 'year' for calctype 'sdd'.  Please check your data and try again.")
		}

		


	} else {
		stop("Did you make a typo? Calctype must be 'dd', 'tdd', or 'sdd'. Please check your code and try again.")
	}

}





temptrend <- function(name, tdat, sig=TRUE, pval=FALSE, colpalette='Blues') {
	require(reshape2)
	require(ggplot2)
	require(RColorBrewer)
	#tdat must have the columns location, year, day, and tmin
	#sig=true means you only want the points with significant slopes
	#pval=true means you want the colors to be given by the p-values
	names(tdat) <- c('loc','year', 'day', 'tmin')
	t <- tdat[tdat$loc==name,]
	t$nday <- paste0('day',t$day)
	t2 <- t[,c('year','nday','tmin')]
	
	tc <- dcast(t2, year ~ nday)
	#print(str(tc))


	rd <- paste0('day',1:366)

	reg <- lapply(rd, function(v) {
		form <- paste(v,'~ year')
		lm(form, data=tc)
		})


	bp <- sapply(1:length(reg), function(i) summary(reg[[i]])$coefficients[2,c(1,4)])
	bp <- as.data.frame(t(bp))
	
	bp <- data.frame(rd,bp, stringsAsFactors=FALSE)
	names(bp) <- c('day','b', 'pval')
	bp$d <- 1:366

	#making the colors for the pvalue if needed
	bp$cuts <- cut(bp$pval, c(0,0.01, 0.05, 0.1, 0.5,1))


	if (sig) {
		sig <- which(bp$pval<0.05)
		bps <- bp[sig,]
		tplot <- ggplot(bps)

		if (pval) {
			 tplot <- tplot + geom_point(aes(x=d, y=b, color=cuts)) + geom_hline(aes(yintercept=0)) + xlim(0,367) + labs(x='Day of the Year', y='Slope of Regression Line', title=paste('Minimum Temp Regression Slopes at', name)) #+ scale_color_brewer(palette=colpalette, values=c(0.01,0.05,0.1,0.5))
		
		} else {
			bps$cols <- sapply(1:length(bps$day), function(i) ifelse(bps$b[i]>0, 'red','blue')) 
			tplot <- tplot + geom_point(aes(x=d, y=b), color=bps$cols) + geom_hline(aes(yintercept=0)) + xlim(0,367) + labs(x='Day of the Year', y='Slope of Regression Line', title=paste('Minimum Temp Regression Slopes at', name))
		
		}
 	
	} else {
		tplot <- ggplot(bp)

		if(pval) {
			tplot <- tplot + geom_point(aes(x=d, y=b, color=cuts)) + geom_hline(aes(yintercept=0)) + xlim(0,367) + labs(x='Day of the Year', y='Slope of Regression Line', title=paste('Minimum Temp Regression Slopes at', name))# + scale_color_brewer(palette=colpalette, values=c(0.01,0.05,0.1,0.5))

		} else {
			bp$cols <- sapply(1:length(bp$day), function(i) ifelse(bp$b[i]>0, 'red','blue'))
			tplot <- tplot + geom_point(aes(x=d, y=b), color=bp$cols) + geom_hline(aes(yintercept=0)) + xlim(0,367) + labs(x='Day of the Year', y='Slope of Regression Line', title=paste('Minimum Temp Regression Slopes at', name))
		
		}

	}

	return(tplot)

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











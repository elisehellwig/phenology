# Author: Robert J. Hijmans
# License GPL3
# Version 0.1  January 2009

diTemp <- function(lat, date, tmin, tmax) {
	
	doyFromDate <- function(date) {
		date <- as.character(date)
		as.numeric(format(as.Date(date), "%j"))
	}

	daylength <- function(lat, doy) {
		if (class(doy) == 'Date' | class(doy) == 'character') { 
			doy <- doyFromDate(doy) 
		}
		lat[lat > 90 | lat < -90] <- NA 
		doy <- doy %% 365
		
		# "A Model Comparison for Daylength as a Function of Latitude and Day of the Year."
		#Ecological Modeling 80 (1995): 87-95.
		P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860*(doy-186)))))
		a <-  (sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(P)) / (cos(lat * pi/180) * cos(P));
		a <- pmin(pmax(a, -1), 1)
		DL <- 24 - (24/pi) * acos(a)
		return(DL)
	}
			
	TC <- 4.0
    P <- 1.5
	dayl <- daylength(lat, doyFromDate(date))
	nigthl <- 24-dayl
    sunris <- 12 - 0.5 * dayl
    sunset <- 12 + 0.5 * dayl
	
	hr <- 1:24
	# a: between midnight and sunrise;
	a <- hr < sunris
	#  b: between sunrise and normal time that tmax is reached (after noon)
	b <- hr < (12+P) & ! a
	#  c: between time of tmax and sunset;
	c <- which(hr < sunset & ! (a | b)	)
	# d: dhour between sunset and midnight;
	d <- which(hr >= sunset)
	a <- which(a)
	b <- which(b)
	
	hrtemp <- vector(length=24)
	tsunst <- tmin+(tmax-tmin)*sin(pi*(dayl/(dayl+2*P)))
	hrtemp[a] <- (tmin-tsunst*exp(-nigthl/TC)+(tsunst-tmin)*exp(-(hr[a]+24-sunset)/TC))/(1-exp(-nigthl/TC))
	hrtemp[b] <- tmin+(tmax-tmin)*sin(pi*(hr[b]-sunris)/(dayl+2*P))
	hrtemp[c] <- tmin+(tmax-tmin)*sin(pi*(hr[c]-sunris)/(dayl+2*P))
	hrtemp[d] <- (tmin-tsunst*exp(-nigthl/TC)+(tsunst-tmin)*exp(-(hr[d]-sunset)/TC))/(1-exp(-nigthl/TC))

	return(hrtemp)
}


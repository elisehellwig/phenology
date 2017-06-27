chill <- function(x, yr, fday, cr, tempname='temp', dayname='sday', parameters=c(1.6, 277, 2.567e18, 1.28888e4, 1.395e5/2.567e18, 1.28888e4-4.1535e3, 277, 298, 309, 1.0), heat=FALSE, totalchill=FALSE, base=283, optimal=298, critical=308) {
	#this is a function that calculates chill accumulation for a chill season,
		#total chill accumulated, and heat accumulated after chill for a season
	#x is a data.frame with the hourly temperature data in it
	#yr is the year containing the flowering even you want to predict
	#fday is the day of flowering (preferably the beginning of it)
	#cr is the chill requirement
		#tempname is the name of the variable with the hourly temperature
		#day name is the name of the variable of the day of the season. the first day of the season
			#is defined as the day you start (potentially) accumulating chill
	#the parameters argument is the vector of parameters 
		#[slp, tetmlt, a1, e1, aa, ee, tempB, temp0, tempC, F]
	#base, critical and optimal are the cardinal temperatures for the heat accumulation model
		#which is from Anderson 1986
	#NOTE: temperatures must be in Kelvin

	#the model is based off of Fishman et al. 1987b, Temperature Dependency of Dormancy Breaking in Plants
	source('R/functions/thermalfunctions.R')


	x <- x[x$year==yr, c(dayname, tempname)]
	names(x) <- c('day', 'temp')

	#calculating parameter values for calculating chill accumulation
	ftmprt <- slp*tetmlt*(x$temp-tetmlt)/x$temp
	sr <- exp(ftmprt)
	xi <- sr/(1+sr)
	xs <- a0*exp(e0/temp)
	ak1 <- a1*exp(-e1/temp)

	 
	
	interS <- rep(0, length(x$day))
	interE <- rep(0, length(x$day))

	interE[1] <- xs[1] - xs[1]*exp(-ak1[1])

	i <- 2 #index
	cu <- rep(0, length(x[,1])) #chill units accumulated

	while(sum(cu)<cr) { #calculating interE, interS and cu

		if (interE[i-1]<1) {
			interS[i] <- interE[i-1]
		} else {
			interS[i] <- interE[i-1]*(1 - xi[i-1])
		}

		interE[i] <- xs[i] - (xs[i]-interS[i])*exp(-ak1[i])


		cu[i] <- interE[i]*xi[i] 
	
		i <- i+1
	}

	crh <- x$day[i] # hour chill requirement was reached

	j <- i #creating a second index so there can be an index for both total chill acumulation and heat accumulation

	if (totalchill) {
		while (x$day[i]<fday) { #calculates the chill accumulated during the rest of the time before flowering

			if (interE[i-1]<1) {
				interS[i] <- interE[i-1]
			} else {
				interS[i] <- interE[i-1]*(1 - xi[i-1])
			}

			interE[i] <- xs[i] - (xs[i]-interS[i])*exp(-ak1[i])


			cu[i] <- interE[i]*xi[i] 
		
			i <- i+1
		}

		
	} else {}

	if (heat) { # calculates heat accumulation using anderson model for the rest of the season until bloom
		heatdays <- crh:(fday-1)
		xh <- x[x$day %in% heatdays, ] #data.frame for calculating heat accumulation

		hu <- anderson(xh$temp, base, optimal, critical)
	} else {}
	

	y <- crh

	if (totalchill) {
		y <- c(y, sum(cu))
	}

	if (heat) {
		y <- c(y, hu)
	}

	return(y)



}

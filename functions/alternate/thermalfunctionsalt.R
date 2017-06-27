

nocrit <- function(Th, Tb, To) {
	#see notebook
	#Tb - base temperature (no GDH below this)
	#To - optimum temperature (maximum GDH here)
	#Th - current temperature

	temp <- Th - Tb

	temp[temp<0] <- 0
	temp[temp >= (To-Tb)] <- To - Tb
	
	tsum <- sum(temp)

	return(tsum)
	}
}


anderson <- function(Th, Tb, To, Tc, sum=TRUE) {
	#from Anderson et al. 1986

	Th[Th<=Tb] <- 0
	Th[Th>Tb & Th<=To] <- ((To - Tb)/2)*(1 + cos(pi + pi*(Th[Th> Tb & Th<=To] - Tb)/(To - Tb) )) 
	Th[Th>To & Th<=Tc] <- (To - Tb)*(1 + cos(pi/2 + pi/2*(Th[Th>To & Th<=Tc] - To)/(Tc - To) ))
	Th[Th>Tc] <- 0

	tsum <- sum(Th)

	if(sum) {
		return(tsum)
	} else {
		return(Th)
	}


}

beta <- function(x, Tb, To, Tc, a=0, b=30) {
	#from Marra 2002
	#x is current temp
	#Tb is base temperature
	#To is optimal temperature
	#Tc is critical temperature

	c <- To
	d <- Tb - Tc
	e <- 2 - (Tb/To)
	f <- Tc/To 
	m <- e - 1/e + f - 2
	n <- f - 1/e + f - 2


	q <- (x - c + d*m)/d
	print((1-q))
	
	y = a + b*(q^(e-1)) * ((1-q)^(f-1)) / ((m^(e-1)) * (n^(f-1))) 

	#tsum <- sum(y)
	return(y)



}

trapezoid <- function(Th, Tb, To, Ts, Tc) {


	pars1 <- c(Tb, To, Ts, Tc)
	#print(pars1)
	
	temp <- Th - Tb

	temp[temp<0] <- 0
	temp[temp>(To-Tb) & temp<=(Ts-Tb)] <- To - Tb
	
	temp[temp>(Ts-Tb) & temp<=(Tc-Tb)] <-  (Tc - Tb) - temp[temp>(Ts-Tb) & temp<=(Tc-Tb)]
	temp[temp>=(Tc-Tb)] <- 0
	

	tsum <- sum(temp)

	return(tsum)
	

}









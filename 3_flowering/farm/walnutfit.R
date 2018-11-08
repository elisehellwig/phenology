start <- proc.time()


library(optimx)

source('generalfunctions.R')
source('thermalfunctions.R')
source('secondlevel.R')

load('davishourlytemperature.RData')
load('walnutseasonlength.RData')

cults <- c('Chandler', 'Franquette', 'Payne')

#################################################################
minslen <- sapply(cults, function(c) 1:(min(w[w$cultivar==c, 'slen'])-1))

ncwalnutoptim <- lapply(1:3, function(i) {
	lapply(minslen[[i]], function(d) {
	optimx(par=c(4, 25), function(x) minrmse(x, fdat=w, tdat=dht, cult=cults[i], typ='nocrit',  sumlength=d, flowername='fday'))
	})
})
save(ncwalnutoptim, file='ncwalnutoptim.Rdata')


end <- proc.time()
timer <- end-start
capture.output(timer,file='time.txt', append=TRUE)

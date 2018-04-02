datapath <-  '/Users/echellwig/Drive/Phenology/data/historydata'
library(phenoclim)
library(plyr)

w <- read.csv(file.path(datapath, 'walnutclean.csv'))
#dht <- read.csv(file.path(datapath, 'davishourlytemp.csv'))


##################################################################
##################################################################
vars <- levels(w$cultivar)

ycw <- yearconversion(w, id.vars='cultivar')
y <- ldply(vars, function(cv) yearconversion(w, id.vars='cultivar', var=cv))
se <- startEnd(y$event1, y$length0, forward=FALSE)

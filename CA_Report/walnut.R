drivepath <- '/Users/echellwig/Drive/Phenology'
library(phenoclim)
library(plyr)
source('functions/helperfunctions.R')

#load temperature data (from NOAA)
temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE) 

#load phenology data (from walnuts)
w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
###################################################

vars <- c('Chandler') #vector of all the walnut cultivars
forms <- c('anderson', 'gdd','gddsimple','linear','flat','triangle', 
           'asymcur') #vector of all the functional form names

forms2 <- c('anderson') 
#list of initial parameter values for each of the functional forms
initpars <- initialparlist(c(3,1,1,1,2,3,3)) 

#seeds for each of the cultivars
seeds<- c(224, 23994, 20399, 22020, 340, 1, 30030, 4950, 111111, 39409, 992, 6)

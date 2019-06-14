drivepath <- '/Volumes/GoogleDrive/My Drive/Phenology'
library(phenoclim)
library(plyr)
library(dplyr)
source('functions/helperfunctions.R')

#load temperature data (from NOAA)
temps <- read.csv(file.path(drivepath, 'data/phenology/dailyhourlytemp.csv'),
                  stringsAsFactors = FALSE) 
temps <- filter(temps, loc=="Davis")

#load phenology data (from walnuts)
w <- read.csv(file.path(drivepath, 'data/prediction/walnutclean.csv'))
###################################################

vars <- levels(w$cultivar) #vector of all the walnut cultivars
forms <- c('anderson', 'gdd','gddsimple','linear','flat','triangle', 
           'asymcur') #vector of all the functional form names

forms2 <- c('anderson') 
#list of initial parameter values for each of the functional forms
initpars <- initialparlist(c(3,1,1,1,2,3,3)) 

#seeds for each of the cultivars
seeds<- c(224, 23994, 20399, 22020, 340, 1, 30030, 4950, 111111, 39409, 992, 6)

###################################################
#DT complex

#creating the ParameterList for the DT model
tpl <- lapply(1:length(forms), function(i) {
    parameterlist(1, 'DT', FALSE, forms[i], initpars[[i]], 60)
})

#Fitting the DT plant model for each cultivar
tpm <- lapply(vars, function(v) {
    pdat <- w[w$cultivar==v, ]
    plantmodel(pdat, temps, tpl, c(0,0,0,0), c(160, 100, 100, 100),
               iterations=200, cores=3L, ensemble=TRUE)
})


names(tpm) <- vars #giving the models names

#saving the DT plant model
saveRDS(tpm, file.path(drivepath, 'Resutls/prediction/DTplantmodel.RDS'))
tpm <- readRDS(file.path(drivepath, 
                         'Results/prediction/DTplantmodel.RDS'))


#crossvalidating the DT plant model
tpmCV <- lapply(1:length(vars), function(i) {
    print(vars[i])
    crossval(tpm[[i]], temps, 5, seeds[i], 'rmsd', c(0,0,0,0), 
                   c(160, 100, 100, 100), cores=3L, ensemble=TRUE)
})
names(tpmCV) <-vars

#saving the crossvalidation
saveRDS(tpmCV, file.path(drivepath, 
                         'Results/prediction/DTplantmodelCV.RDS'))

####################
#DT simple

#creating the Paramterlists for the Simplified DT plant model
tpsl <- lapply(1:length(forms2), function(i) {
    parameterlist(1, 'DT', TRUE, forms2[i], initpars[[i]], 60)
})

#fitting the simplified DT plant model
tpms <- lapply(vars, function(v) {
    pdat <- w[w$cultivar==v, ]
    plantmodel(pdat, temps, tpsl, c(0), c(160),
               iterations=100, cores=3L)
})

names(tpms) <-vars

saveRDS(tpms, file.path(drivepath, 
                        'Results/prediction/DTsimpleplantmodel.RDS'))

######################
#TTT complex

#creating the ParameterLists for the TTT plant models
dpl <- lapply(1:length(forms), function(i) {
    parameterlist(1, 'TTT', FALSE, forms[i], initpars[[i]], 60)
})

#fitting the TTT plant models
dpm <- lapply(vars, function(v) {
    pdat <- w[w$cultivar==v, ]
    plantmodel(pdat, temps, dpl, c(0,0,0,0), c(50000, 100, 100, 100),
               iterations=200, cores=3L, ensemble = TRUE)
})

#giving the TTT plant models names
names(dpm) <-vars

#saving the TTT plant models
saveRDS(dpm, file.path(drivepath, 'Results/prediction/TTTplantmodel.RDS'))

dpm <- readRDS(file.path(drivepath, 'Results/prediction/TTTplantmodel.RDS'))


#Running crossvalidation on the TTT plant models
dpmCV <- lapply(1:length(vars), function(i) {
    print(vars[i])
    crossval(dpm[[i]], temps, 5, seeds[i], 'rmsd', c(0,0,0,0),
             c(50000, 100, 100, 100), cores=3L, ensemble=TRUE)
})

#naming the CV models with the cultivar names
names(dpmCV) <- vars

#saving the crossvalidation results
saveRDS(dpmCV, file.path(drivepath,
                         'Results/prediction/TTTplantmodelCV.RDS'))


######################
#TTT simple

#creating the ParameterList for the simplified TTT plant models
dpls <- lapply(1:length(forms), function(i) {
    parameterlist(1, 'TTT', TRUE, forms[i], initpars[[i]], 60)
})


#fitting the simplified TTT plant models
dpms <- lapply(vars, function(v) {
    print(v)
    pdat <- w[w$cultivar==v, ]
    plantmodel(pdat, temps, dpls, c(0,0,0,0), c(100000,100,100,100),
               iterations=100, cores=3L, ensemble=TRUE)
})
names(dpms) <-vars

#saving simplified TTT plant models
saveRDS(dpms, file.path(drivepath,
                        'Results/prediction/TTTsimpleplantmodel.RDS'))

dpms <- readRDS(file.path(drivepath, 
                          'Results/prediction/TTTsimpleplantmodel.RDS'))
    
    
dpmsCV <- lapply(1:length(vars), function(i) {
    print(vars[i])
    crossval(dpms[[i]], temps, 5, seeds[i], 'rmsd', c(0,0,0,0), 
             c(100000,100,100,100), cores=3L, iterations=200, ensemble=TRUE)
})
names(dpmsCV) <-vars

saveRDS(dpmsCV, file.path(drivepath, 
                         'Results/prediction/TTTsimpleplantmodelCV.RDS'))

###################################################


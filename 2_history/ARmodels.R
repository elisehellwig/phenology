library(plyr)
Rpath <- '~/Drive/Phenology/Results/history'

af <- read.csv(file.path(Rpath, 'almondspring.csv'))

###################################################
afc <- af[af$location=='Chico', ]
N <- tapply(afc$bloom, list(afc$cultivar), length)
vars <- names(N)

datlist <- lapply(vars, function(v) {
    afc[afc$cultivar==v, ]
})

ARlist <- lapply(1:3, function(i) {
    dl <- datlist[[i]]
    n <- N[i]
    
    ARv <- dl[1:(n-1),'bloom']
    ARdat <- dl[2:n, ]
    ARdat$ARbloom <- ARv
    
    ARdat
})

mods <- lapply(1:3, function(i) {
    summary(lm(bloom ~ Heat, data=datlist[[i]]))
})


ARmods <- lapply(1:3, function(i) {
    summary(lm(bloom ~ Heat + ARbloom, data=ARlist[[i]]))
})





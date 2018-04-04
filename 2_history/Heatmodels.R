library(plyr)
Rpath <- '~/Drive/Phenology/Results/history'
source('functions/extractlm.R')

af <- read.csv(file.path(Rpath, 'almondspring.csv'))

###################################################
locs <- c("Chico",'Modesto')

aflist <- lapply(locs, function(l) af[af$location==l, ])
vars <- names(N[[1]])

datlist <- lapply(aflist, function(df) {
    lapply(vars, function(v) df[df$cultivar==v, ])
})

mods <- lapply(1:length(datlist), function(i) {
    lapply(1:length(datlist[[i]]), function(j) {
        summary(lm(bloom ~ Heat, data=datlist[[i]][[j]]))
    })
})


moddat <- ldply(1:length(datlist), function(i) {
    ldply(1:length(datlist[[i]]), function(j) {
        extractLM(mods[[i]][[j]], loc=locs[i], cultivar=vars[j])
    })
})

write.csv(moddat, file.path(Rpath, 'AlmondHeatresults.csv'), row.names = FALSE)




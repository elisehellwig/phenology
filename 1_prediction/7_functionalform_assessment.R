setwd('/Users/echellwig/Research/phenology')
drivepath <- '/Users/echellwig/Drive/Phenology'
library(phenoclim)
library(plyr)
library(reshape2)


source('functions/accumulatedtemps.R')


htemps <- read.csv(file.path(drivepath, 'data/walnutdata/davishourlytemp.csv'), 
                   stringsAsFactors = FALSE)
w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
ms <- read.csv(file.path(drivepath, 'Results/walnutpaper/modelsummary.csv'),
               stringsAsFactors = FALSE)
w$length1 <- w$event2 - w$event1



ff <- c('linear','flat','triangle','asymcur','anderson')
ffrows <- which(ms$form %in% ff)
ms <- ms[ffrows, ]
vars <- levels(w$cultivar)
ctnames <- c('base','optimal','critical')

freqpal <- c('#ffff00',"#FF0000", '#00ff00','#0000ff')
##################################
alltemps <- -5:50
vardf <- lapply(vars, function(v) w[w$cultivar==v, ])

###Grouping the temps the trees experienced and doing frequency values
fullDT<- ldply(1:length(vars), function(i) {
    fdt <- which(ms$cultivar==vars[i] & ms$type=='thermal' & ms$complexity=='full')
    maxlen <- max(ms[fdt, 'length'])
    dt <- tempfreq(vardf[[i]], htemps, maxlen, binsize=3)
    dt$cultivar <- vars[i]
    dt
})
fullDT$type <- 'DT'
fullDT$complexity <- 'full'


fullTTT<- ldply(1:length(vars), function(i) {
    fttt <- which(ms$cultivar==vars[i] & ms$type=='day' & ms$complexity=='full')
    lengths <- TTtoDay(ms[fttt,], vardf[[i]], htemps)
    maxlen <- max(lengths)
    ttt <- tempfreq(vardf[[i]], htemps, maxlen, binsize=3)
    ttt$cultivar <- vars[i]
    ttt
})
fullTTT$type <- 'TTT'
fullTTT$complexity <- 'full'


simplifiedTTT <- ldply(1:length(vars), function(i) {
    sttt <- which(ms$cultivar==vars[i] & ms$type=='day' & ms$complexity=='simpliefied')
    ttt <- tempfreq(vardf[[i]], htemps, vardf[[i]][,'length1'], binsize = 3)
    ttt$cultivar <- vars[i]
    ttt
})
simplifiedTTT$type <- 'TTT'
simplifiedTTT$complexity <- 'simplified'


temptable <- rbind(fullDT, fullTTT, simplifiedTTT)

#######################################################

mcDTfull <- ldply(vars, function(v) {
    msrows <- which(ms$cultivar==v & ms$type=='thermal')
    msv <- ms[msrows, c(ctnames,'form')]
    df <- as.data.frame(collateTT(alltemps, msv[,ctnames], msv$form))
    df$cultivar <- v
    df
})
mcDTfull$type <- 'DT'
mcDTfull$complexity <- 'full'

mcTTTfull <- ldply(vars, function(v) {
    msrows <- which(ms$cultivar==v & ms$type=='day' & ms$complexity=='full')
    msv <- ms[msrows,]
    df <- as.data.frame(collateTT(alltemps, msv[,ctnames], msv$form))
    df$cultivar <- v
    df
})
mcTTTfull$type <- "TTT"
mcTTTfull$complexity <- "full"

mcTTTsimplified <- ldply(vars, function(v) {
    msrows <- which(ms$cultivar==v & ms$type=='day' & ms$complexity=='simplified')
    msv <- ms[msrows,]
    df <- as.data.frame(collateTT(alltemps, msv[,ctnames], msv$form))
    df$cultivar <- v
    df
})
mcTTTsimplified$type <- 'TTT'
mcTTTsimplified$complexity <- 'simplified'


mcT <- rbind(mcDTfull, mcTTTsimplified, mcTTTfull)

#######################################################

mc <- merge(mcT, temptable, all.x=TRUE)
mc[which(is.na(mc$freq3)),'freq3'] <- 0

mc[mc$temp< -3, 'bin'] <- -3
mc[is.na(mc$Freq), 'Freq'] <- 0

write.csv(mc,
          file.path(drivepath, 'Results/functionalformcomparison.csv'), 
          row.names = FALSE)






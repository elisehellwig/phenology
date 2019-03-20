
# Setup -------------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(reshape2)
library(plyr)
library(phenoclim)
source('functions/extractlm.R')

options(stringsAsFactors = FALSE)

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'
phenologypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/phenology'

harv <- readRDS(file.path(historypath, 'harvest.RDS'))
tts <- readRDS(file.path(historypath, 'ThermalTimeSeries.RDS'))
locVar <- read.csv(file.path(historypath, 'SeasonLengthParameters.csv'))


# Almonds -----------------------------------------------------------------

aLocVar <- filter(locVar, crop=='almond', cultivar!='Sonora')

alist <- lapply(1:nrow(aLocVar), function(i) {
    filter(harv, crop=='almond', loc==aLocVar[i, 'loc'], 
           cultivar==aLocVar[i, 'cultivar'])
})

attlist <- lapply(1:nrow(aLocVar), function(i) {
    filter(tts, crop=='almond', loc==aLocVar[i, 'loc'], 
           cultivar==aLocVar[i, 'cultivar'])
})

ALengthTimeMod <- lapply(alist, function(df) {
    lm(length1 ~ year, data=df)
})

ALengthTimeSum <- lapply(ALengthTimeMod, function(mod) summary(mod))


ALengthMod <- lapply(alist, function(df) {
    lm(length1 ~ thermal, data=df)
})

ALengthSum <- lapply(ALengthMod, function(mod) summary(mod))


AThermalTimeMod <- lapply(attlist, function(df) {
    lm(thermal ~ year, data=df)
})

AThermalTimeSum <- lapply(AThermalTimeMod, function(mod) summary(mod))



# Prune -------------------------------------------------------------------


p <- filter(harv, crop=='prune')

pttlist <- lapply(c('Chico','Parlier'), function(l) {
    filter(tts, crop=='prune', loc==l)
})


PThermalTimeMod <- lapply(pttlist, function(df) {
    lm(thermal ~ year, data=df)
})

PThermalTimeSum <- lapply(PThermalTimeMod, function(mod) summary(mod))

PLengthMod <- lm(length1 ~ thermal, data=p)

PLengthTimeMod <- lm(length1 ~ year, data=p)


# Walnut ------------------------------------------------------------------

wLocVar <- filter(locVar, crop=='walnut')

wlist <- lapply(1:nrow(wLocVar), function(i) {
    filter(harv, crop=='walnut', loc==wLocVar[i, 'loc'], 
           cultivar==wLocVar[i, 'cultivar'])
})

wttlist <- lapply(1:nrow(wLocVar), function(i) {
    filter(tts, crop=='walnut', loc==wLocVar[i, 'loc'], 
           cultivar==wLocVar[i, 'cultivar'])
})

WThermalTimeMod <- lapply(wttlist, function(df) {
    lm(thermal ~ year, data=df)
})


WThermalTimeSum <- lapply(WThermalTimeMod, function(mod) summary(mod))


WLengthMod <- lapply(wlist, function(df) {
    lm(length1 ~ thermal, data=df)
})

WLengthSum <- lapply(WLengthMod, function(mod) summary(mod))

WLengthTimeMod <- lapply(wlist, function(df) {
    lm(length1 ~ year, data=df)
})

WLengthTimeSum <- lapply(WLengthTimeMod, function(mod) summary(mod))


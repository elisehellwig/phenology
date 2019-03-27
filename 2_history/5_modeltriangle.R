
# Setup -------------------------------------------------------------------
library(plyr)
library(tidyverse)
library(lubridate)
library(reshape2)
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

extractLM(AThermalTimeMod[[1]])

AThermVars <- rep(paste0('GDH', aLocVar[,'threshold']), each=2)

a1 <- formatLM(aLocVar, ALengthTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response='Season Length', .before=4)

a2 <- formatLM(aLocVar, ALengthMod, dropIntercept = FALSE) %>% 
    add_column(predictor=AThermVars, .before=3) %>% 
    add_column(response='Season Length', .before=4)

a3 <-formatLM(aLocVar, AThermalTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response=AThermVars, .before=4)




a <- rbind(a1,a2,a3)

a <- add_column(a, crop='almond', .before=1)

# Prune -------------------------------------------------------------------

pLocVar <- filter(locVar, crop=='prune')


p <- filter(harv, crop=='prune')

PThermalTimeMod <- lm(thermal ~ year, data=p)

PLengthMod <- lm(length1 ~ thermal, data=p)

PLengthTimeMod <- lm(length1 ~ year, data=p)

p1 <- formatLM(pLocVar, PLengthTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response='Season Length', .before=4)

p2 <- formatLM(pLocVar, PLengthMod, dropIntercept = FALSE) %>% 
    add_column(predictor=paste0('GDH', pLocVar[1,'threshold']), .before=3) %>% 
    add_column(response='Season Length', .before=4)

p3 <-formatLM(pLocVar, PThermalTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response=paste0('GDH', pLocVar[1,'threshold']), .before=4)

p <- rbind(p1,p2,p3)

p <- add_column(p, crop='prune', .before=1)

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


WThermVars <- rep(paste0('GDH', wLocVar[,'threshold']), each=2)

w1 <- formatLM(wLocVar, WLengthTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response='Season Length', .before=4)

w2 <- formatLM(wLocVar, WLengthMod, dropIntercept = FALSE) %>% 
    add_column(predictor=WThermVars, .before=3) %>% 
    add_column(response='Season Length', .before=4)

w3 <-formatLM(wLocVar, WThermalTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response=WThermVars, .before=4)




w <- rbind(w1,w2,w3)

w <- add_column(w, crop='walnut', .before=1)


# Combine and save --------------------------------------------------------


modtable <- rbind(a, p, w)

saveRDS(modtable, file.path(historypath, 'ModelTable.RDS'))




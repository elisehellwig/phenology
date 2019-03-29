
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

#creating indexing data frame
aLocVar <- filter(locVar, crop=='almond', cultivar!='Sonora')
aLocVar30 <- mutate(aLocVar, threshold=30)

#separating harvest data by cultivar and location
alist <- lapply(1:nrow(aLocVar), function(i) {
    filter(harv, crop=='almond', loc==aLocVar[i, 'loc'], 
           cultivar==aLocVar[i, 'cultivar'])
})


#separating thermal time data by cultivar and location
attlist <- lapply(1:nrow(aLocVar), function(i) {
    filter(tts, crop=='almond', loc==aLocVar[i, 'loc'], 
           cultivar==aLocVar[i, 'cultivar'])
})

#running season length over time models
ALengthTimeMod <- lapply(alist, function(df) {
    lm(length1 ~ year, data=df)
})

#running model using thermal time to predcit season length
ALengthMod <- lapply(alist, function(df) {
    lm(length1 ~ thermal, data=df)
})

#running GDH30 to predict season length models
ALength30Mod <- lapply(alist, function(df) {
    lm(length1 ~ thermal30, data=df)
})

#running thermal time over time models.
AThermalTimeMod <- lapply(attlist, function(df) {
    lm(thermal ~ year, data=df)
})


#creating thermal time variable names
AThermVars <- rep(paste0('GDH', aLocVar[,'threshold']), each=2)

#extracting data from season length over time model
a1 <- formatLM(aLocVar, ALengthTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response='Season Length', .before=4)

#extracting data from GDH vs season length model
a2 <- formatLM(aLocVar, ALengthMod, dropIntercept = FALSE) %>% 
    add_column(predictor=AThermVars, .before=3) %>% 
    add_column(response='Season Length', .before=4)

#wxtracting data from thermal time over time model
a3 <-formatLM(aLocVar, AThermalTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response=AThermVars, .before=4)

#extracting data from GDH30 vs. season length model
a4 <- formatLM(aLocVar30, ALength30Mod, dropIntercept = FALSE)  %>% 
    add_column(predictor='GDH30', .before=3) %>% 
    add_column(response='Season Length', .before=4) 

#combining all the model data
a <- rbind(a1,a2,a3,a4)

#adding a column identifying almonds
a <- add_column(a, crop='almond', .before=1)


alist2 <- lapply(1:length(alist), function(i) {
    alist[[i]]$fitlength <- predict(ALengthMod[[i]],
                                    data.frame(thermal=alist[[i]]$thermal))
    alist[[i]]
})


aharv <- ldply(1:length(alist), function(i) {
    alist2[[i]]$fitlength30 <- predict(ALength30Mod[[i]],
                                    data.frame(thermal30=alist[[i]]$thermal30))
    alist2[[i]]
})


# Prune -------------------------------------------------------------------

#creating index data.frame
pLocVar <- filter(locVar, crop=='prune')
pLocVar30 <- mutate(pLocVar, threshold=30)

#filtering out prune data
p <- filter(harv, crop=='prune')

#modeling GDH over time
PThermalTimeMod <- lm(thermal ~ year, data=p)

#Relating season length to GDH
PLengthMod <- lm(length1 ~ thermal, data=p)

#RElating season length to GDH30
PLength30Mod <- lm(length1 ~ thermal30, data=p)

#Looking at season length over time
PLengthTimeMod <- lm(length1 ~ year, data=p)

#extracting data from all the models.
p1 <- formatLM(pLocVar, PLengthTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response='Season Length', .before=4)

p2 <- formatLM(pLocVar, PLengthMod, dropIntercept = FALSE) %>% 
    add_column(predictor=paste0('GDH', pLocVar[1,'threshold']), .before=3) %>% 
    add_column(response='Season Length', .before=4)

p3 <-formatLM(pLocVar, PThermalTimeMod, dropIntercept = FALSE) %>% 
    add_column(predictor='Year', .before=3) %>% 
    add_column(response=paste0('GDH', pLocVar[1,'threshold']), .before=4)

p4 <- formatLM(pLocVar30, PLength30Mod, dropIntercept = FALSE) %>% 
    add_column(predictor='GDH30', .before=3) %>% 
    add_column(response='Season Length', .before=4)

#binding all the model data together
ps <- rbind(p1,p2,p3,p4)

ps <- add_column(ps, crop='prune', .before=1)

p$fitlength <- predict(PLengthMod, 
                       data.frame(thermal=p$thermal))

p$fitlength30 <- predict(PLength30Mod, 
                         data.frame(thermal30=p$thermal30))
# Walnut ------------------------------------------------------------------

wLocVar <- filter(locVar, crop=='walnut')
wLocVar30 <- mutate(wLocVar, threshold=30)

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


WLengthMod <- lapply(wlist, function(df) {
    lm(length1 ~ thermal, data=df)
})

WLength30Mod <- lapply(wlist, function(df) {
    lm(length1 ~ thermal30, data=df)
})


WLengthTimeMod <- lapply(wlist, function(df) {
    lm(length1 ~ year, data=df)
})


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


w4 <- formatLM(wLocVar30, WLength30Mod, dropIntercept = FALSE) %>% 
    add_column(predictor='GDH30', .before=3) %>% 
    add_column(response='Season Length', .before=4)


w <- rbind(w1,w2,w3,w4)

w <- add_column(w, crop='walnut', .before=1)


wlist2 <- lapply(1:length(wlist), function(i) {
    wlist[[i]]$fitlength <- predict(WLengthMod[[i]],
                                    data.frame(thermal=wlist[[i]]$thermal))
    wlist[[i]]
})


wharv <- ldply(1:length(wlist2), function(i) {
    wlist2[[i]]$fitlength30 <- predict(WLength30Mod[[i]],
                                       data.frame(thermal30=wlist[[i]]$thermal30))
    wlist2[[i]]
})

# Combine and save --------------------------------------------------------


modtable <- rbind(a, ps, w)
saveRDS(modtable, file.path(historypath, 'ModelTable.RDS'))

harv <- rbind(aharv, p, wharv)
saveRDS(harv, file.path(historypath, 'harvestfit.RDS'))

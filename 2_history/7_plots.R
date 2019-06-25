
# Setup -------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)

source('functions/triplot.R')

historypath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/history'

harvest <- readRDS(file.path(historypath, 'harvest.RDS'))
asl <- filter(harvest, crop=='almond')
tts <- readRDS(file.path(historypath, 'ThermalTimeSeries.RDS'))
atts <- filter(tts, crop=='almond', cultivar=='Nonpareil')


# plotting attempts -------------------------------------------------------


a <- merge(asl, atts, all=TRUE, by=c('cultivar','loc','year','event1','crop',
                                     'thermal'))

triplot(a, 'Nonpareil', loc=TRUE, threshold=54, alims=c(1995, 2006), 
        bsmooth=TRUE, csmooth=TRUE)


groupvarlabs <- c('Season Length ~ Year', 'Season Length ~ Thermal Sum',
                  'Thermal Sum ~ Year')

a <- select(a, c(cultivar,loc,year,thermal,length1)) 
a$thermal2 <- a$thermal

a1 <- a[complete.cases(a), ]
a1 <- select(a1, cultivar, loc, x=year, y=length1)
a1$groupvar <- 'A'


a2 <- a[complete.cases(a), ]
a2 <- select(a2, cultivar, loc, x=thermal, y=length1)
a2$groupvar <- 'B'

a3 <- a
a3 <- select(a3, cultivar, loc, x=year, y=thermal)
a3$groupvar <- 'C'

afinal <- rbind(a1,a2,a3)
afinal <- filter(afinal, cultivar=='Nonpareil')
afinal$x <- as.integer(afinal$x)


aplot <- ggplot(data=afinal[afinal$groupvar=='A', ]) +
    geom_point(aes(x=x, y=y, shape=loc)) + theme_classic() +
    labs(x="Year", y='Season Length (days)') + xlim(c(1995, 2006)) + 
    guides(shape=FALSE)


bplot <- ggplot(data=afinal[afinal$groupvar=='B', ]) +
    geom_point(aes(x=x, y=y, shape=loc)) + theme_classic() +
    labs(x="Thermal Sum (GDH)", y='Season Length (days)') + guides(shape=FALSE)
    
cplot <- ggplot(data=afinal[afinal$groupvar=='C', ]) +
    geom_point(aes(x=x, y=y, shape=loc)) + theme_classic() +
    labs(x="Year", y='Thermal Sum (GDH)') + guides(shape=FALSE)

legendplot <- ggplot(data=afinal) + geom_point(aes(x=x, y=y, shape=loc)) +
    scale_shape(name='Location')

shapelegend <- getlegend(legendplot)

triplots <- arrangeGrob(aplot,bplot,cplot, nrow=3)
grid.arrange(triplots, shapelegend , ncol = 2, widths = c(3/4,1/4))

names(a)[c(5,9)] <- c('temperature','harvest')

pairs(a[,c('year','temperature','harvest')], col=as.factor(a$loc))

library(dplyr)
library(reshape2)
library(tidyverse)
datapath <- '/Volumes/GoogleDrive/My Drive/Phenology/data/'

options(stringsAsFactors = FALSE)

mt <- read.csv(file.path(datapath,'historydata/monthlytemperatures.csv'))
w <- read.csv(file.path(datapath, 'walnutdata/walnutclean.csv'))
a <- read.csv(file.path(datapath, 'historydata/almondclean.csv'))
p <- read.csv(file.path(datapath, 'historydata/pruneclean.csv'))


# Remove excess -----------------------------------------------------------

mtr <- mt %>% select('loc'='nearest','month'='nmonth','year'='year',
                     'min'='Minimum','max'='Maximum','avg'='Average')
mtc <- reshape(mtr, idvar=c('year','loc'), timevar='month', 
               direction='wide')



# Merge with phenology ----------------------------------------------------

wt <- merge(w, mtc[mtc$loc=='Davis', ], by='year', all.x=TRUE, 
            all.y = FALSE)

p <- p %>% filter(loc %in% c('KAC','NSacValley'),
                  !is.na(day))
p$loc <- recode(p$loc, 'KAC'='Parlier','NSacValley'='Chico')

pt <- merge(p, mtc, by=c('year','loc'), all.x=TRUE, all.y=FALSE) 




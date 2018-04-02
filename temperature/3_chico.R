datapath <- '/Users/echellwig/Drive/Phenology/data/historydata/CIMIS'
library(devtools)
library(rnoaa)
library(ScrapeClim)
library(plyr)
options(stringsAsFactors = FALSE)

fullchico <- read.csv(file.path(datapath, 'ChicoNOAA.csv'))
durfiles<- list.files(file.path(datapath, 'Durham/'), pattern='hourly',
                      full.names = TRUE)

##################################
#clean durham
vars <- c('Stn.Name','Date','Hour..PST.','Air.Temp..C.','qc')

durfull <- ldply(durfiles, function(fn) read.csv(fn) )
dur <- unique(durfull[, vars])
names(dur) <- c('name','date','hour','temp','qc')

dur$date <- as.Date(dur$date, format = '%m/%d/%Y')
missing <- which(is.na(dur$temp) | dur$temp < -100)
dur <- dur[-missing, ]

flaggedrows <- which(dur$qc=='R' | dur$qc=='Y')
flaggeddays <- as.Date(dur[flaggedrows,'date'])

gooddays <- which(!(dur$date %in% flaggeddays))
durultra <- dur[gooddays, ]
durdates <- unique(durultra$date)

durmod <- data.frame(date=durdates,
                     Durmin=sapply(durdates, function(dt) {
                         min(durultra[durultra$date==dt, 'temp'])
                     }),
                     Durmax=sapply(durdates, function(dt) {
                         max(durultra[durultra$date==dt, 'temp'])
                     }))


##################################
#clean chico
chivars <- c('DATE','TMAX','TMIN')
chico <- fullchico[,chivars]
names(chico) <- c('date', 'Chimax', 'Chimin')
chico$date <- as.Date(chico$date)

######################################
#merge

chidata <- merge(chico, durmod, by='date', all=TRUE)
chidf <- chidata[complete.cases(chidata), ]

minmod <- lm(Chimin ~ Durmin, data=chidf)
maxmod <- lm(Chimax ~ Durmax, data=chidf)




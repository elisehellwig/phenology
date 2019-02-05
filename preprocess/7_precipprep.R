data <- '/Volumes/GoogleDrive/My Drive/Phenology/data'
library(tidyverse)
options(stringsAsFactors = FALSE)

noaa <- read.csv(file.path(data, 'raw/climate/precipNOAA.csv')) %>%
    as_tibble()

oldvars <- c('NAME', 'DATE','PRCP')
newvars <- c('location','date','precip')

####################################################################
noaa <- noaa[, oldvars]
names(noaa) <- newvars
noaa$location <- noaa$location %>%
recode("CHICO UNIVERSITY FARM, CA US"="Chico",
       "MODESTO CITY CO AIRPORT, CA US"="Modesto",
       "VISALIA, CA US"="Parlier",
       "DAVIS 2 WSW EXPERIMENTAL FARM, CA US"="Davis")

noaa$year <- str_split(noaa$date, pattern='-', simplify=TRUE)[,1] %>%
    as.numeric()

noaa$month <- str_split(noaa$date, pattern='-', simplify=TRUE)[,2] %>%
    as.numeric()

noaa$winter_year <- ifelse(noaa$month<10, noaa$year, noaa$year+1)
noaa$annual_year <- ifelse(noaa$month>2, noaa$year+1, noaa$year)
noaa$winter <- noaa$month %>%
    as.character() %>%
    recode('11'='winter','12'='winter','1'='winter', .default='not_winter')

seasonalnoaa <- noaa %>%
    filter(winter=='winter', winter_year>1914) %>%
    group_by(location, winter_year) %>%
    summarise(winter=sum(precip))

annualnoaa <- noaa %>%
    filter(annual_year>1914) %>%
    group_by(location, annual_year) %>%
    summarize(annual=sum(precip))


precipitation <- inner_join(seasonalnoaa, annualnoaa, 
                           by=c('location', 'winter_year'='annual_year'))

names(precipitation)[2] <- 'year'

write.csv(precipitation, file.path(data,'history/precipitation.csv'), 
          row.names=FALSE)

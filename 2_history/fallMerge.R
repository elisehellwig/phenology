library(tidyverse)
library(lubridate)

path <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/history'

almond <- read_csv(file.path(path, 'almondseasonlengthdata.csv'))
prune <- read_csv(file.path(path, 'pruneseasonlengthdata.csv'))
walnut <- read_csv(file.path(path, 'walnutseasonlengthdata.csv'))

thermal <- read_csv(file.path(path, 'thermaltimeaccumulation.csv'))

voi <- c('year','cultivar','nearest','slen')
voi_thermal <- c('crop', 'cultivar','year','anderson')
####################################################################
####################################################################

a <- select(almond, voi)
a$crop <- 'almond'

prune$cultivar <- 'French'
p <- select(prune, voi)
p$crop <- 'prune'


w <- select(walnut, voi)
w$crop <- 'walnut'

th <- thermal %>% 
    select(-simplified) %>% 
    rename(heatsum=anderson)


####################################################################
####################################################################

fall <- bind_rows(a, p, w) %>%
    inner_join(th) %>%
    mutate(heatscl=ave(heatsum, cultivar, FUN=scale))


write_csv(fall, file.path(path, 'fall.csv'))


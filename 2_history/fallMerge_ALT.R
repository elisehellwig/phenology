library(tidyverse)
library(lubridate)

path <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/history'

almond <- read_csv(file.path(path, 'almondseasonlengthdata.csv'))
prune <- read_csv(file.path(path, 'pruneseasonlengthdata.csv'))
walnut <- read_csv(file.path(path, 'walnutseasonlengthdata.csv'))

thermal <- read_csv(file.path(path, 'thermaltimeaccumulation.csv'))

####################################################################
####################################################################

a <- select(almond, -contains("_")) %>% 
    add_column(crop='almond')

p <- select(prune, -contains("_")) %>% 
    add_column(crop='prune', cultivar='French')


w <- select(walnut, -contains("_")) %>% 
    add_column(crop='walnut')

th <- thermal %>% 
    select(-simplified, heatsum=anderson)

####################################################################
####################################################################

fall <- bind_rows(a, p, w) %>%
    inner_join(th) %>%
    mutate(heatscl=ave(heatsum, cultivar, FUN=scale))


write_csv(fall, file.path(path, 'fall.csv'))


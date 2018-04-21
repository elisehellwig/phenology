library(tidyverse)
library(lubridate)

inpath <- '/Volumes/GoogleDrive/My Drive/Phenology/Results/history'

almond <- read_csv(file.path(inpath, 'almondseasonlengthdata.csv'))
prune <- read_csv(file.path(inpath, 'pruneseasonlengthdata.csv'))
walnut <- read_csv(file.path(inpath, 'almondseasonlengthdata.csv'))
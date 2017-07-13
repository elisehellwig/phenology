drivepath <- '/Users/echellwig/Drive/Phenology'
datapath <- file.path(drivepath, 'data/walnutdata/')
resultspath <- file.path(drivepath,'Results/')
library(plyr)


w <- read.csv(file.path(datapath,'walnutclean.csv'), stringsAsFactors = FALSE)
vars <- sort(unique(w$cultivar))


###########################################################################
###########################################################################


vardf <- lapply(vars, function(cv) w[w$cultivar==cv, ])

datayrs <- sapply(vardf, function(df) dim(df)[1])

mu <- ldply(vardf, function(df) {
    round(apply(df[,c('event1','event2','length1')], 2, mean))
})

first <- ldply(vardf, function(df) {
    round(apply(df[,c('event1','event2','length1')], 2, quantile, 0.25))
})

third <- ldply(vardf, function(df) {
    round(apply(df[,c('event1','event2','length1')], 2, quantile, 0.75))
})

stringlist <- lapply(1:3, function(i) {
    paste0(mu[,i], "      (", first[,i], ", ", third[,i],')')
})

table1 <- data.frame(cultivar=vars,
                     datayears=datayrs,
                     leafout=stringlist[[1]],
                     harvest=stringlist[[2]],
                     seasonlength=stringlist[[3]],
                     slnum=mu[[3]])

write.csv(table1, file.path(resultspath,'walnutpaper/phenotable1.csv'),
          row.names = FALSE)


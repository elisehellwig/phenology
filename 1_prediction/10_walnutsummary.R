drivepath <- '/Users/echellwig/Drive/Phenology'
datapath <- file.path(drivepath, 'data/walnutdata/')
resultspath <- file.path(drivepath,'Results/')
library(plyr)

options(stringsAsFactors = FALSE)

ms <- read.csv(file.path(resultspath, 'walnutpaper/modelsummary.csv'))
w <- read.csv(file.path(datapath,'walnutclean.csv'))
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


##########################################################

l1 <- which(ms$complexity=='full' & ms$type=='DT')

mte <- expand.grid(c('full','simplified'), c('DT','TTT'))
names(mte) <- c('complex','type')

mte$rmse <- sapply(1:nrow(mte), function(i) {
    mtrows <- which(ms$complexity==mte[i,'complex'] & ms$type==mte[i,'type'])
    mean(ms[mtrows, 'rmse'])
})


mte$rmsediff <- c(rep(mte$rmse[2] - mte$rmse[1], 2),
                  rep(mte$rmse[4] - mte$rmse[3], 2))
mte$avgdiff <- round(mean(mte$rmsediff),2)

write.csv(mte, file.path(resultspath,'walnutpaper/comparecomplexityrmse.csv'),
          row.names = FALSE)


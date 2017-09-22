drivepath <- '/Users/echellwig/Drive/Phenology'
datapath <- file.path(drivepath, 'data/walnutdata/')
resultspath <- file.path(drivepath,'Results/')
library(plyr)
library(ScrapeClim)
library(rnoaa)
library(xts)
library(reshape2)


Davis <- data.frame(id=c('USC00042294'), latitude=c( 38.538698),
                    longitude=c(-121.761336))


temp <- readRDS(file.path(datapath, 'davisdailytemp.RDS'))
m <- read.csv(file.path(datapath, 'monthlytemperatures.csv'))


###########################################################################
###########################################################################

vars <- c('date', 'year', 'month', 'nmonth', 'tmin', 'tmax','Minimum', 
          'Maximum')
indexvars <- c('date','year','month','nmonth')

m1 <- m[m$nearest=='Davis', vars]
m2 <- m1[m1$month %in% 2:10, ]


monthly <- m2[, c(indexvars,'tmin','tmax')]
names(monthly) <- c('date','year','month','name', 'Min','Max')
fiveyear <- m2[, c(indexvars, 'Minimum','Maximum')]
names(fiveyear) <- c('date','year','month','name', 'Min','Max')
indexvars <- c('date','year','month','name')

mm <- melt(monthly, id.vars = indexvars, variable.name = 'variable',
           value.name = 'monthly')
mfy <- melt(fiveyear, id.vars = indexvars, variable.name = 'variable',
            value.name = 'fiveyear')

mmall <- merge(mm, mfy, by=c(indexvars,'variable'))

write.csv(mmall, file.path(datapath, 'davismonthlytemps.csv'), 
          row.names = FALSE)







###########################################################################
###########################################################################
tx <- xts(x = temp[,c('tmin','tmax')], order.by = temp$date)
txmonth <- apply.monthly(tx, mean)
tmonth <- as.data.frame(coredata(txmonth))
tmonth$date <- as.Date(index(txmonth))
tmonth$year <- year(tmonth$date)
tmonth$month <- month(tmonth$date)

mname <- data.frame(month=1:12, monthname=month.name)
tmonth <- merge(tmonth, mname)
tmm <- melt(tmonth, id.vars=c('month','date','year','monthname'))

tmm$monthname <- factor(tmm$monthname,levels(tmm$monthname)[c(5,4,8,1,9,7,6,2,
                                                              12,11,10,3)])

write.csv(tmonth, file.path(drivepath, 'Results/walnutpaper/monthlytemps.csv'),
          row.names = FALSE)

tmm2 <- tmm[tmm$month %in% 3:6,]


tplot <- ggplot(data=tmm)
tplot <- tplot + geom_line(aes(x=year, y=value, color=variable, group=variable))
tplot <- tplot + facet_wrap(~monthname)
tplot <- tplot + scale_color_manual(values=c('#d7191c','#2c7bb6'), 
                                    labels=c('Minimum','Maximum'),
                                    name='Variable')
tplot <- tplot +labs(x="Year", y='Temperature')
tplot


nearby <- meteo_nearby_stations(Davis, radius=100)
saveRDS(nearby, file.path(drivepath, 'data/walnutpaper/nearbyDavis.RDS'))


dav <- locationDownload(Davis, 1910:2016, nearby, r2=0.90,
                     APItoken = 'LtpGDhfftKEwCGGgOeOsfBRCsRawIMaN')


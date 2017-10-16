
source('functions/tempclean.R')
#stations Davis and surrounding (Woodland, Winters, Sac, and Vacaville in no
    #particular order) 'USW00023271' - model had a r2 of 0.85, vaca is .9
davstations <- c('USC00042294','USC00049742','USC00049781','USW00023232',
                 'USC00049200')

dav <- ghncd_download(davstations)
dateNA <- which(is.na(dav$date))
dav2 <- dav[-dateNA, c('id','year','date','tmin','tmax')]


prime <- davstations[1]
secnd<- davstations[-1]

yrs <- 1920:2017

davisdaily <- tempclean(data=dav2, 
                        primary=prime, 
                        secondary=secnd,
                        years=yrs, 
                        dateform="%Y-%m-%d")




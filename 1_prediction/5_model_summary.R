
drivepath <- '/Users/echellwig/Drive/Phenology'
library(phenoclim)
library(plyr)
source('functions/helperfunctions.R')
w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
vars <- levels(w$cultivar)
###################################################

###################################################
#adding ensemble predictions


###################################################
###creating csv of data

tpmCV <- readRDS(file.path(drivepath, 
                           'Results/walnutpaper/DTplantmodelCV.RDS'))

tpmsCV <- readRDS(file.path(drivepath,
                          'Results/walnutpaper/DTsimpleplantmodel.RDS'))

dpmCV <- readRDS(file.path(drivepath,
                           'Results/walnutpaper/TTTplantmodelCV.RDS'))
dpmsCV <- readRDS(file.path(drivepath, 
                            'Results/walnutpaper/TTTsimpleplantmodelCV.RDS'))

###################################################

DT <- ldply(1:length(tpmCV), function(i) {
    plist <- parameters(tpmCV[[i]])
    data.frame(cultivar=vars[i],
               complexity='full',
               type='DT',
               form=extractslots('form', plist),
               length=round(extractslots('modlength', plist)),
               base=round(extractCT(plist, 1), 1),
               optimal=round(extractCT(plist, 2),1),
               critical=round(extractCT(plist,3),1),
               rmse=round(error(tpmCV[[i]]), 2))
})


DTsimple <- ldply(1:length(tpmsCV), function(i) {
    data.frame(cultivar=vars[i], complexity='simplified', type='DT', 
               form='None',
               length=round(extractslots('modlength', parameters(tpms[[i]]))),
               base=NA,
               optimal=NA,
               critical=NA,
               rmse=round(error(tpms[[i]]), 2))
})

TTT <- ldply(1:length(dpmCV), function(i) {
    plist <- parameters(dpmCV[[i]])
    data.frame(cultivar=vars[i],
               complexity='full',
               type='TTT',
               form=extractslots('form', plist),
               length=round(extractslots('modlength', plist)),
               base=round(extractCT(plist, 1), 1),
               optimal=round(extractCT(plist, 2),1),
               critical=round(extractCT(plist,3),1),
               rmse=round(error(dpmCV[[i]]), 2))
})


TTTsimple <- ldply(1:length(dpmsCV), function(i) {
    plist <- parameters(dpmsCV[[i]])
    data.frame(cultivar=vars[i],
               complexity='simplified',
               type='TTT',
               form=extractslots('form', plist),
               length=extractslots('modlength', plist),
               base=round(extractCT(plist, 1), 1),
               optimal=round(extractCT(plist, 2),1),
               critical=round(extractCT(plist,3),1),
               rmse=round(error(dpmsCV[[i]]), 2))
})

modelsummary <- rbind(DT, DTsimple, TTT, TTTsimple)

write.csv(modelsummary, file.path(drivepath, 
                                  'Results/walnutpaper/modelsummary.csv'),
          row.names = FALSE)






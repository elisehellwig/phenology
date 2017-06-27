
drivepath <- '/Users/echellwig/Drive/Phenology'
library(phenoclim)
library(plyr)
source('functions/helperfunctions.R')
temps <- read.csv(file.path(drivepath, 
                            'data/walnutdata/davisdailyhourlytemp.csv'),
                  stringsAsFactors = FALSE)
w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
vars <- levels(w$cultivar)
n <- length(vars)
forms <- c('gdd','gddsimple','linear','flat', 'triangle','asymcur')
gdhforms <- forms[-(1:2)]
gddforms <- forms[1:2]

##################################################

slens <- c(65000, 65000, 21000, 74000, 27000, 21000, 41000, 62000, 60000, 47000,
           26000, 15000)

gdhseq <- lapply(1:n, function(v) {
    c(round(seq(1, slens[v]/3, by=150)), 
      round(seq(slens[v]/3, (slens[v]/3)*2, by=150)),
      round(seq((slens[v]/3)*2, slens[v], by=150)))
})

###################################################

#importing data
lengthpath <- file.path(drivepath, 'Results/walnutpaper/length')

#ddl <- importfiles(lengthpath, 'dayDcomplexplantmodellength', vars, 
                          #'RDS')
                          
dtl1 <- importfiles(lengthpath, 'DTlength1', vars, 'RDS')
dtl2 <- importfiles(lengthpath, 'DTlength2', vars, 'RDS') 

tttl1 <- importfiles(lengthpath, 'TTTlengthGDH1', vars, 'RDS')
tttl2 <- importfiles(lengthpath, 'TTTlengthGDH2', vars, 'RDS')
tttl3 <- importfiles(lengthpath, 'TTTlengthGDH3', vars, 'RDS') 

tttlgdd <- importfiles(lengthpath, 'TTTlengthGDD', vars,'RDS')
###################################################


DTlist <- lapply(1:n, function(i) {
    errorlist <- c(dtl1[[vars[i]]], dtl2[[vars[i]]])
    
    ldply(1:length(errorlist), function(j) {
        data.frame(cultivar=vars[i],
                   type='DT',
                   form=forms,
                   length=j,
                   rmse=round(errorlist[[j]],2))
    })
})

DTdf <- do.call(rbind, DTlist)
DTdf$frac <- DTdf$length/160 

#############################################
##TTT gdh

TTTl <- lapply(1:n, function(i) {
    errorlist <- c(tttl1[[vars[i]]], tttl2[[vars[i]]], tttl3[[vars[i]]])
    
    ldply(1:length(errorlist), function(j) {
        data.frame(cultivar=vars[i],
                   type='TTT',
                   form=gdhforms,
                   length=gdhseq[[i]][j],
                   rmse=round(errorlist[[j]],2),
                   frac=gdhseq[[i]][j]/max(gdhseq[[i]]))
    })
})

TTTdf <- do.call(rbind, TTTl)



#####################################
#gdd lengths
gddseq <- round(seq(1, 4001, length.out=220))
TTTlGDD <- lapply(1:n, function(i) {
    ldply(1:220, function(j) {
        data.frame(cultivar=vars[i],
                   type='TTT',
                   form=gddforms,
                   length=gddseq[j],
                   rmse=round(tttlgdd[[i]][[j]],2))
    })
})

TTTGDDdf <- do.call(rbind, TTTlGDD)
TTTGDDdf$frac <- TTTGDDdf$length/4001
##########
##Anderson
ai <- lapply(1:n, function(i) {
    data.frame(length=c(1:160, gdhseq[[i]]),
               type=c(rep('DT', 160), rep('TTT', length(gdhseq[[i]]))),
               form='anderson')
})

andersonlist <- lapply(1:n, function(i) {
    wv <- w[w$cultivar==vars[i], ]
    aii <- ai[[i]]
    gdhmax <- max(aii$length)
    templist <- extracttemplist(temps, unique(wv$year), 'anderson')[[2]]
    ldply(1:dim(aii)[1], function(j) {
        error <- minrmse(c(4,25,36), wv, templist, aii[j,'type'],
                         'anderson', 1, TRUE, aii[j,'length'],
                         simple=FALSE)
        data.frame(cultivar=vars[i],
                   type=aii[j,'type'],
                   form='anderson',
                   length=aii[j,'length'],
                   rmse=error,
                   frac=seasonfrac(aii[j,'length'], aii[j,'type'], gdhmax)
        )
    })
})

andersondf <- do.call(rbind, andersonlist)
#########


lengthsummary <- rbind(DTdf, TTTdf,TTTGDDdf, andersondf)

write.csv(lengthsummary, file.path(drivepath, 
                                  'Results/walnutpaper/lengthsummary.csv'),
          row.names = FALSE)

#lsum <- read.csv(file.path(drivepath,'Results/walnutpaper/lengthsummary.csv'))
#msm <- read.csv(file.path(drivepath, 'Results/walnutpaper/modelsummary.csv'))
#########################

optlength <- expand.grid(levels(lsum$cultivar), 
                         levels(lsum$form), 
                         levels(lsum$type))

names(optlength) <- c('cultivar','form','type')

optlength$length <- sapply(1:dim(optlength)[1], function(i) {
    rows <- which(lsum$cultivar==optlength[i,'cultivar'] & lsum$type==optlength[i, 'type'] & lsum$form==optlength[i, 'form'])
    minerror <- which.min(lsum[rows,'rmse'])
    lsum[rows[minerror] , 'length']
})

optlength$complexity <- 'full'

msm$length <- round(msm$length)
simplelength <-msm[msm$type=="TTT" & msm$complexity=='simplified', c('cultivar',
                                                                'complexity',
                                                                'type',
                                                                'form',
                                                                'length')]

ensemblerows <- which(simplelength$form=='ensemble')
simplelength <- simplelength[-ensemblerows,]

optlength2 <- rbind(simplelength, optlength)

write.csv(optlength2, file.path(drivepath, 
                               'Results/walnutpaper/optimallength.csv'),
          row.names = FALSE)



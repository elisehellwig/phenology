# http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
getlegend<-function(gplot){
    require(gridExtra)
    tmp <- ggplot_gtable(ggplot_build(gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}



formatPlotData <- function(df, variety=NA) {
    require(dplyr)
    # df - data.frame, needs to have columns: cultivar, loc, year, thermal,
            #length1, 
    # varity - character, name of the cultivar to plot data from
    
    #select only the columns we want
    dfs <- select(df, cultivar, loc, year, thermal, length1)
    
    #select data for the season length ~ year plot 
    df1 <- dfs[complete.cases(dfs), ]
    df1 <- select(df1, cultivar, loc, x=year, y=length1)
    df1$groupvar <- 'A' #give it a label A
    
    #select data for season length ~ thermal sum plot 
    df2 <- dfs[complete.cases(dfs), ]
    df2 <- select(df2, cultivar, loc, x=thermal, y=length1)
    df2$groupvar <- 'B' #give it a label B
    
    #select data for thermal sum ~ year plot 
    df3 <- dfs
    df3 <- select(df3, cultivar, loc, x=year, y=thermal)
    df3$groupvar <- 'C' #give it a label C
    
    #group all data together
    dffinal <- rbind(df1, df2, df3)
    
    # select only the data for the cultivar of interest
    if (!is.na(variety)) {
        dffinal <- filter(dffinal, cultivar==variety)
    }
    
    
    return(dffinal)
}


triplot <- function(df, variety, loc=FALSE, threshold=NA, alims=NA, blims=NA,
                    clims=NA, asmooth=NA, bsmooth=NA, csmooth=NA) {
    # df - data.frame, needs to have columns: cultivar, loc, year, thermal,
    #length1, 
    # varity - character, name of the cultivar to plot data from
    #loc - 
    
    require(ggplot2)
    
    tridf <- formatPlotData(df, variety)
    
    if (!is.na(threshold)) {
        tslab <- paste0('GDH', threshold)
    } else {
        tslab <- 'GDH'
    }
    
    aplot <- ggplot(data=tridf[tridf$groupvar=='A', ]) +
        geom_point(aes(x=x, y=y, shape=loc)) + theme_classic() +
        labs(x="Year", y='Season Length (days)') + guides(shape=FALSE)
    
    if (!is.na(alims[1])) {
        aplot <- aplot + xlim(alims)
    }
    
    bplot <- ggplot(data=tridf[tridf$groupvar=='B', ]) +
        geom_point(aes(x=x, y=y, shape=loc)) + theme_classic() +
        labs(x=tslab, y='Season Length (days)') + guides(shape=FALSE)
    
    if (!is.na(blims[1])) {
        bplot <- bplot + xlim(blims)
    }
    
    cplot <- ggplot(data=tridf[tridf$groupvar=='C', ]) +
        geom_point(aes(x=x, y=y, shape=loc)) + theme_classic() +
        labs(x="Year", y=tslab) + guides(shape=FALSE)
    
    if (!is.na(clims[1])) {
        cplot <- cplot + xlim(clims)
    }
    
    if (loc) {
        legplot <- ggplot(data=tridf) + geom_point(aes(x=x, y=y, shape=loc)) +
            scale_shape(name='Location')
        
        shapelegend <- getlegend(legplot)
    }
    
    
    
    triplots <- arrangeGrob(aplot,bplot,cplot, nrow=3)
    
    if (loc) {
        grid.arrange(triplots, shapelegend , ncol = 2, widths = c(3/4,1/4))
        
    } else {
        grid.arrange(triplots)
    }
    
    
}


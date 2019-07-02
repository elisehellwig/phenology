# http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
getlegend<-function(gplot){
    require(gridExtra)
    tmp <- ggplot_gtable(ggplot_build(gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

locateLabel <- function(plt, alims) {
    
    if (!is.na(alims[1])) {
        xrange <- alims
    } else {
        xrange <- layer_scales(plt)$x$range$range
    }
    
    xval <- xrange[1] - (xrange[2]-xrange[1])/10
    
    yrange <- layer_scales(plt)$y$range$range
    
    yval <- yrange[2] + (yrange[2]-yrange[1])/9
    
    df <- data.frame(x=xval, y=yval)
    
    return(df)
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


triplot <- function(df, variety, moddf, loc=FALSE, threshold=NA, 
                    pointcolor='black', alims=NA, blims=NA, clims=NA, 
                    asmooth=FALSE, bsmooth=FALSE, csmooth=FALSE, 
                    smoothcol=rep('black',3)) {
    require(grid)
    require(gridExtra)
    ### A = season length ~ year
    ### B = season length ~ thermal sum
    ### C = thermal sum ~ year
    # df - data.frame, needs to have columns: cultivar, loc, year, thermal,
    #length1, 
    # varity - character, name of the cultivar to plot data from
    # moddf - data.frame containing linear model parameters and metrics
    #loc - logical, should points be coded by location?
    #threshold - numeric/character, length of thermal time accumulation, for
        #axis labeling
    #alims, blims, clims - numeric (length 2), specifies the x axis limits for
        #the a, b, and c plots respectively
    #asmooth, bsmooth, csmooth - logical, should the linear model be included?
    #scol - character (length 3), the color to plot the linear models
    
    require(ggplot2)
    
    tridf <- formatPlotData(df, variety)
    
    if (!is.na(threshold)) {
        tslab <- paste0('GDH', threshold)
    } else {
        tslab <- 'GDH'
    }
    
    ##################### Season length ~ Year plot #####################
    
    aplot <- ggplot(data=tridf[tridf$groupvar=='A', ]) +
        geom_point(aes(x=x, y=y, shape=loc), color=pointcolor) + 
        theme_classic() + labs(x="Year", y='Season Length (days)') + 
        guides(shape=FALSE)
    
    
    Alab <- locateLabel(aplot, alims)
    Alab$name <- 'A)'
    
    if (!is.na(alims[1]))  {
        aplot <- aplot + xlim(alims)
        Alab$x <- alims[1]
    }
    
    
    if (asmooth) {
        aplot <- aplot + geom_smooth(aes(x=x, y=y),
                                     method=lm, se=FALSE, color=smoothcol[1])
    }
    
    
    
    aplot <- aplot + geom_text(data=Alab, aes(x=x, y=y, label=name), size=6) +
        coord_cartesian(clip='off')
    
    ########### Season length ~ GDH plot ###################
        
    bplot <- ggplot(data=tridf[tridf$groupvar=='B', ]) +
        geom_point(aes(x=x, y=y, shape=loc), color=pointcolor) +
        theme_classic() + labs(x=tslab, y='Season Length (days)') +
        guides(shape=FALSE) 
    
    Blab <- locateLabel(bplot, blims)
    Blab$name <- 'B)'
    
    if (!is.na(blims[1]))  {
        bplot <- bplot + xlim(blims)
        Blab$x <- blims[1]
    }
    
    if (bsmooth) {
        bplot <- bplot + geom_smooth(aes(x=x, y=y), method=lm, se=FALSE, 
                                     color=smoothcol[2])
    }
    
    bplot <- bplot + geom_text(data=Blab, aes(x=x, y=y, label=name), size=6) +
        coord_cartesian(clip='off')
    
    ############### GDH ~ Year Plot ####################
    
    cplot <- ggplot(data=tridf[tridf$groupvar=='C', ]) +
        geom_point(aes(x=x, y=y, shape=loc), color=pointcolor) +
        theme_classic() + labs(x="Year", y=tslab) + guides(shape=FALSE)
    
    Clab <- locateLabel(cplot, clims)
    Clab$name <- 'C)'
    
    if (!is.na(clims[1]))  {
        cplot <- cplot + xlim(clims)
        Clab$x <- clims[1]
    }
    
    if (csmooth) {
        cplot <- cplot + geom_smooth(aes(x=x, y=y),
                                     method=lm, se=FALSE, color=smoothcol[3])
    }
    
    cplot <- cplot + geom_text(data=Clab, aes(x=x, y=y, label=name), size=6) +
        coord_cartesian(clip='off')
    ############# Adding a legend #######################
    
    if (loc) {
        legplot <- ggplot(data=tridf) + geom_point(aes(x=x, y=y, shape=loc)) +
            scale_shape(name='Location')
        
        shapelegend <- getlegend(legplot)
    }
    
    
    ###########Putting it all together ###############
    
    triplots <- arrangeGrob(aplot,bplot,cplot, nrow=3)
    
    if (loc) {
        grid.arrange(triplots, shapelegend , ncol = 2, widths = c(3/4,1/4))
        
    } else {
        grid.arrange(triplots)
    }
    
    
}


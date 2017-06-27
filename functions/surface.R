sloperaster <- function(mat, flip=TRUE, examine=FALSE) {
    
    if (flip) {
        mat <- apply(mat, 2, rev)
    }
    
    ta <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    
    r <- raster(mat, crs=ta, xmn=0, xmx=ncol(mat), ymn=0, ymx=nrow(mat))
    
    if (examine) {
        plot(r)
    }
    
    sloper <- terrain(r, opt='slope', unit='tangent')
    
   return(sloper)
}


slopedf <- function(df, x, y, value, dat=FALSE) {
    m <- matrix(df[,value], nrow=length(unique(df[,y])))
    rslope <- sloperaster(m, flip=FALSE, examine=FALSE)
    
    vslope <- as.vector(as.matrix(rslope))
    
    if (dat) {
        df$slope <- vslope
        return(na.omit(df))
    } else {
        return(vslope)
    }
}



vectorslope <- function(x, y) {
    
    if (length(x) != length(y)) {
        stop('x and y must be the same length.')
    }
    
   slp <-  sapply(1:(length(x)-1), function(i) {
        (y[i+1] - y[i])/(x[i+1] - x[i])
    })
   
   return(slp)
}
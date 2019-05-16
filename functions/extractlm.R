extractLM <- function(mod, col.names=c('coef','pval'), intname='intercept',
                      loc=NA, cultivar=NA) {
    #This function extracts various parameters from an 'lm' object
    # mod - 'lm' or 'summary.lm' object, from which information is to be
        #extracted
    # col.names - character, vector of the parameter categories to be extracted
    # intname - character, what to call the intercept coefficient
    # loc - character, name of the location the data was from
    # cultivar - character, name of the cultivar the data represents
    # OUTPUT - a dataframe containing coefficients, standard errors, tvalues,
        #pvalues, adjusted r2 and location and cultivar labels if available
    
    #convert mod to 'summary.lm' object if it is not already one
    if (!grepl('summary', class(mod))) {
        mod <- summary(mod)
    } 
    
    #create data.frame of the coefficients of the model
    coefs <- as.data.frame(coef(mod))
    
    #give data frame appropriate names
    names(coefs) <- c('coef','stderr','tval','pval')
   
    #rename the intercept so it doesn't have parentheses in the name
    varnames <- c(intname, row.names(coefs)[-1])
    coefs$var <- varnames  #give coefficient names their own column
    row.names(coefs) <- NULL #remove row names
    
    coefs$r2 <- mod$adj.r.squared #add adjusted r2 to data frame
    
    if (!is.na(loc)) { #add location if availiable
        coefs$loc <- loc
    }
    
    if (!is.na(cultivar)) { #add cultivar if available
        coefs$cultivar <- cultivar
    }
    
    return(coefs)
    
}

formatLM <- function(df, lmlist, cols=c('coef', 'pval','r2'), crop=NA,
                     dropIntercept=TRUE) {
    #df - data.frame, contains location ('loc') and cultivar labels for each
        #model in the lmlist
    #lmlist - list, a list of 'lm' or 'summary.lm' objects the data will be
        #extracted from
    #cols - character, the variables to be extracted from the lm object.
        #Options: 'coef', 'stderr', 'tval', 'pval', and 'r2'
    #crop - character, the crop the data represents
    #dropIntercept logical, should the intercept be dropped from the
        #coefficients returned
    
    
    #extract the information from each lm object
    if (class(lmlist)=='lm') {
        moddf <- extractLM(lmlist, loc=df[1,'loc'], cultivar=df[1,'cultivar'])
        
    } else {
        moddf <- ldply(1:nrow(df), function(i) {
            extractLM(lmlist[[i]], loc=df[i,'loc'], cultivar=df[i,'cultivar'])
        })
        
    }
    
    #remove intercept if it is not needed
    if (dropIntercept) {
        moddf <- filter(moddf, var!='intercept')
    } else {
        cols <- c('var', cols)
    }
   
    #create columns of interest
    coi <- c('loc','cultivar', cols)
    
    #select only columns of interest
    newdf <- moddf[,coi]
    
    #add a column identifying crop if it is provided
    if (!is.na(crop)) {
        newdf[,'crop'] <- crop
    }
    
    
    #return lm information data frame.
    return(newdf)
}



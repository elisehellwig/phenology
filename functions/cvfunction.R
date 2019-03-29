

CV <- function(df, predictor, response, k=5, seed=NA, avg=TRUE) {
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    df$fold <- kfold(df, k)
    
    error <- rep(NA, k)
    
    
    for (i in 1:k) {
        train <- df[df$fold!=i, ]
        test <- df[df$fold==i, ]
         
        fmla <- paste(response, '~', predictor)
        
        mod <- lm(as.formula(fmla), data=train)
        
        if (predictor=='1') {
            fit <- rep(predict(mod)[1], nrow(test))
            
        } else {
            newdata <-data.frame(test[,predictor])
            names(newdata) <- predictor
            
            fit <- predict(mod, newdata)
            
        }
        
        
        error[i] <- rmse(fit, test[,response])
        
    }
    
    if(avg) {
        error <- mean(error)
    }
    
    return(error)
    
}
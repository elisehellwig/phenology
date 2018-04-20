loadtidyverse <- function(update=FALSE) {
    tidypacks <- c('tibble','dplyr','ggplot2','purrr','readr','lubridate',
                   'stringr','tidyr','forcats')
    
    if (update) install.packages(tidypacks)
    
    for (pack in tidypacks) {
            
        if(!require(pack, character.only = TRUE, quietly = TRUE, 
                    warn.conflicts = FALSE)) {
            require(pack, quietly=TRUE, character.only = TRUE,
                    warn.conflicts = FALSE)
        }
        
    }
}
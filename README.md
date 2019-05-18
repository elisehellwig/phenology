# phenology: Analyzing the effects of climate change on almond, prune, and walnut phenology

This repository stores the code for my dissertation on climate change and phenology in California.

Information about the file structure, what each of the files do, as well as data documentation can be found on the repository wiki.

__Required R Packages:__ dismo, dplyr, ggplot2, grid, Interpol.T, kableExtra, knitr, lubridate, phenoclim, plyr, raster, reshape2, segmented, tidyverse


## An Explanation of Terminology

####DT vs. TTT
The DT model is the day threshold model. This model accumulates thermal time until a specific day and then uses the amount of thermal time to predict the season length. The TTT model counts the number of days it takes to accumulate a given amount of thermal time, and then uses the number of days to predict the season length.

 
####Simple/simplified
A model is simplified if the season length is considered to be over when the model reaches the model threshold (either in days or in thermal time). The predicted season length for a simplified DT model is just the mean season length.


####Thermal time functional form (form)
A thermal time functional form is the equation used to convert temperature into chill hours, chill portions, growing degree days, or growing degree hours.


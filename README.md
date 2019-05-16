# phenology: Analyzing the effects of climate change on almond, prune, and walnut phenology

This repository stores the code for my dissertation on climate change and phenology in California.

All data is stored in Phenology/data. Documentation for data can be found in the __Data Descriptions__ section.

__Required R Packages:__ dismo, dplyr, ggplot2, grid, Interpol.T, kableExtra, knitr, lubridate, phenoclim, plyr, raster, reshape2, segmented, tidyverse


## Table of Contents

[TOC]

## An Explanation of Terminology

####DT vs. TTT
The DT model is the day threshold model. This model accumulates thermal time until a specific day and then uses the amount of thermal time to predict the season length. The TTT model counts the number of days it takes to accumulate a given amount of thermal time, and then uses the number of days to predict the season length.

 
####Simple/simplified
A model is simplified if the season length is considered to be over when the model reaches the model threshold (either in days or in thermal time). The predicted season length for a simplified DT model is just the mean season length.


####Thermal time functional form (form)
A thermal time functional form is the equation used to convert temperature into chill hours, chill portions, growing degree days, or growing degree hours.

## File Descriptions

This section constitutes descriptions of all the files in this repository, what they do, what other files are required to run them, as well as their data inputs and outputs. These descrptions are general overviews and to not go into specific discussions of the code in each file. For more detailed documentation, please review the comments in the file of interest.

### functions/

#### __cleanTemps.R__
This script contains functions that are used to import, clean, and process temperature data.

* __importCIMIS()__ - takes a directory path as an input. It reads all of the .csv files in that folder into R, merges them into one data.frame and formats them for use later. Only works with data from the CIMIS website.
* __fillinTemps()__ - This function is used to fill in missing temperature data from a given weather station with temperatures from nearby stations by way of linear regression. 
* __FtoC()__ - Converts fahrenheit temperatures to celcius
* __AverageTemps()__ - This function is used to calculate either annual or monthly average temperatures by location.
* __backfillTemps()__ - This function creates hourly temperature data from daily temperature data using a very simple (and not very realistic) interpolation. Only use if no other method of filling in temperature data is available

#### __cvfunction.R__
This script contains a single function that runs a 5-fold cross-validation on a specified univariate linear regression model. Not sure if I actually need this but we will see if it turns up later on in the code.

#### __extractlm.R__

This script contains two functions for extracting various parts of the 'lm' class object.

* __formatLM()__ - This function extracts and nicely formats the coefficients, pvalues and adjusted R2 value from a given 'lm' model or list of 'lm' models. 
* __extractLM()__ - This function is called by __formatLM()__, and does most of the work manipulating the actual 'lm' object. 

#### __tempInterpolation.R__
This script contains 3 functions that automate the interpolation of daily temperatures to hourly temperatures and format the resulting data. The interpolation is done in two parts. First the parameters for doing this interpolation are calibrated based on observed hourly temperature at the given location, and second those parameters are used to do the actual temporal interpolation. 

* __calibrateInterp()__ - This function is called by __InterpTemp()__ and does the parameter calibration for the temperature interpolation. This is specific to California. May need to change inputs for par_calibration() for other locations. 
* __InterpTemp()__ - This function calibrates and interpolates daily temperature data from a given location to hourly temperature data. No daily temperature data in your time series should be missing.
* __MergeDailyHourly()__ - This function combines the interpolated hourly temperatures with the observed hourly temperatures so that interpolated temperatures are only provided in the case where there is no observed hourly temperature for that time. Additionally it merges the hourly temperature data with daily temperature data so that it can be used in the plantmodel and flowermodel functions.

#### __thermaltimesupport.R__
This script has a number of functions that 

*

### 0_preprocess/

#### __1_walnut_prep.R__ 
This script imports raw walnut flowering and harvest data from a number of files. The data is merged and then reformatted. Year-cultivars with multiple dates for a given phenological event have the dates averaged so there is one date for each event, for each cultivar, for each year. Specific cultivars of interest are selected and the data is saved as a csv. 
	
* Input Files:
	* raw/crop/WalnutBreedProgram_Locations.csv
	* raw/crop/walnut1954.csv
	* raw/crop/walnutpope2.csv
	* raw/crop/walnutbloompope.csv
	* raw/crop/WalnutJarvisSheanData2018_10.csv
* Output Files: phenology/walnutclean.csv


#### __2_almond_prep.R__ 
This script imports raw almond flowering and harvest data from three files. The data is reformatted so it all has the same form. Columns are renamed as are some of the locations and phenological events. Specifically, 10% flowering is selected as the flowering event (event1) because it is most consistently reported across the data. Specific cultivars are also extracted. Finally, the data from the three sources is merged together and saved as a csv. Required source file: functions/preprocessfunctions.R

* Input Files:
	* clean/croploc.csv
	* raw/crop/AlmondChicoModestoBloom.csv
	* raw/crop/NSVAlmond.csv
	* raw/crop/RAVTHullsplitandBloom.csv
* Output Files: phenology/almondclean.csv


#### __3_prune_prep.R__ 
This script imports raw prune flowering and harvest data from two files. The data is reformatted so it all has the same form. Specific cultivars are also extracted. Finally, the data from the two sources is merged together and saved as a csv. Required source file: functions/preprocessfunctions.R

* Input Files:
    * clean/croploc.csv
    * raw/crop/french1988.csv
    * raw/crop/NSVPrune.csv
* Output Files: phenology/pruneclean.csv

#### __4_clim_prep.R__ 
This script cleans daily temperature data (min, max) from the [NCDC](https://www.ncdc.noaa.gov/cdo-web/)(Daily Summaries) and hourly temperature data from [CIMIS](https://cimis.water.ca.gov/WSNReportCriteria.aspx) for Chico, Davis, Modesto and Parlier. This included removing data that was clearly incorrect, and filling in missing data. To fill in missing data, temperature data from surrounding weather stations was downloaded for each primary location. This data was then related to the primary location data using a series of linear models. The linear models were then used to predict temperatures for the missing days using model averaging, using R^2 as the measure of goodness of fit (see function fillinTemps() in functions/cleanTemps.R). Daily and hourly temperature time series were then saved as csv files, for later temperature interpolation. Required source file: functions/cleanTemps.R

* Input Files:
    * raw/climate/noaachiconew.csv
    * raw/climate/noaachiconew2.csv
    * raw/climate/noaadavisnew.csv
    * raw/climate/noaamodesto.csv
    * raw/climate/noaamodesto2.csv
    * raw/climate/noaaparlier.csv
    * raw/climate/noaaparlier2.csv
    * All Files in raw/climate/cimis/

* Output Files:
    * clean/noaadavis.csv
    * clean/cimisdavis.csv
    * clean/noaachico.csv
    * clean/cimischicodurham.csv
    * clean/noaaparlier.csv
    * clean/cimisparlier.csv
    * clean/noaamodesto.csv
    * clean/cimismodesto.csv

 * Secondary weather station locations
    * Davis
        * NCDC: Winters, Woodland
        * CIMIS: Bryte, Dixon, Winters, Woodland, Zamora
    * Chico
        * NCDC: Colusa, Marysville, Oroville, Orland
        * CIMIS: Biggs, Orland
    * Modesto
        * NCDC: Denair, Oakdale Woodward Dam, Stockton Metropolitan Airport, Turlock
        * CIMIS: Denair (II), Manteca, Oakdale, Patterson, Tracy 
    * Parlier
        * NCDC: Fresno Yosemite International, Hanford, Lemon Cove, Orange Cove, Visalia 
        * CIMIS: Caruthers, Fresno/F.S.U. USDA, Fresno State, Orange Cove, Visalia 


#### __5_process_clim.R__ 
This script uses the hourly temperature data.frames from __4_prep_clim.R__ to calibrate temperature interpolation functions for each of the 4 locations. It then uses those functions to interpolate the daily temperature data __4_prep_clim.R__. Finally, it merges the hourly temperatures and interpolated hourly temperatures together into a data.frame where there is hourly temperature for all days in the time series but CIMIS data is picked preferentially over interpolated NCDC data. Finally the temperature time series for the four locations are merged and saved. Required source files: functions/tempInterpolation.R, functions/datetime.R

* Input Files:
    * clean/noaachico.csv
    * clean/noaadavis.csv
    * clean/noaamodesto.csv
    * clean/noaaparlier.csv
    * clean/cimischicodurham.csv
    * clean/cimisdavis.csv
    * clean/cimismodesto.csv
    * clean/cimisparlier.csv

* Output Files:
    * phenology/dailyhourlytemp.RDS
    * phenology/dailyhourlytemp.csv

#### __6_monthlytempprep.R__
This script takes the min and max daily temperatures from phenology/dailyhourly.RDS and averages them by month to create a time series of average min and max temperatures for each month (same method as NCDC monthly averages). It also does this for the annual average min and max temperatures. Finally, it creates a 5 year moving average for the annual temperatures and saves all generated data. Required source file:  functions/cleanTemps.R

* Input File: phenology/dailyhourlytemp.RDS

* Output Files:
    * history/annualtemperatures.csv
    * history/monthlytemperatures.csv

#### __7_precipprep.R__
THis script takes precipitation data from the NCDC and prepares it for use predicting flowering. It creates two variables by aggregating the monthly data, annual precipitation and winter precipitation (precip in Nov, Dec, and Jan). Additionally, it sets the year to start at March 1 so that flowering that happens in the spring is predicted by winter precipitation the previous year.

* Input File: raw/climate/precipNOAA.csv
* Output File: history/precipitation.csv



### 1_prediction/

### 2_history/

#### __1_optimize__

This script identifies the optimal length of thermal time accumulation for predicting season length for almonds, walnuts, and prunes. It only does this for a subset of the possible cultivars to cut down on computation time. The optimal values are determined with the plantmodel function from phenoclim, using the ‘asymcur’ thermal time functional form with cardinal temperatures 4, 25, and 36 C (aka the Anderson 1986 model). The output of this script are three objects of the class PlantModel that contain all of the specifications for the optimal model for predicting season length. 

* Input Files: 
    * phenology/dailyhourlytemp.RDS
    * phenology/almondclean.csv
    * phenology/walnutclean.csv
    * phenology/pruneclean.csv

* Output Files:
    * history/SLalmondDTanderson.RDS
    * history/SLwalnutDTanderson.RDS
    * history/SLpruneDTanderson.RDS

#### __2_springMerge.R__

### 3_flowering/

## Data Descriptions

Data used by multiple chapters (Walnut, History or Flowering) is stored in data/phenology. Data used in only one chapter is stored in the data file associated with that chapter.

## File Structure

* __preprocess/__
    * 1_walnut_prep.R
    * 2_almond_prep.R
    * 3_prune_prep.R
    * 4_clim_prep.R
    * 5_process_clim.R
    * 6_monthlytempprep.R
* __1_prediction/__
    * 1_optimize.R
    * __2_surface_DT/__
    	* DTflat.R
    	* DTlinear.R
    	* DTtriangle.R
    * __3_surface_TTT/__
    	* dayflat.R
    	* daylinear.R
    	* daytriangle.R
    * __4_length/__
    	* DTlength.R
    	* DTtemps.R
    	* farm_length.sh
    	* TTTlengthGDD.R
    	* TTTlengthGDH1.R
    	* TTTlengthGDH2.R
    	* TTTlengthGDH3.R
    	* TTTtempsGDD.R
    	* TTTtempsGDH1.R
    	* TTTtempsGDH2.R
    	* TTTtempsGDH3.R
    * 5_model_summary.R
    * 6_length_summary.R
    * 7_functionalform_assessment.R
    * 8_multilevel.R
    * 9_variance.R
    * 10_walnutsummary.R
    * 11_tempsummary.R
* __2_history/__
	* 1_springMerge.R
	* 2_fallMerge.R
* __3_flowering/__
	* __1_farm/__
	* 2_extractparameters.R
	* walnut.R
* __functions/__
	* accumulatedtemps.R
	* autogregressivefunction.R
	* chill.R
	* cleanTemps.R
	* datetime.R
	* extractlm.R
	* generalfunctions.R
	* helperfunctions.R
	* packagetesting.R
	* preprocessfunctions.R
	* reportfunctions.R
	* tempInterpolation.R
	* thermaltimesupport.R


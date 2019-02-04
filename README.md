# phenology: Analyzing the effects of climate change on almond, prune, and walnut phenology

This repository stores the code for my dissertation on climate change and phenology in California.

All data is stored in Phenology/data. Documentation for data can be found in the __Data Descriptions__ section.

__Required R Packages:__ phenoclim, tidyverse, knitr, reshape2, grid, segmented, kableExtra, dismo

## Table of Contents

[TOC]


## File Descriptions

### Preprocess

__1_walnut_prep.R:__ This script imports raw walnut flowering and harvest data from a number of files. The data is merged and then reformatted. Year-cultivars with multiple dates for a given phenological event have the dates averaged so there is one date for each event, for each cultivar, for each year. Specific cultivars of interest are selected and the data is saved as a csv. 
	
* Input Files:
	* raw/crop/WalnutBreedProgram_Locations.csv
	* raw/crop/walnut1954.csv
	* raw/crop/walnutpope2.csv
	* raw/crop/walnutbloompope.csv
	* raw/crop/WalnutJarvisSheanData2018_10.csv
* Output Files: phenology/walnutclean.csv


__2_almond_prep.R:__ This script imports raw almond flowering and harvest data from three files. The data is reformatted so it all has the same form. Columns are renamed as are some of the locations and phenological events. Specifically, 10% flowering is selected as the flowering event (event1) because it is most consistently reported across the data. Specific cultivars are also extracted. Finally, the data from the three sources is merged together and saved as a csv. Required source file: functions/preprocessfunctions.R

* Input Files:
	* clean/croploc.csv
	* raw/crop/AlmondChicoModestoBloom.csv
	* raw/crop/NSVAlmond.csv
	* raw/crop/RAVTHullsplitandBloom.csv
* Output Files: phenology/almondclean.csv


__3_prune_prep.R:__ This script imports raw prune flowering and harvest data from two files. The data is reformatted so it all has the same form. Specific cultivars are also extracted. Finally, the data from the two sources is merged together and saved as a csv. Required source file: functions/preprocessfunctions.R

* Input Files:
    * clean/croploc.csv
    * raw/crop/french1988.csv
    * raw/crop/NSVPrune.csv
* Output Files: phenology/pruneclean.csv

__4_clim_prep.R:__ This script cleans daily temperature data (min, max) from the [NCDC](https://www.ncdc.noaa.gov/cdo-web/)(Daily Summaries) and hourly temperature data from [CIMIS](https://cimis.water.ca.gov/WSNReportCriteria.aspx) for Chico, Davis, Modesto and Parlier. This included removing data that was clearly incorrect, and filling in missing data. To fill in missing data, temperature data from surrounding weather stations was downloaded for each primary location. This data was then related to the primary location data using a series of linear models. The linear models were then used to predict temperatures for the missing days using model averaging, using R^2 as the measure of goodness of fit (see function fillinTemps() in functions/cleanTemps.R). Daily and hourly temperature time series were then saved as csv files, for later temperature interpolation. Required source file: functions/cleanTemps.R

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


__5_process_clim.R__

## Data Descriptions

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
	* 1_cropclimMerge.R
	* 2_thermaltime.R
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


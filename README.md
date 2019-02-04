# phenology: Analyzing the effects of climate change on almond, prune, and walnut phenology

This repository stores the code for my dissertation on climate change and phenology in California.

All data is stored in Phenology/data. Documentation for data can be found in the __Data Descriptions__ section.

__Required R Packages:__ phenoclim, tidyverse, knitr, reshape2, grid, segmented, kableExtra

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


## File Descriptions


## Data Descriptions
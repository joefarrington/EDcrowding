# About this folder

This is a series of files to extract data from Star and predict admissions from ED. 

## Order to run files in

### 1. Get-data-from-flow.R

Gets data from flow (materialised tables on Star) only for patients in ED

Output
- demog_raw 
- flowsheet_raw 
- lab_raw

Note - bed_moves is not retrived as we are using bed_moves as processed by code in the EDCrowding/flow-mapping folder

### 1a Get-data-from-star.R

I have added get-data-from-star.R to get recent months from Star, to add to the flow data which runs to September 2020

Output
- demog_raw (now retrieving from star_test, the refactored database)
- flowsheet_raw - for all ED patients including those retrieved from flow
- lab_raw - for all ED patients

### 2. bed_moves.R

Input (as processed by code in the EDCrowding/flow-mapping folder )
- demog_raw (see note above about two sources for this)
- ED_bed_moves
- ED_csn_summ

using bed moves, a matrix is arranged in the following way: for a patient visit, we have the (mrn, csn, admission datetime, discharge datetime, sex, age, patient eventually admitted or not). Adds an epoch to denote whether this is before COVID, during Surge 1 or after Surge 1

Calculates how much time is spent in each location

Output
- matrix_csn


matrix_csn has the same number of rows as ED_csn_summ - ie all csns we are interested in


### 3. clean_flowsheet_data.R

Takes flowsheet data and removes any csns that are not included in ED_bed_moves. Includes some processing to map the csns in flowsheets to revised csns that were generated in the process of cleaning/creating ED_bed_mvoes. Uses foreign and primary keys on bed_moves to do this.

Cleans non-numeric fields to make them numeric (e.g. separates blood pressure into systolic and diastolic)

Elapsed minutes from point of admission calculated. Names of flowsheet measurements are cleaned

Input
- flowsheet_raw - including those retrieved from both Star and flow 
- ED_bed_moves
- ED_csn_summ
- excluded_csns

Output
- flowsheet_raw_exluded_csns - all data from flowsheets in long matrix
- flowsheet_real - all numeric data from flowsheets in long matrix
- flowsheet_num_results - (long matrix) for each mrn, csn, fk_bed_moves and measurement, a count of the number of measurements
- flowsheet_num_results_with_zero (wide matrix; one row per csn and fk_bed_moves (location) with NAs replaced with zeroes for all non-existent measurements at that location)
- flowsheet_num_results_with_zero_csn_level (wide matrix; one row per csn with NAs replaced with zeroes for all non-existent measurements)

Note that all of these files exclude any csns that did not have flowsheet measurements


### 4. clean_lab_data.R

Takes lab data and removes any csns that are not included in ED_bed_moves. Includes some processing to map the csns in labs to revised csns that were generated in the process of cleaning/creating ED_bed_mvoes. Uses foreign and primary keys on bed_moves to do this.

Input
- lab_raw - including those retrieved from both Star and flow 
- ED_bed_moves
- ED_csn_summ
- excluded_csns

Output
- lab_raw_exluded_csns - all data from labs in long matrix
- lab_real - all numeric data from labs in long matrix
- lab_num_results - (long matrix) for each mrn, csn, fk_bed_moves and local_code, a count of the number of results
- lab_num_results_with_zero (wide matrix; one row per csn and fk_bed_moves (location) with NAs replaced with zeroes for all non-existent lab results at that location)
- lab_num_results_with_zero_csn_level (wide matrix; one row per csn with NAs replaced with zeroes for all non-existent lab results)

Note that all of these files exclude any csns that did not have lab measurements

### 5. Various exploring files 

explore-flowsheet-and-lab-data.R
explore-durations.R

These are doing exploratory data analysis to decide which variables to attach to matrix. Produce multiple charts

### 6. create-design-matrix.R

Creates a design matrix for input into ML. 

Loops through a function to generate a view of the ED department at a given time point (eg 60 min)

Input
- lab_raw
- ED_bed_moves
- ED_csn_summ
- excluded_csns


output-
-matrix_60 [Need to add code for the rest of the time slices]

### 7. Machine learning scripts

- run_xgboost_with_defaults.R 
- run_xgboost_with_defaults_post_Surge1.R 

- run_xgboost_tune_all.R

tunes all params using a maximum entropy grid (NOTE I have edited this due to an error with num_ED_rows_excl_OTF being included erroneously in the model, but haven't tested this). Code includes charts showing performance of models

- run_xgboost_tune_by_step.R

follows a series of steps to tune the best model; steps currently hardcoded

- xgboost_model_evaluation_experimenting.R - includes brier test
- xgboost_model_evaluation.R

various code to evaluate models

- xgboost_model_batch_with_workflow.R
- exboost_model_evaluation_with_trees.4

I have left these in, but they may now be superceded



### 8. Evaluation at the aggregate level

From the predictions made in xgboost, this yields the performance measure for different metrics: distribution evaluation, hosmer-lemeshow statistic, madcap

- evalute-model-aggregate-level-2.R ; contains my code
- evalute-model-aggregate-level.R ; contains Enoch's original code (and functions)


### 9. Other models

- run_glm.R = as it sounds

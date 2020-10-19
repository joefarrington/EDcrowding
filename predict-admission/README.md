# About this folder

This is a series of files to extract data from Star and predict admissions from ED. 
## Order to run files in

### 1. Query.R

Gets data from flow (materialised tables on Star)

Output
- demog_raw
- flowsheet_raw

Note - bed_moves is not retrived as we are using bed_moves as processed by code in the EDCrowding/flow-mapping folder

### 2. bed_moves.R

Input (as processed by code in the EDCrowding/flow-mapping folder )
- ED_bed_moves
- ED_csn_summ

using bed moves, a matrix is arranged in the following way: for a patient visit, we have the (mrn, csn, admission datetime, discharge datetime, department, room, sex, age, patient eventually admitted or not)

Adds an epoch to denote whether this is before COVID, during Surge 1 or after Surge 1

Calculates how much time is spent in each location

Output
- matrix_csn
- matrix_loc

matrix_csn has the same number of rows as ED_csn_summ - ie all csns we are interested in


### 3. clean_flowsheet_data.R

Takes flowsheet data and removes any csns that are not included in ED_bed_moves. Includes some processing to map the csns in flowsheets to revised csns that were generated in the process of cleaning/creating ED_bed_mvoes. Uses foreign and primary keys on bed_moves to do this.

Cleans non-numeric fields to make them numeric (e.g. separates blood pressure into systolic and diastolic)

Elapsed minutes from point of admission calculated. Names of flowsheet measurements are cleaned

Input
- flowsheet_raw
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
- lab_raw
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

### 5. explore-flowsheet-and-lab-data.R

This file is doing exploratory data analysis to decide which variables to attach to matrix. Produces multiple charts

### The remaining files are left from Enoch's legacy code - will be changed or removed in future commits

### 6a. Xgboost_model

models that train and make predictions based on the input date (D) and period of history used for training

Input
- dm_with_everything_simp_col.csv

Output
- prediction.csv

### 6b. Xgboost_model_rpart

same as above but instead of using boost tree with xgboost engine, use rpart that would also calculate the indicator importance

Input
- dm_with_everything_simp_col.csv


Output
- prediction.csv

### 7. Evaluate 

from the predictions made in xgboost, this yields the performance measure for different metrics: distribution evaluation, hosmer-lemeshow statistic, madcap

Input 
- prediction.csv

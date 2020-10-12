# About this folder

This is a series of files to extract data from Star and predict admissions from ED. 
## Order to run files in

### 1. Query.R

Gets data from flow (materialised tables on Star)

Output
- demog_raw.csv
- flowsheet_raw.csv

Note - currently using bed_moves as processed by code in the EDCrowding/flow-mapping folder

### 2. bed_moves.R

Input (as processed by code in the EDCrowding/flow-mapping folder )
- ED_bed_moves
- ED_csn_summ

using bed moves, a matrix is arranged in the following way: for a patient visit, we have the (mrn, csn, admission datetime, discharge datetime, department, room, sex, age, patient eventually admitted or not)

Output
- matrix.csv

### 3. clean_flowsheet_data.R

Input
- flowsheet_raw
- ED_csn_summ

Output
- flowsheet_real - includes date and time for each measurement

and for chart purposes (but not ML)
- flowsheet_real
- flowsheet_num_results (long matrix) for each mrn, csn, fk_bed_moves and mapped_name how many measurements  there
- flowsheet_num_results_with_zero (wide matrix; one row per csn and mapped with zeroes for all non-existent measurements at that date time)


### 4. clean_lab_data.R

Input
- lab_raw
- ED_csn_summ

Output
- lab_real - includes date and time for each measurement

and for chart purposes (but not ML)
- lab_real
- lab_num_results (long matrix) for each mrn, csn, fk_bed_moves and local_code showing how many measurements  there
- lab_num_results_with_zero (wide matrix; one row per csn and mapped with zeroes for all non-existent measurements at that date time)

### 5. explore-flowsheet-and-lab-data.R

This file is doing exploratory data analysis to decide which variables to attach to matrix

### 3. attach_flowsheet.R

attach latest flowsheet reading to visits that apply

Output
- design_matrix.csv

### 4. lab_matrix

attach latest lab result to visits that apply

Output 
- dm_with_demog_flowsheet_labs.csv

### 5. Simplify_flowsheet_cols

flowsheet column names require simplifying ( contains the glossary of simplified terms )

Output
- dm_with_everything_simp_col.csv

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

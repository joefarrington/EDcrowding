# About this folder

This is a series of files to extract data from Star and predict admissions from ED. 
## Order to run files in

### 1. Query.R

bed moves queried from star / flow

Output
- bed_moves_emergency.csv
- demog.csv
- encounter.csv

### 2. bed_moves.R

using bed moves, a matrix is arranged in the following way: for a patient visit, we have the (mrn, csn, admission datetime, discharge datetime, department, room, sex, age, patient eventually admitted or not)

Output
- matrix.csv

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

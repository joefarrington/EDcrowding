# About this folder

This is a series of files to train models to predict admissions from ED. 

## Order to run files in

### 1. get-obs-lab-data-from-star.R

Output
- obs_raw 
- lab_raw

### 2. clean_obs_data.R

Takes observation data and removes any csns that are not included in ED_bed_moves. Includes some processing to map the csns in flowsheets to revised csns that were generated in the process of cleaning/creating ED_bed_movws. Uses foreign and primary keys on bed_moves to do this.

Cleans non-numeric fields to make them numeric (e.g. separates blood pressure into systolic and diastolic)

Names of flowsheet measurements are cleaned

Input
- obs_raw 
- summ (to get presentation and discharge from ED time)

Output
- obs_real - all data from selected flowsheet measurements in long matrix
- obs_to_include (needed for real-time prediction)

Note that this excludes any csns that did not have obs measurements


### 3. clean_lab_data.R

Takes lab data and removes any csns that are not included in ED_bed_moves. Includes some processing to map the csns in labs to revised csns that were generated in the process of cleaning/creating ED_bed_mvoes. Uses foreign and primary keys on bed_moves to do this.

Input
- lab_orders_raw 
- lab_results_raw
- summ

Output
- lab_orders_real
- lab_results_real
- lab_orders_to_include (needed for real-time prediction)

Note that all of these files exclude any csns that did not have lab measurements

### 4. generate_timeslices.R

Input
- lab_real
- obs_real
- moves
- summ

output- a series of design matrices for ML

### 5.  run-ML.R

Runs XGBoost

Inputs
- design matrices from generate-timeslices.R

Outputs
- scores for all tuning rounds
- saved learners for MLR
- save features used in each model (needed for real-time prediction)
- feature importances for each model

### 6. Scripts used for aggregate predictions

These look at csn-level data over history and use it to create empirical parameters (probable time to admission, time-varying Poisson arrivals) used in aggregating predictions. Run these only if new csn-level data is available or training/val/test set dates change

- calc-not-yet-arrived
- calc-time-to-admission

### 7. predict-and-plot-CORU-chart.R

Loads models and creates an aggregate distribution and evaluates it

### 8. compare-models

Need to create this file based on compare-models-with-and-without-location

### plot-ED-examples

Used to create plots of example ED patients for use in presentations only

### Other files 

- run-LR-using-stats
- run-RF

### Saved in the explore folder in case useful - not in current pipeline

- create-lab-test-clusters - no longer needed now that lab batteries are used instead
- 

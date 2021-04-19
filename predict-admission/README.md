# About this folder

This is a series of files to extract data from Star and predict admissions from ED. 

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

Note that all of these files exclude any csns that did not have lab measurements

### 4. generate_timeslices.R

Input
- lab_real
- obs_real
- moves
- summ

output- a series of design matrices for ML


### 5. Various exploring files [now out of date - will need to be updated with new input files]

explore-flowsheet-and-lab-data.R
explore-durations.R

These are doing exploratory data analysis to decide which variables to attach to matrix. Produce multiple charts


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

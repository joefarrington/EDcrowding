# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

Flow schema tables are materialised database tables derived from a one-off extract from Star that was done in early September 2020. They are quick to run but not updated. Star is based on views and very slow to run, but up to date. Use this for data later than September 2020.


### 0. Exploratory file

get-all-bed-move-data-from-Flow-and-Star.R

Retrieves all data from both schemas, in order to do exploratory processing of odd situations. The main goals are:

- find patients who have multiple csns per visit
- find patients who spend time in AEDU
- create a summary dataset of prior visits



## Order to run files in

### 1. Retrieve data

get-ED-data-from-Star_test.R

These two files retrieve and process data from the relevant schema.  

output
- ED_bed_moves_raw
- ED_csn_summ_raw
- visits

plus other files saved from the SQL extracts

- csn_summ - all csns over time
- bed_moves - all bed moves over time
- patient_class - latest patient class
- all_patient_class - patient class audit table

### 1a. Process data

process-ED-data-from-Star_test.R

Does some further processes to clean and group room names

input
- ED_bed_moves_raw
- ED_csn_summ_raw

output
- ED_bed_moves
- ED_csn_summ


### 1b. Create edge list

create-edge-list-using-data.table.R

Does further processing on the bed moves data to (a) identify exits from ED to various locations of interest and (b) create a simplified edge list (ie rows with from and to nodes) that will be used for  network maps

input
- ED_bed_moves
- ED_csn_summ

output
- moves
- edgedf

### 1c. Calc admission class

Does some analysis of which class patients might be in based on their latest patient class, whether they went to ED or not, where they went after ED (if anywhere). Final section gives them a class. 


### 2. Get data on vital signs

get-vital-signs-from-Flow-DB-step.R

This file gets flowsheet data that will be used to identify when a patient first had measurements taken

output 
- ED_flowsheet_raw


### 3. create-edge-list.R

input 
- ED_csn_summ 
- ED_bed_moves
- ED_flowsheet_raw

output
- edgedf


## After that - in any order

### Create-plots-from-encounter-details.R

input
- ED_csn_summ


### calc-edge-stats.R

input
- edgedf
- ED_csn_summ

output (various options)
- edgelist_summ (totals over period)
- edgelist_stats (daily stats)
as above for breach

## calc-node-stats.R

input
- ED_bed_moves
- ED_csn_summ

output
- node_stats


## Python files

### create-viz-from-edge-list.py

input
- edgelist_stats

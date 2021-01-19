# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

Flow schema tables are materialised database tables derived from a one-off extract from Star that was done in early September 2020. They are quick to run but not updated. Star is based on views and very slow to run, but up to date. Use this for data later than September 2020.


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
- ED_csn_summ - updated with admission class



## After that - in any order

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

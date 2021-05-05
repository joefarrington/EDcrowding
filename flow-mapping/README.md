# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

## Order to run files in

### 1. Retrieve data

get-ED-data-from-Star.R

Retrieve and process data from EMAP.  

output (all as tibbles)
- ED_bed_moves_raw
- ED_csn_summ_raw
- visits

plus other files saved from the SQL extracts

- csn_summ - all csns over time
- bed_moves - all bed moves over time
- patient_class - latest patient class
- all_patient_class - patient class audit table

### 1b. Create data tables

create-data.tables.R

Does further processing on the bed moves data to (a) identify exits from ED to various locations of interest and (b) create a simplified edge list (ie rows with from and to nodes) that will be used for network maps

input
- ED_bed_moves_raw
- ED_csn_summ_raw

output (all as data tables)
- moves
- edgedf
- summ - updated version of ED_csn_summ_raw with admission class



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

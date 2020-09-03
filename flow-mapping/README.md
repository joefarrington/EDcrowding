# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

## Order to run files in

### 1. Retrieve and process data

get-ED-data-from-Flow-DB-step.R 
process-ED-data-from-Flow.R

These two files retrieve and process data from Star

output
- ED_bed_moves
- ED_csn_summ
(- ED_bed_moves_extra - currently commented out; adds an additional waiting node)

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

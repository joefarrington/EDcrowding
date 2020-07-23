# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

## Order to run files in

### 1. get-ED-data-from-Star

This removes some encounters 

output
- bed_moves
- ED_csn_summ
- ED_bed_moves 

### 2. create-edge-list.R

input 
- ED_csn_summ (now contains everything that ED_csn_detail previously contained)
- ED_bed_moves

output
- edgedf

### 3. create-encounter-details.R (this has been superceded - no longer needed


## After that - in any order

### Create-plots-from-encounter-details.R

input
- ED_csn_summ


### Process-edge-list-for-analysis.R

input
- edgedf
- ED_csn_summ

output (various options)
- edgelist_summ 
- edgelist_stats

## Create-node-details.R

input
- ED_bed_moves
- ED_csn_summ


## Python files

### create-viz-from-edge-list.py

input
- edgelist_stats

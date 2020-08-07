# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

## Order to run files in

### 1. get-ED-data-from-Star

output
- ED_bed_moves
- ED_csn_summ
- ED_bed_moves_extra 

### 2. create-edge-list.R

input 
- ED_csn_summ 
- ED_bed_moves

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

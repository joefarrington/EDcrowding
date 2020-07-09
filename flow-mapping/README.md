# Order to fun files in

1. get-ED-data-from-Star

output
- bed_moves
- ED_csn_summ
- ED_bed_moves

2. create-edge-list.R

input 
- ED_csn_summ
- ED_bed_moves

output
- edgedf

3. create-encounter-details.R

input
- ED_csn_summ
- ED_bed_moves
- edgedf

output

- ED_csn_detail


After that - in any order

* create-plots-from-encounter-details.R

input
- ED_csn_detail


. process-edge-list-for-analysis.R

input
- edgedf
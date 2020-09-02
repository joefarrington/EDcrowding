load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_clean_MayJunJul_2020-08-06.rda")

# Has vital sign messages and non-imaging ADT which have been omitted. 

a <- ED_bed_moves %>% filter(mrn == "21213633")

# Only non-imaging ADT.  Mrn was merged the next day.

b <- ED_bed_moves %>% filter(mrn == "21219744")

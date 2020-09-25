# About this script
# =================

# This script reads data from EMAP Star using the materialised tables 
# in the flow schema.

# It includes only admissions that involved ED at some point,
# excludes any admissions that included pediatrics in ED
# and extracts all bed_moves for those admissions, including 
# onward destinations from ED. 

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Load bed_move data
# ==================

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

## EITHER get bed moves from Flow
# option to add   age(b.discharge, b.admission) as duration_row

sqlQuery <- "select e.mrn, e.csn, e.ed_arrival_dttm, e.ed_discharge_dttm, e.num_ed_rows,
  b.admission, b.discharge, b.department, b.room, b.bed, b.hl7_location,
  age(b.discharge, b.admission) as duration_row

  from 
  flow.ed_csn_summ e,
  flow.bed_moves b
  where e.mrn = b.mrn
  and e.csn = b.csn
  and e.pk_ed_csn_summ = b.fk_ed_csn_summ
  order by b.mrn, b.csn, b.admission"
sqlQuery <- gsub('\n','',sqlQuery)

start <- Sys.time()
ED_bed_moves_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
print(Sys.time() - start)
# Using the flow materialised tables post optimising took 1.2 seconds for the whole of August
# Using the tables prior to optimising took 3.33 mins for 1-6 August


# save data for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_all_",today(),".rda")
save(ED_bed_moves_raw, file = outFile)
rm(outFile)

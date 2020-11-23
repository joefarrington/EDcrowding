# About this file
# ===============


# load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# load data
# ==========


ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")



# sqlQuery <- "select distinct(d.mrn), d.sex, d.birthdate
#     from flow.demographics d,
#     flow.ed_csn_summ e
#     where e.num_ed_rows > 0
#     and d.mrn = e.mrn
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# save(demog_raw, file = paste0('EDcrowding/predict-admission/data-raw/demog_',today(),'.rda'))
# 
# 

sqlQuery <- "select  DISTINCT f.flowsheet_datetime, f.mrn, f.csn, f.mapped_name, f.result_as_real, f.result_text, f.fk_bed_moves 
  from flow.flowsheet f,
  flow.bed_moves b
  where f.fk_bed_moves = b.pk_bed_moves
  and f.mrn = b.mrn
  and f.csn = b.csn
  and b.department = 'UCH EMERGENCY DEPT'
  and f.mapped_name is not null
  order by  f.fk_bed_moves, f.flowsheet_datetime
"
sqlQuery <- gsub('\n','',sqlQuery)
flowsheet_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
save(flowsheet_raw, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_',today(),'.rda'))



# sqlQuery <- "select  DISTINCT l.result_datetime, l.mrn, l.csn, l.local_code, l.mapped_name, l.result_text, l.reference_range, l.fk_bed_moves 
#   from flow.lab l,
#   flow.bed_moves b
#   where l.fk_bed_moves = b.pk_bed_moves
#   and l.mrn = b.mrn
#   and l.csn = b.csn
#   and b.department = 'UCH EMERGENCY DEPT'
#   order by  l.fk_bed_moves, l.result_datetime
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# lab_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# save(lab_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_raw_',today(),'.rda'))
# 


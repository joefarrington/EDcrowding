# About this script
# =================

# This script reads data from EMAP Star using the view called
# flowsheets. It includes only admissions that involved ED 
# and were within a given date



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Load data
# =========

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

## EITHER get bed moves from Star

sqlQuery <- "select  
  a.mrn,
  a.csn,
  a.flowsheet_datetime,
  a.flowsheet_type,
  a.mapped_name,
  a.result_text,
  a.result_as_real
  
from star.flowsheets a
  join (
          select distinct mrn, csn
          FROM
          star.bed_moves 
          where department = 'UCH EMERGENCY DEPT'
            and admission > '2020-08-01 00:00:00'
            and admission < '2020-08-31 00:00:00'
         ) c 
    on a.mrn = c.mrn 
    and a.csn = c.csn
    
where a.flowsheet_datetime > '2020-07-31 00:00:00'
and a.flowsheet_datetime < '2020-09-02 00:00:00'"
sqlQuery <- gsub('\n','',sqlQuery)

start <- Sys.time()
ED_flowsheet_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
Sys.time() - start

# save for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_flowsheets_August_",today(),".rda")
save(ED_flowsheet_raw, file = outFile)
rm(outFile)
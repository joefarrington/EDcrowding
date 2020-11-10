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

sqlQuery <- "select * from flow.care_site"
sqlQuery <- gsub('\n','',sqlQuery)

care_sites <- as_tibble(dbGetQuery(ctn, sqlQuery))
# Using the flow materialised tables post optimising took 1.2 seconds for the whole of August
# Using the tables prior to optimising took 3.33 mins for 1-6 August


# save data for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/care_sites_",today(),".rda")
save(care_sites, file = outFile)
rm(outFile)

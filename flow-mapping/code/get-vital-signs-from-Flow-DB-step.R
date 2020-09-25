# About this script
# =================

# This script reads data from EMAP Star via a materialised table called
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

file_label <- "all_"

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

## Get flowsheet data from flow
## Note that flow.flowsheet (singular) only contains rows with measurements that took place in ED

sqlQuery <- "select *
  
from flow.flowsheet"

# where a.flowsheet_datetime > '2020-07-31 00:00:00'
# and a.flowsheet_datetime < '2020-09-02 00:00:00'"
sqlQuery <- gsub('\n','',sqlQuery)

start <- Sys.time()
ED_flowsheet_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
print(Sys.time() - start)

# save for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_flowsheets_",file_label,today(),".rda")
save(ED_flowsheet_raw, file = outFile)
rm(outFile)
# About this file
# ===============

# created this file to get all bed moves data, in order to look up information about prior visits
# it collects all bed moves records from flow, and recent records from Star
# for each visit the number of previous visits and the time since the last visit is calculated

# load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# Create functions
# ===============



# load data
# ==========


# Get data from Star ------------------------------------------------------


ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

sqlQuery <- "select  * from
  star.encounters
"
sqlQuery <- gsub('\n','',sqlQuery)
csn_all <- as_tibble(dbGetQuery(ctn, sqlQuery))
save(csn_all, file = paste0('EDcrowding/flow-mapping/data-raw/csn_all_star_',today(),'.rda'))
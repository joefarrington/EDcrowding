
# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)



# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get  data ---------------------------------------------------------


# all patients in ED now

sqlQuery <- "select distinct flowsheet_value_key, flowsheet_row_name
from covid_staging.flowsheet"

sqlQuery %>% gsub('\n','',sqlQuery)
covid_staging_flowsheet_IDs <- data.table(dbGetQuery(ctn, sqlQuery))


# load("~/Devart/covid_staging_flowsheet_IDs.rda")
save(covid_staging_flowsheet_IDs, file = "~/Devart/covid_staging_flowsheet_IDs.rda")

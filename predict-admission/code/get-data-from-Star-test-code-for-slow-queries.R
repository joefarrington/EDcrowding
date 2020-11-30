# About this file
# ===============

# This file collects data from Star, which requires extracting data from non-materialised tables. 
# For flowsheets, this is very slow so this is done by day across a date range
# All patients are retrived (to make the SQL faster) so flowsheet data for non-ED patients, 
# and flowsheet data for ED patients after they leave ED are deleted

# For labs and flowsheets an additional step is required to create a foreign key to bed moves

# Note - this needs bed_moves and csn summary data already processed for the relevant motnhs



# load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# load data
# ==========



# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")






# Get and process flowsheet data ------------------------------------------------------


# This query can't be used on Star - never loads
# 
# sqlQuery <- "select  DISTINCT f.flowsheet_datetime, f.mrn, f.csn, f.mapped_name, f.result_as_real, f.result_text
#   from star.flowsheets f,
#   star.bed_moves b
#   where f.mrn = b.mrn
#   and f.csn = b.csn
#   and b.department = 'UCH EMERGENCY DEPT'
#   and f.mapped_name is not null
#   and date(b.admission) >= '2020-09-01'
#   and date(b.admission) <= '2020-10-31'
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# flowsheet_raw_sepOct <- as_tibble(dbGetQuery(ctn, sqlQuery))
# save(flowsheet_raw_sepOct, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_SepOct_',today(),'.rda'))

# Therefore use alternative approrach
timer <- Sys.time()

date_range <- seq(date('2020-09-01'), date('2020-10-31'), length.out = 61)
i = 1

for (i in seq(1,60,1)) {
  sqlQuery <- paste0("select  flowsheet_datetime, mrn, csn, mapped_name, result_as_real, result_text 
  from star.flowsheets 
  where flowsheet_datetime > '", date_range[i],
                     "' and flowsheet_datetime < '", date_range[i+1],
                     "' and mapped_name in ('ACVPU', 'BLOOD PRESSURE', 'Heart rate', 'National early warning score',
  'Oxygen saturation in Blood', 'Respiratory assist status', 'Respiratory rate',
  'Body temperature')
  ")
  
  sqlQuery <- paste0("select  flowsheet_datetime, mrn, csn, mapped_name, result_as_real, result_text 
  from star.flowsheets 
  where flowsheet_datetime > '", date_range[i],
                       "' and flowsheet_datetime < '", date_range[i+1],
                       "' and mapped_name in (
                     'ACVPU', 
                     'ADULT NON VERBAL PAIN SCORE',
                     'BLOOD PRESSURE', 
                     'Body temperature',
                     'Delivered oxygen flow rate',
                     'Glasgow coma score total',
                     'Heart rate', 
                     'Invasive mean arterial pressure',
                     'INVASIVE VENTILATION YES/NO',
                     'Morphine PCA Total hourly dose',
                     'National early warning score',
                     'Non-invasive mean arterial pressure',
                     'Oxygen concentration breathed',
                     'OXYGEN DELIVERY METHOD',
                     'Oxygen saturation in Blood', 
                     'PAIN SCORE AT MOVEMENT',
                     'PAIN SCORE AT REST',
                     'Respiratory assist status', 
                     'Respiratory rate',
                     'RICHMOND AGITATION SEDATION SCORE',
                     'VENTILATOR YES/NO'
                     )
  ")
    
  
  sqlQuery <- gsub('\n','',sqlQuery)
  
  flowsheet_raw_get <- as_tibble(dbGetQuery(ctn, sqlQuery))
  
  flowsheet_raw_get <- flowsheet_raw_get %>% distinct()
  
  print(date_range[i])
  print("Got this number of rows:")
  print(nrow(flowsheet_raw_get))
  print(Sys.time() - timer)
  timer <- Sys.time()
  
  
  if (i ==1) {
    flowsheet_raw_sepOct <- flowsheet_raw_get
  }
  else {
    flowsheet_raw_sepOct <- flowsheet_raw_sepOct %>% bind_rows(flowsheet_raw_get)
  }
}


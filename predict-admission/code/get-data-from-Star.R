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


# 
# sqlQuery <- "select d.mrn, d.sex, d.birthdate, d.death_date, d.death_indicator
#     from star.demographics d
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# save(demog_raw, file = paste0('EDcrowding/predict-admission/data-raw/demog_',today(),'.rda'))
# 

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


save(flowsheet_raw_sepOct, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_SepOct_',today(),'.rda'))

# by inner joining with ED_csn_summ we can change the csn to the new one, and excluded any previously excluded csns 
# we can also exclude any flowsheet data from after ED

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
flowsheet_raw_SepOct <- ED_csn_summ %>% select(mrn, csn, csn_old, ED_discharge_dttm) %>% 
  rename(csn_new = csn) %>% 
  inner_join(flowsheet_raw_sepOct, by = c("mrn", "csn_old" = "csn")) %>% rename(csn = csn_old) %>% 
  filter(flowsheet_datetime < ED_discharge_dttm)

flowsheet_raw_SepOct <- flowsheet_raw_SepOct %>% select(-csn) %>% 
  rename(csn = csn_new)



save(flowsheet_raw_SepOct, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_SepOct_just_ED_',today(),'.rda'))


# load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")
# 
# ED_bed_moves_SepOct <- ED_bed_moves_SepOct %>% ungroup()
# flowsheet_raw_SepOct$pk_bed_moves = NA
# 
# for (i in 1:nrow(flowsheet_raw_SepOct)) {
#   flowsheet_raw_SepOct$pk_bed_moves[i] <- as.character(ED_bed_moves_SepOct %>% 
#     filter(csn == a$csn, admission < a$flowsheet_datetime, discharge >= a$flowsheet_datetime) %>% select(pk_bed_moves))
# }


# 
# sqlQuery <- "select  DISTINCT l.result_datetime, l.mrn, l.csn, l.local_code, l.mapped_name, l.result_text, l.reference_range
#   from star.labs l,
#   star.bed_moves b
#   where l.mrn = b.mrn
#   and l.csn = b.csn
#   and b.department = 'UCH EMERGENCY DEPT'
#   and date(b.admission) >= '2020-09-01'
#   and date(b.admission) <= '2020-10-31'
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# lab_raw_sepOct <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# save(lab_raw_sepOct, file = paste0('EDcrowding/predict-admission/data-raw/lab_raw_SepOct_',today(),'.rda'))
# 
# 

# About this file
# ===============

# This file collects data from Star, which requires extracting data from non-materialised tables. 
# For flowsheets, this is very slow so the script itereates through a date range for flowsheets
# All patients are retrived (to make the SQL faster) so flowsheet data for non-ED patients, 
# and flowsheet data for ED patients after they leave ED are deleted

# For demog data, this is now coming from star_test

# For labs and flowsheets an additional step is required to create a foreign key to bed moves

# Note - this needs bed_moves and csn summary data already processed for the relevant motnhs
# And merges the new flowsheet and lab data into existing datasets that DO NO include
# any data for the relevant months



# load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

# load data
# ==========

# updated to include relevant data for the months of interest (Sept and Oct)
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda") 
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")



# Get demog data NOTE THIS IS STAR_TEST---------------------------------------------------------

sqlQuery <- "select mrn, 
 date_of_birth, 
 date_of_death, 
 alive, 
 sex
 from star_test.core_demographic d,
  star_test.mrn m
  where m.mrn_id = d.mrn_id
"
sqlQuery <- gsub('\n','',sqlQuery)
demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

save(demog_raw, file = paste0('EDcrowding/predict-admission/data-raw/demog_',today(),'.rda'))



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

# worth saving this as it takes a while to get
save(flowsheet_raw_sepOct, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_SepOct_',today(),'.rda'))

# by inner joining with ED_csn_summ we can change the csn to the new one, and excluded any previously excluded csns 
# we can also exclude any flowsheet data from after ED

flowsheet_raw_SepOct_just_ED <- ED_csn_summ %>% select(mrn, csn, csn_old, arrival_dttm, ED_discharge_dttm) %>% 
  rename(csn_new = csn) %>% 
  inner_join(flowsheet_raw_sepOct, by = c("mrn", "csn_old" = "csn")) %>% 
  rename(csn = csn_new)

flowsheet_raw_SepOct_just_ED <- flowsheet_raw_SepOct_just_ED %>% 
  mutate(keep = flowsheet_datetime > arrival_dttm & flowsheet_datetime < ED_discharge_dttm) %>% 
  filter(keep) %>% select(-keep)


# create a temp dataset of all flowsheet dates times and look up the location of the patient at this time
dttms <- flowsheet_raw_SepOct_just_ED %>% select(mrn, csn, flowsheet_datetime) %>% distinct()
dttms$fk_bed_moves <- NA

for (i in 1:nrow(dttms)) {
  dttms$fk_bed_moves[i] <- as.character(ED_bed_moves_SepOct %>%
    filter(csn == dttms$csn[i], admission < dttms$flowsheet_datetime[i], discharge >= dttms$flowsheet_datetime[i]) %>% 
      select(pk_bed_moves))
}

# join this location to the original data set for Sep and Oct
flowsheet_raw_SepOct_just_ED <- flowsheet_raw_SepOct_just_ED %>% 
  left_join(dttms) %>%  select(-csn_old, -ED_discharge_dttm, -arrival_dttm) %>% distinct()


save(flowsheet_raw_SepOct_just_ED, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_SepOct_just_ED_',today(),'.rda'))

# join the Sep and oct data to the rest of the data we already have

flowsheet_raw <- flowsheet_raw %>% mutate(fk_bed_moves = as.character(fk_bed_moves)) %>% bind_rows(flowsheet_raw_SepOct_just_ED)

save(flowsheet_raw, file = paste0('EDcrowding/predict-admission/data-raw/flowsheet_raw_with_Star_',today(),'.rda'))


# Get and process lab data ------------------------------------------------


sqlQuery <- "select  DISTINCT l.result_datetime, l.mrn, l.csn, l.local_code, l.mapped_name, l.result_text, l.reference_range
  from star.labs l,
  star.bed_moves b
  where l.mrn = b.mrn
  and l.csn = b.csn
  and b.department = 'UCH EMERGENCY DEPT'
  and date(b.admission) >= '2020-09-01'
  and date(b.admission) <= '2020-10-31'
"
sqlQuery <- gsub('\n','',sqlQuery)
lab_raw_sepOct <- as_tibble(dbGetQuery(ctn, sqlQuery))

save(lab_raw_sepOct, file = paste0('EDcrowding/predict-admission/data-raw/lab_raw_SepOct_',today(),'.rda'))


# by inner joining with ED_csn_summ we can change the csn to the new one, and excluded any previously excluded csns 
# we can also exclude any flowsheet data from after ED

lab_raw_SepOct_just_ED <- ED_csn_summ %>% select(mrn, csn, csn_old, arrival_dttm, ED_discharge_dttm) %>% 
  rename(csn_new = csn) %>% 
  inner_join(lab_raw_sepOct, by = c("mrn", "csn_old" = "csn")) %>% 
  rename(csn = csn_new)

lab_raw_SepOct_just_ED <- lab_raw_SepOct_just_ED %>% 
  mutate(keep = result_datetime > arrival_dttm & result_datetime < ED_discharge_dttm) %>% 
  filter(keep) %>% select(-keep)


# create a temp dataset of all flowsheet dates times and look up the location of the patient at this time
dttms <- lab_raw_SepOct_just_ED %>% select(mrn, csn, result_datetime) %>% distinct()
dttms$fk_bed_moves <- NA

for (i in 1:nrow(dttms)) {
  dttms$fk_bed_moves[i] <- as.character(ED_bed_moves_SepOct %>%
                                          filter(csn == dttms$csn[i], admission < dttms$result_datetime[i], discharge >= dttms$result_datetime[i]) %>% 
                                          select(pk_bed_moves))
}

# join this location to the original data set
lab_raw_SepOct_just_ED <- lab_raw_SepOct_just_ED %>% 
  left_join(dttms) %>%  select(-csn_old, -ED_discharge_dttm, -arrival_dttm) %>% distinct()

save(lab_raw_SepOct_just_ED, file = paste0('EDcrowding/predict-admission/data-raw/lab_raw_SepOct_just_ED_',today(),'.rda'))

# join the Sep and oct data to the rest of the data we already have

lab_raw <- lab_raw %>% mutate(fk_bed_moves = as.character(fk_bed_moves)) %>%  bind_rows(lab_raw_SepOct_just_ED)

save(lab_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_raw_with_Star_',today(),'.rda'))


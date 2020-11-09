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

# load data
# ==========


# Get data from Star ------------------------------------------------------


ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

# sqlQuery <- "select  * from
#   flow.bed_moves
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# bed_moves_all <- as_tibble(dbGetQuery(ctn, sqlQuery))
# save(bed_moves_all, file = paste0('EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_',today(),'.rda'))

load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_2020-11-09.rda")

# sqlQuery <- "select  * from
#     star.bed_moves
#     where admission > '2020-09-01'
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# bed_moves_all_star <- as_tibble(dbGetQuery(ctn, sqlQuery))
# save(bed_moves_all_star, file = paste0('EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_star_from_Sep_',today(),'.rda'))

load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_star_from_Sep_2020-11-09.rda")

     
bed_moves_all <- bed_moves_all %>% filter(admission <= today()) %>% 
  bind_rows(bed_moves_all_star  %>% filter(admission <= today())) %>% distinct()

bed_moves_all <- bed_moves_all %>% 
  arrange(mrn, admission) %>% group_by(mrn) %>% 
  mutate(next_csn = lead(csn), 
         next_admission = lead(admission)) 

csn_changed <- bed_moves_all %>% filter(discharge == next_admission, csn != next_csn) %>% select(department, hl7_location, csn, next_csn)

save(csn_changed, file = paste0('EDcrowding/flow-mapping/data-raw/csn_changed_',today(),'.rda'))

visit_summ <- bed_moves_all %>% 
  group_by(mrn, csn) %>% 
  mutate(arrival_dttm = min(admission, na.rm = TRUE)) 

# visit_summ <- visit_summ %>% 
#   mutate(discharge_dttm = max(discharge, na.rm = TRUE))

vs <- visit_summ %>% ungroup() %>% 
  select(mrn, csn, arrival_dttm, discharge_dttm) %>% distinct() %>% filter(!is.na(arrival_dttm)) %>% arrange(mrn, arrival_dttm)

vs <- vs %>% group_by(mrn) %>% 
  mutate(visit_num = 1:n())

vs <- vs %>% 
  mutate(days_since_last_visit = as.numeric(difftime(arrival_dttm, lag(arrival_dttm), units = "days")))


save(vs, file = paste0('EDcrowding/flow-mapping/data-raw/visit_summ_all_flow_and_star_',today(),'.rda'))




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

##  get bed moves from Star

sqlQuery <- "select e.mrn, 
  e.csn,
  e.ed_arrival_dttm,
  e.ed_discharge_dttm,
  e.num_ed_rows,
  b.admission,
  b.discharge,
  b.department,
  b.room,
  b.bed,
  b.hl7_location
  from 
  flow.ed_csn_summ e,
  flow.bed_moves b 
where e.num_ed_rows > 0 
and b.csn = e.csn 
and b.mrn = e.mrn 
and b.fk_ed_csn_summ = e.pk_ed_csn_summ"
sqlQuery <- gsub('\n','',sqlQuery)

bed_moves_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# this returns 506K rows whereas the saved one has 175K rows
#bed_moves = read.csv('F:/Saved/ENOCKUNG/ED project/bed_moves.csv')

# #isolate entries at UCH emergency and remove COVID cases - Enoch's code
# bed_moves1 = bed_moves_raw %>% filter(department == 'UCH EMERGENCY DEPT') # this reduces it to 441,694K
# bed_moves1 = bed_moves1 %>% filter(!grepl('ED^COVID',hl7_location,fixed=TRUE)) # this reduces it to 440,260K
# Note - Enoch's approach only removes csn locations, not the whole csn

# # pick entries from bed_moves with csn that matches those that have gone past UCH emergency
emergency_csn = unique(bed_moves1$csn) #151393 csns
bed_moves_raw %>% summarise(n_distinct(csn)) #151394 csns
# 
# # isolate from all bed_moves those with csn that intersects UCH emergency
# bed_moves2 = bed_moves[is.element(bed_moves$csn,emergency_csn),]

# my version of Enoch's check delivers the same number of rows as the new SQL generates, with 3 rows missing due to a single ED visit which only had a COVID row and no other row
bed_moves2 <- bed_moves_raw %>% filter(!csn %in% emergency_csn)

#write.csv(bed_moves2,'EDcrowding/predict-admission/data-raw/bed_moves_emergency_temp.csv')
save(bed_moves_raw, file = paste0('EDcrowding/predict-admission/data-raw/bed_moves_',today(),'.rda'))


# sqlQuery <- "SELECT *
#   FROM star.encounter
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# encounter <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# write.csv(encounter,'F:/UserProfiles/ENOCKUNG/ED project/encounter.csv')

sqlQuery <- "select distinct(d.mrn), d.sex, d.birthdate
    from flow.demographics d,
    flow.ed_csn_summ e
    where e.num_ed_rows > 0
    and d.mrn = e.mrn
"
sqlQuery <- gsub('\n','',sqlQuery)
demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# demog = encounter
# 
# demog = data.frame('mrn'=demog$mrn, 'birthdate'=demog$birthdate, 'sex'=demog$sex)

# write.csv(demog,'F:/UserProfiles/ENOCKUNG/ED project/demog.csv')
save(demog_raw, file = paste0('EDcrowding/predict-admission/data-raw/demog_',today(),'.rda'))






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

## EITHER get bed moves from Star

sqlQuery <- "select 
  a.mrn,
  a.csn,
  b.arrival_dttm,
  b.discharge_dttm,
  a.admission,
  a.discharge,
  a.department,
  a.room, 
  a.bed,
  a.hl7_location,
  c.num_ED_rows
  from star.bed_moves a
    join (
          select mrn, csn, min(admission) as arrival_dttm, 
          max(discharge) as discharge_dttm FROM
          star.bed_moves b
          group by mrn, csn
          ) b
    on a.mrn = b.mrn 
    and a.csn = b.csn
    join (
          select mrn, csn,
          count(*) as num_ED_rows 
          FROM
          star.bed_moves 
          where department = 'UCH EMERGENCY DEPT'
          group by mrn, csn 
         ) c 
    on a.mrn = c.mrn 
    and a.csn = c.csn
  where a.csn in  
    (
    select csn
    FROM
    star.bed_moves 
    where department = 'UCH EMERGENCY DEPT'
    and date(admission) >= '2020-08-01'
    and date(admission) <= '2020-08-07'
    group by csn
    ) 
  and a.csn not in 
    (
    select csn from star.bed_moves where
    room like ('PAEDS%')
    )
  and c.num_ED_rows >= 1"
sqlQuery <- gsub('\n','',sqlQuery)

start <- Sys.time()
ED_bed_moves_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
Sys.time() - start
# DB Forge took 2.51 for two months and 12.09 for 2020 year to date
# R took 14.28 min for Jan and Feb; 8.36 minutes for Mar Apr; 11.9 for May-Jul
# R took 3.33 mins for 1-6 August


# save data for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_August_",today(),".rda")
save(ED_bed_moves_raw, file = outFile)
rm(outFile)

# About this script
# =================

# This script reads data from EMAP Star using the view called
# flowsheets. It includes only admissions that involved ED 
# and were within a given date



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)


# Load data
# =========

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
  a.flowsheet_datetime,
  a.flowsheet_type,
  a.mapped_name,
  a.result_text,
  a.result_as_real
  
from star.flowsheets a
  join (
          select distinct mrn, csn
          FROM
          star.bed_moves 
          where department = 'UCH EMERGENCY DEPT'
            and admission > '2020-08-01 00:00:00'
            and admission < '2020-08-07 00:00:00'
         ) c 
    on a.mrn = c.mrn 
    and a.csn = c.csn
    
where a.flowsheet_datetime > '2020-08-01 00:00:00'"
sqlQuery <- gsub('\n','',sqlQuery)

start <- Sys.time()
ED_flowsheet_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
Sys.time() - start

# save for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_flowsheets_August_",today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)


library(fuzzyjoin)
flowsheet_location <- fuzzy_left_join(ED_flowsheet_raw %>% filter(flowsheet_datetime < "2020-08-02")
                     %>% select(mrn, csn, flowsheet_datetime, mapped_name),
                ED_bed_moves %>% filter(admission < "2020-08-02") %>% 
                  select(mrn, csn, admission, discharge, dept3, room6),
                by = c("mrn" = "mrn",
                       "csn" = "csn",
                       "flowsheet_datetime" = "admission",
                       "flowsheet_datetime" = "discharge"),
                match_fun = list(`==`, `==`, `>=`, `<=`)) %>% 
  select(-mrn.y, -csn.y) %>% rename(mrn = mrn.x, csn = csn.x)


# Create charts

png("EDCrowding/flow-mapping/media/Location of flowsheet measurement in ED 2020-08-01.png", width = 1077, height = 659)

  
flowsheet_location %>% filter(dept3 == "Still in ED", !is.na(room6), !is.na(mapped_name)) %>% group_by(mapped_name, room6) %>% summarise(total  =n()) %>% 
  ggplot(aes(x = room6, y = total, fill = fct_rev(mapped_name))) + 
  geom_bar(position = "stack", stat = "identity", width = .8) +
  theme_classic() +
  labs(title = "Location of flowsheet data recorded while in ED on 1 August 2020",
       x = "Location",
       y = "Number of recorded measurements",
       fill = "Name of flowsheet record") 
dev.off()  



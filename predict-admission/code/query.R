library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(xgboost)

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = 'UCLVLDDDTAEPS02',
                      port = 5432,
                      user = 'enockung',
                      password = 'emapu8Q15bz7',
                      dbname = "uds")

## EITHER get bed moves from Star

sqlQuery <- "SELECT *
  FROM star.bed_moves bm"
sqlQuery <- gsub('\n','',sqlQuery)

#bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))
bed_moves = read.csv('F:/Saved/ENOCKUNG/ED project/bed_moves.csv')

#isolate entries at UCH emergency and remove COVID cases
bed_moves1 = bed_moves %>% filter(department == 'UCH EMERGENCY DEPT')
bed_moves1 = bed_moves1 %>% filter(!grepl('ED^COVID',hl7_location,fixed=TRUE))

# pick entries from bed_moves with csn that matches those that have gone past UCH emergency
emergency_csn = unique(bed_moves1$csn)

# isolate from all bed_moves those with csn that intersects UCH emergency
bed_moves2 = bed_moves[is.element(bed_moves$csn,emergency_csn),]

write.csv(bed_moves2,'F:/UserProfiles/ENOCKUNG/ED project/bed_moves_emergency.csv')

sqlQuery <- "SELECT *
  FROM star.encounter 
"
sqlQuery <- gsub('\n','',sqlQuery)
encounter <- as_tibble(dbGetQuery(ctn, sqlQuery))

write.csv(encounter,'F:/UserProfiles/ENOCKUNG/ED project/encounter.csv')

sqlQuery <- "SELECT *
  FROM star.demographics
"
sqlQuery <- gsub('\n','',sqlQuery)
encounter <- as_tibble(dbGetQuery(ctn, sqlQuery))
demog = encounter

demog = data.frame('mrn'=demog$mrn, 'birthdate'=demog$birthdate, 'sex'=demog$sex)

write.csv(demog,'F:/UserProfiles/ENOCKUNG/ED project/demog.csv')



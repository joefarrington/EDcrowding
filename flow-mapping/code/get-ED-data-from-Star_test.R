
# About the preprocessing steps -------------------------------------------

# retrieve data
# - deal with missing admission time
# - deal with missing discharge time
# - deal with outlier ages
# - delete patients who died in ED or on the day of being in ED as these were not clinically admitted or discharged


# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Create functions --------------------------------------------------------

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get data ---------------------------------------------------------

# hopital visit summary

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.patient_class, hv.presentation_time, hv.admission_time,   hv.discharge_time, hv.arrival_method, hv.discharge_destination, hv.discharge_disposition
    from star_test.hospital_visit hv,
      star_test.mrn m
  where hv.mrn_id = m.mrn_id
    and hv.patient_class not in ('OUTPATIENT', 'NEW_BORN', 'DAY_CASE', 'SURGICAL ADMISSION')
    order by mrn, csn, admission_time
"
sqlQuery <- gsub('\n','',sqlQuery)
csn_summ <- as_tibble(dbGetQuery(ctn, sqlQuery))

# demographics

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

# bed moves

sqlQuery <- "select m.mrn, hv.encounter as csn, lv.hospital_visit_id, l.location_string, 
lv.admission_time as admission, lv.discharge_time as discharge 
from star_test.hospital_visit hv, 
star_test.mrn m,    
star_test.location_visit lv,
star_test.location l 
where hv.mrn_id = m.mrn_id 
and hv.patient_class not in ('OUTPATIENT', 'NEW_BORN', 'DAY_CASE', 'SURGICAL_ADMISSION') 
and lv.hospital_visit_id = hv.hospital_visit_id 
and l.location_id = lv.location_id 
order by mrn, csn, admission"

sqlQuery <- gsub('\n','',sqlQuery)
bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))


# Initial processing ------------------------------------------------------


# find visits involving ED 

bed_moves <- bed_moves %>% 
  mutate(department = split_location(location_string, 1),
         room2 = split_location(location_string, 1))

bed_moves <- bed_moves %>% 
  mutate(ED_row = case_when(department == "ED" ~ 1,
                                        TRUE ~ 0))

csn_summ <- csn_summ %>% left_join(
  bed_moves %>% group_by(csn) %>% summarise(num_ED_rows = sum(ED_row)) %>% filter(num_ED_rows > 0)
)

ED_csn_summ_raw <- csn_summ %>% filter(!is.na(num_ED_rows))
ED_bed_moves_raw <- bed_moves %>% left_join((ED_csn_summ_raw %>% select(csn, num_ED_rows))) %>% filter(!is.na(num_ED_rows))

# deal with missing admission time in ED_csn_summ

missing_adm_time <- ED_csn_summ_raw %>% filter(is.na(admission_time)) %>% select(csn)
missing_adm_time <- missing_adm_time %>% 
  left_join(
    ED_bed_moves_raw %>% filter(csn %in% missing_adm_time$csn) %>% 
      select(csn, admission) %>% group_by(csn) %>% summarise(admission_time_ = min(admission, na.rm = TRUE))
  )

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(missing_adm_time) %>% 
  mutate(admission_time = case_when(is.na(admission_time) ~ admission_time_,
                                    TRUE ~ admission_time)) %>% select(-admission_time_)

# deal with missing discharge time - some of these will be patients still in
missing_dis_time <- ED_csn_summ_raw %>% filter(is.na(discharge_time)) %>% select(csn, admission_time, num_ED_rows)

missing_dis_time <- missing_dis_time %>% 
  mutate(delete = case_when(admission_time < Sys.Date() - 1 & num_ED_rows == 1 ~ TRUE))

ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(
  missing_dis_time %>% select(csn, delete) 
) %>% filter(is.na(delete))

ED_bed_moves_raw <- ED_bed_moves_raw  %>% left_join(
  missing_dis_time %>% select(csn, delete) 
) %>% filter(is.na(delete)) %>% select(-delete)

# add demographic information 

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(demog_raw) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1915-01-01" ~ NA_integer_,
                         TRUE ~ as.integer(as.period(interval(start = date_of_birth, end = admission_time))$year)))

# delete patients who died on the day of being in ED
died <- ED_csn_summ_raw %>% filter(discharge_destination == "Patient Died", patient_class == "EMERGENCY") %>% 
  mutate(died_in_ED = TRUE)

ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(
  died %>% select(csn, died_in_ED) 
) %>% filter(is.na(died_in_ED)) %>% select(-died_in_ED)

ED_bed_moves_raw <- ED_bed_moves_raw %>% left_join(
  died %>% select(csn, died_in_ED) 
) %>% filter(is.na(died_in_ED)) %>% select(-died_in_ED)


# Save data ---------------------------------------------------------------

# save bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_",today(),".rda")
save(bed_moves, file = outFile)
rm(outFile)

# save ED_bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_all_",today(),".rda")
save(ED_bed_moves_raw, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/csn_summ_",today(),".rda")
save(csn_summ, file = outFile)
rm(outFile)


# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_all_",today(),".rda")
save(ED_csn_summ_raw, file = outFile)
rm(outFile)

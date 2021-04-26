# About the script  -------------------------------------------

# Gets all patients currently in ED



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# clean room data
# function removes bay and chair numbers

clean_room_names <- function(department, room) {
  if (department == "ED" && !is.na(room)) {
    room = gsub("UCHED ", "", room)
    room = gsub("UCH ED ", "", room)
    room = gsub("UCH ", "", room)
    room = gsub("^ED ","",room)  
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub("[0-9]{3}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS CH",room)
    room = gsub("MAJCH","MAJORS CH",room)
    room = gsub("SPECIALTY ASSESSMENT AREA","SAA",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
    room = gsub("SDEC","SDEC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room)  
    room = gsub("OTF POOL","OTF",room)  
    room = gsub("CONS", "", room)
    room = gsub(" ","_",room)  
  }
  else if (grepl("UCHT00CDU",department)) {
    room = "CDU"
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)

# function to group room names
# NB RAT COVID MAJORS could be both - need to check which to prioritise
group_room_names <- function(room) {
  room_ <- case_when(
    length(grep("UTC", room)) >0 ~ "UTC",
    length(grep("MAJ", room)) >0 ~ "MAJORS",
    length(grep("RAT", room)) >0 ~ "RAT",
    length(grep("TRIAGE", room)) >0 ~ "TRIAGE",
    length(grep("SPECIALTY ASSESSMENT AREA", room)) >0 ~ "SAA",
    length(grep("SDEC", room)) >0 ~ "SDEC",
    room %in% c( "null", "WR POOL") ~ "Waiting",
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)



# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get  data ---------------------------------------------------------

# all patients in ED now

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method
    from star.hospital_visit hv,
      star.mrn m,    
      star.location_visit lv,
      star.location l 
  where hv.mrn_id = m.mrn_id
  and hv.discharge_time is NULL
  and hv.admission_time is not NULL
  and lv.hospital_visit_id = hv.hospital_visit_id 
  and l.location_id = lv.location_id 
  and hv.admission_time > NOW() - INTERVAL '3 DAY' 
  and left(l.location_string, 3) ='ED^'
  and lv.discharge_time is null
    order by csn, admission_time desc;"

sqlQuery <- gsub('\n','',sqlQuery)
summ_now <- as_tibble(dbGetQuery(ctn, sqlQuery))
rpt(summ_now)
summ_now <- summ_now %>% mutate(time_since_arrival = difftime(Sys.time(), presentation_time, units = "mins"))


# demographics

sqlQuery <- "select mrn, 
 date_of_birth, 
 sex
 from star.core_demographic d,
  star.hospital_visit hv,
  star.mrn m,    
  star.location_visit lv,
  star.location l 
  where hv.mrn_id = m.mrn_id
  and hv.discharge_time is NULL
  and hv.admission_time is not NULL
  and lv.hospital_visit_id = hv.hospital_visit_id 
  and l.location_id = lv.location_id 
  and hv.admission_time > NOW() - INTERVAL '3 DAY' 
  and left(l.location_string, 3) ='ED^'
  and lv.discharge_time is null
  and m.mrn_id = d.mrn_id
"

sqlQuery <- gsub('\n','',sqlQuery)
demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

# location data

sqlQuery <- "   select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method, lv.admission_time as admission, lv.discharge_time as discharge, l.location_string    
    from star.hospital_visit hv,
      star.mrn m,    
      star.location_visit lv,
      star.location l
      where lv.hospital_visit_id in 
            (
            select hv.hospital_visit_id
            from star.hospital_visit hv,
              star.location_visit lv,
              star.location l 
          where hv.discharge_time is NULL
          and hv.admission_time is not NULL
          and lv.hospital_visit_id = hv.hospital_visit_id 
          and l.location_id = lv.location_id 
          and hv.admission_time > NOW() - INTERVAL '3 DAY' 
          and left(l.location_string, 3) ='ED^'
          and lv.discharge_time is null
            )
      and l.location_id = lv.location_id 
      and lv.hospital_visit_id = hv.hospital_visit_id 
      and hv.mrn_id = m.mrn_id;
"
sqlQuery <- gsub('\n','',sqlQuery)
moves_now <- as_tibble(dbGetQuery(ctn, sqlQuery)) %>% arrange(csn, admission)

# visit history

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.patient_class, 
  hv.presentation_time, hv.admission_time, hv.discharge_time, hv.discharge_destination, hv.discharge_disposition
    from star.hospital_visit hv,
      star.mrn m
      where m.mrn in 
            (
            select m.mrn 
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null
            )
    and hv.mrn_id = m.mrn_id
"
sqlQuery <- gsub('\n','',sqlQuery)
summ_hist <- as_tibble(dbGetQuery(ctn, sqlQuery))

# patient class change history

sqlQuery <- "  select encounter as csn, hospital_visit_id, max(valid_until) as max_emerg_class 
from star.hospital_visit_audit hva,
      star.mrn m
   where m.mrn_id in 
            (
            select m.mrn_id 
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null
            ) 
        and hva.mrn_id = m.mrn_id
  group by encounter, hospital_visit_id"

sqlQuery <- gsub('\n','',sqlQuery)
patient_class_hist <- as_tibble(dbGetQuery(ctn, sqlQuery))

# observation data

sqlQuery <- "  select hv.encounter as csn, hv.admission_time, vo.observation_datetime, vo.unit, vo.value_as_real, vo.value_as_text,
        vo.visit_observation_type_id, vot.id_in_application
    from star.hospital_visit hv,
      star.visit_observation vo,
      star.visit_observation_type vot
  where hv.hospital_visit_id = vo.hospital_visit_id
    and hv.hospital_visit_id in 
        (select hv.hospital_visit_id
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null )
      and vo.observation_datetime < hv.admission_time + INTERVAL'2 days'  
  and vo.visit_observation_type_id = vot.visit_observation_type  
            "

sqlQuery <- gsub('\n','',sqlQuery)
obs <- data.table(dbGetQuery(ctn, sqlQuery))

# lab orders

sqlQuery <- "select hv.encounter as csn, lo.lab_order_id, lo.order_datetime, lo.request_datetime, lo.lab_battery_id, lb.battery_code
  from star.hospital_visit hv,
  star.lab_order lo,
  star.lab_battery lb
  where hv.hospital_visit_id = lo.hospital_visit_id
  and hv.hospital_visit_id in 
        (select hv.hospital_visit_id
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null )
  and lo.lab_battery_id = lb.lab_battery_id;"

sqlQuery <- gsub('\n','',sqlQuery)
lab_orders <- data.table(dbGetQuery(ctn, sqlQuery))

# lab results

sqlQuery <- "  select hv.encounter as csn, lr.lab_order_id, lr.abnormal_flag, lr.comment, lr.units, lr.range_high, lr.range_low, lr.result_last_modified_time, lr.value_as_real, lr.value_as_text, ltd.test_lab_code
    from star.hospital_visit hv,
      star.lab_result lr,
    star.lab_order lo,
    star.lab_test_definition ltd
  where hv.hospital_visit_id = lo.hospital_visit_id
    and lr.lab_order_id = lo.lab_order_id
    and ltd.lab_test_definition_id = lr.lab_test_definition_id
  and hv.hospital_visit_id in 
        (select hv.hospital_visit_id
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null )"

sqlQuery <- gsub('\n','',sqlQuery)
lab_results <- data.table(dbGetQuery(ctn, sqlQuery))

# Check data --------------------------------------------------------------

# check for missing admission time in location
moves_now %>% filter(is.na(admission))
moves_now %>% filter(admission == discharge)
moves_now %>% filter(admission > discharge)

# check for NA age eg patient had no record on demographics table
summ_now %>% filter(is.na(age))

# Process summ data -------------------------------------------------------

# filter out under 18s
summ_now = summ_now %>% left_join(demog_raw) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1915-01-01" ~ NA_integer_,
                         TRUE ~ as.integer(as.period(interval(start = date_of_birth, end = admission_time))$year)))

# flag under 18s but don't delete them
summ_now = summ_now %>% mutate(under_18 = case_when(age >= 18 ~ TRUE,
                                                    is.na(age) ~ FALSE, # retain patients with NA age for now
                                                    TRUE ~ FALSE))

# add visit history

summ_hist = summ_hist %>% left_join(patient_class_hist) 

prior_visits <- summ_hist %>% filter(!is.na(discharge_time)) %>% 
  select(mrn, csn, patient_class, admission_time, discharge_time) %>%  
  filter(patient_class %in% c("EMERGENCY", "INPATIENT"))  %>% 
  left_join(patient_class_hist) %>% 
  mutate(type = case_when(is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "planned_inpatient",
                          !is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "emergency_inpatient",
                          patient_class == "EMERGENCY" ~ "emergency_discharge"))

summ_now = summ_now %>% left_join(
  prior_visits %>% 
    group_by(mrn) %>% 
    summarise(num_adm_after_ED = sum(type == "emergency_inpatient"),
              num_ED_visits = sum(type %in% c("emergency_inpatient", "emergency_discharge"))), 
  by = "mrn"
)

# Process moves data ------------------------------------------------------------

moves_now <- moves_now %>% mutate(department = split_location(location_string, 1))
moves_now <- moves_now %>% mutate(room = split_location(location_string, 2))
moves_now <- moves_now %>% mutate(case_when(department == "ED" | department == "UCHT00CDU" ~ 1,
                            TRUE ~ 0))
moves_now <- moves_now %>% mutate(room3 = clean_room_names(department, room))
moves_now <- moves_now %>% mutate(room4 = group_room_names(room3))
moves_now <- moves_now %>% mutate(duration_row = difftime(discharge, admission, units = "hours"))

# indicate whether row is OTF location
moves_now <- moves_now %>% mutate(OTF_row = case_when(room == "UCHED OTF POOL" ~ 1, TRUE ~ 0))







# Process lab data --------------------------------------------------------

setkey(lab_orders, csn)
lab_orders_real <- merge(lab_orders, summ[,.(csn, first_ED_admission)]) 

setkey(lab_results, csn)

# add elapsed time
lab_orders[, elapsed_mins := as.numeric(difftime(request_datetime, first_ED_admission, units = "mins"))]
lab_results_real[, elapsed_mins := as.numeric(difftime(result_last_modified_time, first_ED_admission, units = "mins"))] 
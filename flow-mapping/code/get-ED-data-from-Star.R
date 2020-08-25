# About this script
# =================

# This script reads data from EMAP Star using the view called
# bed_moves. It includes only admissions that involved ED at some point,
# excludes any admissions that included pediatrics in ED
# and extracts all bed_moves for those admissions, including 
# onward destinations from ED. 
# 
# The script starts by reading data into a dataset called ED_bed_moves_raw 
# which can be saved as it is. It then refines this dataset and saves
# it as ED_bed_moves
# 
# Ward and room names are cleaned as shown in the wiki:
# https://github.com/zmek/EDcrowding/wiki/Data-Dictionary:-ED_bed_moves
#
# There is an option to create ED_bed_moves_extra
# An additional row is added when the wait time in arrival is 
# greater than a given time (currently set as 10 minutes) - creates an optional dataset with the extra rows
#
# NOTE - this script does not join with the encounter table
# That join would be necessary to get encounter_Id
# which is used as the key for patient_property and patient_fact views
# An additional SQL query to the database is probably more efficient
# than including it here. 



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Create functions
# ===============

# hl7_location seems to have more complete info than room and bed for some rows
# but locations are grouped into one string; this function splits them

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# shorten ward names
clean_wardnames5 <- function(x) {
  # If any ward has a name in brackets use the text in brackets as ward name e.g. "UCH T07 SOUTH (T07S)" becomes T07S
  if (grepl("\\(",x))  {
    start <- regexpr("\\(",x)[[1]]
    end <- regexpr("\\)",x)[[1]]
    x <- substr(x,start+1,end-1)
  }
  # Any West Moreland St location becomes Outside Tower
  else if (grepl("WMS",x)) {
    x <- "Outside Tower"
  } 
  # Any EGA location becomes  Outside Tower
  else if (grepl("EGA",x)) {
    x <- "Outside Tower"
  }
  # Any MCC location becomes  Outside Tower
  else if (grepl("MCC",x)) {
    x <- "Outside Tower"
  }
  # Any NHNN location becomes Outside Tower
  else if (grepl("NHNN",x)) {
    x <- "Outside Tower"
  }
  # Any RNTNE location becomes Outside Tower
  else if (grepl("RNTNE",x)) {
    x <- "Outside Tower"
  }
  # Anything with OFF in location becomes OFF
  else if (grepl("OFF",x)) {
    x <- "OFF"
  }
  # Anything with OUT in location becomes Outside Tower
  else if (grepl("OUT",x)) {
    x <- "Outside Tower"
  }
  # Anything with CRF in location becomes CRF
  else if (grepl("CRF",x)) {
    x <- "CRF"
  }
  # Anything with HARLEY in location becomes HARLEY
  else if (grepl("HARLEY",x)) {
    x <- "HARLEY"
  }
  # Anything with LOUNGE in location becomes LOUNGE
  else if (grepl("LOUNGE",x)) {
    x <- "LOUNGE"
  }
  x <- gsub("UCH ","",x) #remove UCH from any name
  x <- gsub("THEATRE SUITE","THR",x)
  x <- gsub(" CRITICAL CARE","",x)
  x <- gsub(" WARD","",x)
  x <- gsub(" SURG ","",x)
  x <- gsub(" SURGICAL ITU","SURG ITU",x)
  
  if (grepl("EMERGENCY DEPT",x)) {
    x <- "ED"
  } 
  if (grepl("EMERGENCY AU",x)) {
    x <- "AMU/EAU"
  }
  if (grepl("ACUTE MEDICAL",x)) {
    x <- "AMU/EAU"
  }
  if (grepl("T00",x)) {
    x <- "AMU/EAU"
  }
  if (grepl("P03 CV|P03 THR",x)) {
    x <- "ICU"
  }
  if (grepl("T03 INTENSIVE CARE",x)) {
    x <- "ICU"
  }
  if (grepl("HDRU",x)) {
    x <- "HDRU"
  }
  if (grepl("T07",x)) { #leave as is for now
    x <- "T07"
  }
  else if (grepl("T08|T09", x)) {
    x <- "T08/09" # leave as is for now
  }
  else if (grepl("T02", x)) {
    x <- "T02" # leave as is for now
  }
  else if (grepl("T[0-9][0-9]",x)) {
    x <- "Tower"
  }
  
  return(x)
}

clean_wardnames5 <- Vectorize(clean_wardnames5)

# clean room data
# function removes bay and chair numbers
clean_room_names <- function(dept, room) {
  if (dept == "ED" && !is.na(room)) {
    room = gsub("UCHED ","",room)
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
#    room = gsub("^UTC [A-z]+ ROOM","UTC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)

# function to group room names
# NB RAT COVID MAJORS could be both - need to check which to prioritise
group_room_names <- function(room) {
  room_ <- case_when(
    length(grep("UTC", room)) >0 ~ "UTC",
    length(grep("MAJORS", room)) >0 ~ "MAJORS",
    length(grep("RAT", room)) >0 ~ "RAT",
    length(grep("PAEDS TRIAGE", room)) >0 ~ "PAEDS TRIAGE",
    length(grep("TRIAGE", room)) >0 ~ "TRIAGE",
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)


# simple function to return whether location denotes admission
calc_admission <- function(dept2, room2, discharge, discharge_dttm, ED_discharge_dttm_excl_OTF) {
  if (dept2 != "ED") {
    return("Admitted")
  }
  else if (room2 == "OTF" && !is.na(ED_discharge_dttm_excl_OTF) 
           # added this to make sure there are later rows - ie patient was eventually admitted
           && discharge < discharge_dttm)
    return("Admitted")
  else {
    return("Still in ED")
  }
}

calc_admission <- Vectorize(calc_admission)


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
    and date(admission) >= '2020-01-01'
    and date(admission) <= '2020-02-29'
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
outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_JanFeb_",today(),".rda")
save(ED_bed_moves_raw, file = outFile)
rm(outFile)


# Basic checks
# ============

# remove rows where admission == discharge; 
# these are rows where a patient spent no time in a location
# to see the number: 
#ED_bed_moves_raw %>% filter(admission == discharge) %>% n_distinct() # 12

# to remove these rows
ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(admission != discharge)

# remove rows with no department information
ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(!is.na(department))

# identify admission rows (first for each encounter)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(arrival_row = ifelse(admission == arrival_dttm, TRUE, FALSE))

# calc row durations
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))

# arrange by arrival time
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  arrange(mrn, csn, admission)

# indicate whether row is ED location
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(ED_row = case_when(department == "UCH EMERGENCY DEPT" ~ 1,
                          TRUE ~ 0))

# set arrival row where there are multiple ED visits in same csn
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(arrival_row = case_when(ED_row == 1 & is.na(lag(department != "UCH EMERGENCY DEPT")) ~ TRUE,
                                 ED_row == 1 & lag(department != "UCH EMERGENCY DEPT") ~ TRUE,
                                  ED_row == 1 & lag(department == "UCH EMERGENCY DEPT") ~ arrival_row,
                                  ED_row != 1  ~ arrival_row))

# create dataset of csns with multiple visits to ED
multiple_ED_bed_moves <- ED_bed_moves_raw %>% filter(arrival_row) %>% 
  group_by(csn) %>% summarise(tot = n()) %>% 
  filter(tot > 1) %>% select(csn) %>% left_join(ED_bed_moves_raw) %>% 
  arrange(mrn, csn, admission)

# count the number of these
num_multiple_ED_visits <- multiple_ED_bed_moves %>% 
  select(csn) %>% distinct() %>%  n_distinct()

# while this number is greater than 0, calculate a new csn, arrival and
# discharge time for the csns with multiple visits to ED 
# and update ED_bed_moves_raw with these

i <- 0
while (num_multiple_ED_visits >0 ) {
  i <- i + 1
  mult_visit_csn <- multiple_ED_bed_moves %>% filter(arrival_row) %>% 
    group_by(csn) %>% summarise(latest_ED_arrival = max(admission)) %>% 
   select(csn, latest_ED_arrival) %>% 
    mutate(csn_new = as.character(as.numeric(csn)*10^i)) %>% 
    select(csn, csn_new, latest_ED_arrival)
  
  multiple_ED_bed_moves <- multiple_ED_bed_moves %>% 
    left_join(mult_visit_csn, by = "csn") %>% 
    mutate(csn_new = case_when(!is.na(latest_ED_arrival) & admission >= latest_ED_arrival ~ csn_new,
                           TRUE ~ csn),
           arrival_dttm_new = case_when(!is.na(latest_ED_arrival) & admission >= latest_ED_arrival ~ latest_ED_arrival,
                           TRUE ~ arrival_dttm))
  
  multiple_ED_bed_moves <- multiple_ED_bed_moves %>% ungroup() %>% 
    group_by(mrn, csn_new, arrival_dttm_new) %>% 
      mutate(discharge_dttm_new = max(discharge))

  ED_bed_moves_raw <- ED_bed_moves_raw %>% left_join(multiple_ED_bed_moves) %>% 
    mutate(csn = case_when(!is.na(csn_new) ~ csn_new,
                           TRUE ~ csn),
           arrival_dttm = case_when(!is.na(arrival_dttm_new) ~ arrival_dttm_new,
                                    TRUE ~ arrival_dttm),
           discharge_dttm = case_when(!is.na(discharge_dttm_new) ~ discharge_dttm_new,
                                      TRUE ~ discharge_dttm)) %>% 
    select(-csn_new, - arrival_dttm_new, -discharge_dttm_new, -latest_ED_arrival)
  
  multiple_ED_bed_moves <- ED_bed_moves_raw %>% filter(arrival_row) %>% 
    group_by(csn) %>% summarise(tot = n()) %>% 
    filter(tot > 1) %>% select(csn) %>% left_join(ED_bed_moves_raw) %>% 
    arrange(mrn, csn, admission)
  
  num_multiple_ED_visits <- multiple_ED_bed_moves %>% 
    select(csn) %>% distinct() %>%  n_distinct()
           
}
# clean up - don't worry if you get warning messages here
rm(mult_visit_csn, multiple_ED_bed_moves)

# get rid of any csn which no longer involve ED
ED_bed_moves_raw <- ED_bed_moves_raw %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(num_ed_rows = sum(ED_row == 1)) %>% filter(num_ed_rows > 1)


# indicate whether row is OTF location
ED_bed_moves_raw <- ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(OTF_row = case_when(room == "OTF" ~ 1,
                            TRUE ~ 0)) 




# Calculating durations
# =====================

# OTF is not considered to be a ED location
# considering how to handle OTF rows, the following shows how many rows do not have OTF as last ED location
# 
# ED_bed_moves_raw %>% filter(room == "OTF") %>% select(csn, room) %>% 
#   left_join(ED_bed_moves_raw %>% 
#               filter(department == "UCH EMERGENCY DEPT") %>% 
#               group_by(csn) %>% 
#               filter(discharge == max(discharge)) %>%
#               select(csn, last_room = room)) %>% 
#   group_by(last_room != "OTF") %>% 
#   summarise(n())

# calculate ED discharge time; this will not be correct for OTF as last row

ED_bed_moves_raw <- ED_bed_moves_raw %>%
  group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row) %>% 
  mutate(ED_discharge_dttm = 
           case_when(ED_row == 1 ~ max(discharge))) %>% 
  ungroup() %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(ED_discharge_dttm = 
           case_when(is.na(ED_discharge_dttm) ~ min(ED_discharge_dttm, na.rm = TRUE),
                     TRUE ~ ED_discharge_dttm)) %>% 
  mutate(ED_duration = difftime(ED_discharge_dttm, arrival_dttm, units = "hours"))
         
         
# ED_bed_moves_raw <- ED_bed_moves_raw %>%
#   group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row) %>% 
#   mutate(ED_discharge_dttm = 
#            case_when(ED_row == 1 ~ max(discharge),
#                       TRUE ~ NA_POSIXct_)) %>% 
#   mutate(ED_duration = 
#            case_when(ED_row == 1 ~ difftime(ED_discharge_dttm, arrival_dttm, units = "hours"),
#                      # next row is a cheat way to get NA for difftime
#                     TRUE ~ difftime(arrival_dttm, arrival_dttm, units = "hours"))) 


# do this again but filter out OTF rows to get a ED duration excluding these
# note this may not be wholly correct for the csns which involve OTF and then a return to ED
ED_bed_moves_raw <- ED_bed_moves_raw %>%
  group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row, OTF_row) %>%
  mutate(ED_discharge_dttm_excl_OTF = 
           case_when(OTF_row == 0 & ED_row == 1 ~ max(discharge),
                     OTF_row == 1 & ED_row == 1 ~ NA_POSIXct_,
                     OTF_row == 0 & ED_row == 0 ~ NA_POSIXct_,
                     OTF_row == 1 & ED_row == 1 ~ NA_POSIXct_)) %>% 
  ungroup() %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(ED_discharge_dttm_excl_OTF = 
           case_when(is.na(ED_discharge_dttm_excl_OTF)~ min(ED_discharge_dttm_excl_OTF, na.rm = TRUE),
                     TRUE ~ ED_discharge_dttm_excl_OTF)) %>% 
  mutate(ED_duration_excl_OTF = 
           difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))
  # mutate(ED_duration_excl_OTF = 
  #          case_when(OTF_row == 0 & ED_row == 1 ~ difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"),
  #                    # next row is a cheat way to get NA for difftime
  #                   TRUE ~ difftime(arrival_dttm, arrival_dttm, units = "hours"))) 

# create final version of ED_duration - select the pre OTF duration when there is an OTF row last, but not otherwise
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(ED_duration_final =
           case_when(sum(OTF_row) > 0 ~ max(ED_duration_excl_OTF, na.rm = TRUE),
                     TRUE ~ max(ED_duration, na.rm = TRUE)),
         ED_discharge_dttm_final =
         case_when(sum(OTF_row) > 0 ~ max(ED_discharge_dttm_excl_OTF, na.rm = TRUE),
                     TRUE ~ max(ED_discharge_dttm, na.rm = TRUE)))

# use row durations to calc the row that denotes admission beyond ED
# in most cases, this will be the OTF row
ED_bed_moves_raw <- ED_bed_moves_raw %>%
  mutate(admission_row = case_when(admission >= ED_discharge_dttm_excl_OTF &
                                   lag(admission) < ED_discharge_dttm_excl_OTF ~ TRUE,
                                 TRUE ~ FALSE))


# Simplify room and dept names
# ============================

# create room2 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room2 = case_when(is.na(room) & hl7_location != "ED^null^null" ~
                             split_location(hl7_location, 2),
                           TRUE ~ room))

# create room2a (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room2a = case_when(is.na(room2) & hl7_location == "ED^null^null" & arrival_row ~ "Arrived",
                           is.na(room2) & hl7_location == "ED^null^null" & !arrival_row ~ "Waiting",
                           TRUE ~ room2))
# create dept2 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(dept2 = clean_wardnames5(department))

# use d to see which room names the function clean_wardnames5 has grouped: 
# d <- ED_bed_moves_raw %>% group_by(department, dept2) %>% summarise(total = n()) 

# create room3 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room3 = clean_room_names(dept2, room2a))

# use e to see which room names the function clean_room_names has grouped: 
# e <- ED_bed_moves_raw %>% filter(dept2 == "ED") %>% 
#   group_by(dept2, room2, room3) %>% summarise(total = n()) 

# to see mapping from room to room3 (note room3 has more values because we used the HL7 field)
# g <- ED_bed_moves_raw %>% 
#   filter(department == "UCH EMERGENCY DEPT") %>% 
#   group_by(room, room3) %>% 
#   summarise(tot = n()) %>% 
#   select(room, room3, tot) %>%
#   pivot_wider(names_from = room3, values_from = tot)

# create dept3 (see wiki for more information)t
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(dept3 = calc_admission(dept2, room2a, discharge, discharge_dttm, ED_discharge_dttm_excl_OTF))


# Create room4 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room4 = group_room_names(room3))

# to see mapping from room3 to room4 (note this function prioritises RAT over MAJORS but that only affects 28 rows)
# h <- ED_bed_moves_raw %>% 
#   filter(department == "UCH EMERGENCY DEPT") %>% 
#   group_by(room3, room4) %>% 
#   summarise(tot = n()) %>% 
#   select(room3, room4, tot) %>%
#   pivot_wider(names_from = room4, values_from = tot)
# 
# # save data on mappings for reference
# 
# outFile = paste0("EDcrowding/flow-mapping/data-output/room_mappings_",today(),".rda")
# save(h, file = outFile)
# rm(outFile)

# create room5 (see wiki for more information)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  group_by(csn) %>%
  # add a column to contain the discharge time of the next row
  mutate(next_location = lead(room4),
         next_dttm = lead(discharge)) %>% 
  # use the date for the next row as the discharge time when next row is diagnostics or Waiting room
  # discharge new is used in create-edge-list.R
  mutate(discharge_new = case_when(next_location %in% c("DIAGNOSTICS", "WAITING ROOM") ~ next_dttm,
                                   TRUE ~ discharge)) %>% select(-next_location, -next_dttm)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  group_by(csn) %>% 
  mutate(room5 = case_when(room4 %in% c("DIAGNOSTICS", "WAITING ROOM") & !arrival_row ~ lag(room4),
                           # the row below has been added for the few cases where an arrival row is DIAGNOSTICS or WAITING ROOM
                           room4 %in% c("DIAGNOSTICS", "WAITING ROOM") & arrival_row ~ lead(room4),
                           TRUE ~ room4))


ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  # create new ED_row flag where last row is OTF
  mutate(ED_row_excl_OTF = case_when(ED_row == 1 & discharge > ED_discharge_dttm_final ~ 0,
                            TRUE ~ ED_row))

# create room6 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  # udpate OTF to either side location when these are the same
  mutate(room6 = case_when(OTF_row == 1 & ED_row_excl_OTF ==1 & lead(room5) == lag(room5) ~ lead(room5),
                                TRUE ~ room5)) 

# create room7 (see wiki for more information)
ED_bed_moves_raw <-ED_bed_moves_raw %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(sum_triage = sum(room5 == "TRIAGE")) %>% 
  mutate(room7 = case_when(sum_triage > 1 & room4 == "TRIAGE" ~ paste0(room4, " Return"),
                           TRUE ~ room4)) %>% select(-sum_triage)

# Final data cleaning
# ===================

# # look at outliers
# ggplot(ED_bed_moves_raw %>% filter(ED_row == 1), # one clear outlier
#        aes(x=1, y = duration_row)) + 
#         geom_boxplot()
# 
# # without that one, majority of long durations rows are OTF (off the floor)
# ggplot(ED_bed_moves_raw %>% filter(ED_row == 1, duration_row < 400), aes(x=room3, y = duration_row)) + 
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))


# remove one outlier
ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(duration_row < 400) %>% 
  arrange(mrn, csn, admission)

# some csns have rows where the discharge time is earlier than the admission time
odd_csn <- ED_bed_moves_raw %>% filter(discharge < admission) %>% select(csn)
# to see the number of csns to remove: 
odd_csn %>% select(csn) %>% n_distinct() #61 in Jan/Feb, 20 in Mar/Apr; 2 in August

# identify visits where someone goes to ED from another location
elsewhere_to_ED_csn <- ED_bed_moves_raw %>% 
  group_by(csn) %>% 
  select(csn, ED_row, arrival_row) %>% 
  mutate(check = case_when(ED_row == 1 & !arrival_row & lag(ED_row) == 0 ~ "B",
                           TRUE ~ "A")) %>% 
  filter(check == "B") %>% select(csn) 
# to see the number of csns identified: 
elsewhere_to_ED_csn %>% select(csn) %>% n_distinct() #only 1 in Jan/Feb; 0 in Mar/Apr; 4 in May/Jun/Jul; 0 in August

# look at csns with duration of more than one day:
long_ED_csn <-  ED_bed_moves_raw %>% 
  filter(!csn %in% elsewhere_to_ED_csn$csn,
         ED_duration_final >  days(1)) %>%
  select(csn, ED_duration_final) %>% distinct()
# to see the number of csns identified: 
long_ED_csn %>% select(csn) %>% n_distinct() #22 in Jan/Feb 6 in Mar/Apr; 8 in May/Jun/Jul; 0 in August

# a couple in May/Jun/Jul are very weird; 
# ED_bed_moves_raw %>% filter(mrn == "93065579") was in OTF for days and then returned to MAJORS then discharge; 
# ED_bed_moves_raw %>% filter(mrn == "21212090") similar to above but returned to UTC then discharge
# ED_bed_moves_raw %>% filter(mrn == "21176662") has two arrival rows in ED then nothing else; remove this one

# several have weird arrival rows
odd_arrival_rows <- ED_bed_moves_raw %>% filter(arrival_row, 
                            hl7_location %in% c("ED^UCHED WR POOL^WR",
                                                "ED^UCHED OTF POOL^OTF")) %>% 
  select(csn) %>% distinct()



# # a few CSNs have multiple iterations through OTF and Waiting room - use this to check
# ED_bed_moves_raw %>% filter(room5 %in% c("DIAGNOSTICS", "WAITING ROOM")) %>% select(csn) %>% distinct()
# 
# ED_bed_moves <- ED_bed_moves %>% 
#   group_by(csn) %>%
#   mutate(prev_location = lag(room4_new)) %>% 
#   mutate(room4_new = case_when(room4_new %in% c("DIAGNOSTICS", "WAITING ROOM") ~ prev_location,
#                                TRUE ~ room4_new))


# create clean version of ED bed moves

ED_bed_moves <- ED_bed_moves_raw %>% 
  filter(
    !csn %in% odd_csn$csn,
    !csn %in% long_ED_csn$csn, # worth doing for May to July
#    !csn %in% odd_arrival_rows$csn, leave these in
    !csn %in% elsewhere_to_ED_csn$csn) 

# Create summary dataset
# =========================

# create summary (one row per csn)
ED_csn_summ <- ED_bed_moves %>% 
  # # adding the following line in to avoid duplicate rows, but ideally this would be corrected at source
  # select(-ED_discharge_dttm_excl_OTF, -ED_duration_excl_OTF) %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm, 
           ED_discharge_dttm, ED_discharge_dttm_final,
           ED_discharge_dttm_excl_OTF, ED_duration_excl_OTF,
           ED_duration, ED_duration_final) %>% 
  summarise(num_ED_rows = sum(ED_row),
            num_ED_row_excl_OTF = sum(ED_row_excl_OTF)) %>% ungroup() %>% 
  filter(num_ED_rows > 0) %>% 
  
  # create breach detail
  mutate(seen4hrs = ifelse(ED_duration < hours(4), "Seen in 4 hours", "Breach"))  
  
  # # join with ED bed_moves (ED rows only) to get last ED location
  # left_join(ED_bed_moves %>% 
  #             filter(ED_row == 1) %>%
  #             group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  #             filter(discharge == ED_discharge_dttm_final) %>% 
  #             mutate(ED_last_loc = room5) %>% 
  #             select(csn, ED_last_loc))

# calculate whether admitted
ED_csn_summ <- ED_csn_summ %>%  
  mutate(ED_last_status = if_else(discharge_dttm == ED_discharge_dttm_final, "Discharged", "Admitted"))

# reorder columns
ED_bed_moves <- ED_bed_moves %>% 
  select(mrn, csn, arrival_dttm, discharge_dttm, admission, discharge, 
         department, dept2, dept3, hl7_location,
         arrival_row, ED_row, OTF_row, ED_row_excl_OTF, duration_row, 
         ED_discharge_dttm, ED_duration, ED_discharge_dttm_excl_OTF, ED_duration_excl_OTF,
         ED_discharge_dttm_final, ED_duration_final, admission_row, everything())


# Save data
# =========

# save ED_bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_August_",today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)

# OR load saved ED_bed_moves

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_","",".rda")
load(inFile)


# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_August_",today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)

# OR LOAD saved data
inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_","",".rda")
load(inFile)




# Add additional waiting row  
# ==========================

# alternative version of ED_bed_moves with additional waiting rows
# This section adds a row where more than a specified length of time
# which is given by time_until_waiting has passed since arrival
# row admission, discharge and duration are updated accordingly

extra_rows <- tribble(
  ~mrn, ~csn, ~arrival_dttm, ~discharge_dttm, ~admission , ~discharge, ~department,
  ~dept2, ~dept3, ~ hl7_location, ~ room, ~ room2, ~room2a, ~room3, ~room4, ~room5, ~room6, ~room7,
  ~bed, ~arrival_row, ~ED_row, ~OTF_row, ~ED_row_excl_OTF, ~num_ed_rows,
  ~duration_row, ~ED_duration, ~ED_duration_excl_OTF, ~ED_duration_final, 
  ~ED_discharge_dttm, ~ ED_discharge_dttm_excl_OTF, ~ED_discharge_dttm_final, ~admission_row, 
  ~discharge_new)

time_until_waiting <- difftime("2020-01-01 00:10:00", "2020-01-01 00:00:00", units = "hours")

ED_bed_moves_extra <- ED_bed_moves

for (i in 1:nrow(ED_bed_moves_extra)) {
  
  if (i%%1000 == 0) {
    print(paste("Processed",i,"rows"))
  }
  
  row <- ED_bed_moves_extra[i,]
  
  if(row$room5 == "Arrived" && row$duration_row > time_until_waiting) { 
    
    # create new row for the additional wait time
    row$admission <- ED_bed_moves_extra$admission[i] + time_until_waiting
    row$duration_row <- ED_bed_moves_extra$duration_row[i] -   time_until_waiting
    row$room2a <- "Waiting"
    row$room3 <- "Waiting"
    row$room4 <- "Waiting"
    row$room5 <- "Waiting"
    row$room6 <- "Waiting"
    row$room7 <- "Waiting"
    extra_rows <- extra_rows %>% add_row(tibble_row(row))
    
    # update the arrival duration to 10 minutes
    ED_bed_moves_extra$duration_row[i] <- time_until_waiting
    ED_bed_moves_extra$discharge[i] <- ED_bed_moves_extra$admission[i] + time_until_waiting
  }
}

ED_bed_moves_extra <- ED_bed_moves_extra %>% dplyr::union(extra_rows) %>% arrange(mrn, csn, admission)

# save ED_bed_moves_extra for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_clean_extra_JanFeb_",today(),".rda")
save(ED_bed_moves_extra, file = outFile)
rm(outFile)



# About this script
# =================

# This script reads data from EMAP Star using the view called
# bed_moves. It includes only admissions that involved ED at some point,
# excludes any admissions that included pediatrics in ED
# and extracts all bed_moves for those admissions, including 
# onward destinations from ED. 
# 
# Ward names are cleaned in various ways described further below
# Room names are cleaned in various ways described further below
#
# Various strange encounters are removed 
#
# Some additional processing is done to include durations 
# in locations with very low numbers (e.g. diagnostics, waiting room)
# into the previous node
#
# OTF locations are excluded from calculations of ED durations where
# these locations are the last of the ED rows
#
# The outputs are
# - ED_bed_moves - one row for each original bed_move row 
# - ED_csn_summ - one row for each unique csn with summary info
#
# The final dataset has the following fields for department info
# - department (as original)
# - dept2 - department simplified by function called clean_ward_names
#   Wards of interest (AMU, ICU, T07 and HDRU, T08/09) retained with others
#   grouped into Tower Other and Outside Tower
# - dept3 - dept2 simplified into "Still in ED" or "Admitted"
# Note: for any encounters where the last row is OTF, dept3 shows these
# as Admitted,while department and dept2 show ED
#
# The final dataset has the following fields for room info
# - room (as original)
# - room2 - as room, but if this is NA, adds room info derived 
#   from the hl7_location field. If there is no room information, 
#   room2 is set to 'Waiting'
# - room3 - simplified by the function clean_room_names which
#   removes bay and room numbers
# - room4 - room3 simplified by the function group_room_names
#   which collapses room into the major categories of TRIAGE, 
#   RAT, UTC etc not distinguishing between COVID and non-COVID
# - room5 - which inserts the previous room when the next one
#   is not used much (diagnostics, waiting room)
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
    length(grep("RAT", room)) >0 ~ "RAT",
    length(grep("MAJORS", room)) >0 ~ "MAJORS",
    length(grep("PAEDS TRIAGE", room)) >0 ~ "PAEDS TRIAGE",
    length(grep("TRIAGE", room)) >0 ~ "TRIAGE",
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)


# simple function to return whether location denotes admission
calc_admission <- function(dept2, room2, discharge, discharge_dttm, pre_OTF_ED_discharge_dttm) {
  if (dept2 != "ED") {
    return("Admitted")
  }
  else if (room2 == "OTF" && !is.na(pre_OTF_ED_discharge_dttm) 
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
          max(discharge) as discharge_dttm,
          max(admission) as last_bed_move
          FROM
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
  and c.num_ED_rows > 1"
sqlQuery <- gsub('\n','',sqlQuery)

start <- Sys.time()
ED_bed_moves2 <- as_tibble(dbGetQuery(ctn, sqlQuery))
Sys.time() - start
# DB Forge took 03:13 for one day and 3.27 for two months and 12.35 for 2020 year to date
# R took 14.98 min for two months of data

# remove rows where admission == discharge; 
# these are rows where a patient spent no time in a location
# to see the number: 
ED_bed_moves2 %>% filter(admission == discharge) %>% n_distinct() # 9

# to remove these rows
ED_bed_moves2 <- ED_bed_moves2 %>% filter(admission != discharge)

# identify admission rows (first for each encounter)
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(arrival_row = ifelse(admission == arrival_dttm, TRUE, FALSE))

# calc row durations
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))

# arrange by arrival time
ED_bed_moves2 <- ED_bed_moves2 %>% 
  arrange(arrival_dttm, admission)

# indicate whether row is ED location
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(ED_row = case_when(department == "UCH EMERGENCY DEPT" ~ 1,
                          TRUE ~ 0))

# indicate whether row is OTF location
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(ED_row = case_when(room == "OTF" ~ 1,
                            TRUE ~ 0))

# save data for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_JanFeb_",today(),".rda")
save(ED_bed_moves2, file = outFile)
rm(outFile)

# OR LOAD EXISTING FILE

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_2020-07-27.rda")
load(inFile)


# Calculating durations
# =====================

# OTF is not considered to be a ED location
# considering how to handle OTF rows, the following shows that 52 encounters do not have OTF as last ED location

ED_bed_moves2 %>% filter(room == "OTF") %>% select(csn, room) %>% 
  left_join(ED_bed_moves2 %>% 
              filter(department == "UCH EMERGENCY DEPT") %>% 
              group_by(csn) %>% 
              filter(discharge == max(discharge)) %>%
              select(csn, last_room = room)) %>% 
  group_by(last_room != "OTF") %>% 
  summarise(n())

# calculate ED discharge time; this will not be correct for OTF as last row
ED_bed_moves2 <- ED_bed_moves2 %>%
  group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row) %>% 
  mutate(ED_discharge_dttm = 
           case_when(ED_row == 1 ~ max(discharge),
                      TRUE ~ NA_POSIXct_)) %>% 
  mutate(ED_duration = 
           case_when(ED_row == 1 ~ difftime(ED_discharge_dttm, arrival_dttm, units = "hours"),
                     # next row is a cheat way to get NA for difftime
                    TRUE ~ difftime(arrival_dttm, arrival_dttm, units = "hours"))) 


# do this again but filter out OTF rows to get a ED duration excluding these
# note this may not be wholly correct for the csns which involve OTF and then a return to ED
ED_bed_moves2 <- ED_bed_moves2 %>%
  group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row, OTF_row) %>%
  mutate(pre_OTF_ED_discharge_dttm = 
           case_when(OTF_row == 0 & ED_row == 1 ~ max(discharge),
                     OTF_row == 1 & ED_row == 1 ~ NA_POSIXct_,
                     OTF_row == 0 & ED_row == 0 ~ NA_POSIXct_,
                     OTF_row == 1 & ED_row == 1 ~ NA_POSIXct_)) %>% 
  mutate(pre_OTF_ED_duration = 
           case_when(OTF_row == 0 & ED_row == 1 ~ difftime(pre_OTF_ED_discharge_dttm, arrival_dttm, units = "hours"),
                     # next row is a cheat way to get NA for difftime
                    TRUE ~ difftime(arrival_dttm, arrival_dttm, units = "hours"))) 

# create final version of ED_duration - select the pre OTF duration when there is an OTF row
ED_bed_moves2 <- ED_bed_moves2 %>% ungroup() %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>%
  mutate(ED_duration_final =
           case_when(sum(OTF_row) > 0 ~ max(pre_OTF_ED_duration, na.rm = TRUE),
                     TRUE ~ max(ED_duration, na.rm = TRUE)), 
         ED_discharge_dttm_final 
         = case_when(sum(OTF_row) > 0 ~ max(pre_OTF_ED_discharge_dttm, na.rm = TRUE),
                     TRUE ~ max(ED_discharge_dttm, na.rm = TRUE)))


# Simplify room and dept names
# ============================

# hl7_location seems to have more complete info in some cases where room and bed are NA
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(room2 = case_when(is.na(room) & hl7_location != "ED^null^null" ~
                             split_location(hl7_location, 2),
                           TRUE ~ room))

# where there is still no room information, assume patient is Waiting
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(room2 = case_when(is.na(room2) & hl7_location == "ED^null^null" ~ "Waiting",
                           TRUE ~ room2))
# shorten ward name
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(dept2 = clean_wardnames5(department))

# use d to see which room names the function clean_wardnames5 has grouped: 
# d <- ED_bed_moves2 %>% group_by(department, dept2) %>% summarise(total = n()) 

# clean room names
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(room3 = clean_room_names(dept2, room2))

# use e to see which room names the function clean_room_names has grouped: 
# e <- ED_bed_moves2 %>% filter(dept2 == "ED") %>% 
#   group_by(dept2, room2, room3) %>% summarise(total = n()) 

# to see mapping from room to room3 (note room3 has more values because we used the HL7 field)
# g <- ED_bed_moves2 %>% 
#   filter(department == "UCH EMERGENCY DEPT") %>% 
#   group_by(room, room3) %>% 
#   summarise(tot = n()) %>% 
#   select(room, room3, tot) %>%
#   pivot_wider(names_from = room3, values_from = tot)

# simple version of department (Admitted or still in ED) used in edge list
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(dept3 = calc_admission(dept2, room2, discharge, discharge_dttm, pre_OTF_ED_discharge_dttm))

# Create grouped room name column (incorporates COVID and non-COVID locations into one category)
ED_bed_moves2 <- ED_bed_moves2 %>% 
  mutate(room4 = group_room_names(room3))

# to see mapping from room3 to room4 (note this function prioritises RAT over MAJORS but that only affects 28 rows)
# h <- ED_bed_moves2 %>% 
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


# save data at this point - prior to any row deletions or row value changes
outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_JanFeb_",today(),".rda")
save(ED_bed_moves2, file = outFile)
rm(outFile)


# Prepare clean version of ED_bed_moves
# ====================================


# # look at outliers
# ggplot(ED_bed_moves2 %>% filter(ED_row == 1), # one clear outlier
#        aes(x=1, y = duration_row)) + 
#         geom_boxplot()
# 
# # without that one, majority of long durations rows are OTF (off the floor)
# ggplot(ED_bed_moves2 %>% filter(ED_row == 1, duration_row < 400), aes(x=room3, y = duration_row)) + 
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))


# remove one outlier
ED_bed_moves2 <- ED_bed_moves2 %>% filter(duration_row < 400) %>% 
  arrange(mrn, csn, admission)

# some csns have rows where the discharge time is earlier than the admission time
odd_csn <- ED_bed_moves2 %>% filter(discharge < admission) %>% select(csn)
# to see the number of csns to remove: 
odd_csn %>% select(csn) %>% n_distinct() #61

# identify visits where someone goes to ED from another location
elsewhere_to_ED_csn <- ED_bed_moves2 %>% 
  group_by(csn) %>% 
  select(csn, ED_row, arrival_row) %>% 
  mutate(check = case_when(ED_row == 1 & !arrival_row & lag(ED_row) == 0 ~ "B",
                           TRUE ~ "A")) %>% 
  filter(check == "B") %>% select(csn) 
# to see the number of csns identified: 
elsewhere_to_ED_csn %>% select(csn) %>% n_distinct() #93

# look at csns with duration of more than one day:
long_ED_csn <-  ED_bed_moves2 %>% 
  filter(!csn %in% elsewhere_to_ED_csn$csn,
         ED_duration_final >  days(1)) %>%
  select(csn, ED_duration_final) %>% distinct()
# to see the number of csns identified: 
long_ED_csn %>% select(csn) %>% n_distinct() #22

# look at admissions with duration of more than one day:
long_ED_csn <-  ED_bed_moves2 %>% 
  filter(!csn %in% elsewhere_to_ED_csn$csn,
         ED_duration_final >  days(1)) %>%
  select(csn, ED_duration_final) %>% distinct()
# to see the number of csns identified: 
long_ED_csn %>% select(csn) %>% n_distinct() #22

# One is genuinely weird - spent 2 months in ED but only one row
ED_bed_moves2 %>% filter(csn == "1018734672")
# others are generally in TAF which may explain their long stays

# this one spent 163 in OTF and then went elsewhere in ED so I didn't pick them up
ED_bed_moves2 %>% filter(csn == "1019997966")

# several have weird arrival rows
odd_arrival_rows <- ED_bed_moves2 %>% filter(arrival_row, 
                            hl7_location %in% c("ED^UCHED WR POOL^WR",
                                                "ED^UCHED OTF POOL^OTF")) %>% 
  select(csn) %>% distinct()


# Process rarely used nodes
# add time in diagnostics and Waiting room into duration of previous node

ED_bed_moves2 <- ED_bed_moves2 %>% 
  group_by(csn) %>%
  # add a column to contain the discharge time of the next row
  mutate(next_location = lead(room4),
         next_dttm = lead(discharge)) %>% 
  # use the date for the next row as the discharge time when next row is diagnostics or Waiting room
  mutate(discharge_new = case_when(next_location %in% c("DIAGNOSTICS", "WAITING ROOM") ~ next_dttm,
                                   TRUE ~ discharge)) %>% 
  mutate(duration_row_new = difftime(discharge_new, admission, units = "hours"))   

ED_bed_moves2 <- ED_bed_moves2 %>% 
  group_by(csn) %>%
  # add a column to contain the previous location
  mutate(prev_location = lag(room4)) %>% 
  mutate(room5 = case_when(room4 %in% c("DIAGNOSTICS", "WAITING ROOM") ~ prev_location,
                               TRUE ~ room4))

# # a few CSNs have multiple iterations through OTF and Waiting room - use this to check
# ED_bed_moves2 %>% filter(room5 %in% c("DIAGNOSTICS", "WAITING ROOM")) %>% select(csn) %>% distinct()
# 
# ED_bed_moves <- ED_bed_moves %>% 
#   group_by(csn) %>%
#   mutate(prev_location = lag(room4_new)) %>% 
#   mutate(room4_new = case_when(room4_new %in% c("DIAGNOSTICS", "WAITING ROOM") ~ prev_location,
#                                TRUE ~ room4_new))


# create clean version of ED bed moves

ED_bed_moves <- ED_bed_moves2 %>% 
  filter(
    !csn %in% odd_csn$csn,
    !csn %in% odd_arrival_rows$csn,
    !csn %in% elsewhere_to_ED_csn$csn,
    !csn == "1019997966",  
    !csn == "1018734672") %>% 
  select(mrn, csn, arrival_dttm, discharge_dttm, admission, discharge, 
         department, dept2, dept3, hl7_location,
         room, room2, room3, room4, room5, bed,
         num_ed_rows, arrival_row, ED_row, OTF_row, duration_row,
         ED_duration_final, ED_discharge_dttm_final)



# Create summary dataset
# =========================

# create summary (one row per csn)
ED_csn_summ <- ED_bed_moves %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm, 
           ED_discharge_dttm, pre_OTF_ED_discharge_dttm, ED_discharge_dttm_final,
           ED_duration, pre_OTF_ED_duration, ED_duration_final) %>% 
  summarise(num_ED_rows = sum(ED_row)) %>% ungroup() %>% 
  filter(num_ED_rows > 0) %>% 
  
  # create breach detail
  mutate(seen4hrs = ifelse(ED_duration < hours(4), "Seen in 4 hours", "Breach")) %>% 
  
  # join with ED bed_moves (ED rows only) to get last ED location
  left_join(ED_bed_moves %>% 
              filter(ED_row == 1) %>%
              group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
              filter(discharge == ED_discharge_dttm_final) %>% 
              mutate(ED_last_loc = room5) %>% 
              select(csn, ED_last_loc))

# calculate whether admitted
ED_csn_summ <- ED_csn_summ %>%  
  mutate(ED_last_status = if_else(discharge_dttm == ED_discharge_dttm_final, "Discharged", "Admitted"))





# Save data
# =========

# save ED_bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_clean_JanFeb_",today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)

# OR load saved ED_bed_moves

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_","",".rda")
load(inFile)


# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_JanFeb_",today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)

# OR LOAD saved data
inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_","",".rda")
load(inFile)



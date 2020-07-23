# About this script
# =================

# This script reads data from EMAP Star using the view called
# bed_moves. It identified admissions that involved ED at some point
# and extracts all bed_moves for those admissions, including 
# onward destinations from ED. 
# 
# Ward names are shortened and grouped to focus only on onward
# wards of interest (AMU, ICU, T07 and HDRU, T08/09) with others
# grouped into Tower Other and Outside Tower
#
# Within ED, room names are cleaned so that all bay numbers
# and chairs are combined 
#
# Treatment rooms in the UTC (plaster, opthamology, treatment) are
# grouped 
#
# Various strange encounters are removed (e.g. getting stuck in OTF)
# and so are any encounters that involve pediatrics in ED
#
# Some additional processing is done to include OTF durations 
# into the durations of previous node 
# and the same is done for nodes with very low numbers (e.g. diagnostics)

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
  if (dept == "ED") {
    room = gsub("UCHED ","",room)
    # room = gsub("BY[0-9]{2}", "BY", room)
    # room = gsub("SR[0-9]{2}", "SR", room)
    # room = gsub("CB[0-9]{2}", "CB", room)
    # if (length( grep("^T[0-9]{2}",room)) == 0) {
    #   room = gsub("[0-9]", "", room)
    # }
    # room = gsub("CHAIR [0-9]{2}", "CHAIR", room)
    # room = gsub("[0-9]{2}", "99", room)
    # room = gsub("[0-9]{2}", " 99", room)
    # room = gsub("MAJ-CHAIR","MAJORS CHAIR",room)
    # room = gsub("RAT-CHAIR","RAT CHAIR",room)
    # room = gsub("RATBED","RATBED",room)
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
calc_admission <- function(dept) {
  if (dept != "ED") {
    return("Admitted")
  }
  else
    return("Still in ED")
}

calc_admission <- Vectorize(calc_admission)


# simple function to return whether location involves pediatrics in ED
calc_pediatric <- function(room) {
  if (length(grep("PAEDS", room)) >0 ) {
    return(TRUE)
  }
  else
    return(FALSE)
}

calc_pediatric <- Vectorize(calc_pediatric)

calc_chair <- function(room) {
  if (length(grep("CHAIR", room)) >0 ) {
    return(TRUE)
  }
  else
    return(FALSE)
}

calc_chair <- Vectorize(calc_chair)

# Load bed_move data
# ==================

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

## EITHER get bed moves from Star

sqlQuery <- "SELECT *
  FROM star.bed_moves bm"
sqlQuery <- gsub('\n','',sqlQuery)

bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))

# remove rows where admission == discharge; 
# these are rows where a patient spent no time in a location
# to see the number: 
bed_moves %>% filter(admission == discharge) %>% n_distinct() # 188

# to remove these rows
bed_moves <- bed_moves %>% filter(admission != discharge)

# join to get encounter_id for each encounter
# encounter_id is used as key on patient_fact

sqlQuery <- "SELECT *
  FROM star.encounter 
"
sqlQuery <- gsub('\n','',sqlQuery)
encounter <- as_tibble(dbGetQuery(ctn, sqlQuery))

bed_moves <- bed_moves %>% left_join(encounter %>% select(encounter_id, encounter), 
                                     by = c("csn" = "encounter"))

# derive admission and discharge dates
bed_moves <- bed_moves %>% group_by(mrn, csn, encounter_id) %>% 
  mutate(arrival_dttm = min(admission), discharge_dttm = max(discharge)) %>% 
  arrange(mrn, csn, admission)

# identify ED rows
bed_moves <- bed_moves %>% 
  mutate(ED_row = ifelse(department == "UCH EMERGENCY DEPT", 1, 0))

# identify admission rows (first for each encounter)
bed_moves <- bed_moves %>% 
  mutate(arrival_row = ifelse(admission == arrival_dttm, TRUE, FALSE))

# save data for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_",today(),".rda")
save(bed_moves, file = outFile)
rm(outFile)

# OR LOAD EXISTING FILE

inFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_2020-07-09.rda")
load(inFile)

# Select only encounters involving ED
# ===================================

# EITHER generate from bed_moves
# Create summary of all encounters  
csn_summ <- bed_moves %>% 
  group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
  summarise(duration = difftime(max(discharge),min(admission), units = "hours"),
            num_ED_rows = sum(ED_row)) %>% ungroup()

# save summary of all encounters for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/csn_summ_",today(),".rda")
save(csn_summ, file = outFile)
rm(outFile)


# select only encounters with one or more bed_moves involving ED
ED_csn_summ <- csn_summ %>% 
  filter(num_ED_rows > 0) 

# join with bed_moves to get duration in ED
ED_csn_summ <- ED_csn_summ %>% 
  left_join(bed_moves %>% 
              filter(ED_row == 1) %>%
              group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
              summarise(ED_discharge_dttm = max(discharge)) %>% 
              select(mrn, csn, encounter_id, arrival_dttm, ED_discharge_dttm),
            by = c("mrn", "csn", "encounter_id", "arrival_dttm"))

# calculate duration in ED
ED_csn_summ <- ED_csn_summ %>% 
  mutate(ED_duration= difftime(ED_discharge_dttm,arrival_dttm, units = "hours"))




# Data cleaning for encounters involving ED
# =========================================

# get bed_moves data for those encounters
ED_bed_moves <- left_join(ED_csn_summ, bed_moves) %>% 
  arrange(arrival_dttm, admission)

ED_bed_moves <- ED_bed_moves %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))

# some bed move rows have no department information 
# identify any csns to which this applies
no_dept_csns <- ED_bed_moves %>% filter(is.na(department)) %>% group_by(mrn, encounter_id, csn) %>% select(csn) %>% distinct()
# check whether any ED bed moves are in this category
ED_bed_moves %>% filter(csn %in% no_dept_csns$csn)

# hl7_location seems to have more complete info in some cases where room and bed are NA
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room2 = case_when(is.na(room) & hl7_location != "ED^null^null" ~
                             split_location(hl7_location, 2),
                           TRUE ~ room))
# shorten ward name
ED_bed_moves <- ED_bed_moves %>% 
  mutate(dept2 = clean_wardnames5(department))

# use d to see which room names the function clean_wardnames5 has grouped: 
d <- ED_bed_moves %>% group_by(department, dept2) %>% summarise(total = n()) 

# apply simple function to denote whether location denotes admission (used in edge list)
ED_bed_moves <- ED_bed_moves %>% 
  mutate(dept3 = calc_admission(dept2))

# clean room names
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room3 = clean_room_names(dept2, room2))

# use e to see which room names the function clean_room_names has grouped: 
e <- ED_bed_moves %>% filter(dept2 == "ED") %>% 
  group_by(dept2, room2, room3) %>% summarise(total = n()) 

# Note, in e there are a lot of rows with "ED" as dept and no room 
e %>% filter(is.na(room3)) 

# to see mapping from room to room3 (note room 3 has more values because we used the HL7 field)
g <- ED_bed_moves %>% 
  filter(department == "UCH EMERGENCY DEPT") %>% 
  group_by(room, room3) %>% 
  summarise(tot = n()) %>% 
  select(room, room3, tot) %>%
  pivot_wider(names_from = room3, values_from = tot)


# Label arrival rows
ED_bed_moves <- ED_bed_moves %>% mutate(room3 = case_when(is.na(room3) & arrival_row ~ "Arrival",
#                                                          is.na(room3) ~ "None", 
                                                          TRUE ~ room3))

# Create grouped room name column
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room4 = group_room_names(room3))

# to see mapping from room3 to room4 (note this function prioritises RAT over MAJORS but that only affects 28 rows)
h <- ED_bed_moves %>% 
  filter(department == "UCH EMERGENCY DEPT") %>% 
  group_by(room3, room4) %>% 
  summarise(tot = n()) %>% 
  select(room3, room4, tot) %>%
  pivot_wider(names_from = room4, values_from = tot)

# save data on mappings for reference

outFile = paste0("EDcrowding/flow-mapping/data-output/room_mappings_",today(),".rda")
save(h, file = outFile)
rm(outFile)


# apply function to denote whether location involves pediatrics
ED_bed_moves <- ED_bed_moves %>% 
  mutate(pediatric_row = calc_pediatric(room3))


# look at outliers

ggplot(ED_bed_moves %>% filter(ED_row == 1), # one clear outlier
       aes(x=1, y = duration_row)) + 
        geom_boxplot()

# without that one, majority of long durations rows are OTF (off the floor)
ggplot(ED_bed_moves %>% filter(ED_row == 1, duration_row < 400), aes(x=room3, y = duration_row)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))

# remove one outlier
ED_bed_moves <- ED_bed_moves %>% filter(duration_row < 400) %>% 
  arrange(mrn, csn, admission)


# Handle unwanted and weird encounters
# ====================================


# work out csns to ignore if involve pediatrics 
pediatric_csn <- ED_bed_moves %>% group_by(mrn, csn, arrival_dttm) %>% 
  summarise(tot = sum(pediatric_row)) %>% 
  filter(tot > 0) %>% 
  select(mrn, csn) %>% distinct() # 4723

# some csns have rows where the discharge time is earlier than the admission time
odd_csn <- ED_bed_moves %>% filter(discharge < admission) %>% select(csn)
# to see the number of csns removed: 
odd_csn %>% select(csn) %>% n_distinct() #111

# some ED rows have no information about which room someone was in
no_room_csn <-  ED_bed_moves %>% filter(is.na(room4)) %>% select(csn)
# to see the number of csns removed:
no_room_csn %>% select(csn) %>% n_distinct() #8


# this code identifies visits where someone goes to ED from another location
elsewhere_to_ED_csn <- ED_bed_moves %>% 
  group_by(csn) %>% 
  select(csn, ED_row, arrival_row) %>% 
  mutate(check = case_when(ED_row == 1 & !arrival_row & lag(ED_row) == 0 ~ "B",
                           TRUE ~ "A")) %>% 
  filter(check == "B") %>% select(csn)

# still 92 csns with more than one day:
long_ED_csn <-  ED_bed_moves %>% 
  filter(!csn %in% elsewhere_to_ED_csn$csn,
         as.numeric(ED_duration, units = "days") > 1) %>%
  select(csn) %>% distinct()

# of these, some are stuck in OTF and TAF
stuck_in_otf_taf_csn <- ED_bed_moves %>% 
  filter(csn %in% long_ED_csn$csn) %>%
  group_by(csn) %>% filter(ED_row == 1) %>%
  filter(admission == max(admission), room4 %in% c("OTF", "TAF")) %>% select(csn)
# Note that doing two separate filters in the above code - picked up 5 more this way

# One is genuinely weird - spent 2 months in ED but only one row
ED_bed_moves %>% filter(csn == "1018734672")

# this one spent 163 in OTF and then went elsewhere in ED so I didn't pick them up
ED_bed_moves %>% filter(csn == "1019997966")

# several have weird arrival rows
odd_arrival_rows <- ED_bed_moves %>% filter(arrival_row, 
                            hl7_location %in% c("ED^UCHED WR POOL^WR",
                                                "ED^UCHED OTF POOL^OTF")) %>% 
  select(csn) %>% distinct()

# otherwise room4 is NA - ducking this for now
room4_NA <- ED_bed_moves %>% filter(is.na(room4)) %>% 
  select(csn) %>% distinct()

# create clean version of ED bed moves

ED_bed_moves <- ED_bed_moves %>% 
  filter(
    !csn %in% pediatric_csn$csn,
    !csn %in% no_room_csn,    
    !csn %in% odd_csn$csn,
    !csn %in% odd_arrival_rows$csn,
    !csn %in% room4_NA$csn,
    !csn %in% elsewhere_to_ED_csn$csn,
    !csn %in% stuck_in_otf_taf_csn$csn,
    !csn == "1019997966",  
    !csn == "1018734672")


# create clean version of ED_csn_summ

ED_csn_summ <- ED_bed_moves %>% 
  group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
  summarise(duration = difftime(max(discharge),min(admission), units = "hours"),
            num_ED_rows = sum(ED_row)) %>% ungroup() %>% 
  filter(num_ED_rows > 0) %>% 

  # join with ED bed_moves (ED rows only) to get duration in ED
  left_join(ED_bed_moves %>% 
              filter(ED_row == 1) %>%
              group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
              summarise(ED_discharge_dttm = max(discharge)) %>% 
              select(mrn, csn, encounter_id, arrival_dttm, ED_discharge_dttm),
            by = c("mrn", "csn", "encounter_id", "arrival_dttm")) %>% 

  # calculate duration in ED
  mutate(ED_duration= difftime(ED_discharge_dttm,arrival_dttm, units = "hours")) %>% 
  
  # create breach detail
  mutate(seen4hrs = ifelse(ED_duration < hours(4), "Seen in 4 hours", "Breach")) %>% 
  
  # join with ED bed_moves (ED rows only) to get last ED location
  left_join(ED_bed_moves %>% 
              filter(ED_row == 1) %>%
              group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
              filter(discharge == max(discharge)) %>% 
              mutate(ED_last_loc = room4) %>% 
              select(mrn, csn, encounter_id, arrival_dttm, discharge_dttm, ED_last_loc),
            by = c("mrn", "csn", "encounter_id", "arrival_dttm", "discharge_dttm")) %>% 
  
  # calculate whether admitted
  mutate(ED_last_status = ifelse(discharge_dttm == ED_discharge_dttm, "Discharged", "Admitted"))


# Process OTF into other nodes
# ============================
# add time in OTF, diagnostics and Waiting room into duration of previous node

ED_bed_moves <- ED_bed_moves %>% 
  group_by(csn) %>%
  # add a column to contain the discharge time of the next row
  mutate(next_location = lead(room4),
         next_dttm = lead(discharge)) %>% 
  # use the date for the next row as the discharge time when next row is OTF, diagnostics or Waiting room
  mutate(discharge_new = case_when(next_location %in% c("DIAGNOSTICS", "OTF", "WAITING ROOM") ~ next_dttm,
                                   TRUE ~ discharge)) %>% 
  mutate(duration_row_new = difftime(discharge_new, admission, units = "hours"))   

ED_bed_moves <- ED_bed_moves %>% 
  group_by(csn) %>%
  # add a column to contain the previous location
  mutate(prev_location = lag(room4)) %>% 
  mutate(room4_new = case_when(room4 %in% c("DIAGNOSTICS", "OTF", "WAITING ROOM") ~ prev_location,
                           TRUE ~ room4))

# a few CSNs have multiple iterations through OTF and Waiting room
ED_bed_moves %>% filter(room4_new %in% c("DIAGNOSTICS", "OTF", "WAITING ROOM")) %>% select(csn) %>% distinct()

ED_bed_moves <- ED_bed_moves %>% 
  group_by(csn) %>%
  mutate(prev_location = lag(room4_new)) %>% 
  mutate(room4_new = case_when(room4_new %in% c("DIAGNOSTICS", "OTF", "WAITING ROOM") ~ prev_location,
                               TRUE ~ room4_new))


# Save data
# =========

# save ED_bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_",today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)

# OR load saved ED_bed_moves

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_","2020-07-22",".rda")
load(inFile)


# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_",today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)

# OR LOAD saved data
inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_","2020-07-09",".rda")
load(inFile)



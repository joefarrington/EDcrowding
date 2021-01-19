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


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")



# Create functions
# ===============

save_chart = function(chart_title, g, width = 1077, height = 659) {
  png(paste0("EDcrowding/flow-mapping/media/", chart_title, ".png"), width = width, height = height) 
  print(g)
  dev.off()
}


# hl7_location seems to have more complete info than room and bed for some rows
# but locations are grouped into one string; this function splits them

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# 
# get_node <- function(dept, room) {
#   if (dept == "ED") {
#     node <- room
#   }
#   else {
#     node <- dept
#   }
# }



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
    #    room = gsub("^UTC [A-z]+ ROOM","UTC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room)  
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

get_edgelist <- function(sites) {
  
  edgelist <- tribble(
    ~csn,
    ~from,
    ~to,
    ~dttm)
  
  from_node = as.character(sites$location[1])
  
  for (j in 1:nrow(sites)) {
    if (j != nrow(sites)) {
      
      to_node <- as.character(sites$location[j+1])
      
      # new code to remove unwanted rows flipping between Arrived, Waiting and back to Arrived, added 24/11
      if(to_node == "Arrived" && j > 1) {
        to_node <- from_node
      }
      
      # in any of the following cases, ignore the to node and move to next row
      if (!(to_node %in% c("OTF", "DIAGNOSTICS", "WAITING ROOM")) && !is.na(to_node)) {
        if (from_node != to_node) {
          
          edgelist <- edgelist %>% add_row(tibble_row(
            csn = sites$csn[j],
            from = from_node,
            to = to_node,
            dttm = sites$discharge[j]
          ))
        }
        from_node <- to_node
      }
    }
    else {
      # if (!(to_node %in% c("OTF", "DIAGNOSTICS", "WAITING ROOM")) && !is.na(to_node)) {
      # 
      #   edgelist <- edgelist %>% add_row(tibble_row(
      #     csn = sites$csn[j],
      #     from = to_node,
      #     to = "Discharged",
      #     dttm = sites$discharge_dttm[j]
      #   ))
      # }
      # else {
      
      # added new code here - need to check it works
      
      edgelist <- edgelist %>% add_row(tibble_row(
        csn = sites$csn[j],
        from = from_node,
        to = sites$patient_class[j],
        dttm = sites$discharge[j]
      ))
      #      }
    }
  }
  return(edgelist)
}


# for  group of encounters, this function generates an edge list of 
# all moves between locations, with a timestamp for each move
# locations are provided to it in a tibble called locations
# locations needs 5 attributes:  csn, location, time of admission to location, 
# time of discharge from location


get_care_site_flows <- function(locations) {
  
  # filter only those locations with more than one room move
  locations_gt1 <- locations %>% 
    group_by(csn) %>% 
    summarise("num_loc_changes" = n_distinct(location)) %>% 
    filter(num_loc_changes > 1)
  
  # then create edge list for all changes in location
  
  edgedf <- tribble(
    ~csn,
    ~from,
    ~to,
    ~dttm)
  
  for (i in (1:nrow(locations_gt1))) {
    
    if (i%%100 == 0) {
      print(paste("Processed",i,"locations"))
    }
    visit_csn = locations_gt1$csn[i]
    num_locations = locations_gt1$num_loc_changes[i]
    
    #get all visit detail records for this visit occurence
    sites <- locations %>% filter(csn == visit_csn) %>% arrange(admission)
    
    # create edgelist if the visit involved change of site
    edgelist <- get_edgelist(sites)
    if (nrow(edgelist) > 0) {
      edgedf <- edgedf %>% add_row(edgelist)
    }
  }
  return(edgedf)
}


# load data
# ==========

# bed moves - get all data including outpatient

sqlQuery <- "select m.mrn, hv.encounter as csn, lv.hospital_visit_id, l.location_string, 
lv.admission_time as admission, lv.discharge_time as discharge 
from star_test.hospital_visit hv, 
star_test.mrn m,    
star_test.location_visit lv,
star_test.location l 
where hv.mrn_id = m.mrn_id 
and lv.hospital_visit_id = hv.hospital_visit_id 
and l.location_id = lv.location_id 
order by mrn, csn, admission"

sqlQuery <- gsub('\n','',sqlQuery)
all_bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))

# # save bed_moves for later use - couldn't do this - took too long
# 
# outFile = paste0("EDcrowding/flow-mapping/data-raw/all_bed_moves_",today(),".rda")
# save(all_bed_moves, file = outFile)
# rm(outFile)


load("~/EDcrowding/flow-mapping/data-raw/bed_moves_2020-12-08.rda")


# process data ------------------------------------------------------------



bed_moves <- bed_moves %>% 
  mutate(department = split_location(location_string, 1))

bed_moves <- bed_moves %>% 
  mutate(room = split_location(location_string, 2))


bed_moves <- bed_moves %>% 
  mutate(CDU = if_else(department == "UCHT00CDU" , TRUE, FALSE),
         EDU = if_else(department %in% c("AECU", "T01ECU") , TRUE, FALSE)) # apparently there has been a name change since Dec?? 

bed_moves <- bed_moves %>% 
  mutate(SAA = if_else(grepl("SAA|SPECIALTY ASSESSMENT AREA", room), TRUE, FALSE),
         SDEC = if_else(grepl("SDEC", room), TRUE, FALSE))

bed_moves %>% select(csn) %>% n_distinct()

bm_summ = bed_moves %>% group_by(csn) %>% 
  summarise(in_CDU = sum(CDU) >0 ,
            in_EDU = sum(EDU) > 0,
            in_SAA = sum(SAA) > 0,
            in_SDEC = sum(SDEC) > 0)

bm_summ %>% select(csn) %>% n_distinct()

load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-12-08.rda")
bm_summ = bm_summ %>% left_join(csn_summ %>% 
                                  mutate(patient_class = case_when(patient_class == "EMERGENCY" ~ "EMERGENCY_class",
                                                                   patient_class == "INPATIENT" ~ "INPATIENT_class",
                                                                   TRUE ~ patient_class)))


# Most of the EDU cases are not in my subset of ED patients because they don't have patient class of emergency or inpatient

all_bed_moves <- all_bed_moves %>% 
  mutate(department = split_location(location_string, 1))

all_bm_summ = all_bed_moves %>% filter(department %in% c("AECU", "T01ECU")) %>% select(csn) %>% distinct() %>% 
  mutate(in_EDU = TRUE) %>% left_join(csn_summ)



# looking at AEDU and these name changes
a = all_bm_summ %>% arrange(admission_time) %>% left_join(all_bed_moves)

a %>% filter(department %in% c("AECU", "T01ECU")) %>% group_by(department, date(admission_time)) %>% summarise(tot =n()) %>% 
  ggplot(aes(x = `date(admission_time)`, y = tot, fill = department)) + geom_bar(stat = "identity")

# Get summary information on each -----------------------------------------

bm_summ %>% filter(in_SAA) %>% group_by(patient_class) %>% summarise(n())
bm_summ %>% filter(in_CDU) %>% group_by(patient_class) %>% summarise(n())
bm_summ %>% filter(in_SDEC) %>% group_by(patient_class) %>% summarise(n())
bm_summ %>% filter(in_EDU) %>% group_by(patient_class) %>% summarise(n())

all_bm_summ %>% filter(in_EDU)  %>% group_by(patient_class) %>% summarise(n())


# Create bed move data for each type --------------------------------------

# set location to room level for SAA and SDEC

saa_bed_moves = bm_summ %>% filter(in_SAA) %>% select(csn, patient_class) %>% left_join(bed_moves) %>% 
  mutate(room3 = clean_room_names(department, room)) %>% 
  mutate(location = group_room_names(room3))

sdec_bed_moves = bm_summ %>% filter(in_SDEC) %>% select(csn, patient_class) %>% left_join(bed_moves)  %>% 
  mutate(room3 = clean_room_names(department, room)) %>% 
  mutate(location = group_room_names(room3))

# set location to dept level for CDU and AEDU

cdu_bed_moves = bm_summ %>% filter(in_CDU) %>% select(csn, patient_class) %>% left_join(bed_moves) %>% 
  mutate(location = department)

edu_bed_moves = bm_summ %>% filter(in_EDU) %>% select(csn, patient_class) %>% left_join(bed_moves) %>% 
  mutate(location = department)



# Create edgelist for each ------------------------------------------------

# SAA - create edgelist
edgedf_saa <- get_care_site_flows(saa_bed_moves)

edgedf_saa %>% select(csn) %>% n_distinct()
saa_bed_moves %>% select(csn) %>% n_distinct() # 2 only had one location

# to subset with earliest SAA - 

edgedf_saa <- edgedf_saa %>% left_join(
  edgedf_saa %>% filter(to == "SAA") %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)

# SDEC - create edgelist
edgedf_sdec <- get_care_site_flows(sdec_bed_moves)

edgedf_sdec %>% select(csn) %>% n_distinct()
sdec_bed_moves %>% select(csn) %>% n_distinct() # same

edgedf_sdec <- edgedf_sdec %>% left_join(
  edgedf_sdec %>% filter(to == "SDEC") %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)

# CDU - create edgelist

edgedf_cdu <- get_care_site_flows(cdu_bed_moves)

edgedf_cdu %>% select(csn) %>% n_distinct()
cdu_bed_moves %>% select(csn) %>% n_distinct() # 3 only had one location - therefore discharged from CDU

edgedf_cdu <- edgedf_cdu %>% left_join(
  edgedf_cdu %>% filter(to == "UCHT00CDU") %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)


# EDU - create edgelist

edgedf_edu <- get_care_site_flows(edu_bed_moves)

## NB edgelist not working for edu - need to check this

edgedf_edu %>% select(csn) %>% n_distinct()
edu_bed_moves %>% select(csn) %>% n_distinct() # 18 had only one location

edgedf_edu <- edgedf_edu %>% left_join(
  edgedf_edu %>% filter(to %in% c("AECU", "T01ECU")) %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)


# Looking at destinations after SAA --------------------------------------


edgedf_saa %>% filter(from == "SAA") %>% group_by(to) %>% summarise(tot = n()) %>% arrange(desc(tot))
edgedf_saa %>% filter(from == "SAA") %>% select(csn) %>% n_distinct()

edgedf_saa <- edgedf_saa %>% left_join(
  # get earliest location after this location that was not ED - 
  # note - need to join with edgedf to work out if before or after otherwies if came from other wards than ED, this doesn't pick them up
  saa_bed_moves %>% left_join(
    edgedf_saa %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "ED") %>%  group_by(csn) %>% summarise(min_not_ED = min(admission))
) %>%  mutate(first_not_ED = case_when(is.na(min_not_ED) ~ NA,
                                      min_not_ED == dttm ~ TRUE,
                                      TRUE ~ FALSE)) %>% select(-min_not_ED)

# next location after admission 

saa_summ <- bm_summ %>% filter(in_SAA) %>%   
  mutate(presentation_time = if_else(is.na(presentation_time), admission_time, presentation_time))%>% 
  left_join(
  edgedf_saa %>% filter(first_not_ED) %>% left_join(saa_bed_moves %>% select(csn, admission, department), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% 
    rename(dttm_left_ED = dttm , moved_to = department)
) %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time, units = "hours"))

# one patient has two ED visis - is a huge outlier
saa_summ %>% filter(!is.na(time_to_admit), time_to_admit < 30) %>% ggplot(aes(x = time_to_admit)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks =  seq(0,30,1), limits = c(0,30)) +
  labs(title = "Time from presentation at ED to admission for SAA patients were later admitted to a ward", 
       x = "Elapsed time (hours)") +
  geom_vline(xintercept = quantile(saa_summ$time_to_admit, .5, na.rm = TRUE), 
             linetype = "dotted", color = "red", size = 1)

saa_summ %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for SAA patients were later admitted", 
       x = "First location after ED")

# Looking at destinations after SDEC --------------------------------------


edgedf_sdec %>% filter(from == "SDEC") %>% group_by(to) %>% summarise(tot = n()) %>% arrange(desc(tot))
edgedf_sdec %>% filter(from == "SDEC") %>% select(csn) %>% n_distinct()
edgedf_sdec %>% select(csn) %>% n_distinct()

edgedf_sdec <- edgedf_sdec %>% left_join(
  # get earliest location after this location that was not ED - 
  # note - need to join with edgedf to work out if before or after otherwies if came from other wards than ED, this doesn't pick them up
  sdec_bed_moves %>% left_join(
    edgedf_sdec %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "ED") %>%  group_by(csn) %>% summarise(min_not_ED = min(admission))
) %>% 
  mutate(first_not_ED = case_when(is.na(min_not_ED) ~ NA,
                                      min_not_ED == dttm ~ TRUE,
                                      TRUE ~ FALSE)) %>% select(-min_not_ED)



# next location after admission 

# for a short time T01ECU had 'null' as department

sdec_bed_moves <- sdec_bed_moves %>% 
  mutate(department = case_when(department == "null" & grepl("T01ECU", location_string) ~ "T01ECU",
                                TRUE ~ department))

sdec_bed_moves %>% select(csn) %>% n_distinct()

sdec_summ <- bm_summ %>% filter(in_SDEC)  %>%
  mutate(presentation_time = if_else(is.na(presentation_time), admission_time, presentation_time))%>% 
  left_join(
  edgedf_sdec %>% filter(first_not_ED) %>% left_join(sdec_bed_moves %>% select(csn, admission, department), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% distinct() %>% 
    rename(dttm_left_ED = dttm , moved_to = department)
) %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time))

sdec_summ %>% select(csn) %>% n_distinct()

# NB two patients went back to ED from T01 !! These are not handled here


sdec_summ %>% filter(!is.na(time_to_admit), time_to_admit < 30) %>% ggplot(aes(x = time_to_admit)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks =  seq(0,30,1), limits = c(0,30)) +
  labs(title = "Time from presentation at ED to admission for SDEC patients who were later admitted to a ward", 
       x = "Elapsed time (hours)") +
  geom_vline(xintercept = quantile(sdec_summ$time_to_admit, .5, na.rm = TRUE), 
             linetype = "dotted", color = "red", size = 1)

sdec_summ %>% filter(!is.na(time_to_admit)) %>% select(csn) %>% n_distinct() 

# only 556 have a time to admit - why? 
# looks like some of the others have not yet been discharged
# and 20 never had an emergency class
sdec_summ %>% left_join(patient_class) %>% filter(is.na(max_emerg_class))


sdec_summ %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for SDEC patients were later admitted", 
       x = "First location after ED")

# to look at those that returned to ED or have null - seomthing weird going on with them !!
# the recent ones have null look like they haven't been discharged yet ?? 
s = sdec_summ %>% filter(moved_to == "ED") %>% select(csn) %>% left_join(sdec_bed_moves)

# handling patients who are still in hospital (therefore dttm for last row is NA) 
# in such cases first_not_ED has not been set because discharge - used for leftjoin to dttm above - was NA) 
# edgedf_sdec <- edgedf_sdec %>% group_by(csn) %>% mutate(last_dttm = lag(dttm))

no_first_not_ED <- edgedf_sdec %>% group_by(csn) %>% summarise(n = sum(first_not_ED)) %>% 
  filter(is.na(n)) %>% select(-n) %>% left_join(bm_summ %>% select(csn, patient_class)) %>% 
  filter(patient_class == "INPATIENT_class") %>% select(-patient_class)

# 65 inpatients have no first_not_ED set

# create a new first not ED to include these
edgedf_sdec <- edgedf_sdec %>%  
  left_join(no_first_not_ED %>% mutate(no_first_not_ED = TRUE)) %>% 
  mutate(first_not_ED2 = case_when(is.na(dttm) & no_first_not_ED ~ TRUE,
                                   TRUE ~ first_not_ED))
# this one has 620 rows (618 distinct csns) as opposed to the 556 shown in the original chart

# now recalculate time to admit

sdec_summ_x <- bm_summ %>% filter(in_SDEC)  %>%
  mutate(presentation_time = if_else(is.na(presentation_time), admission_time, presentation_time))%>% 
  left_join(
    edgedf_sdec %>% 
      # where there is dttm, use the previous dttm (the time they moved to the admitting department)
      group_by(csn) %>% mutate(dttm = case_when(is.na(dttm) ~ lag(dttm), 
                                                TRUE ~ dttm)) %>% 
      filter(first_not_ED2) %>% left_join(sdec_bed_moves %>% select(csn, admission, department), 
                                                       by = c("csn", "dttm" = "admission")) %>% 
      select(csn, dttm, department) %>% distinct() %>% 
      rename(dttm_left_ED = dttm , moved_to = department)
  ) %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time))



# and redraw charts

sdec_summ_x %>% filter(!is.na(time_to_admit), time_to_admit < 30) %>% ggplot(aes(x = time_to_admit)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks =  seq(0,30,1), limits = c(0,30)) +
  labs(title = "Time from presentation at ED to admission for SDEC patients who were later admitted to a ward", 
       x = "Elapsed time (hours)") +
  geom_vline(xintercept = quantile(sdec_summ$time_to_admit, .5, na.rm = TRUE), 
             linetype = "dotted", color = "red", size = 1)

sdec_summ_x %>% filter(!is.na(time_to_admit)) %>% select(csn) %>% n_distinct() 

sdec_summ_x %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for SDEC patients who were later admitted", 
       x = "First location after ED")


# Looking at destinations after CDU --------------------------------------


edgedf_cdu %>% filter(from == "UCHT00CDU") %>% group_by(to) %>% summarise(tot = n()) %>% arrange(desc(tot))
edgedf_cdu %>% filter(from == "UCHT00CDU") %>% select(csn) %>% n_distinct()

edgedf_cdu <- edgedf_cdu %>% left_join(
  # get earliest location after this location that was not ED - 
  # note - need to join with edgedf to work out if before or after otherwies if came from other wards than ED, this doesn't pick them up
  cdu_bed_moves %>% left_join(
    edgedf_cdu %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "UCHT00CDU") %>%  group_by(csn) %>% summarise(min_not_ED = min(admission))
) %>%  mutate(first_not_ED = case_when(is.na(min_not_ED) ~ NA,
                                       min_not_ED == dttm ~ TRUE,
                                       TRUE ~ FALSE)) %>% select(-min_not_ED)

# create an extra column for the first not ED or CDU
edgedf_cdu <- edgedf_cdu %>% left_join(
  cdu_bed_moves %>% left_join(
    edgedf_cdu %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "UCHT00CDU", department != "ED") %>%  group_by(csn) %>% summarise(min_not_ED_or_CDU = min(admission)) 
)  %>%  mutate(first_not_ED_or_CDU = case_when(is.na(min_not_ED_or_CDU) ~ NA,
                                               min_not_ED_or_CDU == dttm ~ TRUE,
                                        TRUE ~ FALSE)) %>% select(-min_not_ED_or_CDU)



# next location after admission - looking at first location which is not ED
# note that this double counts two csns because they return to ED so have two distinct values of dttm_left_ed

cdu_summ <- bm_summ %>% filter(in_CDU) %>% left_join(
  edgedf_cdu %>% filter(first_not_ED) %>% left_join(cdu_bed_moves %>% select(csn, admission, department) %>% distinct(), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% distinct() %>% 
    rename(dttm_left_ED = dttm , moved_to = department) %>% distinct()
)  %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time))

cdu_summ %>% filter(is.na(dttm_left_ED)) # 1552
cdu_summ %>% filter(!is.na(dttm_left_ED)) %>% group_by(moved_to) %>% summarise(n())

# next location after admission - - looking at first location which is not ED

cdu_summ_2 <- bm_summ %>% filter(in_CDU) %>%
  mutate(presentation_time = if_else(is.na(presentation_time), admission_time, presentation_time)) %>% 
  left_join(
    edgedf_cdu %>% filter(first_not_ED_or_CDU) %>% left_join(cdu_bed_moves %>% select(csn, admission, department) %>% distinct(), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% distinct() %>% 
    rename(dttm_left_ED_and_CDU = dttm , moved_to = department)
) %>% mutate(time_to_admit = difftime(dttm_left_ED_and_CDU, presentation_time))

cdu_summ_2 %>% filter(is.na(dttm_left_ED_and_CDU)) # 1540
cdu_summ_2 %>% filter(!is.na(dttm_left_ED_and_CDU)) %>% group_by(moved_to) %>% summarise(n())


cdu_summ_2 %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for CDU patients were later admitted", 
       x = "First location after ED and CDU")


cdu_summ_2 %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for CDU patients were later admitted", 
       x = "First location after ED and CDH")

cdu_summ_2 %>% filter(!is.na(time_to_admit), time_to_admit < 30) %>% 
  ggplot(aes(x = time_to_admit)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks =  seq(0,30,1), limits = c(0,30)) +
  labs(title = "Time from presentation at ED to admission for CDU patients who were later admitted to a ward", 
       x = "Elapsed time (hours)") +
  geom_vline(xintercept = quantile(cdu_summ_2$time_to_admit, .5, na.rm = TRUE), 
               linetype = "dotted", color = "red", size = 1)

# Looking at when the class was changed for inpatients
cdu_bed_moves %>% left_join(patient_class) %>% 
  filter(discharge > max_emerg_class, patient_class == "INPATIENT_class") %>% 
  group_by(csn) %>% filter(discharge == min(discharge, na.rm = TRUE)) %>% 
  group_by(department)  %>% summarise(count = n()) %>% 
  ggplot(aes(x = department, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Location of CDU patient at the moment when patient class changed from emergency to inpatient", 
       x = "Location")

# Save data so far --------------------------------------------------------


outFile = paste0("EDcrowding/flow-mapping/data-raw/saa_bed_moves_",today(),".rda")
save(saa_bed_moves, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/cdu_bed_moves_",today(),".rda")
save(cdu_bed_moves, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/sdec_bed_moves_",today(),".rda")
save(sdec_bed_moves, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edu_bed_moves_",today(),".rda")
save(edu_bed_moves, file = outFile)



outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_saa_",today(),".rda")
save(edgedf_saa, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_cdu_",today(),".rda")
save(edgedf_cdu, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_sdec_",today(),".rda")
save(edgedf_sdec, file = outFile)





outFile = paste0("EDcrowding/flow-mapping/data-raw/bm_summ_",today(),".rda")
save(bm_summ, file = outFile)



# Missing ED bed moves ----------------------------------------------------
# this has been copied from exploring-consquences-of-filters so that it can use the same edge list creation scripts


load("~/EDcrowding/flow-mapping/data-raw/missing_ED_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/patient_class_2020-12-08.rda")


# patient class history

# patient class change information - get history

sqlQuery <- "select encounter as csn, patient_class, valid_from, valid_until from star_test.hospital_visit_audit"

sqlQuery <- gsub('\n','',sqlQuery)
all_patient_class <- as_tibble(dbGetQuery(ctn, sqlQuery))



# missing ED csns

missing_ED = missing_ED_bed_moves %>% select(csn) %>% distinct()
missing_ED <- missing_ED %>% inner_join(csn_summ) # any extras are more recent than csn_summ so expet csn num to reduce

missing_ED %>% select(csn) %>% n_distinct()

all_patient_class_ =  all_patient_class %>% inner_join(missing_ED_bed_moves %>% select(csn) %>% distinct()) %>% 
  filter(patient_class %in% c("EMERGENCY")) %>% 
  group_by(csn) %>% summarise(earliest_emerg_class = min(valid_from))

missing_ED <- missing_ED %>% 
  left_join(
    all_patient_class_ 
  )

missing_ED <- missing_ED %>% 
  left_join(patient_class %>% select(csn, max_emerg_class))

# some cans seems to have different mrns ??  so need to exclude mrn in this join


missing_ED_bed_moves <- missing_ED_bed_moves %>% select(-hospital_visit_id) %>% 
  mutate(department = split_location(location_string, 1),
         room = split_location(location_string, 2))

missing_ED_bed_moves <- missing_ED_bed_moves %>% inner_join(missing_ED)
missing_ED_bed_moves %>% select(csn) %>% n_distinct()

missing_ED_bed_moves <- missing_ED_bed_moves  %>% 
  select(-arrival_method, -discharge_destination, -discharge_disposition, -discharge_time) 

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  mutate(in_ED_class = if_else(admission < max_emerg_class, TRUE, FALSE))

missing_ED_bed_moves %>% filter(in_ED_class) %>% 
  group_by(department) %>% summarise(n())

# find first row

arrival_row = missing_ED_bed_moves %>% group_by(csn) %>% summarise(min_admission = min(admission))

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  left_join(arrival_row)

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  mutate(arrival_row = if_else(admission == min_admission, TRUE, FALSE))

missing_ED_bed_moves %>% filter(arrival_row) %>% 
  group_by(department) %>% summarise(tot = n()) %>% arrange(desc(tot))

m =missing_ED_bed_moves %>% filter(arrival_row) %>% 
  mutate(min_admission_before_admission_time = if_else(min_admission < admission_time, TRUE, FALSE),
         min_admission_at_admission_time = if_else(min_admission == admission_time, TRUE, FALSE), 
         min_admission_before_pres_time = if_else(min_admission < presentation_time, TRUE, FALSE), 
         has_earliest_emerg_class_before_admission_time = if_else(earliest_emerg_class < admission_time, TRUE, FALSE), 
         has_max_emerg_class_before_admission_time = if_else(max_emerg_class < admission_time, TRUE, FALSE), 
         has_earliest_emerg_class_before_min_admission = if_else(earliest_emerg_class < min_admission, TRUE, FALSE), 
         admission_time_to_earliest_emerg_classs = difftime(earliest_emerg_class , admission_time, units = "mins"),
         admission_time_to_max_emerg_class = difftime(max_emerg_class, admission_time, units = "mins"),
         min_admission_to_earliest_emerg_class = difftime(earliest_emerg_class, min_admission, units = "mins"),
         min_admission_to_max_emerg_class = difftime(max_emerg_class, min_admission, units = "mins"))

m %>% group_by(in_ED_class) %>% summarise(n())
m %>% group_by(min_admission_before_admission_time) %>% summarise(n())
m %>% group_by(min_admission_at_admission_time) %>% summarise(n())

m %>% group_by(min_admission_before_pres_time) %>% summarise(n())
m %>% group_by(has_earliest_emerg_class_before_admission_time) %>% summarise(n())
m %>% group_by(has_max_emerg_class_before_admission_time) %>% summarise(n())
m %>% group_by(has_earliest_emerg_class_before_min_admission) %>% summarise(n())

# to create table in my google sheet
m1 =m %>% group_by(department, patient_class) %>% summarise(tot = n()) %>% 
  pivot_wider(names_from = patient_class, values_from = tot, values_fill = 0) %>% arrange(desc(INPATIENT))

# get a sense of the order of each timestamp

m_long = m %>% select(csn, min_admission, admission_time, earliest_emerg_class, max_emerg_class) %>% 
  pivot_longer(min_admission:max_emerg_class, names_to = "timestamp", values_to = "time") %>% 
  arrange(csn, time)


# to see how many went to different locations
missing_ED_bed_moves %>% select(csn, department) %>% distinct() %>% 
  group_by(csn) %>% summarise(tot = n()) %>% filter(tot >1)

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  mutate(location = department)



edgedf_missing <- get_care_site_flows(missing_ED_bed_moves)

# to see what their arrival department was (does not seem quite the same as m1)
edgedf_missing %>% group_by(csn) %>% summarise(dttm = min(dttm)) %>% left_join(edgedf_missing) %>% group_by(from) %>% summarise(n())


# Looking at when the class was changed
missing_ED_bed_moves %>% filter(discharge > max_emerg_class, patient_class == "INPATIENT") %>% 
  group_by(csn) %>% filter(discharge == min(discharge, na.rm = TRUE)) %>% 
  group_by(department)  %>% summarise(count = n()) %>% 
  ggplot(aes(x = department, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Location of patient at the moment when patient class changed from emergency to inpatient", 
       x = "Location")
  

# Looking at when the class was changed (detail)
missing_ED_bed_moves %>% filter(discharge > max_emerg_class, patient_class == "INPATIENT") %>% 
  group_by(csn) %>% filter(discharge == min(discharge, na.rm = TRUE)) %>% 
  group_by(department)  %>% summarise(count = n()) %>% filter(grepl("^T", department)) %>% 
  ggplot(aes(x = department, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Location of patient at the moment when patient class changed from emergency to inpatient", 
       subtitle = "Tower locations only",
       x = "Location")


edgelist_summ = edgedf_missing  %>%  
  mutate(to = case_when(to == "EMERGENCY" ~ "EMERGENCY_discharge",
                        to == "INPATIENT" ~ "INPATIENT_discharge",
                        TRUE ~ to)) %>% 
  group_by(from, to) %>%
  summarise(weight = n())


setwd("/data/zelking1/EDcrowding")


outFile <- paste0("missing_ED_edgelist_summ_2020-12-14.csv")
write_delim(edgelist_summ, path = outFile, delim = ',')

outFile = paste0("edgedf_missing_ED_2020-12-rda")

save(edgedf_missing, file = outFile)

outFile = "missing_ED_bed_moves_2020-12-14.rda"

save(missing_ED_bed_moves, file = outFile)

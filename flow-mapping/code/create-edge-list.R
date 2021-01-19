# ===============
# About this file
# ===============
# Loads data on ED patients locations
# Formats a rransition matrix and a network mapping input
# Code for edge list has been taken from create-edge-list-old.R



# Load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)

# Define functions
# ================

# # define level of granularity for node
# # if in ED use room, else node set to Admission for all wards
# 
# get_node <- function(dept, room) {
#   if (dept == "Still in ED") {
#     node <- room
#   }
#   else {
#     node <- dept
#   }
# }

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

get_node <- function(department, room) {
  if (department != "ED") {
    room <- department
  }
  return(room)
}
get_node <- Vectorize(get_node)

# for a single encounter, this function generates an edge list of 
# moves between locations, with a timestamp
# locations are provided to it in a tibble called sites
# sites needs 5 attributes: csn, location, time of admission to location, 
# and time of discharge from location

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
    else {
        edgelist <- edgelist %>% add_row(tibble_row(
          csn = sites$csn[j],
          from = from_node,
          to = if_else(is.na(discharge[j]), "Still in hospital", sites$patient_class[j]),
          dttm = sites$discharge[j]
        ))
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
  
  edgedf <- edgedf %>% left_join(
    # get earliest location that was not ED  
    locations %>% left_join(
      edgedf %>% select(csn, dttm), by = c("csn", "discharge" = "dttm")
    ) %>% 
      filter(department != "ED") %>%  group_by(csn) %>% summarise(earliest_not_ED = min(admission))
  ) 
  
  return(edgedf)
}




# Load data
# =========
# 
# load("~/EDcrowding/flow-mapping/data-raw/missing_ED_2020-12-08.rda")
# 
# ED_bed_moves <- missing_ED_bed_moves %>% 
#   mutate(location = split_location(location_string, 1))

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-12-08.rda")

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-08.rda")
# load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-12-08.rda")


# Create and save edge list for given list of dates --------------------------------------


for (date_ in c("2020-10-15", "2020-07-15", "2020-04-15", "2020-01-15")) {
  
  locations <- ED_csn_summ %>% filter(date(presentation_time) == date_) %>% select(csn, patient_class) %>% 
    left_join(ED_bed_moves) %>% mutate(location = get_node(department, room4))
  
  edgedf <- get_care_site_flows(locations)
  
  name_ <- paste0("edgedf_", gsub("-", "", date_))
  assign(name_, edgedf)
  
  outFile = paste0("EDcrowding/flow-mapping/data-raw/",name_, ".rda")
  save(name_, file = outFile)
}



# Save edgelist -----------------------------------------------------------


# save edge list for future use
outFile = paste0("EDcrowding/flow-mapping/data-raw/missing_ED_edgelist_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)

# OR load edge list if already saved


# Save adjacancy matrix ---------------------------------------------------



# in the version created on 24/11 there is a problem with patients going from Arrived to Waiting and back again
# I have (I think corrected this in the function called )

# a temp fix

edgedf <- edgedf %>% mutate(edge = paste0(from, to)) %>% filter(edge != "WaitingArrived") %>% 
  select(-edge) %>% 
  # adding distinct will get rid of duplicate Arrived - Waiting rows
  distinct()

# for adjacency matrix
adj_matrix <- edgedf %>%
  group_by(from, to) %>%
  summarise(tot = n()) %>%
  filter(!from %in% c( "Admitted", "DIAGNOSTICS", "OTF")) %>%
  pivot_wider(names_from = to, values_from = tot) %>%
  column_to_rownames("from") %>%
  replace(is.na(.), 0)

outFile <- paste0("EDcrowding/data-prep-for-ML/data-output/adjacency-matrix-April19-to-Feb20",today(),".csv")


# Save for network mapping ------------------------------------------------


for (date_ in c("2020-10-15", "2020-07-15", "2020-04-15", "2020-01-15")) {
  
  name_ <- paste0("edgedf_", gsub("-", "", date_))
  edgedf <- get(name_)
  
  edgelist_summ = edgedf %>% filter(is.na(earliest_not_ED) | earliest_not_ED >= dttm) %>%  
    mutate(to = case_when(to == "EMERGENCY" ~ "EMERGENCY_class",
                          to == "INPATIENT" ~ "INPATIENT_class",
                          TRUE ~ to)) %>% 
    group_by(from, to) %>%
    summarise(weight = n()) 
  
  file_ <- paste0("edgelist_summ_",gsub("-", "", date_) )
  
  outFile <- paste0("EDcrowding/flow-mapping/data-output/",file_,".csv")
  write_delim(edgelist_summ, path = outFile, delim = ',')
  
}

# for simple matrix input to network mapping (more details in get-edge-stat)


outFile <- paste0("EDcrowding/flow-mapping/data-output/missing_ED_edgelist_summ_",today(),".csv")
write_delim(edgelist_summ, path = outFile, delim = ',')




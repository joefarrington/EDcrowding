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




# Load data
# =========

load("~/EDcrowding/flow-mapping/data-raw/missing_ED_2020-12-08.rda")

ED_bed_moves <- missing_ED_bed_moves %>% 
  mutate(location = split_location(location_string, 1))

#load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-12-08.rda")

ED_bed_moves <- ED_bed_moves %>% left_join(csn_summ %>% select(csn, patient_class))


# Create edge list
# ================

# create edge list which contains one row per bed move per patient with a time stamp
# note this is currently using room6, which is the grouped version of room and
# that it subsumes OTF into the previous node 
edgedf <- get_care_site_flows(ED_bed_moves)

# save edge list for future use
outFile = paste0("EDcrowding/flow-mapping/data-raw/missing_ED_edgelist_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)

# OR load edge list if already saved



# in the version created on 24/11 there is a problem with patients going from Arrived to Waiting and back again
# I have (I think corrected this in the function called )

# a temp fix

edgedf <- edgedf %>% mutate(edge = paste0(from, to)) %>% filter(edge != "WaitingArrived") %>% 
  select(-edge) %>% 
  # adding distinct will get rid of duplicate Arrived - Waiting rows
  distinct()
#
# Process data
# ============

# for adjacency matrix
adj_matrix <- edgedf %>%
  group_by(from, to) %>%
  summarise(tot = n()) %>%
  filter(!from %in% c( "Admitted", "DIAGNOSTICS", "OTF")) %>%
  pivot_wider(names_from = to, values_from = tot) %>%
  column_to_rownames("from") %>%
  replace(is.na(.), 0)

outFile <- paste0("EDcrowding/data-prep-for-ML/data-output/adjacency-matrix-April19-to-Feb20",today(),".csv")
write.csv2(adj_matrix, file = outFile, row.names = TRUE)

# for simple matrix input to network mapping (more details in get-edge-stat)
edgelist_summ = edgedf %>%
  mutate(to = case_when(to == "EMERGENCY" ~ "EMERGENCY_class",
                        to == "INPATIENT" ~ "INPATIENT_class",
                        TRUE ~ to)) %>% 
  group_by(from, to) %>%
  summarise(weight = n()) 

outFile <- paste0("EDcrowding/flow-mapping/data-output/missing_ED_edgelist_summ_",today(),".csv")
write_delim(edgelist_summ, path = outFile, delim = ',')




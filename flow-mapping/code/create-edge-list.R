# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Define functions
# ================

# hl7_location seems to have more complete info than room and bed for some rows
# therefore this function splits hl7_location into bed and room

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)


# for any encounter, this function generates an edge list of sites 
get_edgelist <- function(sites) {
  
  edgelist <- tribble(
    ~mrn,
    ~csn,
    ~from,
    ~to,
    ~dttm)
  
  from_node = as.character(sites$room2[1])
  for (j in 1:nrow(sites)) {
    if (j != nrow(sites)) {
      to_node <- as.character(sites$room2[j + 1] )
      if (from_node != to_node) {
        
        edgelist <- edgelist %>% add_row(tibble_row(
          mrn = sites$mrn[j],
          csn = sites$csn[j],
          from = from_node,
          to = to_node,
          dttm = sites$discharge[j]
        ))
      }
      from_node <- to_node
    }
  }
  return(edgelist)
}

get_care_site_flows <- function(visits) {
  # look up the number of visit details within each encounter
  visits_check <- visits %>% 
    group_by(mrn, csn) %>% 
    summarise("num_visit_details" = n(), "earliest_visit_ts" = min(admission))
  
  # filter only those visits with more than one site move
  visits_gt1 <- visits_check %>% 
    filter(num_visit_details > 1)
  
  # then create edge list for all visits
  
  edgedf <- tribble(
    ~mrn,
    ~csn,
    ~from,
    ~to,
    ~dttm)
  
  for (i in (1:nrow(visits_gt1))) {
    visit_csn = visits_gt1$csn[i]
    num_visits = visits_gt1$num_visit_details[i]
    
    #get all visit detail records for this visit occurence
    sites <- visits %>% filter(csn == visit_csn) %>% arrange(admission)
    
    # create edgelist if the visit involved change of site
    edgelist <- get_edgelist(sites)
    if (nrow(edgelist) > 0) {
      edgedf <- edgedf %>% add_row(edgelist)
    }
  }
  return(edgedf)
}



# Load bed move data
# ==================

# see Exploring star 3.R

# Analyse data
# ============

# hl7_location seems to have more complete info than room and bed for some rows
ED_bed_moves %>% filter(hl7_location != "ED^Null^Null", is.na(room)) %>% summarise(n())
ED_bed_moves %>% filter(hl7_location != "ED^Null^Null", is.na(bed)) %>% summarise(n())

# visits <- bed_moves %>% mutate(dept2 = split_location(hl7_location, 1),
#                               room2 = split_location(hl7_location, 2),
#                               bed2 = split_location(hl7_location, 3)) %>% 
#   arrange(mrn, csn, admission)

edgedf <- get_care_site_flows(ED_bed_moves)

# save data for future use
setwd("//uclcmddprafss21/Home/zelking1/Documents/EDcrowding/flow-mapping")
outFile = paste0("data-raw/ED_edge_list_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)



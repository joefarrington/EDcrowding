
# About the preprocessing steps -------------------------------------------

# retrieve data
# - deal with missing admission time
# - deal with missing discharge time
# - deal with outlier ages



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


# Database connection -----------------------------------------------------


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get  data ---------------------------------------------------------

# hopital visit summary

sqlQuery <- "select * from star.encounters 
"

sqlQuery <- gsub('\n','',sqlQuery)
csn_summ <- as_tibble(dbGetQuery(ctn, sqlQuery))

print(paste0("csn_summ", Sys.Date()))
rpt(csn_summ) 

# locations

sqlQuery <- "select * from icu_audit.locations "
sqlQuery <- gsub('\n','',sqlQuery)
locations <- as_tibble(dbGetQuery(ctn, sqlQuery))

locations <- locations %>% mutate(department = split_location(hl7_location, 1))
locations = unique(locations %>% select(department, epicdepartmentname))
locations = data.table(locations)

outFile = paste0("EDcrowding/flow-mapping/data-raw/locations_",today(),".rda")
save(locations, file = outFile)


# Save data ---------------------------------------------------------------

# save csn_summ for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/csn_summ_star_",today(),".rda")
save(csn_summ, file = outFile)
rm(outFile)


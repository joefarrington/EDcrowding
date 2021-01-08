
# About this file ---------------------------------------------------------

# Creates an edge list for locations for all patients; 
# Note that current locations are not known 

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

#######

# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}



# Load data ---------------------------------------------------------------
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_2021-01-06.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_2021-01-06.rda")


moves <- data.table(ED_bed_moves %>% 
                       mutate(location = case_when(department == "ED"~ room4, 
                                                   TRUE ~ department)) %>% 
                                select(csn, admission, discharge, department, location, covid_pathway)) 

# need to add a condition here if more null departments appear - for now just check
#ED_bed_moves %>% filter(department == "null") %>% group_by(department) %>% summarise(n())
moves[department == "null", department := "T01ECU"]

# add lead csn, admission and location - see https://rdrr.io/cran/data.table/man/shift.html
cols = c("csn","admission","discharge", "department", "location")
leadcols = paste("lead", cols, sep="_")
lagcols = paste("lag", cols, sep="_")



# update with next row's discharge time when locations are repeated
#moves[csn == lead_csn & location == lead_location, discharge := lead_discharge]
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
moves[csn == lead_csn & location == lead_location, drop_row := TRUE]
# NB there is an error here; csn 1017530862 moved three times within T01 and now has dttm mismatch
rpt(moves) 
rpt(moves[(drop_row)])


# remove rows where location is repeated
moves <- moves[is.na(drop_row)]
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
moves[csn == lag_csn & admission != lag_discharge, amend_row := TRUE]
rpt(moves[(amend_row)])
moves[csn == lag_csn & admission != lag_discharge, admission := lag_discharge]


# delete cols not needed

set(moves, NULL , c("drop_row", "amend_row", "lag_csn", "lag_location", "lag_department",
                    "lag_admission", "lag_discharge", "lead_admission", "lead_discharge"), NULL)
rpt(moves)

# update lead location where csns change
moves[, lead_csn := if_else(csn != lead_csn, NA_character_, lead_csn)]
moves[, lead_department := if_else(csn != lead_csn, NA_character_, lead_department)]
moves[, lead_location := if_else(csn != lead_csn, NA_character_, lead_location)]

# calc row duration
moves[, "row_duration" := difftime(discharge, admission, units = "mins")]

# identify ED exits
moves[, "ED_exit" := if_else(lead_department !="ED" & department == "ED", TRUE, FALSE)]
moves[, "num_ED_exit" := sum(ED_exit, na.rm = TRUE), by = csn]

# identify if ED exit is less than 15 min - only handles cases where there is more than one ED exit 
# do this by checking duration of next row
moves[, ED_exit_short := if_else(num_ED_exit > 1 & ED_exit & csn == lead_csn & shift(row_duration, 1, NA, "lead") < 15, TRUE, FALSE)]
# Create new ED_exit_short variable as the original only relates to visits with more than one ED exit
moves[, ED_exit_short2 := if_else(ED_exit & csn == lead_csn & 
                                    shift(row_duration, 1, NA, "lead") < 15 &
                                    lead_department == "ED", TRUE, FALSE)]

# identify exits to relevant locations 
moves[, "exit_to_EAU" := if_else(ED_exit & !ED_exit_short2 & department =="ED" & lead_department == "EAU", 1, 0)]
moves[, "exit_to_CDU" := if_else(ED_exit & !ED_exit_short2 & department =="ED" & lead_department == "UCHT00CDU", 1, 0)]
moves[, "exit_to_EDU" := if_else(ED_exit & !ED_exit_short2 & department =="ED" & lead_department %in% c("T01ECU", "AECU"), 1, 0)]
moves[, "exit_to_T01" := if_else(ED_exit & !ED_exit_short2 & department =="ED" & lead_department == "T01", 1, 0)]

moves[, "visited_EAU" := sum(department == "EAU", na.rm = TRUE) > 0, by = csn]
moves[, "visited_CDU" := sum(department == "UCHT00CDU", na.rm = TRUE) > 0, by = csn]
moves[, "visited_EDU" := sum(department  %in% c("T01ECU", "AECU"), na.rm = TRUE) > 0, by = csn]
moves[, "visited_T01" := sum(department == "T01", na.rm = TRUE) > 0, by = csn]

# group exits via relevant locations
moves[, "obs" := case_when(department == "UCHT00CDU" ~ 1, 
                                   location == "SAA" ~ 1, 
                                   TRUE ~ 0)]
moves[, "same_day" := case_when(department %in% c("T01ECU", "AECU") ~ 1,
                           location == "SDEC" ~ 1, 
                           TRUE ~ 0)]
moves[, "acute" := case_when(department == "EAU" ~ 1, 
                             department == "T01" ~ 1, 
                             TRUE ~ 0)]

moves[, "visited_obs" := sum(obs, na.rm = TRUE) > 0, by = csn]
moves[, "visited_same_day" := sum(same_day, na.rm = TRUE) > 0, by = csn]
moves[, "visited_acute" := sum(acute, na.rm = TRUE) >0, by = csn]

# identify whether visit limited to observation or same day locations
moves[, "ED" := if_else(department == "ED", 1, 0)]
moves[, "ED_obs_same_day" := ED + obs + same_day]
moves[, "outside":= sum(ED_obs_same_day == 0, na.rm = TRUE) > 0, by = csn]


# identify rows containing final location
#moves[, .SD[which.max(discharge)], by = csn]
moves[, "final_admission" := max(admission), by = csn]
moves[, "final_location" := location[which(admission == final_admission)], by = csn]
moves[, "final_dept" := department[which(admission == final_admission)], by = csn]

# identify rows containing first location
moves[, "min_admission" := min(admission), by = csn]
moves[, "first_location" := location[which(admission == min_admission)], by = csn]
moves[, "first_dept" := department[which(admission == min_admission)], by = csn]

# # get final ED time
# moves[, "max_dept" := max(discharge), by=list(csn, department)]


# For edge list processing --------



# create final edge list
edgedf <- moves_[,.(csn, from = location, to = lead_location, 
                     from_dept = department, to_dept = lead_department, dttm = discharge)]






# Save data ---------------------------------------------------------------

setkey(moves, csn)
outFile = paste0("EDcrowding/flow-mapping/data-raw/moves_",today(),".rda")
save(moves, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)


# About this file ---------------------------------------------------------

# Creates an edge list for locations for all patients; 
# Note that current locations are not known 

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)



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

# add lead csn, admission and location - see https://rdrr.io/cran/data.table/man/shift.html
cols = c("csn","admission","discharge", "department", "location")
leadcols = paste("lead", cols, sep="_")
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
rpt(moves) 


# update with next row's discharge time when locations are repeated
moves[csn == lead_csn & location == lead_location, discharge := lead_discharge]
moves[csn == lead_csn & location == lead_location, drop_row := TRUE]
# NB there is an error here; csn 1017530862 moved three times within T01 and now has dttm mismatch
rpt(moves) 

# remove rows where location is repeated
moves <- moves[is.na(drop_row)]
moves[, drop_row := NULL]
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
moves[, "to_EAU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "EAU", 1, 0)]
moves[, "to_CDU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "UCHT00CDU", 1, 0)]
moves[, "to_EDU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "T01ECU", 1, 0)]
moves[, "to_T01" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "T01", 1, 0)]

moves[, "num_to_EAU" := sum(to_EAU, na.rm = TRUE), by = csn]
moves[, "num_to_CDU" := sum(to_CDU, na.rm = TRUE), by = csn]
moves[, "num_to_EDU" := sum(to_EDU, na.rm = TRUE), by = csn]
moves[, "num_to_T01" := sum(to_T01, na.rm = TRUE), by = csn]

# group exits via relevant locations
moves[, "via_obs" := case_when(ED_exit & !ED_exit_short & department =="ED" & lead_department == "UCHT00CDU" ~ 1, 
                               location == "SAA" ~ 1, 
                               location == "SDEC" ~ 1, 
                               TRUE ~ 0)]
moves[, "via_day_path" := case_when(ED_exit & !ED_exit_short & department =="ED" & lead_department == "EAU" ~ 1, 
                                    ED_exit & !ED_exit_short & department =="ED" & lead_department == "T01ECU" ~ 1, 
                               TRUE ~ 0)]

moves[, "num_via_obs" := sum(via_obs, na.rm = TRUE), by = csn]
moves[, "num_via_day_path" := sum(via_day_path, na.rm = TRUE), by = csn]

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


# create final edge list
edgedf <- moves[,.(csn, from = location, to = lead_location, 
                     from_dept = department, to_dept = lead_department, dttm = discharge)]






# Save data ---------------------------------------------------------------

setkey(moves, csn)
outFile = paste0("EDcrowding/flow-mapping/data-raw/moves_",today(),".rda")
save(moves, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)

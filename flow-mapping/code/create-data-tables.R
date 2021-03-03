
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
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_2021-03-02.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_2021-03-02.rda")


moves <- data.table(ED_bed_moves_raw %>% 
                       mutate(location = case_when(department == "ED" & room4 == "TRIAGE" ~ "Waiting",
                                                   department == "ED" ~ room4, 
                                                   TRUE ~ department)) %>% 
                      select(csn, admission, discharge, department, location) %>% 
                      arrange(csn, admission))
setkey(moves, csn)

# Set up column names for lead and lag functionality
cols = c("csn","admission","discharge", "department", "location")
leadcols = paste("lead", cols, sep="_")
lagcols = paste("lag", cols, sep="_")

# Deal with OTF rows first -----------------------------------------------------------
# remove rows where OTF and set location to be same as previous
# check moves[csn == "1017913821"]

moves[, "otf" := case_when(location == "OTF" ~ 1, 
                           TRUE ~ 0)]
moves[, num_OTF := sum(otf), by = csn]

# # to get summary of numbers involved - load summ first
# moves = merge(moves, summ[,.(csn, adm)], all.x = TRUE)
# m = moves[, .(N = uniqueN(csn)), by = .(adm, num_OTF)]
# m %>% arrange(adm, num_OTF) %>%  pivot_wider(names_from = num_OTF, values_from = N)
# # to save csns for later reference
# moves_ = moves[num_OTF > 0]

# reset lead and lag values
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]

# find rows where OTF is last row
moves[csn == lag_csn & location == "OTF" & csn != lead_csn, otf_row_to_drop_last_row := TRUE]
rpt(moves) 
rpt(moves[(otf_row_to_drop_last_row)]) # this is number of csns where a row will be dropped
moves <- moves[is.na(otf_row_to_drop_last_row)]
rpt(moves) 
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
moves[csn == lag_csn & admission != lag_discharge] # checking - should be zero as only last row was deleted

# find rows where OTF is first row - check moves[csn == "1022475707"]
moves[csn != lag_csn & location == "OTF" & csn == lead_csn, otf_row_to_drop_first_row := TRUE]
rpt(moves) 
rpt(moves[(otf_row_to_drop_first_row)]) # this is number of csns where a row will be dropped
moves <- moves[is.na(otf_row_to_drop_first_row)]
rpt(moves) 
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
moves[csn == lead_csn & lead_admission != discharge] # checking - should be zero as only first row was deleted

# drop whole csns if they have outrageously long OTF rows
# calc row duration
moves[, "row_duration_temp" := difftime(discharge, admission, units = "hours")]
# moves[row_duration_temp > 4 & location == "OTF"] %>% ggplot(aes(x = row_duration_temp, y = reorder(csn, row_duration_temp), fill = lead_department)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Duration of OTF rows with more than 4 hours duration, with next location",
#        x = "Duration of OTF row (hours)",
#        fill = "Next location",
#        y = "csn") +
#   scale_x_continuous(breaks = seq(0,180, 20))

moves[, problematic_duration := row_duration_temp > 10 & location == "OTF"]
moves[, drop_csn := sum(problematic_duration) > 0, by = csn]
moves = moves[!(drop_csn)]
rpt(moves)

# find other rows with OTF 
moves[csn == lag_csn & location == "OTF", otf_row_to_drop := TRUE]
rpt(moves) 
rpt(moves[(otf_row_to_drop)])
# remove OTF rows
moves <- moves[is.na(otf_row_to_drop)]
rpt(moves)

# update admission date of the remaining rows
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
moves[csn == lag_csn & admission != lag_discharge, amend_row := TRUE] 
rpt(moves[(amend_row)])
moves[csn == lag_csn & admission != lag_discharge, admission := lag_discharge]

# redo first and last row checks in case there were repeated OTF rows at beginning or end
# find rows where OTF is last row
moves[csn == lag_csn & location == "OTF" & csn != lead_csn, otf_row_to_drop_last_row := TRUE]
rpt(moves[(otf_row_to_drop_last_row)]) # this is number of csns where a row will be dropped
moves <- moves[is.na(otf_row_to_drop_last_row)]

# find rows where OTF is first row - check moves[csn == "1022475707"]
moves[csn != lag_csn & location == "OTF" & csn == lead_csn, otf_row_to_drop_first_row := TRUE]
rpt(moves[(otf_row_to_drop_first_row)]) # this is number of csns where a row will be dropped
moves <- moves[is.na(otf_row_to_drop_first_row)]
rpt(moves) 

# remove any lag and lead rows for avoidance of error
set(moves, NULL , c("otf_row_to_drop", "otf_row_to_drop_last_row", "otf_row_to_drop_first_row", "num_OTF", "otf",
                    "amend_row", "drop_csn", "row_duration_temp", "problematic_duration", 
                    "lag_csn", "lag_location", "lag_department", "lag_admission", "lag_discharge",  
                    "lead_csn", "lead_location", "lead_department", "lead_admission", "lead_discharge"), NULL)




# Deal with repeated locations --------------------------------------------
# update with next row's discharge time when locations are repeated

# add lead and lag csn, admission and location - see https://rdrr.io/cran/data.table/man/shift.html
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
# find rows where locations are repeated so the first one can be dropped
moves[csn == lead_csn & location == lead_location, drop_row := TRUE]
rpt(moves) 
rpt(moves[(drop_row)]) # this is number of csns where a row will be dropped
# remove rows where location is repeated
moves <- moves[is.na(drop_row)]
rpt(moves) 

# update admission date of the remaining row
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
moves[csn == lag_csn & admission != lag_discharge, amend_row := TRUE]
rpt(moves[(amend_row)])
moves[csn == lag_csn & admission != lag_discharge, admission := lag_discharge]

# remove any lag and lead rows for avoidance of error
set(moves, NULL , c("drop_row", "amend_row", 
                    "lag_csn", "lag_location", "lag_department", "lag_admission", "lag_discharge",  
                    "lead_csn", "lead_location", "lead_department", "lead_admission", "lead_discharge"), NULL)





# Other -------------------------------------------------------------------

# reset lead columns
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]

# update lead location where csns change
moves[, lead_department := if_else(csn != lead_csn, NA_character_, lead_department)]
moves[, lead_location := if_else(csn != lead_csn, NA_character_, lead_location)]
moves[, lead_csn := if_else(csn != lead_csn, NA_character_, lead_csn)]


# calc row duration
moves[, "row_duration" := difftime(discharge, admission, units = "mins")]

# identify ED exits
moves[, "ED_exit" := if_else(lead_department !="ED" & department == "ED", TRUE, FALSE)]
moves[, "num_ED_exit" := sum(ED_exit, na.rm = TRUE), by = csn]

# identify if ED exit is less than 15 min 
moves[, ED_exit_short := if_else(ED_exit & csn == lead_csn & 
                                    shift(row_duration, 1, NA, "lead") < 15 &
                                    lead_department == "ED", TRUE, FALSE)]

# identify exits to relevant locations 
moves[, "exit_to_EAU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "EAU", 1, 0)]
moves[, "exit_to_CDU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "UCHT00CDU", 1, 0)]
# moves[, "exit_to_EDU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department %in% c("T01ECU", "AECU"), 1, 0)]
# moves[, "exit_to_T01" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "T01", 1, 0)]

moves[, "visited_EAU" := sum(department == "EAU", na.rm = TRUE) > 0, by = csn]
moves[, "visited_CDU" := sum(department == "UCHT00CDU", na.rm = TRUE) > 0, by = csn]
# moves[, "visited_EDU" := sum(department  %in% c("T01ECU", "AECU"), na.rm = TRUE) > 0, by = csn]
# moves[, "visited_T01" := sum(department == "T01", na.rm = TRUE) > 0, by = csn]

# group exits via relevant locations
moves[, "obs" := case_when(department == "UCHT00CDU" ~ 1, 
                                   TRUE ~ 0)]
moves[, "same_day" := case_when(location == "SDEC" ~ 1, 
                           TRUE ~ 0)]
# moves[, "acute" := case_when(department == "EAU" ~ 1, 
#                              department == "T01" ~ 1, 
#                              TRUE ~ 0)]

moves[, "visited_obs" := sum(obs, na.rm = TRUE) > 0, by = csn]
moves[, "visited_same_day" := sum(same_day, na.rm = TRUE) > 0, by = csn]
# moves[, "visited_acute" := sum(acute, na.rm = TRUE) >0, by = csn]

# identify whether visit limited to observation or same day locations (referred to as inside)
moves[, "ED" := if_else(department == "ED", 1, 0)]
moves[, "ED_obs_same_day" := ED + obs + same_day]
moves[, "outside" := ED_obs_same_day == 0]
moves[, "visited_outside":= sum(ED_obs_same_day == 0, na.rm = TRUE) > 0, by = csn]
moves[, "inside_exit" := if_else(!outside & !is.na(lead_csn) & shift(outside, 1, NA, "lead"), TRUE, FALSE)]


# get last inside rows
moves[(!outside), last_inside := if_else(discharge == max(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]
moves[ED == 1, last_ED := if_else(discharge == max(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]

last_inside_ = unique(moves[(last_inside), list(csn, discharge)])
setnames(last_inside_, "discharge", "last_inside_discharge")

last_ED_ = unique(moves[(last_ED), list(csn, discharge)])
setnames(last_ED_, "discharge", "last_ED_discharge")

moves = merge(moves, last_inside_, all.x = TRUE)
moves = merge(moves, last_ED_, all.x = TRUE)
rm(last_ED_, last_inside_)

# get first ED rows
moves[ED == 1, first_ED := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_ED_ = unique(moves[(first_ED), list(csn, admission)])
setnames(first_ED_, "admission", "first_ED_admission")
moves = merge(moves, first_ED_, all.x = TRUE)
rm(first_ED_)

# get first outside rows (note these may not be the same as last inside rows in the case of multiple ED exits)
moves[ED_exit == 1, first_ED_exit := if_else(discharge == min(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]
moves[inside_exit == 1 & shift(outside, 1, NA, "lead"), first_inside_exit := if_else(discharge == min(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]

# add first  non ED location 
first_outside_ED = unique(moves[(first_ED_exit), list(csn, lead_admission)])
setnames(first_outside_ED, "lead_admission", "first_outside_ED_admission")
moves = merge(moves, first_outside_ED, all.x = TRUE)

# add first 'proper' location (ie not obs or same day)
first_outside_proper_ = unique(moves[(first_inside_exit), list(csn, lead_admission, lead_location)])
setnames(first_outside_proper_, "lead_admission", "first_outside_proper_admission")
setnames(first_outside_proper_, "lead_location", "first_outside_proper")
moves = merge(moves, first_outside_proper_, all.x = TRUE)
rm(first_outside_proper_)

# identify first location
moves[, "first_admission" := min(admission), by = csn]
moves[, "first_location" := location[which(admission == first_admission)], by = csn]
moves[, "first_dept" := department[which(admission == first_admission)], by = csn]


# # identify final location
moves[, "final_admission" := max(admission), by = csn]
moves[, "final_location" := location[which(admission == final_admission)], by = csn]
moves[, "final_dept" := department[which(admission == final_admission)], by = csn]




# Assign final classification ---------------------------------------------

direct_adm <- moves[(!visited_same_day) & !visited_obs & visited_outside, unique(csn)]
indirect_adm <- moves[(visited_same_day & !visited_obs & visited_outside) |
                        ((!visited_same_day) & visited_obs & visited_outside) |
                        (visited_same_day & visited_obs & visited_outside), unique(csn)]
indirect_dis <- moves[((!visited_same_day) & visited_obs & !visited_outside) |
                        (visited_same_day & !visited_obs & !visited_outside) |
                        (visited_same_day & visited_obs & !visited_outside), unique(csn)]
direct_dis <- moves[(!visited_same_day) & !visited_obs & !visited_outside, unique(csn)]

summ <- data.table(ED_csn_summ_raw %>% filter(csn %in% moves$csn) %>% 
                     mutate(adm = case_when(csn %in% direct_adm ~ "direct_adm",
                                            csn %in% indirect_adm ~ "indirect_adm",
                                            csn %in% indirect_dis ~ "indirect_dis",
                                            csn %in% direct_dis ~ "direct_dis")))
setkey(summ, csn)
rpt(summ)

# Add relevant transition time to summary table -------------------------------------------

summ <- merge(summ, (unique(moves[,.(csn, first_ED_admission, first_outside_ED_admission, 
                                     first_outside_proper_admission, last_ED_discharge, last_inside_discharge, 
                                     first_outside_proper)])))

# For edge list processing --------

# create final edge list
edgedf <- moves[,.(csn, from = location, to = lead_location, 
                     from_dept = department, to_dept = lead_department, dttm = discharge)]






# Save data ---------------------------------------------------------------

outFile = paste0("EDcrowding/flow-mapping/data-raw/moves_",today(),".rda")
save(moves, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/summ_",today(),".rda")
save(summ, file = outFile)
rm(outFile)


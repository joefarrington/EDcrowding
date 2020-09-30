# About this script
# =================

# The script reads dataset called ED_bed_moves 
# and creates two extra variables (room5 and room6)
# plus an additional waiting row



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# # Load bed_move data
# # ==================

file_label <- "all_"
inFile <- paste0("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_",file_label,"2020-09-30.rda")
load(inFile)

ED_bed_moves_temp <- ED_bed_moves

# temp fix to get rid of rogue rows with only one entry
ED_bed_moves_temp <- ED_bed_moves_temp %>% 
  filter(!(arrival_row & num_ed_rows == 1 & room4 %in% c("DIAGNOSTICS", 'WAITING ROOM')))


# Simplify room and dept names
# ============================

print("Room and dept names")
timer <- Sys.time()


# create room5 (see wiki for more information)

ED_bed_moves_temp <- ED_bed_moves_temp %>% 
  group_by(csn) %>%
  # add a column to contain the discharge time of the next row
  mutate(next_location = lead(room4),
         next_dttm = lead(discharge)) %>% 
  # use the date for the next row as the discharge time when next row is diagnostics or Waiting room
  # discharge new is used in create-edge-list.R
  mutate(discharge_new = case_when(next_location %in% c("DIAGNOSTICS", "WAITING ROOM") ~ next_dttm,
                                   TRUE ~ discharge)) %>% select(-next_location, -next_dttm)

ED_bed_moves_temp <- ED_bed_moves_temp %>% 
  group_by(csn) %>% 
  mutate(room5 = case_when(room4 %in% c("DIAGNOSTICS", "WAITING ROOM") & !arrival_row ~ lag(room4),
                           # the row below has been added for the few cases where an arrival row is DIAGNOSTICS or WAITING ROOM
                           room4 %in% c("DIAGNOSTICS", "WAITING ROOM") & arrival_row ~ lead(room4),
                           TRUE ~ room4))


# create room6 (see wiki for more information)
ED_bed_moves_temp <- ED_bed_moves_temp %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  # udpate OTF to either side location when these are the same
  mutate(room6 = case_when(OTF_row == 1 & ED_row_excl_OTF ==1 & lead(room5) == lag(room5) ~ lead(room5),
                                TRUE ~ room5)) 


# Final data cleaning
# ===================

print(Sys.time() - timer)
print("Final cleaning")
timer <- Sys.time()



# Save data
# =========

# save ED_bed_moves for later use

ED_bed_moves <- ED_bed_moves_temp

outFile = paste0("EDcrowding/data-prep-for-ML/data-raw/ED_bed_moves_",file_label,today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)



# # Add additional waiting row  
# # ==========================
# 
# # alternative version of ED_bed_moves with additional waiting rows
# # This section adds a row where more than a specified length of time
# # which is given by time_until_waiting has passed since arrival
# # row admission, discharge and duration are updated accordingly
# 

print(Sys.time() - timer)
print("Additional waiting row")
timer <- Sys.time()


extra_rows <- tribble(
  ~mrn, ~csn, ~arrival_dttm, ~discharge_dttm, ~admission , ~discharge, ~department,
  ~dept2, ~dept3, ~ hl7_location, ~ room, ~ room2, ~room2a, ~room3, ~room4, ~room5, ~room6, ~room7,
  ~bed, ~arrival_row, ~ED_row, ~OTF_row, ~ED_row_excl_OTF, ~num_ed_rows, ~ED_arrival_dttm,
  ~ED_discharge_dttm, ~ ED_discharge_dttm_excl_OTF, ~ED_discharge_dttm_final, ~admission_row, ~discharge_new)

time_until_waiting <- difftime("2020-01-01 00:10:00", "2020-01-01 00:00:00", units = "hours")

ED_bed_moves_extra <- ED_bed_moves %>% select(-duration_row, -duration_hours, -duration_days, -duration_mins, -duration_secs)

for (i in 1:nrow(ED_bed_moves_extra)) {

  if (i%%5000 == 0) {
    print(paste("Processed",i,"rows"))
  }

  row <- ED_bed_moves_extra[i,]

  if(row$room5 == "Arrived" && difftime(row$discharge, row$admission, units = "hours" ) > time_until_waiting) {

    # create new row for the additional wait time
    row$admission <- ED_bed_moves_extra$admission[i] + time_until_waiting
    row$room2a <- "Waiting"
    row$room3 <- "Waiting"
    row$room4 <- "Waiting"
    row$room5 <- "Waiting"
    row$room6 <- "Waiting"
    row$room7 <- "Waiting"
    extra_rows <- extra_rows %>% add_row(tibble_row(row))

    # update the discharge time to 10 minutes after arrival
    ED_bed_moves_extra$discharge[i] <- ED_bed_moves_extra$admission[i] + time_until_waiting
  }
}

ED_bed_moves_extra <- ED_bed_moves_extra %>% dplyr::union(extra_rows) %>% arrange(mrn, csn, admission)

# save ED_bed_moves_extra for later use

outFile = paste0("EDcrowding/data-prep-for-ML/data-raw/ED_bed_moves_clean_extra_",file_label,today(),".rda")
save(ED_bed_moves_extra, file = outFile)
rm(outFile)



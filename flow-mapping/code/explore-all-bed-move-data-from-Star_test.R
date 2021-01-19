# About this file
# ===============


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)


# Load data ---------------------------------------------------------------

# # raw data with only ED visits
# load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_all_2020-11-30.rda")
# load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_2020-11-30.rda")

# all raw data
# load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-11-30.rda")
load("~/EDcrowding/flow-mapping/data-raw/bed_moves_2020-11-30.rda")



# Look at department -------------------------------------------------


bed_moves_proc <- bed_moves %>% 
  arrange(mrn, admission) %>% 
  group_by(mrn) %>% 
  mutate(next_department = lead(department))


bed_moves_proc <- bed_moves_proc %>% group_by(mrn) %>% 
  mutate(next_csn = lead(csn))


bed_moves_proc <- bed_moves_proc %>% 
  group_by(mrn) %>% 
  mutate(next_admission = lead(admission))

save(bed_moves_proc, file = paste0('EDcrowding/flow-mapping/data-raw/bed_moves_proc_',today(),'.rda'))

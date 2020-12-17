
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
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-12-17.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-17.rda")



edgedf <- data.table(ED_bed_moves %>% 
                       mutate(location = case_when(department == "ED"~ room4, 
                                                   TRUE ~ department)) %>% 
                                select(csn, admission, discharge, location, covid_pathway) %>% 
                       left_join(ED_csn_summ %>% 
                                   select(csn, discharge_time, last_ED_discharge_time)))

# add lead csn, admission and location - see https://rdrr.io/cran/data.table/man/shift.html
cols = c("csn","admission","discharge", "location")
leadcols = paste("lead", cols, sep="_")
edgedf[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]

rpt(edgedf) #173545

# look for rows where lead admission does not equal discharge of current row - 256 csns
# some of these have gaps between the Waiting row and beginning of next row; 
# others have row overlaps
lead_row_mismatch_csn <- unique(edgedf[csn==lead_csn & discharge != lead_admission, csn])

# update to remove these csns
edgedf <- edgedf[!csn %in% lead_row_mismatch_csn]
rpt(edgedf) #173289

# update with next row's discharge time when locations are repeated
edgedf[csn == lead_csn & location == lead_location, discharge := lead_discharge]
rpt(edgedf) #173289

edgedf <- edgedf[csn != lead_csn | location != lead_location]
rpt(edgedf)



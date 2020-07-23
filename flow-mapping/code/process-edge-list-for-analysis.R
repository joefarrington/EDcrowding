# About this script
# ================

# Loads edge list, which contains one row per bed move per patient
# and has a timestamp. 
#
# This summarises the detailed file into a summary of patients moving from
# one node to another. While summarising, it can do the following
# 1. Subset the edge list by date
# 2. Produce either total or daily average statistics about the edges 
# (ie how many admitted and how many breach on each edge)
# 3. Eliminate edges under a minimum weight



# Load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)


# Define functions
# ================



# this function takes an edge list and returns a summary for use in network analysis
# or other analysis of edges

calc_edge_stats <- function(edgelist, from_date, to_date, detail = FALSE, stats = FALSE) {
  if (detail) {
    
    if(stats) { # group by day first
      edgelist_day_stats <- edgelist %>% 
        filter(date(dttm) >= date(from_date), date(dttm) <= date(to_date), from != "Admitted") %>% # note this will truncate overnight encounters
        mutate (edge = paste0(from,"~",to)) %>% 
        group_by(date = date(dttm), from, to, edge) %>% 
        summarise(weight = n(),
                  pct_disc = sum(ED_last_status == "Discharged")/n(),
                  pct_breach = sum(seen4hrs == "Breach")/n())
      
      edgelist_stats <- edgelist_day_stats %>% 
        group_by(from, to, edge) %>% 
        summarise(weight_mean = mean(weight),
                  weight_lQ = quantile(weight, 0.25),
                  weight_uQ = quantile(weight, 0.75),
                  pct_disc_mean = mean(pct_disc, na.rm = TRUE),
                  pct_disc_lQ = quantile(pct_disc, 0.25, na.rm = TRUE),
                  pct_disc_uQ = quantile(pct_disc, 0.75, na.rm = TRUE),
                  pct_breach_mean = mean(pct_breach, na.rm = TRUE),
                  pct_breach_lQ = quantile(pct_breach, 0.25, na.rm = TRUE),
                  pct_breach_uQ = quantile(pct_breach, 0.75, na.rm = TRUE),
                  ) %>% 
        arrange(desc(weight_mean))
        } 
    else { # skip the grouping by day
      edgelist_stats <- edgelist %>% 
        filter(date(dttm) >= date(from_date), date(dttm) <= date(to_date), from != "Admitted") %>% # note this will truncate overnight encounters
        mutate (edge = paste0(from,"~",to)) %>% 
        group_by(from, to, edge) %>% 
        summarise(weight = n(),
                  pct_disc = sum(ED_last_status == "Discharged")/n(),
                  pct_breach = sum(seen4hrs == "Breach")/n()
                  ) %>% 
        arrange(desc(weight))
    }
    return(edgelist_stats)
  }
  else {
    return(
      edgelist %>% 
        filter(date(dttm) >= date(from_date), date(dttm) <= date(to_date), from != "Admitted") %>% # note this will truncate overnight encounters
        mutate (edge = paste0(from,"~",to)) %>% 
        group_by(from, to, edge) %>% 
        summarise(weight = n()) %>% arrange(desc(weight))
    )
  }
}




# Create transition matrix from a given edgelist
create_transition_matrix <- function(edgelist) {
  
  transition <- edgelist %>% 
    group_by(from, to) %>%
    summarise(n = sum(weight)) %>%
    mutate(freq = n / sum(n)) %>% 
    arrange(desc(n)) %>% 
    select(-n) %>%
    pivot_wider(names_from = to, values_from = freq) %>% 
    column_to_rownames("from") %>% 
    replace(is.na(.), 0) 
  
  transition <- transition %>% 
    select(colnames(transition %>% select(-Discharged)), Discharged)
  
  # # check all rows sum to one
  # transition %>%
  #   mutate(check = rowSums(.[1:ncol(transition)]))
  
  # reorder cols by most likely route from Arrival
  # and rows by most lik
  col_order <- order(transition[1,], decreasing = TRUE)
  row_order <- order(transition[,ncol(transition)])
  transition <- transition[row_order,col_order]
  
  return(transition)
}


# function to select only edges with a minimum weight
keep_edges <- function(edgelist_summ, min_weight) {
  # remove minority edges
  keep <- edgelist_summ %>% filter(weight > min_weight)  %>% select(from, to, weight)
  
  # name the edges
  keep <- keep %>% mutate(edge = paste0(from,"~",to)) %>% arrange(from,desc(weight))
}


# Load data
# =========

# load edge list
load("~/EDcrowding/flow-mapping/data-raw/ED_edgelist_full_grouped_2020-07-22.rda")

# load encounter details
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_2020-07-22.rda")



# Create edge summaries
# ====================

# just Jan and Feb rows

edgelist_summ_JanFeb <- calc_edge_stats(edgedf %>% left_join(ED_csn_summ %>% select(csn, ED_last_status, seen4hrs)), 
                                        "2020-01-01", "2020-02-29",  # note - dates are inclusive
                                        detail = TRUE, stats = FALSE)


edgelist_stats_JanFeb <- calc_edge_stats(edgedf %>% left_join(ED_csn_summ %>% select(csn, ED_last_status, seen4hrs)), 
                                         "2020-01-01", "2020-02-29",  # note - dates are inclusive
                                        detail = TRUE, stats = TRUE)

edgelist_summ_JanFeb_breach <- calc_edge_stats(edgedf %>% left_join(ED_csn_summ %>% select(csn, ED_last_status, seen4hrs)) %>% 
                                                 filter(seen4hrs == "Breach"), 
                                               "2020-01-01", "2020-02-29",  # note - dates are inclusive
                                               detail = TRUE, stats = FALSE)


edgelist_stats_JanFeb_breach <- calc_edge_stats(edgedf %>% left_join(ED_csn_summ %>% select(csn, ED_last_status, seen4hrs)) %>% 
                                                 filter(seen4hrs == "Breach"), 
                                               "2020-01-01", "2020-02-29",  # note - dates are inclusive
                                               detail = TRUE, stats = TRUE)


outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_summ_JanFeb_",today(),".csv")
write.csv(edgelist_summ_JanFeb, file = outFile, row.names = FALSE)

outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_stats_JanFeb_",today(),".csv")
write.csv(edgelist_stats_JanFeb, file = outFile, row.names = FALSE)

outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_summ_JanFeb_breach_",today(),".csv")
write.csv(edgelist_summ_JanFeb_breach, file = outFile, row.names = FALSE)

outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_stats_JanFeb_breach_",today(),".csv")
write.csv(edgelist_stats_JanFeb_breach, file = outFile, row.names = FALSE)


# Create transition matrix
# ========================


# all rows in edge list
transition <- create_transition_matrix(edgelist_summ)

outFile <- paste0("EDcrowding/flow-mapping/data-output/transition-matrix-grouped-",today(),".csv")
write.csv2(transition, file = outFile, row.names = TRUE)


# just Jan and Feb
transition_JanFeb <- create_transition_matrix(edgelist_summ_JanFeb)

outFile <- paste0("EDcrowding/flow-mapping/data-output/transition-matrix-grouped-JanFeb-",today(),".csv")
write.csv(transition_JanFeb, file = outFile, row.names = TRUE)



# Clean edge list for network diagram
# ===================================


# look for the right cut point for eliminating edges
ggplot(edgelist_summ_JanFeb, aes(x=1, y = weight)) + 
  geom_boxplot() # wide band at bottom of box plot


ggplot(edgelist_summ_JanFeb %>% filter(weight < 500), aes(x=1, y = weight)) + 
  geom_boxplot() # zooming in suggests 100 is a reasonable cut point


# get reduced edgelist for Jan and Feb
keep <- keep_edges(edgelist_summ_JanFeb, 50)

# write reduced edgelist to file
outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_summ_grouped_JanFeb_",today(),".csv")
write.csv(keep, file = outFile, row.names = FALSE)


# get reduced edgelist for Jan and Feb breachless flows

# get reduced edgelist for Jan and Feb
keep <- keep_edges(edgelist_summ_JanFeb_breachless, 50)

outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_summ-grouped_JanFeb_breachless_",today(),".csv")
write.csv(keep, file = outFile, row.names = FALSE)


# get reduced edgelist for Jan and Feb breach flows

# get reduced edgelist for Jan and Feb
keep <- keep_edges(edgelist_summ_JanFeb_breach, 50)

outFile <- paste0("EDcrowding/flow-mapping/data-output/edgelist_summ-grouped_JanFeb_breach_",today(),".csv")
write.csv(keep, file = outFile, row.names = FALSE)


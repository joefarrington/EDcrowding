# from https://www.data-imaginist.com/2017/ggraph-introduction-layouts/


library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

library(ggraph)
library(igraph)
library(ggalluvial)


# Create functions (copied from elsewhere)
# ===============

get_node <- function(dept, room) {
  if (dept == "Still in ED") {
    node <- room
  }
  else {
    node <- dept
  }
}

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

# ================
# Create arc graph
# ================

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_JanFeb_2020-09-01.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_clean_with_meas_JanFeb_2020-09-01.rda")

long_triage_csn <- ED_bed_moves_with_meas %>% filter(room7 == "TRIAGE", duration_row > hours(2)) %>% select(csn)
ED_bed_moves_long_triage <- ED_bed_moves_with_meas %>% filter(csn %in% long_triage_csn$csn)

# Create edge list for long triage rows

edgedf_long_triage <- tribble(
  ~mrn,
  ~csn,
  ~from,
  ~to,
  ~dttm)

current_csn = ED_bed_moves_long_triage$csn[1]

for (i in (1:nrow(ED_bed_moves_long_triage))) {
  
  if (i%%1000 == 0) {
    print(paste("Processed",i,"rows"))
  }
  
  from_node <- get_node(as.character(ED_bed_moves_long_triage$dept3[i]), as.character(ED_bed_moves_long_triage$room7[i]))
  
  if(i != nrow(ED_bed_moves_long_triage)) {
    
    if (ED_bed_moves_long_triage$csn[i+1] == current_csn) {
      
      to_node <- get_node(as.character(ED_bed_moves_long_triage$dept3[i+1]), as.character(ED_bed_moves_long_triage$room7[i+1]))
      
      if (from_node != to_node) {
        edgedf_long_triage <- edgedf_long_triage %>% add_row(tibble_row(
          mrn = ED_bed_moves_long_triage$mrn[i],
          csn = ED_bed_moves_long_triage$csn[i],
          from = from_node,
          to = to_node,
          dttm = ED_bed_moves_long_triage$discharge_new[i]
        ))
      }
    }
    else {# write last row for current csn - but skip if only one row
      
      if (ED_bed_moves_long_triage$num_ED_rows[i] > 1) {
        edgedf_long_triage <- edgedf_long_triage %>% add_row(tibble_row(
          mrn = ED_bed_moves_long_triage$mrn[i],
          csn = ED_bed_moves_long_triage$csn[i],
          from = to_node,
          to = "Discharged",
          dttm = ED_bed_moves_long_triage$discharge_dttm[i]
        ))
        
      }
      
      current_csn <- ED_bed_moves_long_triage$csn[i+1]
    }
  }
}

edgedf_long_triage <- edgedf_long_triage %>% mutate(
  ignore = case_when(from == "Admitted" & to == "Discharged" ~ 1,
                     TRUE ~ 0)) %>% 
  filter(ignore == 0)


# note - dates are inclusive
from_date <- "2020-01-02"
to_date <- "2020-01-10"

# creates totals for the period
edgelist_summ <- calc_edge_stats(edgedf_long_triage %>% left_join(ED_csn_summ %>% select(csn, ED_last_status, seen4hrs)),
                                 from_date, to_date,
                                 detail = TRUE, stats = FALSE)


# to get node names in right order
node_order = as_tibble(c(
  "Meas pre arrival",
  "Arrived" ,
  "Waiting",
  "WAITING ROOM",
  "Meas post arrival",
  "RAT",
  "RESUS",
  "TRIAGE",
  "TRIAGE Return",  
  "UTC",
  "DIAGNOSTICS",
  "MAJORS",
  "TAF",
  "OTF",
  "Admitted",
  "Discharged"
)) %>% mutate(from_order = row_number(),
              to_order = row_number()) 

# generate graph directly from data frame

edgelist_summ %>% 
  left_join(node_order %>% select(-to_order), by = c("from" = "value")) %>% 
  arrange(from_order) %>% 
  left_join(node_order %>% select(-from_order), by = c("to" = "value")) %>% 
  arrange(from_order, to_order) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "linear") +
  geom_edge_arc(alpha = .25, 
                 aes(width = weight)) +
  geom_node_point(color = "blue", size = 2) + 
  geom_node_text(aes(label = name),  repel = TRUE)+
  labs(title = 'All patients with over 2 hours in triage 2-10 January', 
       subtitle = '',
       legend = "Number of patients") +
  theme(legend.position = "None")


edgelist_summ %>% 
  left_join(node_order %>% select(-to_order), by = c("from" = "value")) %>% 
  arrange(from_order) %>% 
  left_join(node_order %>% select(-from_order), by = c("to" = "value")) %>% 
  arrange(from_order, to_order) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(alpha = .25, aes(width = weight)) + 
  geom_node_point(color = "blue", size = 2) + 
  geom_node_text(aes(label = name),  repel = TRUE)

# =====================
# Create alluvial chart
# =====================


ED_bed_moves_long_triage <- ED_bed_moves_long_triage %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(elapsed_time_from = difftime(admission, arrival_dttm, units = "mins"),
         elapsed_time_to = difftime(discharge, arrival_dttm, units = "mins"))

time_array <- seq(0,240,15)


location <- tribble(
  ~csn,
  ~time_slot,
  ~room7)

for (i in (1:nrow(ED_bed_moves_long_triage))) {
  
  if (i%%1000 == 0) {
    print(paste("Processed",i,"rows"))
  }
  
  for (k in 1:length(time_array)) {
    if (ED_bed_moves_long_triage$elapsed_time_from[i] <= time_array[k] &
        ED_bed_moves_long_triage$elapsed_time_to[i] > time_array[k])
      
      location <- location %>% add_row(tibble_row(
        csn = ED_bed_moves_long_triage$csn[i],
        time_slot = time_array[k],
        room7 =ED_bed_moves_long_triage$room7[i]
      ))
  }
}


df <- long_triage_csn[sample(nrow(long_triage_csn), 100), ] %>% left_join(location)

png("EDCrowding/flow-mapping/media/Alluvial chart showing 100 random patients with long triage.png", width = 1077, height = 659)

df %>% filter(!room7 %in% c( "OTF", "RESUS")) %>% 
  mutate(case_when(room7 == "Meas post arrival" ~ "Arrived + flowsheet",
                   TRUE ~ room7)) %>% 
  ggplot(aes(x = time_slot, stratum = fct_rev(room7), alluvium = mrn,
                    fill = fct_rev(room7), label = room7)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  scale_x_continuous(breaks = time_array) +
  theme(legend.position = "bottom") +
  labs(title = "Sample of 10 patients with more than 2 hours in Triage",
       subtitle = "Location according to Star, with the location updated when a first flowsheet is recorded",
       x = "Minutes elapsed since arrival in ED",
       y = "Number of patients",
       fill = "Location") +
  theme_classic()  + 
  guides(fill = guide_legend(reverse=TRUE), ncol = 1)  +
  theme(legend.position = "bottom")  

dev.off()

# Trying with ED_bed_moves
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_JanFeb_2020-09-01.rda")
ED_bed_moves_long_triage <- ED_bed_moves %>% filter(csn %in% long_triage_csn$csn)
ED_bed_moves_long_triage <- ED_bed_moves_long_triage %>% rename(num_ED_rows = num_ed_rows)

# then run above code


df2 <- df %>% select(csn) %>% left_join(location)

png("EDCrowding/flow-mapping/media/Alluvial chart showing 100 random patients with long triage.png", width = 1077, height = 659)

df2 %>%  
  ggplot(aes(x = time_slot, stratum = fct_rev(room7), alluvium = mrn,
             fill = fct_rev(room7), label = room7)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  scale_x_continuous(breaks = time_array) +
  theme(legend.position = "bottom") +
  labs(title = "Sample of 100 patients with more than 2 hours in Triage",
       subtitle = "Location according to Star, with the location updated when a first flowsheet is recorded",
       x = "Minutes elapsed since arrival in ED",
       y = "Number of patients",
       fill = "Location") +
  theme_classic()  + 
  guides(fill = guide_legend(reverse=TRUE), ncol = 1)  +
  theme(legend.position = "bottom")  

dev.off()

# ================================
# Heatmap for long triage patients
# ================================

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_August_2020-09-03.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_August_2020-09-03.rda") # load different version of bed moves
load("~/EDcrowding/flow-mapping/data-raw/ED_flowsheets_August_2020-09-23.rda")

# remove references to Triage return
ED_bed_moves <- ED_bed_moves %>% mutate(room7 = ifelse(room7 == "TRIAGE Return", "TRIAGE", room7))
ED_bed_moves <- ED_bed_moves %>% mutate(room7 = factor(room7, levels = c("Arrived","TRIAGE","MAJORS","RAT","RESUS","UTC")))

long_triage_csn <- ED_bed_moves %>% filter(room7 == "TRIAGE", duration_row > hours(2)) %>% select(csn)

sample <- long_triage_csn[sample(nrow(long_triage_csn), 50), ] %>%  select(csn)

ED_bed_moves_long_triage <- ED_bed_moves %>% filter(csn %in% sample$csn)
ED_bed_moves_long_triage <- ED_bed_moves_long_triage %>% rename(num_ED_rows = num_ed_rows)

## Time series
ED_bed_moves_long_triage <- ED_bed_moves_long_triage %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(elapsed_time_from = difftime(admission, arrival_dttm, units = "mins"),
         elapsed_time_to = difftime(discharge, arrival_dttm, units = "mins"))

time_array <- seq(0,240,5)


location <- tribble(
  ~mrn,
  ~csn,
  ~time_slot,
  ~room7)

for (i in (1:nrow(ED_bed_moves_long_triage))) {
  
  if (i%%1000 == 0) {
    print(paste("Processed",i,"rows"))
  }
  
  for (k in 1:length(time_array)) {
    if (ED_bed_moves_long_triage$elapsed_time_from[i] <= time_array[k] &
        ED_bed_moves_long_triage$elapsed_time_to[i] > time_array[k])
      
      location <- location %>% add_row(tibble_row(
        mrn = ED_bed_moves_long_triage$mrn[i],
        csn = ED_bed_moves_long_triage$csn[i],
        time_slot = time_array[k],
        room7 =ED_bed_moves_long_triage$room7[i]
      ))
  }
}

df <- sample %>%  select(csn) %>% left_join(location)
df <- df %>% left_join(ED_bed_moves_long_triage %>% ungroup() %>%  select(csn, ED_duration_final, room7, admission, discharge ))


df_flow <- df %>% select(csn, arrival_dttm, discharge_dttm, ED_duration_final) %>% distinct() %>% left_join(ED_flowsheet_raw)
df_flow <- df_flow %>% mutate(time_slot = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  filter(as.numeric(ED_duration_final*60) >= time_slot) %>%  
  select(mrn, csn, time_slot) %>% distinct()

# Heat map wihtout flowsheet measurements


png("EDCrowding/flow-mapping/media/Heat map showing 50 random patients with long triage.png", width = 1077, height = 659)

df %>% filter(as.numeric(ED_duration_final*60) >= time_slot) %>% 
  ggplot(aes(x = time_slot, y = csn, fill = fct_rev(room7))) +
  geom_tile() +
  scale_x_continuous(breaks = seq(0,240,15)) +
  theme(legend.position = "bottom") +
  labs(title = "Location since arrival in ED",
       subtitle = "Sample of 50 randomly selected patients who spent more than 2 hours in triage location",
       x = "Minutes elapsed since arrival in ED",
       y = "Patient csn",
       fill = "Location") +
  theme_classic()  + 
  guides(fill = guide_legend(reverse=TRUE), ncol = 1)  +
  theme(legend.position = "bottom")  

dev.off()


# Heat map with flowsheet measurements

png("EDCrowding/flow-mapping/media/Heat map showing 50 random patients with long triage and flowsheet measurements - August - sample b.png", width = 1077, height = 659)

df %>% filter(as.numeric(ED_duration_final*60) >= time_slot) %>% 
  ggplot(aes(x = time_slot, y = csn, fill = fct_rev(room7))) +
  geom_tile() +
  scale_x_continuous(breaks = seq(0,240,15)) +
  geom_point(aes(x = time_slot, y = csn), data = df_flow %>% filter(time_slot < 240), shape = 8, fill = "black",
             color = "black", size = 2)+
  theme(legend.position = "bottom") +
  labs(title = "Location since arrival in ED with time of flowsheet measurements",
       subtitle = "Sample of 50 randomly selected patients who spent more than 2 hours in triage location; times when measurements were taken are shown as stars",
       x = "Minutes elapsed since arrival in ED",
       y = "Patient csn",
       fill = "Location") +
  theme_classic()  + 
  guides(fill = guide_legend(reverse=TRUE), nrow = 1)  +
  theme(legend.position = "bottom")  +
scale_fill_manual(values =  c("#F8766D" , "#00BA38", "#619CFF", "#F564E3")) +
  geom_vline(xintercept = 10, 
             color = "red", size=1)

dev.off()

# =================================
# Heat map random group of patients
# =================================

sample2 <- ED_bed_moves[sample(nrow(ED_bed_moves), 50), ] %>%  select(csn)

ED_bed_moves_random <- ED_bed_moves %>% filter(csn %in% sample2$csn)
ED_bed_moves_random <- ED_bed_moves_random %>% rename(num_ED_rows = num_ed_rows)

ED_bed_moves_random <- ED_bed_moves_random %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(elapsed_time_from = difftime(admission, arrival_dttm, units = "mins"),
         elapsed_time_to = difftime(discharge, arrival_dttm, units = "mins"))

time_array <- seq(0,240,5)


location2 <- tribble(
  ~mrn,
  ~csn,
  ~time_slot,
  ~room7)

for (i in (1:nrow(ED_bed_moves_random))) {
  
  if (i%%1000 == 0) {
    print(paste("Processed",i,"rows"))
  }
  
  for (k in 1:length(time_array)) {
    if (ED_bed_moves_random$elapsed_time_from[i] <= time_array[k] &
        ED_bed_moves_random$elapsed_time_to[i] > time_array[k])
      
      location2 <- location2 %>% add_row(tibble_row(
        mrn = ED_bed_moves_random$mrn[i],
        csn = ED_bed_moves_random$csn[i],
        time_slot = time_array[k],
        room7 =ED_bed_moves_random$room7[i]
      ))
  }
}

df2 <- sample2 %>%  select(csn) %>% left_join(location2)
df2 <- df2 %>% left_join(ED_bed_moves_random %>% ungroup() %>%  select(csn, ED_duration_final, room7, admission, discharge ))

# order by time spent in arrival to make chart logical in order
chart_order <- ED_bed_moves_random %>% filter(room7 == "Arrived") %>% ungroup() %>% select(csn, duration_row) %>% arrange(duration_row) %>% rownames_to_column() 
df2 <- df2 %>% left_join(chart_order) %>% mutate(chart_order = as.numeric(rowname))

# Adding flowsheet data
df2_flow <- df2 %>% select(csn, arrival_dttm, discharge_dttm, ED_duration_final) %>% distinct() %>% left_join(ED_flowsheet_raw)
df2_flow <- df2_flow %>% mutate(time_slot = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  filter(as.numeric(ED_duration_final*60) >= time_slot) %>%  
  select(mrn, csn, time_slot) %>% distinct()

# Chart without flowsheet measurements
png("EDCrowding/flow-mapping/media/Heat map showing 50 random patients with long triage.png", width = 1077, height = 659)

df2 %>% filter(as.numeric(ED_duration_final*60) >= time_slot) %>% 
  ggplot(aes(x = time_slot, y = reorder(csn, chart_order), fill = fct_rev(room7))) +
  geom_tile() +
  scale_x_continuous(breaks = seq(0,240,15)) +
  theme(legend.position = "bottom") +
  labs(title = "Location since arrival in ED",
       subtitle = "Sample of 50 patients with more than 2 hours in Triage",
       x = "Minutes elapsed since arrival in ED",
       y = "Patient csn",
       fill = "Location") +
  theme_classic()  + 
  guides(fill = guide_legend(reverse=TRUE), ncol = 1)  +
  theme(legend.position = "bottom")  

dev.off()


# Chart with flowsheet measurements


png("EDCrowding/flow-mapping/media/Heat map showing 50 random patients with long triage and flowsheet measurements - August sample b.png", width = 1077, height = 659)

df2 %>% filter(as.numeric(ED_duration_final*60) >= time_slot) %>% 
  ggplot(aes(x = time_slot, y = csn, fill = fct_rev(room7))) +
  geom_tile() +
  scale_x_continuous(breaks = seq(0,240,15)) +
  geom_point(aes(x = time_slot, y = csn), data = df2_flow %>% filter(time_slot < 240), shape = 8, fill = "black",
             color = "black", size = 2)+
  theme(legend.position = "bottom") +
  labs(title = "Location since arrival in ED with time of flowsheet measurements",
       subtitle = "Sample of 50 randomly selected patients; times when measurements were taken are shown as stars",
       x = "Minutes elapsed since arrival in ED",
       y = "Patient csn",
       fill = "Location") +
  theme_classic()  + 
  guides(fill = guide_legend(reverse=TRUE), ncol = 1)  +
  theme(legend.position = "bottom")  +
  scale_fill_manual(values =  c("#F8766D" ,"#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")) +
  geom_vline(xintercept = 10, 
             color = "red", size=1)


dev.off()


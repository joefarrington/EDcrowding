# Reporting back to Sonya and Ken 

# What is happening to Triage
# ===========================

# when are measurements entered? 

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_JanFeb_2020-08-26.rda")
arrived <- ED_bed_moves %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(next_location = lead(room7)) %>% 
  filter(room7 == "Arrived", next_location %in% c("TRIAGE", "RAT")) %>% 
  select(csn, room7, next_location, admission, discharge, duration_row) %>% 
  arrange(csn, admission)

arrived <- arrived %>% ungroup() %>% mutate(id = row_number())

load("~/EDcrowding/flow-mapping/data-raw/ED_flowsheets_JanFeb_2020-08-26.rda")
arrived_flowsheets <- ED_flowsheet_raw %>% 
  select(mrn, csn, flowsheet_datetime) %>% 
  arrange(mrn, csn, flowsheet_datetime) %>% 
  inner_join(arrived)

arrived_flowsheets <-arrived_flowsheets %>% 
  distinct() %>% 
  mutate(flowsheet_while_arrived = case_when(flowsheet_datetime > admission &
                                                        flowsheet_datetime <= discharge ~ 1,
                                                      TRUE ~ 0)) %>% 
  filter(flowsheet_while_arrived > 0) %>% 
  mutate(time_to_next_location = difftime(discharge,flowsheet_datetime, units = "mins"))

# chart showing timing that elapsed after the last flowsheet taken in Arrived state, before moving to next location (TRIAGE or RAT)
arrived_flowsheets  %>% group_by(csn, duration_row, next_location) %>% 
  summarise(num_flowsheets = sum(flowsheet_while_arrived),
            max_time_to_next_location = min(time_to_next_location)) %>% 
  ggplot( aes(x=as.numeric(max_time_to_next_location), color=next_location, fill=next_location)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  facet_grid(next_location ~.) +
  labs(title = "Timing between last flowsheet measurement and next change of location for arrival rows only",
       x = "Minutes after last flowsheet before change of location to RAT or TRIAGE") +
  scale_x_continuous(limits = c(0,180), breaks = seq(0,180,10)) +
  theme_classic() +
  theme(legend.position="none") 


triage <- ED_bed_moves %>% 
  filter(room7 == "TRIAGE")%>% 
  select(csn, room7, admission, discharge, duration_row) %>% 
  arrange(csn, admission)

triage_flowsheets <- ED_flowsheet_raw %>% 
  select(mrn, csn, flowsheet_datetime) %>% 
  arrange(mrn, csn, flowsheet_datetime) %>% 
  inner_join(triage)

triage_flowsheets <- triage_flowsheets %>% 
  mutate(flowsheet_while_triage = case_when(flowsheet_datetime > admission &
                                             flowsheet_datetime <= discharge ~ 1,
                                           TRUE ~ 0)) %>% 
  filter(flowsheet_while_triage > 0) %>% 
  mutate(time_to_next_location = difftime(discharge,flowsheet_datetime, units = "mins"))

# chart showing timing that elapsed after the last flowsheet taken in triage, before moving to next location 
triage_flowsheets  %>% group_by(csn, duration_row) %>% 
  summarise(num_flowsheets = sum(flowsheet_while_triage),
            max_time_to_next_location = min(time_to_next_location)) %>% 
  ggplot( aes(x=as.numeric(max_time_to_next_location))) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  labs(title = "Timing between last flowsheet measurement and next change of location for triage rows only",
       x = "Minutes after last flowsheet before change of location") +
#  scale_x_continuous(limits = c(0,180), breaks = seq(0,180,10)) +
  theme_classic() +
  theme(legend.position="none") 


# Nodes of admission
# ==================

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_NovDec_2020-08-25.rda")
NovDec <- ED_bed_moves_raw

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_JanFeb_2020-08-11.rda")
JanFeb <- ED_bed_moves_raw

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_MarApr_2020-08-03.rda")
MarApr <- ED_bed_moves2

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_MayJunJul_2020-08-03.rda")
MayJul <- ED_bed_moves2

ED_bed_moves <- NovDec %>% 
  dplyr::union(JanFeb) %>% 
  dplyr::union(MarApr) %>% 
  dplyr::union(MayJul) 

ED_bed_moves <- ED_bed_moves %>% arrange(mrn, csn, admission)

admissions <- ED_bed_moves %>% 
  filter(department != "UCH EMERGENCY DEPT") %>% 
  group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(admission_row = case_when(admission == min(admission) ~ 1, 
                                   TRUE ~ 0)) %>% 
  filter(admission_row == 1)
  
depts <- admissions %>% group_by(department) %>% summarise(tot = n())

outFile <- paste0("EDcrowding/flow-mapping/data-output/nodes_of_admission_",today(),".csv")
write.csv(depts, file = outFile, row.names = FALSE)

# Direct entry to UTC or RAT
# ==========================

ED_bed_moves <- ED_bed_moves %>%
  group_by(csn) %>%
  mutate(prev_location = lag(room7),
         prev_prev_location = lag(room7,2)) 

ED_bed_moves %>% filter(room6 == "RAT") %>% 
  group_by(prev_location) %>% summarise(total = n()) %>% arrange(desc(total))
ED_bed_moves %>% filter(room6 == "RAT") %>% 
  group_by(prev_prev_location) %>% summarise(total = n()) %>% arrange(desc(total))
ED_bed_moves %>% filter(room6 == "UTC") %>% 
  group_by(prev_location) %>% summarise(total = n()) %>% arrange(desc(total))

# Looking at waiting
# ==================

ED_bed_moves <- ED_bed_moves_extra
ED_bed_moves %>% filter(ED_row == 1) %>%  group_by(room4) %>% summarise(n())
# looking at where I could cut the arrivals into those that barely wait at all
ED_bed_moves %>% filter(room4 == "Waiting", duration_row < .25) %>% 
  ggplot(aes(x=1, y = duration_row)) + 
  geom_boxplot()

# shows that big chunk are finished Waiting within 10 minutes
# note - have to run this before doing the logic to create the additional rows to see full distribution
ggplot(ED_bed_moves %>% filter(room4 == "Arrived"), 
       aes(x=as.numeric(duration_row)*60)) + geom_histogram(binwidth = 5, colour = "white") +
  scale_x_continuous(n.breaks = 20) + labs(x = "Duration of first row")

ED_bed_moves %>% 
  mutate(lessthan10 = case_when(room4 == "Waiting" & duration_row <=1/6 ~ 1,
                                TRUE ~ 0)) %>%filter(room4 == "Waiting") %>% group_by(lessthan10) %>% summarise(n())

# therefore I have added an additional row to make a post arrival of more than 10 minutes a wait time
ggplot(ED_bed_moves %>% filter(room5 == "Arrived"), 
       aes(x=as.numeric(duration_row)*60)) + geom_histogram(binwidth = 1, colour = "white") 
ggplot(ED_bed_moves %>% filter(room4 == "Waiting"), 
       aes(x=as.numeric(duration_row)*60)) + geom_histogram(binwidth = 5, colour = "white") +
  scale_x_continuous(n.breaks = 20)




# Looking at TRIAGE
# ================
# Is there a different between triage with and without beds
ED_bed_moves %>%  filter(room == 'ADULT TRIAGE') %>% group_by(bed) %>% summarise(n())
# no - almost all triage rows have now room info 

# how much do people bounce in and out of triage
ED_bed_moves %>% group_by(arrival_dttm, csn) %>% filter(room == 'ADULT TRIAGE') %>% summarise(tot_triage = n()) %>% 
  group_by(date(arrival_dttm)) %>% summarise(av_tot_triage = mean(tot_triage)) %>% 
  ggplot(aes(x = `date(arrival_dttm)`, y = av_tot_triage)) + geom_line()


# how much do people bounce in and out of triage
ED_bed_moves %>% group_by(arrival_dttm, csn) %>% filter(room == 'ADULT TRIAGE') %>% summarise(tot_triage = n()) %>% 
  filter(tot_triage > 1) %>% 
  group_by(date(arrival_dttm)) %>% summarise(num_gt1 = n()) %>% 
  ggplot(aes(x = `date(arrival_dttm)`, y = num_gt1)) + geom_line()


# this shows that the number of TRIAGE after another location are quite small (at least for Jan and Feb)

ED_bed_moves <- ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(sum_triage = sum(room5 == "TRIAGE"))

ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(sum_triage = sum(room5 == "TRIAGE")) %>% 
  mutate(room_temp = case_when(sum_triage > 1 & room5 == "TRIAGE" ~ paste0(room5, " after ", lag(room5)),
                                                          TRUE ~ room5)) %>%  filter(ED_row == 1) %>% group_by(room7) %>% summarise(n())


ED_bed_moves <- ED_bed_moves %>% 
  mutate(room_temp = case_when(sum_triage > 1 & room5 == "TRIAGE" ~ paste0(room5, " after ", lag(room5)),
                                          TRUE ~ room5))


# MRNs for Mo and Rebecca
# =======================


# code to select some mrns to check with Mo and Rebecca
a <- ED_bed_moves %>% filter(room6 == "Waiting") %>% arrange(desc(duration_row)) # seen quickly

# long wait times
b <- ED_bed_moves2 %>% filter(csn == "1020007278") %>% 
  select(-arrival_dttm, -discharge_dttm, -hl7_location, -num_ed_rows) %>%
  rename(row_start_time = admission, row_end_time = discharge) %>% 
  mutate(minutes_in_this_location = as.integer(round(difftime(row_end_time, row_start_time, units = "mins"),0)))

a <- ED_bed_moves %>% filter(room6 == "Arrived") %>% arrange(duration_row)


b <- ED_bed_moves2 %>% filter(csn == "1019974386") %>% 
  select(-arrival_dttm, -discharge_dttm, -hl7_location, -num_ed_rows) %>%
  rename(row_start_time = admission, row_end_time = discharge) %>% 
  mutate(minutes_in_this_location = as.integer(round(difftime(row_end_time, row_start_time, units = "mins"),0)))



# where do the Waiting group go next?
ED_bed_moves  %>% 
  group_by(csn) %>%
  # add a column to contain the discharge time of the next row
  mutate(next_location_after_waiting = lead(room4)) %>% filter(room6 == "Waiting") %>% group_by(next_location_after_waiting) %>% summarise(n())

# where do the admission rows come from?
a <- ED_bed_moves  %>% 
  left_join(ED_csn_summ %>% select(csn, ED_discharge_dttm)) %>% 
  group_by(csn) %>%
  mutate(admission_row = if_else(admission == ED_discharge_dttm,TRUE, FALSE)) %>% 
  mutate(prev_location = lag(room4))
b <- a %>% 
  filter(admission_row) %>% group_by(prev_location) %>% summarise(n())


a %>% filter(admission_row) %>% 
  group_by(OTF_row) %>% summarise(n()) %>% 
  mutate(location_before_admission = lead(room4)) %>% filter(OTF_row == 1) %>% group_by(next_location_after_OTF) %>% summarise(n())

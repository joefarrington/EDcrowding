# Reporting back to Sonya and Ken 

# Looking at TRIAGE
# ================
# Is there a different between triage with and without beds
ED_bed_moves %>%  filter(room == 'ADULT TRIAGE') %>% group_by(hl7_location) %>% summarise(n())
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
  mutate(room7 = case_when(sum_triage > 1 & room5 == "TRIAGE" ~ paste0(room5, " after ", lag(room5)),
                                                          TRUE ~ room5)) %>%  filter(ED_row == 1) %>% group_by(room7) %>% summarise(n())


ED_bed_moves <- ED_bed_moves %>% 
  mutate(room7 = case_when(sum_triage > 1 & room5 == "TRIAGE" ~ paste0(room5, " after ", lag(room5)),
                                          TRUE ~ room5))

# Looking at waiting
# ==================

# looking at where I could cut the arrivals into those that barely wait at all
ED_bed_moves %>% filter(room4 == "Waiting", duration_row < .25) %>% 
  ggplot(aes(x=1, y = duration_row)) + 
         geom_boxplot()

# shows that big chunk are finished Waiting within 10 minutes
# note - have to run this before doing the logic to create the additional rows to see full distribution
ggplot(ED_bed_moves %>% filter(room4 == "Arrived"), 
       aes(x=as.numeric(duration_row)*60)) + geom_histogram(binwidth = 5, colour = "white") +
  scale_x_continuous(n.breaks = 20)

ED_bed_moves %>% mutate(lessthan10 = case_when(room4 == "Waiting" & duration_row <=1/6 ~ 1,
                                               TRUE ~ 0)) %>% filter(room4 == "Waiting") %>% group_by(lessthan10) %>% summarise(n())

# therefore I have added an additional row to make a post arrival of more than 10 minutes a wait time
ggplot(ED_bed_moves %>% filter(room5 == "Arrived"), 
       aes(x=as.numeric(duration_row)*60)) + geom_histogram(binwidth = 1, colour = "white") 
ggplot(ED_bed_moves %>% filter(room4 == "Waiting"), 
       aes(x=as.numeric(duration_row)*60)) + geom_histogram(binwidth = 5, colour = "white") +
  scale_x_continuous(n.breaks = 20)


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

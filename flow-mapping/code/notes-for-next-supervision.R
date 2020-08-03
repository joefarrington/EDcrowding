# Reporting back to Sonya and Ken 

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


# this shows that the number of TRIAGE after are quite small (at least for Jan and Feb)

ED_bed_moves <- ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
  mutate(sum_triage = sum(room5 == "TRIAGE"))

ED_bed_moves %>% mutate(room6 = case_when(sum_triage > 1 & room5 == "TRIAGE" ~ paste0(room5, " after ", lag(room5)),
                                                          TRUE ~ room5)) %>%  filter(ED_row == 1) %>% group_by(room6) %>% summarise(n())


ED_bed_moves <- ED_bed_moves %>% mutate(room6 = case_when(sum_triage > 1 & room5 == "TRIAGE" ~ paste0(room5, " after ", lag(room5)),
                                          TRUE ~ room5))


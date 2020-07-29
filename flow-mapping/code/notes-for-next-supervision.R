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

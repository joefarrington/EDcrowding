ED_bed_moves_raw <- ED_bed_moves_raw %>% group_by(csn) %>% mutate(num_ED_rows2 = sum(department == "UCH EMERGENCY DEPT"))

ED_bed_moves_raw %>% 
  filter((department == "UCH EMERGENCY DEPT")) %>% 
  filter(num_ED_rows2 == 1, discharge_dttm != discharge)

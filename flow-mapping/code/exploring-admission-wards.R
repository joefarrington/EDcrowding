

# shorten ward names
clean_wardnames6 <- function(x) {
  # If any ward has a name in brackets use the text in brackets as ward name e.g. "UCH T07 SOUTH (T07S)" becomes T07S
  if (grepl("EMERGENCY DEPT",x)) {
    x <- "ED"
  } 
  else if (grepl("EMERGENCY AU",x)) {
    x <- "EAU"
  }
  else if (grepl("ACUTE MEDICAL",x)) {
    x <- "AMU"
  }
  else if (grepl("CLIN DECISION",x)) {
    x <- "CDU"
  }
  else if (grepl("T07",x)) { 
    x <- "T07"
  }
  else if (grepl("T06",x)) { 
    x <- "T06"
  }
  else if (grepl("T09",x)) { 
    x <- "T09"
  }
  else if (grepl("T08",x)) { 
    x <- "T08"
  }
  else if (grepl("T10",x)) { 
    x <- "T10"
  }
  else if (grepl("P03|T03",x)) {
    x <- "ICU/Theatres"
  }
  else if (grepl("T[0-9][0-9]",x)) {
    x <- "Tower Other"
  }
  # Any NHNN location becomes Outside Tower
  else {
    x <- "Outside Tower"
  }

  return(x)
}

clean_wardnames6 <- Vectorize(clean_wardnames6)


# =========
# Explore data
# ===========

# find all admission rows - first get admissions without an OTF
admission_ward <- ED_bed_moves %>% filter(admission_row, OTF_row != 1)

# then add the rows following an OTF row
admission_ward <- admission_ward %>%  dplyr::union(ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% filter(lag(admission_row), lag(OTF_row == 1)))

# returns 26692 rows

# compare with numbers in summary table
ED_csn_summ %>% group_by(ED_last_status) %>% summarise(n() )

# missing rows - 517 csns
missing_csns <- ED_csn_summ %>% filter(ED_last_status == "Admitted", !csn %in% admission_ward$csn) %>%
  select(csn) %>% left_join(ED_bed_moves %>% select(csn, admission, discharge, admission_row, department, hl7_location, duration_row, OTF_row))

# all of these appear to be admission_row is the last row
missing_csns %>% filter(admission_row, discharge == discharge_dttm)

# and OTF is the last row
missing_csns %>% filter(admission_row, discharge == discharge_dttm, OTF_row == 1)
# so it's fine that they are missing

# 4 seem to be in ED; these have two consecutive OTF rows!
still_in_ED <- admission_ward %>% filter(dept2 == "ED")  %>%
  select(csn) %>% left_join(ED_bed_moves %>% select(csn, admission, discharge, admission_row, department, hl7_location, duration_row, OTF_row))

# only one of these is ultimately admitted
ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% filter(lag(admission_row,2), lag(OTF_row)==1, lag(OTF_row,2)==1)

# therefore change the final calc of admission ward to: 
admission_ward <- ED_bed_moves %>% filter(admission_row, OTF_row != 1)
admission_ward <- admission_ward %>%  dplyr::union(ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
                                                     filter(lag(admission_row), lag(OTF_row == 1), OTF_row !=1)) # this will exclude the four with two OTF rows

admission_ward <- admission_ward %>%  dplyr::union(ED_bed_moves %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
                                                     filter(lag(admission_row,2), lag(OTF_row)==1, lag(OTF_row,2)==1)) # this will pick up the one that is admitted

# final number of admissions = 26,689



# =============
# Explore wards
# =============

# Exploring admission ward
ward_count <- admission_ward %>% group_by(dept_) %>% summarise(n())


ward_count_gp <- admission_ward %>% group_by(department, dept2) %>% summarise(tot = n()) %>% pivot_wider(names_from = dept2, values_from = tot)


admission_ward <- admission_ward %>% mutate(dept_ = clean_wardnames6(department))
ward_count_gp <- admission_ward %>% group_by(department, dept_) %>% summarise(tot = n()) %>% pivot_wider(names_from = dept_, values_from = tot)

png("EDCrowding/flow-mapping/media/Admissions from ED to hospital wards by month.png", width = 1077, height = 659)

admission_ward %>% 
  group_by(dept_, year_month = paste0(year(ED_discharge_dttm_excl_OTF),sprintf("%02d",month(ED_discharge_dttm_excl_OTF)))) %>%
  summarise(tot = n()) %>% 
  filter(!year_month %in% c("201904", "202009")) %>% 
  ggplot(aes(x = year_month, y = tot, fill = fct_rev(dept_))) + geom_bar(stat = "identity") +
  theme_classic() + 
  labs(title = "Admissions originating in UCLH ED by month and admitting ward",
        x = "Year and month",
        y = "Number of admissions",
       fill = "Admitting ward")

dev.off()



# =============
# Explore CDU
# =============

# Number of CDU admissions - 2409

ED_bed_moves %>% filter(department == "UCH T00 CLIN DECISION")  %>% select(csn) %>% n_distinct()

# with bed moves
CDU_visits <- ED_bed_moves %>% filter(department == "UCH T00 CLIN DECISION")  %>%
  select(csn) %>% distinct() %>% left_join(ED_bed_moves %>% select(csn, admission, discharge, admission_row, department, hl7_location, duration_row, OTF_row)) %>% left_join(ED_csn_summ %>% select(csn, ED_last_status))

after_CDU <- CDU_visits %>% group_by(csn) %>% 
   filter(lag(department) == "UCH T00 CLIN DECISION") %>% select(mrn, csn, department, admission, admission_row, discharge, discharge_dttm, ED_last_status)

ended_in_CDU <- CDU_visits %>% select(csn) %>% distinct() %>% left_join(ED_bed_moves) %>% group_by(csn) %>% filter(discharge == max(discharge)) %>% 
  filter(department == "UCH T00 CLIN DECISION")

ended_in_admission <- CDU_visits %>% select(csn) %>% distinct() %>% left_join(ED_bed_moves) %>% group_by(csn) %>% filter(discharge == max(discharge)) %>% 
  filter(department != "UCH T00 CLIN DECISION")

# note - may need to add CDU rows to ED_row
# look at dept2 
# look at processing of mlutiple ED visits 

ED_bed_moves <- ED_bed_moves %>% 
  mutate(OTF_row2 = case_when(room == "OTF" | department == "UCH T00 CLIN DECISION" ~ 1,
                                                            TRUE ~ 0)) 

# just realised these are completely different things - CDU is in ED when it look like it is out; OTF is the opposite

# looking at number of these
ED_bed_moves <- ED_bed_moves %>% group_by(csn) %>% 
  mutate(num_OTF_rows = sum(OTF_row == 1),
         num_OTF2_rows = sum(OTF_row2 == 1)) 

mult_OTF_CDU <- ED_bed_moves %>% group_by(csn) %>% mutate(num_OTF_CDU_rows = sum(OTF_row == 1 )) %>% filter(num_OTF_rows > 1)


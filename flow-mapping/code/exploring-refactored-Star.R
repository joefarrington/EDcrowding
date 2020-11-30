
# new csn
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_all_2020-11-27.rda")


load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_CDUinED_2020-11-16.rda")
ED_csn_summ_CDU <- ED_csn_summ
ED_csn_summ_CDU <- ED_csn_summ_CDU %>% rename(adm_CDU = adm)

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")


# Comparing this with old ED_csn_summ
compare <- ED_csn_summ_raw %>% 
  select(csn, patient_class) %>%  
  left_join(ED_csn_summ %>% select(csn_old, adm), by = c("csn" = "csn_old")) %>% 
  left_join(ED_csn_summ_CDU %>% select(csn_old, adm_CDU), by = c("csn" = "csn_old"))

b = bed_moves %>% inner_join(compare %>% filter(patient_class == "INPATIENT", !adm) %>% select(csn, patient_class))

# experimenting with joint probability distribution

preds_file <- paste0("~/EDcrowding/predict-admission/data-output/preds_",today(),".rda")
load(preds_file)

load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-27.rda")


# pick a random dttm

r = summ[date(presentation_time) == '2020-06-21' ]

# pick a random time to sample
sample_time <- r$presentation_time[20] + hours(15)
summ[presentation_time < sample_time &
       last_ED_discharge > sample_time] # all patients who presented before that time who were still in ED at that time

d = r[presentation_time < sample_time &
           last_ED_discharge > sample_time, .(csn, presentation_time, elapsed = difftime(sample_time, presentation_time, units = "mins"))]

d[, timeslice := case_when(elapsed < 15 ~ "task000",
                           elapsed < 30 ~ "task015",
                           elapsed < 60 ~ "task030",
                           elapsed < 120 ~ "task060",
                           elapsed < 180 ~ "task120",
                           elapsed < 240 ~ "task180",
                           elapsed < 300 ~ "task240",
                           elapsed < 360 ~ "task300",
                           TRUE ~ "task360")]



load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-27.rda")


# To get time to admission for each timeslice ----------------------------

summ[,ED_duration := difftime(last_ED_discharge, presentation_time, units = "mins")]

summ[,timeslice0 := 1]
summ[,timeslice15 := if_else(ED_duration > 15, 1, 0)]
summ[,timeslice30 := if_else(ED_duration > 30, 1, 0)]
summ[,timeslice60 := if_else(ED_duration > 60, 1, 0)]
summ[,timeslice120 := if_else(ED_duration > 120, 1, 0)]
summ[,timeslice180 := if_else(ED_duration > 180, 1, 0)]
summ[,timeslice240 := if_else(ED_duration > 240, 1, 0)]
summ[,timeslice300 := if_else(ED_duration > 300, 1, 0)]
summ[,timeslice360 := if_else(ED_duration > 360, 1, 0)]


summ %>% select(csn, adm, ED_duration, timeslice0:timeslice360) %>% filter(adm %in% c("direct_adm", "indirect_adm"), ED_duration > 0) %>% 
  pivot_longer(timeslice0:timeslice360) %>% mutate(ts = as.numeric(gsub("timeslice", "", name))) %>% 
  filter(value == 1) %>% 
  mutate(tta = ED_duration - ts) %>% 
  group_by(ts) %>% summarise(N = n(), lt4_prop = sum(tta < 4*60)/n()) 

         
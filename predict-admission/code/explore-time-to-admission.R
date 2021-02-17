# experimenting with joint probability distribution

preds_file <- paste0("~/EDcrowding/predict-admission/data-output/preds_",today(),".rda")
load(preds_file)

load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-27.rda")


# pick a random dttm

summ = summ[date(presentation_time) == '2020-06-21' ]

# pick a random time to sample
sample_time <- summ$presentation_time[20] - hours(5)
summ[presentation_time < sample_time &
       last_ED_discharge > sample_time] # all patients who presented before that time who were still in ED at that time

d = summ[presentation_time < sample_time &
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
summ[,timeslice60 := if_else(ED_duration > 60, 1, 0)]
summ[,timeslice120 := if_else(ED_duration > 120, 1, 0)]
summ[,timeslice180 := if_else(ED_duration > 180, 1, 0)]
summ[,timeslice240 := if_else(ED_duration > 240, 1, 0)]
summ[,timeslice300 := if_else(ED_duration > 300, 1, 0)]
summ[,timeslice360 := if_else(ED_duration > 360, 1, 0)]


summ[ED_duration < 24*60] %>% ggplot(aes(x = ED_duration)) + geom_histogram()
summ[,.N, by = list(adm, timeslice)] %>% 
  ggplot(aes(x = as.factor(timeslice), y = N, fill = adm)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  #  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: mutually exclusive timeslices") 


# generate long version of this for chart
summ_timeslice_long = summ[ED_duration < 24*60] %>% select(csn, adm, ED_duration, starts_with("timeslice")) %>% select(-timeslice) %>% 
  pivot_longer(c(timeslice15:timeslice360, timeslice0), names_to = "timeslice", values_to = "value") %>% 
  mutate(timeslice = gsub("timeslice", "", timeslice)) 

summ_timeslice_long = data.table(summ_timeslice_long)

# Number in each timeslice
summ_timeslice_long %>% 
  group_by(adm, timeslice) %>% summarise(N = sum(value)) %>% 
  ggplot(aes(x = factor(timeslice, levels = c(0, 15, 60, 120, 180,  240, 300, 360)), y = N, fill = adm)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  #  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: inclusive timeslices") 



# to get times to admission by timeslice
summ_timeslice_long[, timeslice := as.numeric(timeslice)]
chart_data2 = summ_timeslice_long[adm %in% c("direct_adm", "indirect_adm") & value > 0, list(.N, 
                                                                                             as.numeric(quantile(ED_duration - timeslice, .25)),
                                                                                             as.numeric(median(ED_duration - timeslice)), 
                                                                                             as.numeric(quantile(ED_duration - timeslice, .75))), by = timeslice] 
setnames(chart_data2, "V2", "Q25")
setnames(chart_data2, "V3", "Median")
setnames(chart_data2, "V4", "Q75")

chart_data2 = chart_data2 %>% pivot_longer(N:Q75, names_to = "quartile")

chart_data2 %>% 
  mutate(quartile = factor(quartile, levels = c("Q25", "Median", "Q75"))) %>% 
  filter(quartile != "N") %>% 
  ggplot(aes(x = as.factor(timeslice), y = value, col = quartile, group = quartile)) + geom_point() + geom_line() +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Elapsed time to onward admission \nafter beginning of timeslice", 
       x = "Timeslice",
       title = "Time to admission: inclusive timeslices - admitted patients only") +
  theme(legend.position = "bottom")

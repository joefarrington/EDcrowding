

# Plot a chart with an example patient data -------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
        

# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/dm240_2021-04-14.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-04-13.rda")


load("~/EDcrowding/predict-admission/data-raw/lab_orders_real_2021-04-19.rda")
load("~/EDcrowding/predict-admission/data-raw/obs_real_2021-04-19.rda")


# Find examples with obs and labs -----------------------------------------

examples = dm240[o_has ==1 & p_num_battery > 0 & l_num > 4 & in_set == "Test", csn]



summ[, ED_duration := case_when(is.na(first_outside_proper_admission) ~ as.numeric(difftime(last_ED_discharge, first_ED_admission, units = "mins")),
                           TRUE ~ as.numeric(difftime(first_outside_proper_admission, first_ED_admission, units = "mins")))]

summ[csn %in% examples, .N, by = adm]
summ[csn %in% examples, adm] # to pick examples

# Explore locations -------------------------------------------------------


loc <- moves[csn %in% examples[1:5]] # all admitted
summ[csn %in% loc$csn, .N, by = adm]

loc <- moves[csn %in% examples[c(1,2,3,10,16)]]  # all admitted
summ[csn %in% loc$csn, .N, by = adm]

loc <- moves[csn %in% examples[c(1,2,3,18,26)]]  # 3 discharged
summ[csn %in% loc$csn, .N, by = adm]

final_set = examples[c(1,2,3,18,26)]

loc_lookup = data.table(csn = unique(loc$csn), pat_num = seq(1, length(unique(loc$csn)), 1))
loc <- merge(loc, loc_lookup)

# record of wihch pat_nums these are
# > summ[csn %in% loc$csn, .(adm, csn)]
# adm        csn
# 1:   direct_adm 1016779836
# 2:   direct_adm 1016782955
# 3:   direct_adm 1016992223
# 4: indirect_dis 1025184522
# 5:   direct_dis 1025291688

# loc <- merge(loc, summ[,.(csn, ED_duration)])

# remove rows where admission occurs to locations before arrival at ED
loc <- loc[admission >= first_ED_admission]

loc[, admission_e := as.numeric(difftime(admission, first_ED_admission, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, first_ED_admission, units = "mins"))]


# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)
loc <- loc[discharge <= first_outside_proper_admission | is.na(first_outside_proper_admission)]

loc <- loc[,.(pat_num,  admission_e, discharge_e, location, row_duration)]

p1 = loc[admission_e < 240] %>% 
  mutate(discharge_e = if_else(discharge_e > 240, 240, discharge_e),
         row_duration = discharge_e - admission_e,) %>% 
  mutate(location = factor(location, levels = c("Waiting", "RAT", "RESUS", "MAJORS", "SDEC")))  %>% 
 ggplot(aes(fill = location, xmin = admission_e, xmax = discharge_e, ymin = 0, ymax = 1)) + geom_rect() +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(pat_num~.) +
  facet_grid(pat_num~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Location in ED by elapsed time (in mins) for five example patients",
       # x = "Elapsed time in ED (mins)",
       x = NULL,
       y = NULL,
       fill = "Location") +
  theme(legend.position = "bottom")  
  





# Explore observations ----------------------------------------------------

obs <- obs_real[csn %in% final_set]
obs <- merge(obs, loc_lookup)

# freq_obs = obs[, .N, by = obs_name][N>50]

obs[, e_floor := floor(elapsed_mins/5)*5]
p2 = obs[e_floor <= 240, .N, by = .(pat_num, obs_name, e_floor)] %>% 
  filter(!obs_name %in% c("Bloodpressure_dia", "Painscoreverbalatrest")) %>% 
  mutate(obs_name = case_when(obs_name == "Bloodpressure_sys" ~ "Bloodpressure",
                              obs_name == "Painscoreverbalonmovement" ~ "Painscore",
                              TRUE ~ obs_name)) %>% 
  ggplot(aes(x = e_floor, y = N, fill = obs_name)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(pat_num~.) +
  facet_grid(pat_num~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Observations measured in ED by elapsed time (mins)",
       x = NULL,
       # x = "Elapsed time in ED (mins)",
       fill = "Observation type") +
  theme(legend.position = "bottom")  + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=10))

# 
# # as above but coloured according to whether triage or obs
# p2a = obs[e_floor <= 240, .N, by = .(pat_num, obs_name, e_floor)] %>% 
#   mutate(obs_name = case_when(obs_name %in% c("ACVPU", "NEWS", "GCStotal") ~ "Triage", 
#                               TRUE ~ "Other")) %>% 
#   filter(!obs_name %in% c("Bloodpressure_dia", "Painscoreverbalatrest")) %>% 
#   mutate(obs_name = case_when(obs_name == "Bloodpressure_sys" ~ "Bloodpressure",
#                               obs_name == "Painscoreverbalonmovement" ~ "Painscore",
#                               TRUE ~ obs_name)) %>% 
#   ggplot(aes(x = e_floor, y = N, fill = obs_name)) + geom_bar(stat = "identity") +
#   scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
#   theme_grey(base_size = 16) +
#   facet_grid(pat_num~.) +
#   facet_grid(pat_num~., switch = "y") +
#   theme(strip.text.y.left = element_text(angle = 0)) +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   labs(title = "Observations measured in ED by elapsed time (mins)",
#        x = NULL,
#        # x = "Elapsed time in ED (mins)",
#        fill = "Observation type") +
#   theme(legend.position = "bottom")  + 
#   guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
#   theme(legend.text=element_text(size=10)) +
#   theme(legend.title=element_text(size=10)) + guides(fill = guide_legend(reverse=T))




# Explore labs ------------------------------------------------------------


labs <- lab_orders_real[csn %in% final_set]
labs <- merge(labs, loc_lookup, by = "csn")

labs[, e_floor := floor(elapsed_mins/5)*5]
p3 = labs[e_floor <= 240, .N, by = .(pat_num, battery_code, e_floor)] %>% mutate(N = 1) %>% 
  ggplot() + geom_bar(stat = "identity", aes(x = e_floor, y = N, fill = factor(battery_code))) +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(pat_num~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Lab tests ordered in ED by elapsed time (mins)",
       # x = "Elapsed time in ED (mins)",
       x = NULL,
       fill = "Lab battery code") +
  theme(legend.position = "bottom") + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))

# plot of results

lab_results <- lab_results_real[csn %in% final_set & test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC")]
lab_results <- merge(lab_results, loc_lookup, by = "csn")
# lab_results[!is.na(abnormal_flag), result := paste(test_lab_code, abnormal_flag)]
lab_results[, result := if_else(is.na(abnormal_flag), "In range", "Out of range")]

lab_results[, e_floor := floor(elapsed_mins/5)*5]
p4 = lab_results[e_floor <= 240, .N, by = .(pat_num, test_lab_code, result, e_floor)] %>% mutate(N = 1) %>% 
  ggplot(aes(x = e_floor, y = N, fill = factor(test_lab_code), col = fct_rev(result))) + geom_bar(stat = "identity", alpha = 0.5, size = 2) +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(pat_num~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Lab tests results returned to ED by elapsed time (mins)",
       # x = "Elapsed time in ED (mins)",
       x = NULL,
       fill = "Lab test", 
       col = "Result") +
  theme(legend.position = "bottom") + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))




# Plots -------------------------------------------------------------------


library("gridExtra")
png("~/EDcrowding/predict-admission/media/five-example-patients-inc-2-discharge-loc-obs.png", res = 300, width = 297 , height = 210, units = "mm")
grid.arrange(p1, p2,
             ncol = 1, nrow = 2)

dev.off()

png("~/EDcrowding/predict-admission/media/five-example-patients-inc-2-discharge-labs.png", res = 300, width = 297 , height = 210, units = "mm")
grid.arrange(p3, p4,
             ncol = 1, nrow = 2)

dev.off()



# Plot a chart with an example patient data -------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
        

# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/dm240_2021-03-16.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-16.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-03-16.rda")


load("~/EDcrowding/predict-admission/data-raw/lab_real_2021-03-16.rda")
load("~/EDcrowding/predict-admission/data-raw/obs_real_2021-03-16.rda")


# Find examples with obs and labs -----------------------------------------

examples = dm240[o_has ==1 & p_num_events > 0 & l_num > 4, csn]



summ[, ED_duration := case_when(is.na(first_outside_proper_admission) ~ as.numeric(difftime(last_ED_discharge, first_ED_admission, units = "mins")),
                           TRUE ~ as.numeric(difftime(first_outside_proper_admission, first_ED_admission, units = "mins")))]



# Explore locations -------------------------------------------------------


loc <- moves[csn %in% examples[1:5]] # all admitted
loc <- moves[csn %in% examples[c(1,2,3,10,16)]] # 3 admitted, 3 discharges

# record of wihch csns these are
# > summ[csn %in% loc$csn, .(adm, csn)]
# adm        csn
# 1: direct_adm 1019936246
# 2: direct_adm 1020535843
# 3: direct_adm 1021046926
# 4: direct_dis 1024748888
# 5: direct_dis 1025291688

# loc <- merge(loc, summ[,.(csn, ED_duration)])

# remove rows where admission occurs to locations before arrival at ED
loc <- loc[admission >= first_ED_admission]

loc[, admission_e := as.numeric(difftime(admission, first_ED_admission, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, first_ED_admission, units = "mins"))]


# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)
loc <- loc[discharge <= first_outside_proper_admission | is.na(first_outside_proper_admission)]

loc <- loc[,.(csn,  admission_e, discharge_e, location, row_duration)]

p1 = loc[admission_e < 240] %>% 
  mutate(discharge_e = if_else(discharge_e > 240, 240, discharge_e),
         row_duration = discharge_e - admission_e) %>% 
 ggplot(aes(y = fct_rev(csn), x = row_duration, fill = location)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  labs(title = "Location in ED by elapsed time (in mins) for five example patients",
       # x = "Elapsed time in ED (mins)",
       x = NULL,
       y = NULL,
       fill = "Location") +
  theme(legend.position = "bottom") + guides(fill = guide_legend(reverse=T))
  





# Explore observations ----------------------------------------------------
obs <- obs_real[csn %in% examples[1:5]] # all admitted
obs <- obs_real[csn %in% examples[c(1,2,3,10,16)]]


# freq_obs = obs[, .N, by = obs_name][N>50]

obs[, e_floor := floor(elapsed_mins/5)*5]
p2 = obs[e_floor <= 240, .N, by = .(csn, obs_name, e_floor)] %>% 
  filter(!obs_name %in% c("Bloodpressure_dia", "Painscoreverbalatrest")) %>% 
  mutate(obs_name = case_when(obs_name == "Bloodpressure_sys" ~ "Bloodpressure",
                              obs_name == "Painscoreverbalonmovement" ~ "Painscore",
                              TRUE ~ obs_name)) %>% 
  ggplot(aes(x = e_floor, y = N, fill = obs_name)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(csn~.) +
  facet_grid(csn~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Observations measured in ED by elapsed time (mins)",
       x = NULL,
       # x = "Elapsed time in ED (mins)",
       fill = "Observation type") +
  theme(legend.position = "bottom")  + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=10))


# as above but coloured according to whether triage or obs
p2a = obs[e_floor <= 240, .N, by = .(csn, obs_name, e_floor)] %>% 
  mutate(obs_name = case_when(obs_name %in% c("ACVPU", "NEWS", "GCStotal") ~ "Triage", 
                              TRUE ~ "Other")) %>% 
  filter(!obs_name %in% c("Bloodpressure_dia", "Painscoreverbalatrest")) %>% 
  mutate(obs_name = case_when(obs_name == "Bloodpressure_sys" ~ "Bloodpressure",
                              obs_name == "Painscoreverbalonmovement" ~ "Painscore",
                              TRUE ~ obs_name)) %>% 
  ggplot(aes(x = e_floor, y = N, fill = obs_name)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(csn~.) +
  facet_grid(csn~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Observations measured in ED by elapsed time (mins)",
       x = NULL,
       # x = "Elapsed time in ED (mins)",
       fill = "Observation type") +
  theme(legend.position = "bottom")  + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=10)) + guides(fill = guide_legend(reverse=T))




# Explore labs ------------------------------------------------------------


labs <- lab_real[csn %in% examples[c(1,2,3,10,16)]]


labs[, e_floor := floor(elapsed_mins/5)*5]
p3 = labs[e_floor <= 240, .N, by = .(csn, cluster, e_floor)] %>% mutate(N = 1) %>% 
  ggplot(aes(x = e_floor, y = N, fill = factor(cluster))) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0,60,120,180,240), limits = c(0,240)) +
  theme_grey(base_size = 16) +
  facet_grid(csn~., switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Lab test results received in ED by elapsed time (mins)",
       # x = "Elapsed time in ED (mins)",
       x = NULL,
       fill = "Lab test cluster") +
  theme(legend.position = "bottom") + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))



ex = "1020535843"


# Plots -------------------------------------------------------------------


library("gridExtra")
grid.arrange(p1, p2a, p3,
             ncol = 1, nrow = 3)

png("~/EDcrowding/dissertation/five-example-patients-inc-2-discharge-simple.png", res = 300, width = 297 , height = 210, units = "mm")
dev.off()

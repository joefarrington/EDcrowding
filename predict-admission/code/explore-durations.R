# from https://www.data-imaginist.com/2017/ggraph-introduction-layouts/


library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)



# Create functions 
# ===============

# Save chart
save_chart = function(chart_title, g, width = 1077, height = 659) {
  png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = width, height = height) 
  print(g)
  dev.off()
}


# =========
# Load data
# =========

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-10-14.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-10-14.rda")


# create boxplot of durations
# ===========================

ED_durations <- ED_bed_moves %>% ungroup() %>% 
  filter(ED_row_excl_OTF == 1) %>% 
  select(mrn, csn, room4, epoch, admission, discharge) %>% 
  mutate(loc_duration = difftime(discharge, admission, units = "mins")) %>% 
  group_by(mrn, csn, epoch, room4) %>% 
  summarise(loc_duration = sum(as.numeric(loc_duration))) 


ED_durations_waiting_combined <- ED_bed_moves %>% ungroup() %>% 
  filter(ED_row_excl_OTF == 1) %>% 
  select(mrn, csn, room4, epoch, admission, discharge) %>% 
  mutate(loc_duration = difftime(discharge, admission, units = "mins"),
         room4 = case_when(room4 %in% c("Arrived", "TRIAGE", "WAITING ROOM", "Waiting") ~ "Waiting",
                           TRUE ~ room4)) %>% 
  group_by(mrn, csn, epoch, room4) %>% 
  summarise(loc_duration = sum(as.numeric(loc_duration)))
# %>% 
#   pivot_wider(names_from = room4, names_prefix = "mins_", values_from = loc_duration, values_fill = 0)

# # trying applying a function across all rows - takes too long
# gtzero <- function(x) (x/x)
# ED_durations %>%
#   pivot_wider(names_from = room4, names_prefix = "mins_", values_from = loc_duration, values_fill = 0) %>% 
#   mutate(across(starts_with("mins")), gtzero)


chart_title = "Frequency of use of ED locations (including outliers)"
save_chart(chart_title, 
  ED_durations %>% ungroup() %>% 
    left_join(ED_csn_summ %>% select(csn, adm)) %>% 
    group_by(epoch, adm, room4) %>% 
    summarise(num_uses = sum(loc_duration>0)) %>% 
    ggplot(aes(y = num_uses, x = room4, fill = adm)) + geom_bar(stat = "identity") + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    labs(title = chart_title, x = "Location",
         subtitle = "Arrived = no location information provided in an arrival row\nWaiting = no location information provided in a non-arrival row",
         fill = "Admitted",
         y = "Number of encounters") +
    facet_wrap(epoch~., ncol = 3, scales = "free_y")
)

quant_95 <- ED_durations %>% ungroup() %>% 
  group_by(room4) %>% 
  summarise(quant_95 = quantile(loc_duration, 0.95, na.rm = TRUE))


chart_title = "Boxplots of duration in ED locations (outliers capped at 95th centile)"
save_chart(chart_title, 
             
  ED_durations %>%  ungroup() %>% 
    left_join(ED_csn_summ %>% select(csn, adm)) %>%
    left_join(quant_95) %>% 
    mutate(loc_duration = case_when(loc_duration > quant_95 ~ quant_95,
                                    TRUE ~ loc_duration)) %>% 
    ggplot(aes(y = loc_duration/60, x = room4, fill = adm, col = adm)) + geom_boxplot(alpha = 0.4) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    labs(y = "Time in location (hours)", color = NULL, fill = NULL, x = "Admitted",
         subtitle = "Arrived = no location information provided in an arrival row\nWaiting = no location information provided in a non-arrival row",
         title = chart_title)+
    theme(legend.position = "bottom") +
    facet_wrap(~epoch)
)  

chart_title = "Boxplots of duration in ED locations (excluding TAF, outliers capped at 95th centile)"
save_chart(chart_title, 
           
           ED_durations %>%  ungroup() %>% 
              filter(room4 != "TAF") %>% 
              left_join(ED_csn_summ %>% select(csn, adm)) %>%
              left_join(quant_95) %>% 
              mutate(loc_duration = case_when(loc_duration > quant_95 ~ quant_95,
                                              TRUE ~ loc_duration)) %>% 
              ggplot(aes(y = loc_duration/60, x = room4, fill = adm, col = adm)) + geom_boxplot(alpha = 0.4) + 
              theme_classic() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
              labs(y = "Time in location (hours)", color = NULL, fill = NULL, x = "Admitted",
                   subtitle = "Arrived = no location information provided in an arrival row\nWaiting = no location information provided in a non-arrival row",
                   title = chart_title)+
              theme(legend.position = "bottom") +
              facet_wrap(~epoch)

)


quant_95_waiting_combined <- ED_durations_waiting_combined %>% ungroup() %>% 
  group_by(room4) %>% 
  summarise(quant_95 = quantile(loc_duration, 0.95, na.rm = TRUE))

chart_title = "Boxplots of duration in ED locations (Waiting areas combined, outliers capped at 95th centile)"
save_chart(chart_title, 
           
           ED_durations_waiting_combined %>%  ungroup() %>% 
             filter(room4 != "TAF") %>% 
             left_join(ED_csn_summ %>% select(csn, adm)) %>%
             left_join(quant_95_waiting_combined) %>% 
             mutate(loc_duration = case_when(loc_duration > quant_95 ~ quant_95,
                                             TRUE ~ loc_duration)) %>% 
             ggplot(aes(y = loc_duration/60, x = room4, fill = adm, col = adm)) + geom_boxplot(alpha = 0.4) + 
             theme_classic() +
             theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
             labs(y = "Time in location (hours)", color = NULL, fill = NULL, x = "Admitted",
                  subtitle = "Waiting combines Arrived, WAITING ROOM, Waiting and TRIAGE",
                  title = chart_title)+
             theme(legend.position = "bottom") +
             facet_wrap(~epoch)
           
)

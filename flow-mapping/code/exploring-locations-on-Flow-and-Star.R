

# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# Looking at locations


# By week -----------------------------------------------------------------


load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_CDUinED_2020-11-16.rda")

ED_bed_moves <- ED_bed_moves %>% filter(ED_row ==1) %>% 
  mutate(room3 = case_when(room3 == "MAJ3" ~ "MAJORS",
                           room3 == "RAT CHAIR" ~ "RAT",
                           room3 == "MAJ" ~ "MAJORS",
                           room3 %in% c("Arrived", "WAITING", "WAITING ROOM", "Waiting") ~ "Waiting",
                           room3 == "UCH ED SPECIALTY ASSESSMENT AREA" ~ "SAA",
                           TRUE ~ room3))


ED_bed_moves <- ED_bed_moves %>% 
  mutate(week = week(arrival_dttm)) %>% 
  mutate(week_ = case_when(nchar(week) == 1 ~ paste0(0,week),
                           TRUE ~ as.character(week)))

ED_bed_moves <- ED_bed_moves %>% 
  mutate(year_week = paste0(year(arrival_dttm),week_))

ED_bed_moves <- ED_bed_moves %>% 
  mutate(year_week = case_when(year_week == "201953" ~ "202001",
                               TRUE ~ year_week))

ED_bed_moves <- ED_bed_moves %>% 
  mutate(room3_ = factor(room3, levels = 
                                                    c(
                                                      "ADULT TRIAGE",
                                                      "ED TRIAGE",
                                                      "Waiting",
                                                      "CDU",
                                                      "COVID MAJORS",
                                                      "NON COVID MAJORS",
                                                      "MAJORS",
                                                      "OTF",
                                                      "RAT",
                                                      "RAT CHAIR",
                                                      "RAT COVID MAJORS",
                                                      "RESUS",
                                                      "TAF",
                                                      "SAA",
                                                      "UTC",
                                                      "COVID UTC",
                                                      "ED NON COVID UTC",
                                                      "NON COVID UTC",
                                                      "UTC OPTHALMOLOGY ROOM",
                                                      "UTC PLASTER ROOM",
                                                      "UTC POOL",
                                                      "UTC TREATMENT ROOM",
                                                      "UTCCB",
                                                      "DIAGNOSTICS"
                                                      
                                                    )))



room_summ_ <- ED_bed_moves %>%  filter(ED_row ==1) %>% group_by(room3_, year_week) %>% summarise(tot = n())

png("EDcrowding/flow-mapping/media/Use of locations by week 2.png", width = 1200, height = 1200)

room_summ_ %>% 
  filter(! room3_ %in% c("CDU", "ED TRIAGE", "UTC OPTHALMOLOGY ROOM", "UTC TREATMENT ROOM")) %>% 
  ggplot(aes(x = year_week, y = tot)) + geom_bar(stat = "identity") +   theme_classic() +theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(room3_~., switch = "y")+
  theme(strip.text.y.left = element_text(angle = 0)) +

  labs(title = "Use of ED locations by week since beginning of Epic", x = "week", y = "Total patient numbers") +
  theme(        axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
        ) 
  

dev.off()



# By month ----------------------------------------------------------------




room_summ <- ED_bed_moves %>% filter(ED_row ==1) %>% 
  mutate(year_month = paste0(year(arrival_dttm), month(arrival_dttm))) %>% 
  group_by(room3_, year_month) %>% summarise(tot = n()) 

room_summ <- room_summ %>% mutate(year_month = factor(year_month, levels = 
                                           c(  "20194",
                                               "20195",
                                               "20196",
                                               "20197",
                                               "20198",
                                               "20199",
                                               "201910",
                                               "201911",
                                               "201912",
                                               "20201",
                                               "20202",
                                               "20203",
                                               "20204",
                                               "20205",
                                               "20206",
                                               "20207",
                                               "20208",
                                               "20209",
                                               "202010"
                                           ), labels = 
                                           c(  "201904",
                                               "201905",
                                               "201906",
                                               "201907",
                                               "201908",
                                               "201909",
                                               "201910",
                                               "201911",
                                               "201912",
                                               "202001",
                                               "202002",
                                               "202003",
                                               "202004",
                                               "202005",
                                               "202006",
                                               "202007",
                                               "202008",
                                               "202009",
                                               "202010"
                                           )))


png("EDcrowding/flow-mapping/media/Use of locations by month.png", width = 1200, height = 1200)


room_summ %>% ggplot(aes(x = year_month, y = tot)) + geom_bar(stat = "identity") +  theme_classic() +theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(room3_~., switch = "y")+
  theme(strip.text.y.left = element_text(angle = 0)) +
  
  labs(title = "Use of ED locations by month since beginning of Epic", x = "Month", y = "Total patient numbers") +
  theme(        axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
  ) 
dev.off()

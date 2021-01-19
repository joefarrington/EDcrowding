


# Libraries --------------------------------------------------------------


library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create function  --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}




# Data --------------------------------------------------------------------


load("~/EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_2020-12-07.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-12.rda")


# flowsheet data
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-11-05.rda")
flowsheet_real <- flowsheet_real %>% inner_join(final_csns)

# lab data
load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-11-05.rda")
lab_real <- lab_real %>% inner_join(final_csns)



# Look at timing of flowsheet data ----------------------------------------

summ = data.table(ED_csn_summ)
m = unique(moves[, .(csn, last_ED_discharge)])
summ = summ[m, on = "csn"]


fs = data.table(flowsheet_real %>% select(csn, flowsheet_datetime) %>% distinct())
lab = data.table(lab_real %>% select(csn, result_datetime) %>% distinct())

setkey(fs, csn)
setkey(lab, csn)

fs = merge(fs, summ[,.(csn, presentation_time, last_ED_discharge)], all.x = TRUE)
lab = merge(lab, summ[,.(csn, presentation_time, last_ED_discharge)], all.x = TRUE)

fs[, elapsed := round(as.numeric(difftime(flowsheet_datetime, presentation_time, units = "mins")),0)]
lab[, elapsed := round(as.numeric(difftime(result_datetime, presentation_time, units = "mins")),0)]

fs[, in_ED := if_else(flowsheet_datetime < last_ED_discharge, TRUE, FALSE)]
lab[, in_ED := if_else(result_datetime < last_ED_discharge, TRUE, FALSE)]

fs[, type := "flowsheet"]
lab[, type := "lab"]

both = rbind(fs[(in_ED),.(csn, elapsed, type)], lab[(in_ED),.(csn, elapsed, type)])

both[elapsed < 360 & elapsed > 0, .N, by = list(type,elapsed)] %>% ggplot(aes(x = elapsed, y = N, col = type)) + geom_line() +
  scale_x_continuous(breaks = seq(0,360, 15)) + 
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Frequency of times when lab and flowsheet results recorded in ED", 
       x = "Minutes after presentation time")
  
  
  


                                  
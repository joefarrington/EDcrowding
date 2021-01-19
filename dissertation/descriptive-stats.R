


# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)

library(summarytools)
library(mStats) # ctable, tabodds, mhor


# Load data ---------------------------------------------------------------

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_all_2020-11-27.rda")

# Table 1 Barrak and Cohen - means, range and CI --------------------------

# age
descr(ED_csn_summ_raw$age)
t.test(ED_csn_summ_raw$age)
summary(ED_csn_summ_raw$age, na.rm = TRUE)

# sex
table(ED_csn_summ_raw$sex)
prop.table(table(ED_csn_summ_raw$sex))

# primary outcome (admitted/discharged)
table(ED_csn_summ_raw$patient_class)
prop.table(table(ED_csn_summ_raw$patient_class))  # currently lower than I would expect 



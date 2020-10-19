


library(dplyr)
library(lubridate)
library(tidyverse)


load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_NA_2020-10-07.rda")


# replace NAs with zero
lab_num_results_with_zero <- lab_num_results_with_zero %>%
  mutate_at(vars(colnames(lab_num_results_with_zero)[4:ncol(lab_num_results_with_zero)]), replace_na, 0)



# save
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_",today(),".rda")
save(lab_num_results_with_zero, file = outFile)

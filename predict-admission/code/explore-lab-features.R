
# About this file ---------------------------------------------------------

# Explores lab data


# Load libraries ----------------------------------------------------------


library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(readr)



# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_real_2021-03-01.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-01.rda")


# Explore -----------------------------------------------------------------

summ[,ED_duration := case_when(is.na(first_outside_proper_admission) ~ difftime(last_ED_discharge, first_ED_admission, units = "mins"),
                               TRUE ~ difftime(first_outside_proper_admission, first_ED_admission, units = "mins"))]

lab_real <- merge(lab_real, summ[, .(csn, adm, ED_duration)])
lab_real[, adm := case_when(adm %in% c("direct_adm", "indirect_adm") ~ 1,
                            TRUE ~ 0)]



lab_real <- lab_real[elapsed_mins >= -120 & elapsed_mins < ED_duration]

test_count <- lab_real[, .N, by = .(test_lab_code, adm)] %>% pivot_wider(names_from = adm, values_from = N)

lab_real

test_count = lab_real[!is.na(oor_high), .(count = .N, 
                                          prop_oor_h = sum(oor_high)/.N,
                                          prop_oor_l = sum(oor_low)/.N), by = .(test_lab_code, adm)]

test_count[, prop_in_range := 1 - prop_oor_h - prop_oor_l ]


# how many are in or out of range? 
test_count[count > 1000] %>% pivot_longer(prop_oor_h:prop_in_range) %>%  ggplot(aes(x = factor(adm), y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "fill") + facet_wrap(test_lab_code~ .)


# difference betwween admitted and not
lab_real[!is.na(oor_high) & !is.na(oor_low), .(count = .N, 
                             prop_oor_h = sum(oor_high)/.N,
                             prop_oor_l = sum(oor_low)/.N), by = .(adm)]

# how many per can
count_by_csn = lab_real[, .N, by = .(test_lab_code, csn)]

# time to get labs back
lab_real[elapsed_mins < 400, min(elapsed_mins), by = csn] %>% ggplot(aes(x = V1)) + geom_histogram(bins = 30)





# looking at labs with feature importance
load("~/EDcrowding/predict-admission/data-output/imps_2021-03-08.rda")

imps = imps[grep("^p_", feature)]
imps = imps[,test_lab_code := gsub("^p_num_", "", feature)]
imps = imps[!timeslice %in% c("task000", "task015", "task030")]

imps[, .(importance = mean(importance)), by = test_lab_code]

imp_test_count = merge(test_count, imps[, .(importance = mean(importance)), by = test_lab_code], by = "test_lab_code")
#imp_test_count = merge(test_count, imps[, .(importance, test_lab_code, timeslice), by = test_lab_code], by = "test_lab_code")


# plot of lab tests with feature importances > 0.001
imp_test_count[importance > 	0.001] %>% pivot_longer(prop_oor_h:prop_in_range) %>%  ggplot(aes(x = factor(adm), y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "fill") + facet_wrap(test_lab_code~ .)


lab_real[test_lab_code == "TDDI" & oor_high]


# Which lab results co-occur - creating adjacency matrix ----------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_real_2021-03-01.rda")

# summarise by lab test code to get raw numbers
l1 = lab_real[, .N, by = test_lab_code]
write.csv(l1, "lab_test_nums.csv")

# count number of tests returned within each timestamp
l2 = lab_real[, .N, by = .(csn, result_last_modified_time, test_lab_code)]

# pivot to wide data frame to get all tests within each timestamp
l3 = l2 %>% pivot_wider(names_from = test_lab_code, values_from = N)
# l4 = l3 %>% select(-csn, - result_last_modified_time)
# l4 = unique(l4)
# l4 = data.table(l4)
# l4[, row_id := seq_len(nrow(l4))]

# create an edgelist by counting the number of times each lab test co-occurs
codes = unique(l2$test_lab_code)

edgelist = data.table()

for (i in 1:length(codes)) {
  print(i)
  from = codes[i]
  for (j in 1:length(codes)) {
    to = codes[j]
    vector_from = get(from, l3)
    vector_to = get(to, l3)
    weight = sum(vector_to * vector_from, na.rm = TRUE)
    edge = data.table(from = from, to = to, weight = weight)
    edgelist = bind_rows(edgelist, edge)
  }
}

save(edgelist, file = "~/EDcrowding/predict-admission/data-raw/lab_test_edgelist.rda")

lab_test_adj = edgelist %>% pivot_wider(names_from = to, values_from = weight)


# Network analysis --------------------------------------------------------

library(igraph)

# Create an igraph network object from the two-mode matrix: 
net2 <- graph_from_incidence_matrix(links2)



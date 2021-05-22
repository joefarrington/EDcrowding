# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(Metrics)



# Create functions --------------------------------------------------------

rsq <- function (x, y) cor(x, y) ^ 2

# Load data ---------------------------------------------------------------

tsk_ids = "val"
model_features = "alop"
use_dataset = "Pre"

prob_dist_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features, "_", use_dataset, "_", 
                        tsk_ids, "_", Sys.Date(),".rda")
load(prob_dist_file)

pe_Pre = prob_dist[[2]]
pe_Pre[, dataset := use_dataset]


use_dataset = "Post"

prob_dist_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features, "_", use_dataset, "_", 
                        tsk_ids, "_", Sys.Date(),".rda")
load(prob_dist_file)

pe_Post = prob_dist[[2]]
pe_Post[, dataset := use_dataset]

# later add other results
pe_all = bind_rows(pe_Pre, pe_Post)




# Evaluate point estimates ------------------------------------------------


pe_results = data.table()

for (dataset_ in unique(pe_all$dataset)) {
  
  for (dist_ in unique(pe_all$dist)) {
    
    pe_subset = pe_all[dataset == dataset_ & dist == dist_]
    
    res_ = data.table(dataset = dataset_,
                      dist = dist_,
                      rmse = rmse(pe_subset$truth, pe_subset$expected_value),
                      mae = mae(pe_subset$truth, pe_subset$expected_value),
                      rsq = rsq(pe_subset$truth, pe_subset$expected_value))
    
    pe_results = bind_rows(pe_results, res_)
  }
  
}



# Plot results ------------------------------------------------------------

  

pe_Pre[grep("Inc", dist)] %>% 
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = truth, col = "truth")) +
  geom_point(aes(y = expected_value, col = "expected value")) +
  geom_line(aes(y = truth, col = "truth")) +
  geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = quantile95, ymin = quantile5), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "days") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  facet_grid(.~dist)


# Compare with actual -----------------------------------------------------

before_covid_num_in_location_2021_04_21 <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/before_covid_num_in_location_2021-05-22.csv")
before_val = data.table(before_covid_num_in_location_2021_04_21 %>% select(DateTime, adm))

# checking again - saved just the validation dates for pre covid using data-prep-for-ML
load("~/EDcrowding/data-prep-for-ML/data-output/temp_file_to_check_adm_pre_covid.rda")
before_val[date(DateTime) == "2019-11-19"]

# to get same time window as Joe is using, need to use lead(adm) in the row beloe
# this is because in that file, the data at 2019-11-19 06:00:00 are for the hour leading up to that time
setDT(before_val)[, adm_in_4 := frollsum(lead(adm), 4, align = "left")]
setDT(before_val)[, adm_in_8 := frollsum(lead(adm), 8, align = "left")]

pe_Pre_ = merge(pe_Pre, before_val[, .(time_of_report = DateTime, true_adm_in_4 = adm_in_4, true_adm_in_8  = adm_in_8)], by = "time_of_report", all.x = TRUE)
pe_Pre_[dist == unique(pe_Pre$dist)[4]] %>% ggplot(aes(x = time_of_report)) +
  geom_line(aes(y = truth, col = "from this script")) +
  geom_line(aes(y = true_adm_in_4, col = "from file for Joe")) +
  geom_point(aes(y = truth, col = "from this script")) +
  geom_point(aes(y = true_adm_in_4, col = "from file for Joe")) +
  scale_x_datetime(breaks = "days") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 


# one row mismatches
pe_Pre_[dist == unique(pe_Pre$dist)[4] & true_adm_in_4 != truth]

pe_subset = pe_Pre_[dataset == dataset_ & dist == "Inc not yet arrived; NGBoost poisson: 4 hours"]
rmse(pe_subset$truth, pe_subset$expected_value)
rmse(pe_subset$true_adm_in_4, pe_subset$expected_value)
rsq(pe_subset$truth, pe_subset$expected_value)
rsq(pe_subset$true_adm_in_4, pe_subset$expected_value)

pe_subset = pe_Pre_[dataset == dataset_ & dist == "Inc not yet arrived; NGBoost poisson: 8 hours"]
rmse(pe_subset$truth, pe_subset$expected_value)
rmse(pe_subset$true_adm_in_8, pe_subset$expected_value)
rsq(pe_subset$truth, pe_subset$expected_value)
rsq(pe_subset$true_adm_in_8, pe_subset$expected_value)

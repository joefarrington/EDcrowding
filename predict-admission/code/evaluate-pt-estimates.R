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
model_features = c("alop", "aop")
use_dataset = c("Pre", "Post")
model_output_date = '2021-05-25'

pe_all = data.table()

for (model_features_ in model_features) {
  for (dataset_ in use_dataset) {
    
    prob_distributionfile = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features_, "_", dataset_, "_", 
                            tsk_ids, "_", model_output_date,".rda")
    load(prob_distributionfile)
    
    pe = prob_dist[[2]]
    
    pe[, dataset := dataset_]
    pe[, model_features := model_features_]
    
    pe_all = bind_rows(pe_all, pe)
  }
}

# create aggregate distribution just for summing the results
pe_all[, distribution := case_when((is.na(time_window) & !inc_nya) ~ paste("In ED only"),
                            !inc_nya ~ paste("In ED only"),
                              TRUE ~ paste(dist))]


# Evaluate point estimates ------------------------------------------------


pe_results = data.table()

for (dataset_ in use_dataset)  {
  for (model_features_ in model_features) {
    for (distribution_ in unique(pe_all$distribution)) {
      
      pe_subset = pe_all[model_features == model_features_ & 
                           dataset == dataset_ &
                           distribution == distribution_]
      
      res_ = data.table(dataset = dataset_,
                        model_features = model_features_,
                        dist = unique(pe_subset$dist),
                        distribution = distribution_,
                        time_window = unique(pe_subset$time_window), 
                        rmse = rmse(pe_subset$truth, pe_subset$expected_value),
                        mae = mae(pe_subset$truth, pe_subset$expected_value),
                        rsq = rsq(pe_subset$truth, pe_subset$expected_value))
      
      pe_results = bind_rows(pe_results, res_)
    }
  }
}

pe_results[, dist := as.factor(dist)]
pe_results[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]
pe_results[, inc_true := grepl("true", distribution)]
pe_results[, time_window := ifelse(is.na(time_window), "none", as.character(time_window))]
setorderv(pe_results, c("inc_true", "model_features", "dataset", "time_window", "dist"))



pe_wide = pe_results %>% select(-distribution) %>% filter(!inc_true) %>% pivot_wider(names_from = time_window, values_from = rmse:rsq) %>% select(-inc_true)
pe_wide = pe_wide %>% select(model_features, dataset,  dist, rmse_4, mae_4, rsq_4, rmse_8, mae_8, rsq_8, everything())

outFile = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_point_estimates_", 
                 tsk_ids, "_", Sys.Date(),".csv")
write.csv(pe_wide, file = outFile, row.names = FALSE)


# Plot results ------------------------------------------------------------

  
pe_all[, inc_true := grepl("true", distribution)]
pe_all[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]


pe_all[(inc_nya & !inc_true & !(grepl("Boost", distribution))  & model_features == "alop")] %>% 
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = truth, col = "truth")) +
  geom_point(aes(y = expected_value, col = "expected value")) +
  geom_line(aes(y = truth, col = "truth")) +
  geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = quantile95, ymin = quantile5), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "days") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  # facet_wrap(time_window~dataset, nrow = 2, scales = "free") +
  facet_grid(time_window~dataset, scales = "free") +
  labs(title = "Plots of expected values versus actual numbers admitted by time window and dataset",
       subtitle = "Using empirical poisson for not yet admitted", 
       x = "Validation set date", y = "Number admitted", legend = NULL) +
  theme(legend.position = "bottom") + theme(legend.title = element_blank())
# +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())

pe_all[(inc_nya & time_window == 4 & dataset == "Post")] %>% 
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = truth, col = "truth")) +
  geom_point(aes(y = expected_value, col = "expected value")) +
  geom_line(aes(y = truth, col = "truth")) +
  geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = quantile95, ymin = quantile5), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "days") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  facet_wrap(.~dist, nrow = 4) +
  labs(title = "Post Covid validation set") +
  theme(legend.position = "bottom")



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

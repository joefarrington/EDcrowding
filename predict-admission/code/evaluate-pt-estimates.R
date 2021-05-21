# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(Metrics)



# Create functions --------------------------------------------------------

rsq <- function (x, y) cor(x, y) ^ 2

# Load data ---------------------------------------------------------------

model_features = "alop"
use_dataset = "Pre"

prob_dist_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features, "_", use_dataset, "_", 
                        tsk_ids, "_", Sys.Date(),".rda")

pe_Pre = prob_dist[[2]]
pe_Pre[, dataset := use_dataset]


use_dataset = "Post"

prob_dist_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features, "_", use_dataset, "_", 
                        tsk_ids, "_", Sys.Date(),".rda")

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






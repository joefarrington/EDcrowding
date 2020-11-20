# About this file ---------------------------------------------------------

# here I was tyring to understand why I can't get the sequence of recipe + fomula to workflow to succeed
# currently I preprocess the data and then use this to fit on
# this is OK until I want to run the same pre-processing on the validation set
# I'm suspicious because I got amazing results on the validation set


# Create functions --------------------------------------------------------

save_chart = function(chart_title, g, width = 1077, height = 659) {
  png(paste0("EDcrowding/predict-admission/model-output/", chart_title, ".png"), width = width, height = height) 
  print(g)
  dev.off()
}

# Load libraries ----------------------------------------------------------

library(tidymodels)
library(dplyr)
library(lubridate)
library(xgboost)
library(vip)


# Set parallel processing -------------------------------------------------

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores - 2)

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-09.rda")


dm <- matrix_60 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  select(-mrn, -csn_old, -ED_duration_final, -epoch, -csn
  ) %>% 
  select(adm, age, sex, gt70, everything())


# Train test split --------------------------------------------------------

set.seed(123)
dm_split_train_test <- initial_split(dm, strata = adm, prop = 4/5)
dm_train <- training(dm_split_train_test)

# dm_test <- testing(dm_split)

# save 10% of rhe original dataset (7/8 of training set) as validation set 
dm_split_train_val <- initial_split(dm_train, strata = adm, prop = 7/8)
dm_train_train <- training(dm_split_train_val)
dm_train_val <- testing(dm_split_train_val)


# Set up formula ----------------------------------------------------------

arrchars = colnames(dm %>% select(hour_of_arrival:num_ED_rows))
locdurations = colnames(dm)[grep("^mins_", colnames(dm))]
locvisited = colnames(dm)[grep("^visited_", colnames(dm))]
demog = c('age','sex')
counts = colnames(dm %>% select(num_fs_results:l_num_PROT))
# flow_values = colnames(dm %>% select(fs_min_bp_sys:fs_latest_resp_rate))
# lab_values = colnames(dm %>% select(l_min_BA:l_latest_APTM))

var_arrchars <- paste('+',paste0(arrchars,collapse='+'),sep='')
var_locations <- paste('+',paste0(locvisited,collapse='+'),sep='') # note - missing lodurations at the moment
var_demog <- paste('+',paste0(demog,collapse='+'),sep='')
var_counts <- paste('+',paste0(counts,collapse='+'),sep='')
# var_flow <- paste('+',paste0(flow_values,collapse='+'),sep='')
# var_labs <- ifelse(length(labs)==0,'',paste('+',paste0(lab_values,collapse='+'), sep=''))

class_formula<-function(...) as.formula(paste0("adm~1",...,collapse='+'))
formula <- class_formula(var_demog, var_arrchars, var_locations, var_counts)


# Pre-processing ----------------------------------------------------------

dm_recipe <- recipe(formula = formula, 
                    data = dm_train_train
                    # data = dm_train_train %>% 
                    # 
                    # select(-starts_with("fs_"),
                    #        -starts_with("l_")
                    #)
                    
) %>% 
#  update_role(csn, new_role = "id") %>% # was getting error here because formula doesn't include csn
  step_mutate(adm = as.factor(adm)) %>% 
  step_mutate(hour_of_arrival = as.factor(hour_of_arrival)) %>% 
  step_mutate(month = as.factor(month)) %>% 
  #  step_mutate(year = as.factor(year)) %>% 
  step_mutate(weekend = as.factor(weekend)) %>% 
  step_mutate(night = as.factor(night)) %>%
  step_zv(all_predictors()) 

dm_train_train_preprocessed <- dm_recipe %>% 
  prep(dm_train_train) %>% juice()

# Cross validation --------------------------------------------------------

set.seed(2020)
dm_folds <- dm_train_train %>%   #  not sure if this shoudl be dm_train_train or proc_dm_train_train
  prep(dm_recipe, new_data = .) %>%
  rsample::vfold_cv(v = 5)


# Create a model object ------------------------------------------------------------

xgb_spec <- boost_tree(
  
) %>% 
  set_engine("xgboost",scale_pos_weight=tune()) %>%  
  set_mode("classification")

xgb_wf <- workflow() %>% 
  add_recipe(dm_recipe) %>% 
  add_model(xgb_spec) 

param_grid <- expand.grid(scale_pos_weight = c(0.2, 1, 5))


# Run tuning --------------------------------------------------------------

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = param_grid, 
  metrics = metric_set(mcc, f_meas, roc_auc, accuracy, kap)
)


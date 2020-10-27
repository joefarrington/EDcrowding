# About this file
# ===============

# load libraries
# ==============

library(tidymodels)
library(dplyr)
library(lubridate)
library(xgboost)


# Set parallel processing -------------------------------------------------



# speed up computation with parrallel processing (optional)
# from https://www.r-bloggers.com/2020/05/using-xgboost-with-tidymodels/
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
#registerDoParallel(cores = all_cores)
# Got this error: Error in serialize(data, node$con) : error writing to connection
# Read that sometimes you have to leave one core open
# therefore trying only 4 cores
registerDoParallel(cores = 4)


# load data
# ==============

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-10-21.rda")

# identify columns with only one value
one_value <- matrix_60 %>% summarise_all(n_distinct) %>%  
  pivot_longer(cols = colnames(matrix_60), values_to = "count") %>% 
  filter(count == 1) %>% select(name)

# identify columsn with only one row having a value other than zero
only_one_non_zero <- matrix_60 %>% summarise_all(funs(sum(.==0))) %>%  
  pivot_longer(cols = colnames(matrix_60), values_to = "count") %>% 
  filter(count == nrow(matrix_60)-1) %>% select(name)

dm <- matrix_60 %>% 
  filter(age >= 18) %>% 
  select(-mrn, -csn, -csn_old, -birthdate, -ED_duration_final,
         -one_value$name, -only_one_non_zero$name,
         # -colnames(matrix_60)[grep("ideal_weight", colnames(matrix_60))],
         # -colnames(matrix_60)[grep("rass", colnames(matrix_60))],
         # -colnames(matrix_60)[grep("art_pressure", colnames(matrix_60))]
  ) %>% 
  mutate(admitted = as.factor(adm),
         adm_year = as.factor(year),
         adm_month = as.factor(month),
         adm_weekend = as.factor(weekend),
         adm_night = as.factor(night),
         adm_hour = as.factor(hour_of_arrival),
         adm_epoch = as.factor(epoch)) %>% 
  select(-adm, -year, -month, -day_of_week, -weekend, -night, -hour_of_arrival, -epoch) %>% 
  select(admitted, age, sex, everything())



# ## columns groups
#
adm_chars = colnames(dm)[grep("^adm_", colnames(dm))]
loc_durations = colnames(dm)[grep("^mins_|num_ED_row", colnames(dm))]
demog = c('age','sex')
flow = colnames(dm)[grep("^fs_", colnames(dm))]
labs = colnames(dm)[grep("^l_", colnames(dm))]



# train test split
set.seed(123)
dm_split <- initial_split(dm, strata = admitted, prop = 3/4)
dm_train <- training(dm_split)
dm_test <- testing(dm_split)




class_formula<-function(...) as.formula(paste0("admitted~1",...,collapse='+'))
# names for groups of features
var_adm_chars <- paste('+',paste0(adm_chars,collapse='+'),sep='')
var_locations <- paste('+',paste0(loc_durations,collapse='+'),sep='')
var_demog <- paste('+',paste0(demog,collapse='+'),sep='')
var_flow <- paste('+',paste0(flow,collapse='+'),sep='')
var_labs <- ifelse(length(labs)==0,'',paste('+',paste0(labs,collapse='+'), sep=''))

# formula 
formula = class_formula(var_demog, var_adm_chars, var_locations)
# 
# # try fitting this model
# xgb_spec_fit <- 
#   xgb_spec %>% 
#   fit(formula, dm_train)
# 
# # look at results
# classification_metrics(xgb_spec_fit,dm_train)

# Tuning a model
# ==============


# # set up model specification
# xgb_spec <- boost_tree(
#   trees = 1000, 
#   tree_depth = tune(), min_n = tune(), 
#   loss_reduction = tune(),                     ## first three: model complexity
#   sample_size = tune(), mtry = tune(),         ## randomness
#   learn_rate = tune(),                         ## step size
# ) %>% 
#   set_engine("xgboost",scale_pos_weight=5) %>% 
#   set_mode("classification")

# set up model specification
xgb_spec <- boost_tree(
  trees = tune(), 
  # tree_depth = tune(), min_n = tune(), 
  # loss_reduction = tune(),                     ## first three: model complexity
  # sample_size = tune(), mtry = tune(),         ## randomness
  # learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost",scale_pos_weight=5) %>% 
  set_mode("classification")



tree_grid <- grid_regular(trees(), levels = 8)

# 
# 
# # set up hyper parameter grid
# xgb_grid <- grid_latin_hypercube(
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), dm_train),
#   learn_rate(),
#   size = 10
# )

# xgb_grid


# set up workflow
xgb_wf <- workflow() %>%
  add_formula(formula) %>%
  add_model(xgb_spec)

# xgb_wf

# set up cross validation
set.seed(123)
dm_folds <- vfold_cv(dm_train, v = 5, strata = admitted)

# tune using grid
set.seed(234)
# xgb_res <- tune_grid(
#   xgb_wf,
#   resamples = dm_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )

xgb_res <- tune_grid(
  xgb_wf,
  resamples = dm_folds,
  grid = tree_grid,
  metrics = metric_set(accuracy, roc_auc, ppv, npv, sens, spec)
)

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations_60-mins_tune-trees-8_",today(),".rda")
save(xgb_res, file = outFile)



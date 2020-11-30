# About this file ---------------------------------------------------------

# runs XGboost with default params


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

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-23.rda")

# once star_test is done, check for duplicates mrns for same csn; 
matrix_60 %>% ungroup() %>% select(mrn, csn) %>% group_by(csn) %>% 
  summarise(tot = sum(n())) %>% filter(tot > 1) %>% arrange(desc(tot))


dm <- matrix_60 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  select(-mrn, -csn_old, -ED_duration_final
  ) %>% 
  select(adm, age, sex, gt70, everything())


# Train test split --------------------------------------------------------

set.seed(123)
dm_split_train_test <- initial_split(dm, strata = adm, prop = 4/5)
dm_train <- training(dm_split_train_test)

# dm_test <- testing(dm_split)

# create validation set 
dm_split_train_val <- initial_split(dm_train, strata = adm, prop = 7/8)
dm_train_train <- training(dm_split_train_val)
dm_train_val <- testing(dm_split_train_val)


# Pre-processing ----------------------------------------------------------

dm_recipe <- recipe(adm ~ ., 
                    data = dm_train_train
                    ) %>% 
  update_role(csn, new_role = "id") %>% 
  step_mutate(adm = as.factor(adm)) %>% 
  step_mutate(hour_of_arrival = as.factor(hour_of_arrival)) %>% 
  step_mutate(month = as.factor(month)) %>% 
#  step_mutate(year = as.factor(year)) %>% 
  step_mutate(weekend = as.factor(weekend)) %>% 
  step_mutate(night = as.factor(night)) %>%
  step_zv(all_predictors()) %>% 
  prep()


proc_dm_train_train <- dm_recipe %>% bake(
  dm_train_train
)


proc_dm_train_val <- dm_recipe %>% bake(
  dm_train_val
)

# Cross validation --------------------------------------------------------

set.seed(2020)
dm_folds <- proc_dm_train_train %>%  #  not sure if this shoudl be dm_train_train or proc_dm_train_train
  bake(dm_recipe, new_data = .) %>%
  rsample::vfold_cv(v = 5)


# Set up formula ----------------------------------------------------------

arrchars = colnames(proc_dm_train_train %>% select(hour_of_arrival:num_ED_rows))
locdurations = colnames(proc_dm_train_train)[grep("^mins_", colnames(proc_dm_train_train))]
locvisited = colnames(proc_dm_train_train)[grep("^visited_", colnames(proc_dm_train_train))]
demog = c('age','sex', 'gt70')
counts = colnames(proc_dm_train_train %>% select(num_fs_results:l_num_ESR))
# flow_values = colnames(proc_dm_train_train %>% select(fs_min_bp_sys:fs_latest_resp_rate))
# lab_values = colnames(proc_dm_train_train %>% select(l_min_BA:l_latest_APTM))

var_arrchars <- paste('+',paste0(arrchars,collapse='+'),sep='')
var_locations <- paste('+',paste0(locvisited,collapse='+'),sep='') # note - missing lodurations at the moment
var_demog <- paste('+',paste0(demog,collapse='+'),sep='')
var_counts <- paste('+',paste0(counts,collapse='+'),sep='')
# var_flow <- paste('+',paste0(flow_values,collapse='+'),sep='')
# var_labs <- ifelse(length(labs)==0,'',paste('+',paste0(lab_values,collapse='+'), sep=''))

class_formula<-function(...) as.formula(paste0("adm~1",...,collapse='+'))
formula <- class_formula(var_demog, var_arrchars, var_locations, var_counts)


# Set up model ------------------------------------------------------------

xgb_spec <- boost_tree(
  
) %>% 
  set_engine("xgboost",scale_pos_weight=tune()) %>%  
  set_mode("classification")

xgb_wf <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_formula(formula)

class_weight <- sum(proc_dm_train_train$adm == FALSE)/sum(proc_dm_train_train$adm == TRUE)
param_grid <- expand.grid(scale_pos_weight = c(0.2, 1, class_weight, 5))

# Run tuning --------------------------------------------------------------

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = param_grid, 
  metrics = metric_set(mcc, f_meas, roc_auc, accuracy, kap, precision, recall, ppv, npv)
)


# Save results ------------------------------------------------------------

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_60-mins_tune-scale_pos_weight_new_dataset_",today(),".rda")
save(xgb_res, file = outFile)


# Evaluate results --------------------------------------------------------


load("~/EDcrowding/predict-admission/data-output/xgb_results_60-mins_tune-scale_pos_weight_new_dataset_2020-11-23.rda")


xgb_res %>% collect_metrics() %>%
  filter(!.metric %in% c("npv", "ppv", "recall", "precision")) %>%
  ggplot(aes(x = scale_pos_weight, y = mean, color = .metric)) +
  geom_point(size = 3) + geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = as.numeric(param_grid$scale_pos_weight)) +
  labs(title = "Tuning for trees with 60 min model with 5 fold validation",
       y = "Mean score on metric across 5 folds")

best_mod = select_best(xgb_res, "mcc")

# run best model on whole training set
xgb_spec_full <- boost_tree(
  
) %>%
  set_engine("xgboost",scale_pos_weight=best_mod$scale_pos_weight) %>%
  set_mode("classification")

xgb_wf_full <- workflow() %>%
  add_model(xgb_spec_full) %>%
  add_formula(formula)

# fit to the training data
xgb_fit_full <- fit(xgb_wf_full, data = proc_dm_train_train)

# score predictions

classification_metrics<-function(fit,data) {
  pred<-bind_cols(
    truth=data$adm,
    predict(fit,data,type="class"),
    predict(fit,data,type="prob")
  )
  print(paste0("Baseline=",mean(data$adm==T)))
  print(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second"))
  print(pred %>% metrics(truth,.pred_class))
  print(pred %>% conf_mat(truth, .pred_class))
  print(pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot())
  return(pred)
}


# then predict on validation set
pred_train <- classification_metrics(xgb_fit_full, proc_dm_train_train)
pred_val <- classification_metrics(xgb_fit_full, proc_dm_train_val)


outFile <- paste0("EDcrowding/predict-admission/data-output/xgb_pred_60-mins_tune-scale_pos_weight_class_weight_val_set_",today(),".rda")
save(pred_val, file = outFile)

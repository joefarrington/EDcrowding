# About this file ---------------------------------------------------------



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
registerDoParallel(cores = 4)


# Set paramaters ----------------------------------------------------------

covars <- c("demog", "arrchars", "locdurations")
timeslice <- "60_mins"
parameter_to_tune <- "tree_depth"

# Load data ---------------------------------------------------------------


load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-10-21.rda")


dm <- matrix_60 %>% 
  filter(age >= 18) %>% 
  select(-mrn, -csn_old, -birthdate, -ED_duration_final
  ) %>% 
  select(adm, age, sex, everything())


# Train test split --------------------------------------------------------

set.seed(123)
dm_split <- initial_split(dm, strata = adm, prop = 3/4)
dm_train <- training(dm_split)
# dm_test <- testing(dm_split)


# Pre-processing ----------------------------------------------------------

dm_recipe <- recipe(adm ~ ., 
                    data = dm_train %>% 
                      select(-starts_with("fs_"),
                             -starts_with("l_")
                      )) %>% 
  update_role(csn, new_role = "id") %>% 
  step_mutate(adm = as.factor(adm)) %>% 
  step_mutate(hour_of_arrival = as.factor(hour_of_arrival)) %>% 
  step_mutate(month = as.factor(month)) %>% 
  step_mutate(year = as.factor(year)) %>% 
  step_mutate(weekend = as.factor(weekend)) %>% 
  step_mutate(night = as.factor(night)) %>%
  step_zv(all_predictors()) %>% 
  prep()


proc_dm_train <- dm_recipe %>% bake(
  dm_train %>%
    select(-starts_with("fs_"), -starts_with("l_"))
)


# Cross validation --------------------------------------------------------

set.seed(2020)
dm_folds <- proc_dm_train %>%  #  not sure if this shoudl be dm_train or proc_dm_train
  bake(dm_recipe, new_data = .) %>%
  rsample::vfold_cv(v = 5)



# Model specification -----------------------------------------------------

xgb_spec <- boost_tree(
  trees = 500, 
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune(),
) %>% 
  set_engine("xgboost",scale_pos_weight=5) %>% 
  set_mode("classification")


# set up formula
arrchars = colnames(dm)[grep("^adm_", colnames(dm))]
locdurations = colnames(dm)[grep("^mins_|num_ED_row", colnames(dm))]
demog = c('age','sex')
counts = colnames(dm)[grep("^has_|num_fs|num_l", colnames(dm))]
flow = colnames(dm)[grep("^fs_", colnames(dm))]
labs = colnames(dm)[grep("^l_", colnames(dm))]

var_arrchars <- paste('+',paste0(arrchars,collapse='+'),sep='')
var_locations <- paste('+',paste0(locdurations,collapse='+'),sep='')
var_demog <- paste('+',paste0(demog,collapse='+'),sep='')
var_counts <- paste('+',paste0(counts,collapse='+'),sep='')
var_flow <- paste('+',paste0(flow,collapse='+'),sep='')
var_labs <- ifelse(length(labs)==0,'',paste('+',paste0(labs,collapse='+'), sep=''))

class_formula<-function(...) as.formula(paste0("adm~1",...,collapse='+'))
formula <- class_formula(var_demog, var_arrchars, var_locations, var_counts)

xgb_wf <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_formula(formula)

# set up parameter grid
set.seed(234)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), proc_dm_train),
  learn_rate(),
  size = 20
)

# Run tuning --------------------------------------------------------------

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = xgb_grid, 
  metrics = metric_set(f_meas, roc_auc, accuracy, kap, precision, recall, ppv, npv)
)


# Save results ------------------------------------------------------------

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations-counts_60-mins_tune-all-2_",today(),".rda")
save(xgb_res, file = outFile)


# # Evaluate results --------------------------------------------------------
# 
# save_chart("demo-arrchars-locdurations-counts_60-mins_tune-tree-depth-8",
#            xgb_tuned_tree_depth %>% collect_metrics() %>% 
#              ggplot(aes(x = tree_depth, y = mean, color = .metric)) +
#              geom_point(size = 3) + geom_line() +
#              theme_classic() +
#              scale_x_continuous(breaks = as.numeric(tree_depth_grid$tree_depth)) +
#              labs(title = "Tuning for tree depth with 60 min model with 5 fold validation", 
#                   y = "Mean score on metric across 5 folds")
# )
# 
# 
# best_auc <- select_best(xgb_tuned_tree_depth, "roc_auc")   
# 
# 
# # Fit best model
# final_xgb <- finalize_workflow(
#   xgb_wf,
#   best_auc # using the model that is best for accuracy
# )
# 
# final_xgb_fit <- fit(final_xgb, proc_dm_train)
# 
# 
# # save predictions
# pred<-bind_cols(
#   truth=proc_dm_train$adm,
#   predict(final_xgb_fit,proc_dm_train,type="class"),
#   predict(final_xgb_fit,proc_dm_train,type="prob")
# )
# 
# 
# outFile <- paste0("EDcrowding/predict-admission/data-output/xgb_pred_demo-arrchars-locdurations-counts_60-mins_tune-tree-depth-8_",today(),".rda")
# save(pred, file = outFile)
# 
# 
# 
# # Look at confusion matrix ------------------------------------------------
# 
# cm <- pred %>% conf_mat(truth, .pred_class) 
# cm %>% tidy() %>% 
#   
#   summary(cm)
# 
# 
# # Look at variable importance ---------------------------------------------
# 
# 
# p <- final_xgb %>%
#   fit(data = proc_dm_train) %>%
#   pull_workflow_fit() %>%
#   vip(geom = "point")
# 
# save_chart("demo-arrchars-locdurations-counts_60-mins_tune-tree-depth-8_best-auc_VIP",
#            p + 
#              labs(title = "Variable importance after tuning tree depth with 60 min model - best AUC model", 
#                   x = "Variable",
#                   y = "Importance")
# )
# 
# 

# About this file ---------------------------------------------------------

# Trying to follow the approach outlined here
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
# to tune step by step

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

# covars <- c("demog", "arrchars", "locdurations")
# timeslice <- "60_mins"
# parameter_to_tune <- "tree_depth"

# Load data ---------------------------------------------------------------


load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-10-21.rda")


dm <- matrix_60 %>% 
  filter(age >= 18) %>% 
  select(-mrn, -csn_old, -birthdate, -ED_duration_final, 
         # realised that num ED rows has count across all of ED
         -num_ED_row_excl_OTF 
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
  


# STEP 1: TUNE TREES -----------------------------------------------------

# Trying to follow the approach outlined here
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/

xgb_spec <- boost_tree(
  trees = tune(), 
  tree_depth = 5, # suggested by blog; also best kap was achieved with tree_depth = 5 last week
  min_n = 1, # suggested by blog "A smaller value is chosen because it is a highly imbalanced class problem and leaf nodes can have smaller size groups."
#  loss_reduction = tune(),
  sample_size = .8, # suggested by blog
  mtry = 20, # there are 26 predictors; 0.8 * 26 is 20
  learn_rate = 0.1,
) %>% 
  set_engine("xgboost",scale_pos_weight=5, gamma = 0) %>%  # gamma suggested by blos
  set_mode("classification")


# set up formula
arrchars = colnames(proc_dm_train)[grep("^adm_", colnames(proc_dm_train))]
locdurations = colnames(proc_dm_train)[grep("^mins_|num_ED_row", colnames(proc_dm_train))]
demog = c('age','sex')
counts = colnames(proc_dm_train)[grep("^has_|num_fs|num_l", colnames(proc_dm_train))]
flow = colnames(proc_dm_train)[grep("^fs_", colnames(proc_dm_train))]
labs = colnames(proc_dm_train)[grep("^l_", colnames(proc_dm_train))]

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

# from https://juliasilge.com/blog/animal-crossing/
tree_grid <- grid_regular(trees(), levels = 11)

# # set up parameter grid
# set.seed(234)
# # xgb_grid <- grid_latin_hypercube(
# #   tree_depth(),
# #   min_n(),
# #   loss_reduction(),
# #   sample_size = sample_prop(),
# #   finalize(mtry(), proc_dm_train),
# #   learn_rate(),
# #   size = 20
# # )
# 
# 
# xgboostParams <- dials::parameters(
#   min_n(),
#   tree_depth(),
#   learn_rate(),
#   finalize(mtry(),select(proc_dm_train,-adm)),
#   sample_size = sample_prop()
# )
# 
# xgb_grid <- dials::grid_max_entropy(xgboostParams, size = 100)


# Run tuning --------------------------------------------------------------

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = tree_grid, 
  metrics = metric_set(mcc, f_meas, roc_auc, accuracy, kap, precision, recall, ppv, npv)
)


# Save results ------------------------------------------------------------

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations-counts_60-mins_tune-trees",today(),".rda")
save(xgb_res, file = outFile)


save_chart("demo-arrchars-locdurations-counts_60-mins_tune-trees",
           xgb_res %>% collect_metrics() %>% 
             ggplot(aes(x = trees, y = mean, color = .metric)) +
             geom_point(size = 3) + geom_line() +
             theme_classic() +
             scale_x_continuous(breaks = as.numeric(tree_grid$trees)) +
             labs(title = "Tuning for trees with 60 min model with 5 fold validation", 
                  y = "Mean score on metric across 5 folds")
)

best_mod <- select_best(xgb_res, "mcc")

# Fit best model
final_xgb <- finalize_workflow(
  xgb_wf,
  best_mod # using the model that is best for accuracy
)

final_xgb_fit <- fit(final_xgb, proc_dm_train)


# save predictions
pred<-bind_cols(
  truth=proc_dm_train$adm,
  predict(final_xgb_fit,proc_dm_train,type="class"),
  predict(final_xgb_fit,proc_dm_train,type="prob")
)


outFile <- paste0("EDcrowding/predict-admission/data-output/xgb_pred_demo-arrchars-locdurations-counts_60-mins_tune-trees-best-mcc",today(),".rda")
save(pred, file = outFile)


# Look at variable importance 

p <- final_xgb %>%
  fit(data = proc_dm_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

p

save_chart("vip_demo-arrchars-locdurations-counts_60-mins_tune-trees-best-mcc",
           p +
             labs(title = "Variable importance after tuning number of trees with 60 min design matrix - best mcc model",
                  x = "Variable",
                  y = "Importance")
          
)

# show results

print(pred %>% metrics(truth,.pred_class))
print(pred %>% conf_mat(truth, .pred_class))
print(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second"))

save_chart("AUC_demo-arrchars-locdurations-counts_60-mins_tune-trees-best-mcc",
  print(pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()) +
    annotate("text", x = .9, y = .00, label = paste0("Area under ROC: ",
                                                     round(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second") %>% select(.estimate),2)))
)

# Look at confusion matrix ------------------------------------------------

cm <- pred %>% conf_mat(truth, .pred_class) 

save_chart("conf-mat_demo-arrchars-locdurations-counts_60-mins_tune-trees-best-mcc",
           
  cm %>% autoplot(type = "heatmap") + 
    labs(title = "Confusion matrix after tuning number of trees with 60 min design matrix - best mcc model") +theme(text = element_text(size=rel(3.5)))
)




# STEP 2: TUNE TREE DEPTH AND MIN_N -----------------------------------------------------

xgb_spec <- boost_tree(
  trees = 400, 
  tree_depth = tune(), 
  min_n = tune(), 
  sample_size = .8, # suggested by blog
  mtry = 20, # there are 26 predictors; 0.8 * 26 is 20
  learn_rate = 0.1,
) %>% 
  set_engine("xgboost",scale_pos_weight=5, gamma = 0) %>%  # gamma suggested by blog
  set_mode("classification")


xgb_wf <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_formula(formula)


param_grid <- expand.grid(tree_depth = seq(3, 9, 2),
                          min_n = seq(1,5,2))

# Run tuning

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = param_grid, 
  metrics = metric_set(mcc, f_meas, roc_auc, accuracy, kap, precision, recall, ppv, npv)
)


# Save results

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations-counts_60-mins_tune-depth-minn",today(),".rda")
save(xgb_res, file = outFile)

save_chart("demo-arrchars-locdurations-counts_60-mins_tune-depth", width = 500,
           xgb_res %>% collect_metrics() %>% 
             filter(.metric %in% c("roc_auc", "accuracy", "kap", "f_meas", "mcc")) %>% 
             ggplot(aes(x = tree_depth, y = mean, color = .metric)) +
             scale_x_continuous(breaks = param_grid$tree_depth) +
             
             geom_point(size = 1) + geom_line() +
             theme_classic() +
             labs(title = "Tuning for tree depth with 60 min model with 5 fold validation", 
                  y = "Mean score on metric across 5 folds")
)

save_chart("demo-arrchars-locdurations-counts_60-mins_tune-minn", width = 500,
           xgb_res %>% collect_metrics() %>% 
             filter(.metric %in% c("roc_auc", "accuracy", "kap", "f_meas", "mcc")) %>% 
             ggplot(aes(x = min_n, y = mean, color = .metric)) +
             scale_x_continuous(breaks = param_grid$min_n) +
            
             geom_point(size = 1) + geom_line() +
             theme_classic() +
             labs(title = "Tuning for min_n with 60 min model with 5 fold validation", 
                  y = "Mean score on metric across 5 folds")
)



best_mod <- select_best(xgb_res, "mcc")




# STEP 3: TUNE GAMMA -----------------------------------------------------

xgb_spec <- boost_tree(
  trees = 400, 
  tree_depth = 7, # best tree depth from previous round
  min_n = 3,  # best min_n from previous round
  sample_size = .8, # suggested by blog
  mtry = 20, # there are 26 predictors; 0.8 * 26 is 20
  learn_rate = 0.1,
) %>% 
  set_engine("xgboost",scale_pos_weight=5, gamma = tune()) %>%  # gamma suggested by blog
  set_mode("classification")


xgb_wf <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_formula(formula)


param_grid <- expand.grid(gamma = seq(0, 0.5, 0.1))

# Run tuning

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = param_grid, 
  metrics = metric_set(mcc, f_meas, roc_auc, accuracy, kap, precision, recall, ppv, npv)
)


# Save results

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations-counts_60-mins_tune-gamma",today(),".rda")
save(xgb_res, file = outFile)


save_chart("demo-arrchars-locdurations-counts_60-mins_tune-gamma", 
           xgb_res %>% collect_metrics() %>% 
             filter(.metric %in% c("roc_auc", "accuracy", "kap", "f_meas", "mcc")) %>% 
             ggplot(aes(x = gamma, y = mean, color = .metric)) +
             scale_x_continuous(breaks = param_grid$gamma) +
             
             geom_point(size = 1) + geom_line() +
             theme_classic() +
             labs(title = "Tuning for gamma with 60 min model with 5 fold validation", 
                  y = "Mean score on metric across 5 folds")
)




best_mod <- select_best(xgb_res, "mcc")


# STEP 4: TUNE SAMPLE_SIZE AND MTRY() -----------------------------------------------------

xgb_spec <- boost_tree(
  trees = 400, 
  tree_depth = 7, 
  min_n = 3, 
  sample_size = tune(), # suggested by blog
  mtry = tune(), # there are 26 predictors; 0.8 * 26 is 20
  learn_rate = 0.1,
) %>% 
  set_engine("xgboost",scale_pos_weight=5, gamma =0.4) %>%  # best gamma from previous round
  set_mode("classification")


xgb_wf <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_formula(formula)

# from earlier attempts
xgb_grid <- grid_latin_hypercube(
  sample_size = sample_prop(),
  finalize(mtry(), proc_dm_train)

)

# or more straightforward - ### note these are hardcoded!!
param_grid <- expand.grid(sample_size = seq(0.2, 1, 0.2),
                          mtry = seq(5, 25, 5)) 



# Run tuning

xgb_res <- tune_grid(
  object = xgb_wf, 
  resamples = dm_folds, 
  grid = param_grid, 
  metrics = metric_set(mcc, f_meas, roc_auc, accuracy, kap, precision, recall, ppv, npv)
)


# Save results

outFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations-counts_60-mins_tune-mtry-sample-size",today(),".rda")
save(xgb_res, file = outFile)


save_chart("demo-arrchars-locdurations-counts_60-mins_tune-mtry", 
           xgb_res %>% collect_metrics() %>% 
             filter(.metric %in% c("roc_auc", "accuracy", "kap", "f_meas", "mcc")) %>% 
             ggplot(aes(x = mtry, y = mean, color = .metric)) +
             scale_x_continuous(breaks = param_grid$mtry) +
             
             geom_point(size = 1) + geom_line() +
             theme_classic() +
             labs(title = "Tuning for mtry with 60 min model with 5 fold validation", 
                  y = "Mean score on metric across 5 folds")
)


save_chart("demo-arrchars-locdurations-counts_60-mins_tune-sample-size", 
           xgb_res %>% collect_metrics() %>% 
             filter(.metric %in% c("roc_auc", "accuracy", "kap", "f_meas", "mcc")) %>% 
             ggplot(aes(x = sample_size, y = mean, color = .metric)) +
             scale_x_continuous(breaks = param_grid$sample_size) +
             
             geom_point(size = 1) + geom_line() +
             theme_classic() +
             labs(title = "Tuning for sample size with 60 min model with 5 fold validation", 
                  y = "Mean score on metric across 5 folds")
)



best_mod <- select_best(xgb_res, "mcc")




# STEP 5: TUNE ALPHA  -----------------------------------------------------


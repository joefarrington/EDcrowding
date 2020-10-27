# About this file
# ===============


# from tidyverse training
# https://juliasilge.com/blog/xgboost-tune-volleyball/



# Create functions --------------------------------------------------------

# Save chart
save_chart = function(chart_title, g, width = 1077, height = 659) {
  png(paste0("EDcrowding/predict-admission/model-output/", chart_title, ".png"), width = width, height = height) 
  print(g)
  dev.off()
}


# load libraries
# ==============

library(tidymodels)
library(dplyr)
library(lubridate)
library(xgboost)
library(vip)



# load data to recreate dm_train
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


# Read tuning results
# ===================


load("~/EDcrowding/predict-admission/data-output/xgb_results_demo-arrchars-locdurations_60-mins_tune-trees-8_2020-10-27.rda")

# recreate tuning model specification
xgb_spec <- boost_tree(
  trees = tune(), 
  # tree_depth = tune(), min_n = tune(), 
  # loss_reduction = tune(),                     ## first three: model complexity
  # sample_size = tune(), mtry = tune(),         ## randomness
  # learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost",scale_pos_weight=5) %>% 
  set_mode("classification")



# recreate workflow
xgb_wf <- workflow() %>%
  add_formula(formula) %>%
  add_model(xgb_spec)


# visualising the result

# xgb_res %>%
#   collect_metrics() %>%
#   filter(.metric == "roc_auc") %>%
#   select(mean, mtry:sample_size) %>%
#   pivot_longer(mtry:sample_size,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")
# 
# xgb_res %>%
#   collect_metrics() %>%
#   filter(.metric == "accuracy") %>%
#   select(mean, mtry:sample_size) %>%
#   pivot_longer(mtry:sample_size,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "Accurancy")


save_chart("demog-admchar-mins_tune-trees-8",
  xgb_res %>% collect_metrics() %>% 
    ggplot(aes(x = trees, y = mean, color = .metric)) +
    geom_point(size = 3) + geom_line() +
    theme_classic() +
    scale_x_continuous(breaks = as.numeric(tree_grid$trees)) +
    labs(title = "Tuning for trees with 60 min model with 5 fold validation", 
         y = "Mean score on metric across 5 folds")
)


show_best(xgb_res, "roc_auc")

# best_auc <- select_best(xgb_res, "roc_auc") 
# a way to select best auc when you don't want the model with one tree
best_auc <- xgb_res %>% 
  collect_metrics() %>% 
  filter(trees != 1, .metric == "roc_auc") %>% 
  select(trees, .config, .mean = mean) %>% 
  filter(.mean == max(.mean)) %>% select(-.mean)
# best_acc <- select_best(xgb_res, "accuracy") #Model09 is best for accuracy
# best_ppv <- select_best(xgb_res, "ppv") #Model09 is best for accuracy

best_auc


final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc # using the model that is best for accuracy
)

final_xgb

# print a plot showing which variables are most important

final_xgb %>%
  fit(data = dm_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

# note - may need to fit with set_engine( xxx, importance = "permutation") to get params out 
# Julie Silge does this in sf_trees demo  https://juliasilge.com/blog/sf-trees-random-tuning/

# fit the final model
final_xgb_fit <- fit(final_xgb, dm_train)

# # make predictions
# predict(final_xgb_fit, dm_train)

# or try collect_predictions - this works after last_fit which you use when applying to the test set
# not sure if it works otherwise



pred<-bind_cols(
  truth=dm_train$admitted,
  predict(final_xgb_fit,dm_train,type="class"),
  predict(final_xgb_fit,dm_train,type="prob")
)


outFile <- paste0("EDcrowding/predict-admission/data-output/xgb_pred_demo-arrchars-locdurations_60-mins_tune-trees-8_best-auc-excl-1-tree_",today(),".rda")
save(pred, file = outFile)


cm <- pred %>% conf_mat(truth, .pred_class)
# 
classification_metrics<-function(fit,data) {
  pred<-bind_cols(
    truth=data$admitted,
    predict(fit,data,type="class"),
    predict(fit,data,type="prob")
  )
  print(paste0("Baseline=",mean(data$admitted==T)))
  print(pred %>% metrics(truth,.pred_class))
  print(pred %>% conf_mat(truth, .pred_class))
  # print(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second"))
  # print(pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot())
  return(pred)
}
classification_metrics(final_xgb_fit, dm_train)


mods <- xgb_res %>% collect_metrics %>% select(trees, .config) %>% distinct()

scores <- tribble(
  ~model,
  ~trees,
  ~.metric,
  ~.estimator,
  ~.estimate
)

for (i in (1:nrow(mods))) {
  model = mods[i,]
  print(model)
  final_xgb <- finalize_workflow(
    xgb_wf,
    model 
  )
  final_xgb_fit <- fit(final_xgb, dm_train)
  pred<-bind_cols(
    truth=dm_train$admitted,
    predict(final_xgb_fit,dm_train,type="class")
  )
  cm <- pred %>% conf_mat(truth, .pred_class)
  sum <- summary(cm)
  sum$model <- mods$.config[i]
  sum$trees <- mods$trees[i]
  print(sum)
  scores <- scores %>% bind_rows(sum)
}
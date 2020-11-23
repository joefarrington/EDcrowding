# About this file
# ===============

# Here I'm going back to an earlier model to try to understand why it was so good.
# Realised that mtry was set to be too large given the number of features allowed in the model - could this be why? 

# from tidyverse training
# https://juliasilge.com/blog/xgboost-tune-volleyball/



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


load("~/EDcrowding/predict-admission/data-output/xgb_results_60_2020-10-22.rda")


# recreate tuning model specification
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost",scale_pos_weight=5) %>% 
  set_mode("classification")



# recreate workflow
xgb_wf <- workflow() %>%
  add_formula(formula) %>%
  add_model(xgb_spec)


# visualising the result

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Accurancy")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc") #Model03 is best for AUC
best_acc <- select_best(xgb_res, "accuracy") #Model09 is best for accuracy
best_auc


final_xgb <- finalize_workflow(
  xgb_wf,
  best_acc # using the model that is best for accuracy
)

final_xgb

# print a plot showing which variables are most important

final_xgb %>%
  fit(data = dm_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


# fit the final model
final_xgb_fit <- fit(final_xgb, dm_train)
 

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
  print(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second"))
  print(pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot())
  return(pred)
}
pred <- classification_metrics(final_xgb_fit, dm_train)

# Trying with Brier scores to evaluate which samples are overfit ----------

# calculate Brier score
pred <- pred %>% 
  mutate(brier = (.pred_TRUE - (as.numeric(truth)-1))^2)

# add preds to dm
dm_with_pred <- proc_dm_train %>% bind_cols(pred) %>% 
  select(- starts_with("l_"), - starts_with("fs_")) %>% 
  select(adm, .pred_TRUE, brier, everything()) %>% arrange(desc(brier))

dm_false_positive <- dm_with_pred %>% 
  filter(adm == "FALSE", .pred_class == "TRUE") 

dm_false_positive %>% 
  filter(mins_RESUS > 15) %>% 
  ggplot(aes(x = mins_RESUS, y = brier)) + geom_point() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "False positives: minutes in resus by brier score")


dm_with_pred  %>%  filter(mins_RESUS > 0) %>% 
  ggplot(aes(x = mins_RESUS, y = brier, col = adm)) + geom_point() +
  labs(title = "Minutes in Resus by Brier score",
       subtitle = "Approx 1300 patients of the 20K false positives spent more than 15 min in Resus", 
       y = "Brier score (0 is best score possible)",
       colour = "Admitted") +
  theme(legend.position = "bottom")

dm_with_pred  %>%  filter(mins_RAT > 0) %>% 
  ggplot(aes(x = mins_RAT, y = brier, col = adm)) + geom_point() +
  labs(title = "Minutes in RAT by Brier score",
       y = "Brier score (0 is best score possible)",
       colour = "Admitted") +
  theme(legend.position = "bottom")



dm_with_pred  %>% filter(mins_Waiting > 0) %>% 
  ggplot(aes(x = mins_Waiting, y = brier, col = adm)) + geom_point() +
  labs(title = "Minutes in waiting by Brier score",
       y = "Brier score (0 is best score possible)",
       colour = "Admitted") +
  theme(legend.position = "bottom")

dm_with_pred  %>% filter(mins_Waiting > 0) %>% 
  ggplot(aes(x = mins_Waiting, y = brier, col = adm)) + geom_point() +
  labs(title = "Minutes in waiting by Brier score",
       y = "Brier score (0 is best score possible)",
       colour = "Admitted") +
  theme(legend.position = "bottom")

dm_with_pred  %>% 
  ggplot(aes(x = age, y = brier, col = adm)) + geom_point() +
  labs(title = "Age by Brier score",
       y = "Brier score (0 is best score possible)",
       colour = "Admitted") +
  theme(legend.position = "bottom")



         
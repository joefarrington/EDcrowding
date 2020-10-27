# About this file
# ===============


# from tidyverse training
# https://juliasilge.com/blog/xgboost-tune-volleyball/



# load libraries
# ==============


library(dplyr)
library(lubridate)

library(tidymodels)
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

model = "demo-arrchars-locdurations"
timeslice = "60-mins"
inFile = paste0("EDcrowding/predict-admission/data-output/xgb_results_",model,"_", timeslice,"_2020-10-22.rda")
load(inFile)

parameters_used <- xgb_res %>% collect_metrics()
outFile = paste0("EDcrowding/predict-admission/data-output/xgb_metrics_",model,"_", timeslice,"_2020-10-22.rda")
save(parameters_used, file = outFile)


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
  labs(x = NULL, y = "AUC",
       title = "Parameters used in model tuning with resulting AUC")

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
  labs(x = NULL, y = "Accuracy",
       title = "Parameters used in model tuning with resulting Accuracy")

show_best(xgb_res, "roc_auc")
show_best(xgb_res, "accuracy")

best_auc <- select_best(xgb_res, "roc_auc") #Model03 is best for AUC
best_acc <- select_best(xgb_res, "accuracy") #Model09 is best for accuracy



final_xgb_acc <- finalize_workflow(
  xgb_wf,
  best_acc # using the model that is best for accuracy
)


final_xgb_auc <- finalize_workflow(
  xgb_wf,
  best_auc # using the model that is best for AUC
)

# fit the final models
final_xgb_fit_acc <- fit(final_xgb_acc, dm_train)
final_xgb_fit_auc <- fit(final_xgb_auc, dm_train)


# make predictions based on both models
pred<-bind_cols(
  truth=dm_train$admitted,
  class_auc = predict(final_xgb_fit_auc,dm_train,type="class"),
  prob_auc = predict(final_xgb_fit_auc,dm_train, type="prob"),
  class_acc = predict(final_xgb_fit_acc,dm_train,type="class"),
  prob_acc = predict(final_xgb_fit_acc,dm_train, type="prob")
)

# save predictions 
outFile = paste0("EDcrowding/predict-admission/data-output/xgb_predicitions_",model,"_", timeslice,"_2020-10-22.rda")
save(pred, file = outFile)


# print charts
chart_title <- paste0("Results for best accuracy model: ", model, "_", timeslice)
save_chart(chart_title,
           pred %>% conf_mat(truth, .pred_class) %>% autoplot(type = "heatmap"),
           width = 200, height = 200
           )

chart_title <- paste0("Results for best AUC model: ", model, "_", timeslice)
save_chart(chart_title,
           pred %>% conf_mat(truth, .pred_class) %>% autoplot(type = "heatmap"),
           width = 200, height = 200
)


chart_title <- paste0("ROC for best accuracy model: ", model, "_", timeslice)



p2 <- pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()

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
classification_metrics(final_xgb_fit, dm_train)



# print a plot showing which variables are most important

p3 <- final_xgb %>%
  fit(data = dm_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

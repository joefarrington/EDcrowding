# About this file
# ===============

# given the patient information, including current stage, flowsheet, labs, a model is trained with a certain period
# of history and then prediction is performed on the current state of the ED. Prediction will yield a TRUE/FALSE for
# each patient as well as a probability

# load libraries
# ==============

library(tidymodels)
# library(discrim)
library(dplyr)
# library(lubridate)
library(xgboost)
# library(parsnip)
# library(caret)
# library(tune)
# library(dials)
# library(scales)
# library(yardstick)


# load data
# ==============

load("~/EDcrowding/predict-admission/data-raw/matrix_15_2020-10-20.rda")

# remove columns that are not needed e.g. csn, mrn, ...
dm <- matrix_15 %>% 
  filter(age >= 18) %>% 
  select(-mrn, -csn, -csn_old, -birthdate, -ED_duration_final) %>% 
  mutate(admitted = as.factor(adm),
         adm_year = as.factor(year),
         adm_month = as.factor(month),
         adm_weekend = as.factor(weekend),
         adm_night = as.factor(night),
         adm_hour = as.factor(hour_of_arrival),
         adm_epoch = as.factor(epoch)) %>% 
  select(-adm, -year, -month, -day_of_week, -weekend, -night, -hour_of_arrival, -epoch) %>% 
  select(admitted, age, sex, everything())


# identify columns with only one value
one_value <- dm %>% summarise_all(n_distinct) %>%  
  pivot_longer(cols = admitted:l_latest_GLU, values_to = "count") %>% 
  filter(count == 1) %>% select(name)

# identify columsn with only one row having a value other than zero
only_one_non_zeroe <- dm %>% summarise_all(funs(sum(.==0))) %>%  
  pivot_longer(cols = admitted:l_latest_GLU, values_to = "count") %>% 
  filter(count == nrow(dm)-1) %>% select(name)

dm <- dm %>% select(-one_value$name, -only_one_non_zeroe$name)

## columns groups

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


# while looking through this i tried applying a normalisation to all numeric variables
# discovered some variable that only appear once e.g fs_num_rass, l_num_TDDI, 

prep_for_ml <- function(df) {
  recipe(admitted~.,data=df) %>% 
    step_normalize(all_numeric()) %>% 
    step_dummy(starts_with("adm_")) %>% 
    step_dummy(matches("sex")) %>% 
#    step_downsample(admitted) %>% 
    prep %>% bake(df)
}

dm_train_prepped <- dm_train %>% prep_for_ml()

adm_chars = colnames(dm_train_prepped)[grep("^adm_", colnames(dm_train_prepped))]
loc_durations = colnames(dm_train_prepped)[grep("^mins_|num_ED_row", colnames(dm_train_prepped))]
demog = c('age','sex_MALE','sex_UKNOWN')
flow = colnames(dm_train_prepped)[grep("^fs_", colnames(dm_train_prepped))]
labs = colnames(dm_train_prepped)[grep("^l_", colnames(dm_train_prepped))]


# train data
fit<-(function(){
  class_formula<-function(...) as.formula(paste0("admitted~1",...,collapse='+'))
  
  # names for groups of features
  var_adm_chars <- paste('+',paste0(adm_chars,collapse='+'),sep='')
  var_locations <- paste('+',paste0(loc_durations,collapse='+'),sep='')
  var_demog <- paste('+',paste0(demog,collapse='+'),sep='')
  var_flow <- paste('+',paste0(flow,collapse='+'),sep='')
  var_labs <- ifelse(length(labs)==0,'',paste('+',paste0(labs,collapse='+'), sep=''))
  
  # formula 
  formula = class_formula(var_locations, var_flow, var_labs)
  
  # models
  gbt_model<-boost_tree(mode="classification") %>% set_engine("xgboost",scale_pos_weight=5)
  ## boost_tree
  # xgb_model <- boost_tree(mode="classification",
  #                         tree_depth = NULL,
  #                         mtry = NULL,
  #                         trees = NULL,
  #                         learn_rate = NULL,
  #                         loss_reduction = NULL,
  #                         min_n = NULL,
  #                         sample_size = NULL,
  #                         stop_iter = NULL) %>% set_engine("xgboost") 
  
  gbt_model %>% fit(formula,dm_train)  
  
})()


classification_metrics<-function(fit,data) {
  pred<-bind_cols(
    truth=data$admitted,
    predict(fit,data,type="class"), 
    predict(fit,data,type="prob")
  )
  print(paste0("Baseline=",mean(data$admitted==T)))
  print(pred %>% metrics(truth,.pred_class))
  print(pred %>% conf_mat(truth, .pred_class))
  print(pred %>% roc_auc(truth,.pred_TRUE))
  print(pred %>% roc_curve(truth,.pred_TRUE) %>% autoplot())
  return(pred)
}
classification_metrics(fit,dm_train %>% prep_for_ml())




# OR from tidyverse training


# set up model specification
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = 10, min_n = 20, 
  mtry = 150,        
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec



# set up model specification
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec


# set up hyper parameter grid
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), dm_train),
  learn_rate(),
  size = 1
)

xgb_grid


# set up workflow
xgb_wf <- workflow() %>%
  add_formula(admitted ~ .) %>%
  add_model(xgb_spec)

xgb_wf

# set up cross validation
set.seed(123)
dm_folds <- vfold_cv(dm_train, v = 5, strata = admitted)





set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = dm_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)



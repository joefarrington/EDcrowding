## Following the steps in https://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html

library(xgboost)
library(Matrix)
library(data.table)
library(vcd)
library(SHAPforxgboost)
library(dplyr)
library(tidymodels)
library(tidyverse)



# Create functions --------------------------------------------------------

# Function to one-hot encode factors

# from https://stackoverflow.com/questions/39905820/how-to-one-hot-encode-factor-variables-with-data-table
one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){
  # One-Hot-Encode unordered factors in a data.table
  # If cols = "auto", each unordered factor column in dt will be encoded. (Or specifcy a vector of column names to encode)
  # If dropCols=TRUE, the original factor columns are dropped
  # If dropUnusedLevels = TRUE, unused factor levels are dropped
  
  # Automatically get the unordered factor columns
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  # Build tempDT containing and ID column and 'cols' columns
  tempDT <- dt[, cols, with=FALSE]
  tempDT[, ID := .I]
  setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
  for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
  
  # One-hot-encode
  if(dropUnusedLevels == TRUE){
    newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = T, fun = length)
  } else{
    newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = F, fun = length)
  }
  
  # Combine binarized columns with the original dataset
  result <- cbind(dt, newCols[, !"ID"])
  
  # If dropCols = TRUE, remove the original factor columns
  if(dropCols == TRUE){
    result <- result[, !cols, with=FALSE]
  }
  
  return(result)
}


# Load data ---------------------------------------------------------------



load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-23.rda")


dm <- matrix_60 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  select(-mrn, -csn_old, -ED_duration_final
  ) %>% mutate(sex = as.factor(sex))
# %>% 
#   select(adm, age, sex, visited_Waiting:visited_MAJORS, visited_UTC)
# select(adm, age, sex, quarter, day_of_week,
#        weekend, night, time_of_day, days_since_last_visit, 
#        num_prior_visits, num_ED_rows, visited_Waiting:visited_MAJORS, visited_UTC)



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

# dm_recipe <- recipe(adm ~ ., 
#                     data = dm_train_train
# ) %>% 
#  # update_role(csn, new_role = "id") %>% 
#   step_mutate(adm = as.factor(adm)) %>% 
#   step_mutate(hour_of_arrival = as.factor(hour_of_arrival)) %>% 
#   step_mutate(month = as.factor(month)) %>% 
#   #  step_mutate(year = as.factor(year)) %>% 
#   step_mutate(weekend = as.factor(weekend)) %>% 
#   step_mutate(night = as.factor(night)) %>%
#   step_zv(all_predictors()) %>% 
#   prep()
# 
# 
# proc_dm_train_train <- dm_recipe %>% bake(
#   dm_train_train
# )
# 
# 
# proc_dm_train_val <- dm_recipe %>% bake(
#   dm_train_val
# )


# Trying with base package ------------------------------------------------



# note - I took sex out as its a dummy variable and I haven't coded it yet
proc_dm_train_train <- data.table(dm_train_train %>% select(-csn), keep.rownames = FALSE)
proc_dm_train_train <- one_hot(cols = "auto", dt = proc_dm_train_train,  dropUnusedLevels=TRUE)

y_var <- "adm"

dataX <- as.matrix(proc_dm_train_train[,-..y_var])
output_vector = as.numeric(proc_dm_train_train[,adm] == TRUE) # making sure they are coded correctly

# according to xgboost help, set booster which booster to use, can be gbtree or gblinear. Default: gbtree.
bst <- xgboost(data = dataX, 
               label = output_vector, 
               scale_pos_weight = 1,
               nrounds = 5, 
               objective = "binary:logistic")


# Look at training set performance ----------------------------------------



pred <- predict(bst, dataX)

pred_xgb_base <-bind_cols(
  truth=as.factor(proc_dm_train_train$adm),
  predict(bst, dataX)
)
colnames(pred_xgb_base) <- c("truth", ".pred_TRUE")
pred_xgb_base <- pred_xgb_base  %>% mutate(.pred_class = ifelse(.pred_TRUE > .5, TRUE, FALSE))
pred_xgb_base <- pred_xgb_base  %>% mutate(.pred_class = as.factor(.pred_class))


pred_xgb_base %>% roc_auc(truth,.pred_TRUE, event_level = "second")
pred_xgb_base %>% metrics(truth,.pred_class)
pred_xgb_base %>% conf_mat(truth, .pred_class)
pred_xgb_base %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()


# Look at validation set performance --------------------------------------

proc_dm_train_val <- data.table(dm_train_val %>% select(-csn), keep.rownames = FALSE)
proc_dm_train_val <- one_hot(cols = "auto", dt = proc_dm_train_val,  dropUnusedLevels=TRUE)


dataX_val <- as.matrix(proc_dm_train_val[,-..y_var])
output_vector = as.numeric(proc_dm_train_val[,adm] == TRUE) # making sure they are coded correctly

pred_xgb_base_val <-bind_cols(
  truth=as.factor(proc_dm_train_val$adm),
  predict(bst, dataX_val)
)
colnames(pred_xgb_base_val) <- c("truth", ".pred_TRUE")
pred_xgb_base_val <- pred_xgb_base_val  %>% mutate(.pred_class = ifelse(.pred_TRUE > .5, TRUE, FALSE))
pred_xgb_base_val <- pred_xgb_base_val  %>% mutate(.pred_class = as.factor(.pred_class))


pred_xgb_base_val %>% roc_auc(truth,.pred_TRUE, event_level = "second")
pred_xgb_base_val %>% metrics(truth,.pred_class)
pred_xgb_base_val %>% conf_mat(truth, .pred_class)
pred_xgb_base_val %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()


# Shap values -------------------------------------------------------------



shap_values <- shap.values(xgb_model = bst, X_train = dataX)

# to get values out of this: 
shap_values$mean_shap_score
shap_values$shap_score

mean(shap_values$shap_score$age) # doesn't equal shap_values$mean_shap_score for age ???

shap_long <- shap.prep(xgb_model = bst, X_train = dataX) # worked this time!

# shap_long[mean_value > .005]
# 
# shap_summ <- as_tibble(shap_long) %>% group_by(variable) %>% summarise(mean_value = mean(mean_value))

shap.plot.summary(shap_long, dilute = 10)

shap.plot.summary.wrap1(mod, X = dataX)


# Trying to implement insights from Scott Lundberg blog -------------------
# https://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html

# get base importances from xgboost
# this only seems to return 27 features
importance <- as_tibble(xgb.importance(feature_names = colnames(dataX), model = bst))

shap_tibble <- as_tibble(labels(shap_values$mean_shap_score)) %>% rename(Feature = value) %>% 
  bind_cols(Mean_Shap = shap_values$mean_shap_score)

importance <- importance %>% left_join(shap_tibble)

# add shap values


importance %>%  
  pivot_longer(Gain:Mean_Shap, names_to = "importance_type", values_to = "values") %>% 
  mutate(Feature = fct_reorder(Feature, values)) %>% 
  ggplot(aes(x = Feature, y = values, fill = importance_type)) + geom_bar(stat = "identity") +
  facet_wrap( ~ importance_type) + coord_flip() + theme(legend.position = "none") +
  labs(title = "Model importances for 60 min timeslice excluding admission characteristics - Post Surge1")



# Other code that might be useful -----------------------------------------



# from https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# I was looking for hot do do tuning and found this which only tunes by number of rounds
params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

bstcv <- xgb.cv(data = dataX, 
               label = output_vector, 
               params = params, 
               nfold = 5, 
               showsd = T, 
               stratified = T, 
               print_every_n = 10, 
               early_stopping_rounds = 80,
               maximize = F,
               nrounds = 100)

# best iteration error was 18
min(bstcv$evaluation_log$test_error_mean)

# running on test set to check test set accuracy
bstcv_test <- xgb.train (params = params, data = dataX, 
                   label = output_vector, 
                   nrounds = 18, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, 
                   early_stopping_rounds = 10,
                   maximize = F , 
                   eval_metric = "error")




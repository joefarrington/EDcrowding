# About this file
# ==============

# Runs logistic regression 


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)
library(caret)



# load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-23.rda")


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
                    # data = dm_train_train %>% 
                    # 
                    # select(-starts_with("fs_"),
                    #        -starts_with("l_")
                    #)
                    
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
  dm_train_val)


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
formula <- class_formula(var_demog, var_locations)



# explore variables -------------------------------------------------------

# 
# 
# x <- Smarket[,1:8]
# y <- Smarket[,9]
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# featurePlot(x=x, y=y, plot="density", scales=scales)


# fit GLM model -----------------------------------------------------------
# code from there: https://www.datacamp.com/community/tutorials/logistic-regression-R

# note - I had to remove the 1 at the beginning of the formula to get this to work

# removed + days_since_last_visit + num_prior_visits + all lab results as these were created probabilities of 1 or 0
# removed hour_of_arrival as same as time_of_day
# removed month - covered by quarter
# removed day of week
# removed fs_num_bp_sys + fs_num_resp_rate


glm.fit <- glm(adm ~  age + sex + gt70+ quarter + 
                 weekend + night + time_of_day  + visited_Waiting + visited_RAT + 
                 visited_RESUS + visited_MAJORS + visited_DIAGNOSTICS + visited_UTC + 
                 visited_TAF + visited_SAA + num_ED_rows+ 
                 num_fs_results + has_fs_results + 
                 num_fs_types + num_fs_events + fs_num_bp_dia + 
                 fs_num_heart_rate + fs_num_o2_sat + fs_num_temp + fs_num_acvpu + 
                 fs_num_news  + fs_o2_sat_lt90 + fs_o2_sat_lt94 + 
                 fs_news_medium + fs_news_high + num_lab_results + has_lab_results + 
                 num_lab_types + num_lab_events ,
               data = proc_dm_train_train,
               family = binomial)

glm_stats <- summary(glm.fit)
coeff <- glm_stats$coefficients
vars <- rownames(coeff)

glm_coeff <- as_tibble(coeff)
glm_coeff$vars <- rownames(coeff)

glm_coeff <- glm_coeff %>% select(vars, everything())



# On training set -------------------------------------------------------------

pred_glm_train <-bind_cols(
  truth=proc_dm_train_train$adm,
  predict(glm.fit,newdata = proc_dm_train_train,type="response"),
)
colnames(pred_glm_train) <- c("truth", ".pred_TRUE")
pred_glm_train <- pred_glm_train  %>% mutate(.pred_class = ifelse(.pred_TRUE > .5, TRUE, FALSE))
pred_glm_train <- pred_glm_train  %>% mutate(.pred_class = as.factor(.pred_class))


pred_glm_train %>% roc_auc(truth,.pred_TRUE, event_level = "second")
pred_glm_train %>% metrics(truth,.pred_class)
pred_glm_train %>% conf_mat(truth, .pred_class)
pred_glm_train %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()


# On validation set -------------------------------------------------------


pred_glm <-bind_cols(
  truth=proc_dm_train_val$adm,
  predict(glm.fit,newdata = proc_dm_train_val,type="response"),
)
colnames(pred_glm) <- c("truth", ".pred_TRUE")
pred_glm <- pred_glm  %>% mutate(.pred_class = ifelse(.pred_TRUE > .5, TRUE, FALSE))
pred_glm <- pred_glm  %>% mutate(.pred_class = as.factor(.pred_class))


pred_glm %>% roc_auc(truth,.pred_TRUE, event_level = "second")
pred_glm %>% metrics(truth,.pred_class)
pred_glm %>% conf_mat(truth, .pred_class)
pred_glm %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()


outFile <- paste0("EDcrowding/predict-admission/data-output/logistic_pred_60-mins_val_set_",today(),".rda")
save(pred_glm, file = outFile)

# With tidy models --------------------------------------------------------

logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  translate()

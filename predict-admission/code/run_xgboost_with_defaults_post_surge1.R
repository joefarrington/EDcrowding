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

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-09.rda")


dm <- matrix_60 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  select(-mrn, -csn_old, -ED_duration_final
  ) %>% 
  select(adm, age, sex, gt70, everything())


# Train test split --------------------------------------------------------

set.seed(123)
dm_split <- initial_split(dm, strata = adm, prop = 3/4)
dm_train <- training(dm_split)
# dm_test <- testing(dm_split)


# Pre-processing ----------------------------------------------------------

dm_recipe <- recipe(adm ~ ., 
                    data = dm_train
                      # data = dm_train %>% 
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


proc_dm_train <- dm_recipe %>% bake(
  dm_train
    # dm_train %>%
    # 
    # select(-starts_with("fs_"), -starts_with("l_"))
)


# Cross validation --------------------------------------------------------

set.seed(2020)
dm_folds <- proc_dm_train %>%  #  not sure if this shoudl be dm_train or proc_dm_train
  bake(dm_recipe, new_data = .) %>%
  rsample::vfold_cv(v = 5)


# Set up formula ----------------------------------------------------------

arrchars = colnames(proc_dm_train %>% select(hour_of_arrival:num_ED_rows))
locdurations = colnames(proc_dm_train)[grep("^mins_", colnames(proc_dm_train))]
locvisited = colnames(proc_dm_train)[grep("^visited_", colnames(proc_dm_train))]
demog = c('age','sex', 'gt70')
counts = colnames(proc_dm_train %>% select(num_fs_results:l_num_ESR))
# flow_values = colnames(proc_dm_train %>% select(fs_min_bp_sys:fs_latest_resp_rate))
# lab_values = colnames(proc_dm_train %>% select(l_min_BA:l_latest_APTM))

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
  set_engine("xgboost",scale_pos_weight=5) %>%  
  set_mode("classification")

xgb_wf <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_formula(formula)


# Fit model ---------------------------------------------------------------

xgb_fit <- fit(xgb_wf, data = proc_dm_train)

# get predictions
pred<-bind_cols(
  truth=proc_dm_train$adm,
  predict(xgb_fit,proc_dm_train,type="class"),
  predict(xgb_fit,proc_dm_train,type="prob")
)


outFile <- paste0("EDcrowding/predict-admission/data-output/xgb_pred_post_surge1_60-mins_default-params",today(),".rda")
save(pred, file = outFile)



# Look at results ---------------------------------------------------------


# Look at variable importance 

p <- xgb_fit %>%
  fit(data = proc_dm_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point", num_features = 20)

p

save_chart("vip_post_surge1_60-mins_default_params",
           p +
             labs(title = "Variable importance with default params on 60 min design matrix",
                  x = "Variable",
                  y = "Importance")
           
)

# show results

print(pred %>% metrics(truth,.pred_class))
print(pred %>% conf_mat(truth, .pred_class))
print(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second"))

save_chart("AUC_post_surge1_60-mins_default_params",
           print(pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()) +
             annotate("text", x = .9, y = .00, label = paste0("Area under ROC: ",
                                                              round(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second") %>% select(.estimate),2)))
)

# Look at confusion matrix ------------------------------------------------

cm <- pred %>% conf_mat(truth, .pred_class) 

save_chart("conf-mat_post_surge1_60-mins_tune-default-params",
           
           cm %>% autoplot(type = "heatmap") + 
             labs(title = "Confusion matrix with default params on 60 min design matrix") +theme(text = element_text(size=rel(3.5)))
)

# Try changing the probability threshold

pred <- pred %>% 
  mutate(.pred_TRUE.6 = factor(case_when(.pred_TRUE>0.6 ~ TRUE,
                                         TRUE ~ FALSE)),
         .pred_TRUE.7 = factor(case_when(.pred_TRUE>0.7 ~ TRUE,
                                         TRUE ~ FALSE)),
         .pred_TRUE.75 = factor(case_when(.pred_TRUE>0.75 ~ TRUE,
                                          TRUE ~ FALSE)),
         .pred_TRUE.8 = factor(case_when(.pred_TRUE>0.8 ~ TRUE,
                                         TRUE ~ FALSE)),
         .pred_TRUE.9 = factor(case_when(.pred_TRUE>0.9 ~ TRUE,
                                         TRUE ~ FALSE)))



cm75 <- pred %>% conf_mat(truth, .pred_TRUE.75) 

save_chart("conf-mat-75-post_surge1_60-mins_tune-default-params",
           
           cm75 %>% autoplot(type = "heatmap") + 
             labs(title = "Confusion matrix with default params on 60 min design matrix and threshold of .75") +theme(text = element_text(size=rel(3.5)))
)




# Explore incorrect predictions -------------------------------------------

# calculate Brier score
pred <- pred %>% 
  mutate(brier = (.pred_TRUE - (as.numeric(truth)-1))^2)

# add preds to dm
dm_with_pred <- proc_dm_train %>% bind_cols(pred) %>% 
  select(adm, .pred_TRUE, brier, everything()) %>% arrange(desc(brier))

dm_false_positive <- dm_with_pred %>% 
  filter(adm == "FALSE", .pred_TRUE>0.75) 



# Try changing the probability threshold

pred <- pred %>% 
  mutate(.pred_TRUE.6 = factor(case_when(.pred_TRUE>0.6 ~ TRUE,
                                  TRUE ~ FALSE)),
         .pred_TRUE.7 = factor(case_when(.pred_TRUE>0.7 ~ TRUE,
                                         TRUE ~ FALSE)),
         .pred_TRUE.75 = factor(case_when(.pred_TRUE>0.75 ~ TRUE,
                                         TRUE ~ FALSE)),
         .pred_TRUE.8 = factor(case_when(.pred_TRUE>0.8 ~ TRUE,
                                         TRUE ~ FALSE)),
         .pred_TRUE.9 = factor(case_when(.pred_TRUE>0.9 ~ TRUE,
                                         TRUE ~ FALSE)))

pred %>% conf_mat(truth, .pred_TRUE.8) 

print(pred %>% metrics(truth,.pred_TRUE.75))
print(pred %>% conf_mat(truth, .pred_TRUE.75))



# Explore results ---------------------------------------------------------


resus_false <- dm_false_positive %>% 
  filter(mins_RESUS > 15) %>% select(csn) %>% left_join(ED_csn_summ)

b <- resus_false %>% select(mrn) %>% distinct() %>%  
  left_join(bed_moves_all)

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

dm_with_pred  %>% 
  ggplot(aes(x = adm, y = num_prior_visits)) + geom_boxplot() 

dm_with_pred  %>% 
  group_by(adm, night) %>% summarise(tot = n()) 

dm_with_pred  %>% 
  ggplot(aes(x = num_fs_results, y = brier, col = adm)) + geom_point() +
  labs(title = "XXX by Brier score",
       y = "Brier score (0 is best score possible)",
       colour = "Admitted") +
  theme(legend.position = "bottom")
  
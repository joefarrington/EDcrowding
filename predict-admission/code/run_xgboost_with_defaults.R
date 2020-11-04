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
registerDoParallel(cores = 4)

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
  step_mutate(year = as.factor(year)) %>% 
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
formula <- class_formula(var_demog, var_arrchars, var_locations, var_counts, var_flow, var_labs)


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


outFile <- paste0("EDcrowding/predict-admission/data-output/xgb_pred_all_60-mins_default-params",today(),".rda")
save(pred, file = outFile)



# Look at results ---------------------------------------------------------


# Look at variable importance 

p <- xgb_fit %>%
  fit(data = proc_dm_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

p

save_chart("vip_demo-arrchars-locdurations-counts_60-mins_default_params",
           p +
             labs(title = "Variable importance with default params on 60 min design matrix",
                  x = "Variable",
                  y = "Importance")
           
)

# show results

print(pred %>% metrics(truth,.pred_class))
print(pred %>% conf_mat(truth, .pred_class))
print(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second"))

save_chart("AUC_demo-arrchars-locdurations-counts_60-mins_default_params",
           print(pred %>% roc_curve(truth,.pred_TRUE, event_level = "second") %>% autoplot()) +
             annotate("text", x = .9, y = .00, label = paste0("Area under ROC: ",
                                                              round(pred %>% roc_auc(truth,.pred_TRUE, event_level = "second") %>% select(.estimate),2)))
)

# Look at confusion matrix ------------------------------------------------

cm <- pred %>% conf_mat(truth, .pred_class) 

save_chart("conf-mat_demo-arrchars-locdurations-counts_60-mins_tune-default-params",
           
           cm %>% autoplot(type = "heatmap") + 
             labs(title = "Confusion matrix with default params on 60 min design matrix") +theme(text = element_text(size=rel(3.5)))
)



# Explore incorrect predictions -------------------------------------------

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

pred %>% conf_mat(truth, .pred_TRUE.75) 

print(pred %>% metrics(truth,.pred_TRUE.75))
print(pred %>% conf_mat(truth, .pred_TRUE.75))

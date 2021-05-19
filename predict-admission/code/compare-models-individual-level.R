
# Compare with and without location features ------------------------------



load("~/EDcrowding/predict-admission/data-output/xgb_scores_2021-03-29_aop.rda")
aop = scores

load("~/EDcrowding/predict-admission/data-output/xgb_scores_2021-03-29_alop.rda")
alop = scores

scores = bind_rows(alop, aop)


# Plots comparing with and without location features ----------------------


s = data.table(scores[tsk_ids == "val"  & model_features == model_features]  %>%
                 pivot_longer(logloss) %>% select(model_features, timeslice, name, value, tuning_round, dttm))


# all rounds
s[, .SD[which.max(dttm)], by = list(model_features, timeslice, tuning_round)] %>%
  filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>% 
  mutate(model_features = factor(model_features, levels = c("aop", "alop"), labels = c("without location", "with location"))) %>% 
  ggplot(aes(x = factor(tuning_round, levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")), y = value,
              colour = model_features, group = model_features)) +
  geom_line() + facet_grid(. ~ timeslice) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title = "Log loss values after each round of tuning - comparing models with and without location features", 
       x = "Tuning round", 
       y = "Log loss value", 
       colour = "Model") +
  theme(legend.position = "bottom")


# without base and reduce_lr rounds
s[, .SD[which.max(dttm)], by = list(model_features, timeslice, tuning_round)] %>%
  filter(tuning_round %in% c("nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha")) %>% 
  mutate(model_features = factor(model_features, levels = c("aop", "alop"), labels = c("without location", "with location"))) %>% 
  ggplot(aes(x = factor(tuning_round, levels = c("nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha")), y = value,
             colour = model_features, group = model_features)) +
  geom_line() + facet_grid(. ~ timeslice) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title = "Log loss values after each round of tuning - comparing models with and without location features", 
       x = "Tuning round", 
       y = "Log loss value", 
       colour = "Model") +
  theme(legend.position = "bottom")


# Final parameter values --------------------------------------------------


params = scores[tsk_ids == "val" & 
                  tuning_round == "alpha",
                .SD[which.min(logloss)], by = list(model_features, timeslice)]



# Comparing XGB with RF ---------------------------------------------------



load("~/EDcrowding/predict-admission/data-output/rf_scores_2021-04-14.rda")
rf_scores = scores

rf_scores =  rf_scores[tsk_ids == "val" & tuning_round == "sample.fraction" & model_features == model_features
                       , .SD[which.min(logloss)], by = list(timeslice)]

load("~/EDcrowding/predict-admission/data-output/xgb_scores_2021-04-19.rda")
xgb_scores = scores

xgb_scores = xgb_scores[tsk_ids == "val" & tuning_round == "reduce_lr" & model_features == model_features
                        , .SD[which.min(logloss)], by = list(timeslice)]

# Process -----------------------------------------------------------------

p1 = bind_rows(rf_scores[, .(timeslice, tsk_ids, model_features, logloss, bbrier, auc, classifier = "RF" )],
               xgb_scores[, .(timeslice, tsk_ids, model_features, logloss, bbrier, auc, classifier = "XGB" )]) %>% 
  ggplot(aes(x = timeslice, y = logloss, col = classifier, group = classifier)) + geom_line() + geom_point() +
  labs(title = "Comparing logloss results by timeslice for XGBoost and Random Forest")

p2 = bind_rows(rf_scores[, .(timeslice, tsk_ids, model_features, logloss, bbrier, auc, classifier = "RF" )],
               xgb_scores[, .(timeslice, tsk_ids, model_features, logloss, bbrier, auc, classifier = "XGB" )]) %>% 
  ggplot(aes(x = timeslice, y = auc, col = classifier, group = classifier)) + geom_line() + geom_point() +
  labs(title = "Comparing AUC results by timeslice for XGBoost and Random Forest")

library(gridExtra)
grid.arrange(p1, p2,
             ncol = 1, nrow = 2)


# Compare pre and post Covid ----------------------------------------------

load("~/EDcrowding/predict-admission/data-output/xgb_alop_scores_2021-05-18.rda")

scores[tsk_ids == "val" & tuning_round == "final_preds" & dttm > '2021-05-18 18:16:14'] %>% 
  pivot_longer(c(logloss, auc, prauc)) %>%  filter(name != "bbrier") %>% 
  ggplot(aes(x = timeslice, y = value, group = dataset, col = dataset)) + 
    geom_point() + geom_line() + facet_wrap(name~., ncol = 1, scales = "free_y")


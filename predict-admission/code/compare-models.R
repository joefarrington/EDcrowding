



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

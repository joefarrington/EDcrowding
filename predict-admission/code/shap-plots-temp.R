


# I managed to create mean shap values by exluding the adm label which otherwise causes an error
shap_values = shap.values(learner$model, dm360p[tsk_train_ids, 3:ncol(dm360p)])

shap_tibble <- as_tibble(labels(shap_values$mean_shap_score)) %>% rename(Feature = value) %>% 
  bind_cols(Mean_Shap = shap_values$mean_shap_score)

importance %>% left_join(shap_tibble) %>% 
  pivot_longer(Gain:Mean_Shap, names_to = "importance_type", values_to = "values") %>% 
  mutate(Feature = fct_reorder(Feature, values)) %>% 
  ggplot(aes(x = Feature, y = values, fill = importance_type)) + geom_bar(stat = "identity") +
  facet_wrap( ~ importance_type) + coord_flip() + theme(legend.position = "none") +
  labs(title = "Model importances for 60 min timeslice excluding admission characteristics - Post Surge1")


# train leaner on task

pred = as.data.table(learner$predict(tsk, row_ids = tsk_val_ids))
x = merge(dm360p[tsk_val_ids], pred, by = row_id)
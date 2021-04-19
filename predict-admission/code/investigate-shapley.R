# toy example with penguins data

data("penguins", package = "mlr3data")
str(penguins)
set.seed(1)


penguins = na.omit(penguins)
task_peng = TaskClassif$new(id = "Species", backend = penguins, target = "species")


learner2 = mlr_learners$get("classif.ranger")
learner2$predict_type = "prob"
learner2$train(task_peng)
learner2$model

x = penguins[which(names(penguins) != "species")]
model2 = Predictor$new(learner2, data = penguins, y = "species")
x.interest = data.frame(penguins[1, ])
shapley = Shapley$new(model, x.interest = x.interest)
plot(shapley)


# trying IML

dm480p
x_ = data.frame(dm480p[task480_val_ids])[which(names(dm480p) != c("adm", "csn"))]

library(iml)
model = Predictor$new(model = learner, data = x_, y = "adm")
# NOTE - IML doesn't work becuase it can't deal with missing values


# trying DALEX

library(DALEX)
library(DALEXtra)
# create explainer



name_ts <- paste0("dm", ts_, "p")
ts = get(name_ts)
y_ = ts[tsk_val_ids, adm]==1 # y should be numeric

data_ = ts[tsk_val_ids]
data_[, adm:= NULL]



explain_ <- DALEXtra::explain_mlr3(model = learner,  
                           data = data_,
                           y = y_) # note y_ has value 1 for admitted and 2 for discharged

mp = model_parts(explain_)
plot(mp, max_vars = 12, show_boxplots = FALSE)

selected_variables <- c("a_prop_adm_from_ED", "a_beforeED",
                        "a_tod6", "a_num_prior_adm_after_ED")
# partial dependency
pd <- model_profile(explain_,
                         variables = selected_variables)$agr_profiles







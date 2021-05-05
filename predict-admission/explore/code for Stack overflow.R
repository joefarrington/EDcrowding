library(mlr3)
library(mlr3learners)

# generate imbalanced dataset
sigma1 <- diag(rep(2,3))
sigma2 <- diag(rep(5,3))
a <- MASS::mvrnorm(n=500,  mu=c(0,0,0), Sigma=sigma1); minor <- data.frame(a,Y="1")
b <- MASS::mvrnorm(n=9500, mu=c(1,1,1), Sigma=sigma2); major <- data.frame(b,Y="0")
data <- rbind(minor, major)
data$Y <- as.factor(data$Y)

# check the imbalanced nature of the data
table(data$Y)
sum(data$Y == 1)/sum(data$Y == 0)

# create task and learner
task = TaskClassif$new(id = "imbalanced", backend = data ,target="Y") 
learner = lrn("classif.ranger", predict_type = "prob")

# attempting to set class weight
# inspect the parameters 
learner$param_set$values
# returns ParamDBL for class.weights parameter

# use a double 
learner$param_set$values = insert_named(
  learner$param_set$values,
  list(
    "class.weights" = sum(data$Y == 1)/sum(data$Y == 0)
  )
)

# then when you try to run the model

set.seed(17L)
rr = resample(tsk, learner, rsmp("cv"), store_models = TRUE)

# you get this error
# Error in ranger::ranger(dependent.variable.name = task$target_names, data = task$data(),  : 
# Error: Number of class weights not equal to number of classes.


# trying ranger 
library(ranger)
dt <- data.frame(x = runif(100), y = factor(sample(0:1, 100, replace = TRUE)))
rr <- ranger(y ~ x, data = dt, class.weights = c(0.5, 0.95))

# trying ranger in MLR3

library(mlr3)
library(mlr3learners)
task = TaskClassif$new(id = "imbalanced", backend = dt ,target="y") 
learner = lrn("classif.ranger", predict_type = "prob")

learner$param_set$values = insert_named(
  learner$param_set$values, list("sample.fraction" = c(0.05, 0.95))
)

# Error in self$assert(xs) : 
#Assertion on 'xs' failed: class.weights: Must have length 1

# But if you run the model with a ParamDBL (a solo value) ranger in MLR3 returns an error
learner$param_set$values = insert_named(
  learner$param_set$values, list("class.weights" = 0.05)
)

# Error in ranger::ranger(dependent.variable.name = task$target_names, data = task$data(),  : 
# Error: Number of class weights not equal to number of classes.


# Trying balance classes --------------------------------------------------


library(ranger)
rf <- ranger(Species ~ ., iris, replace = TRUE, keep.inbag = TRUE, 
             sample.fraction = c(0.01, 0.1, 0.5))
inbag <- do.call(cbind, rf$inbag.counts)

colSums(inbag[iris$Species == "setosa", ]) # floor(0.01*150) = 1
colSums(inbag[iris$Species == "versicolor", ]) # floor(0.1*150) = 15
colSums(inbag[iris$Species == "virginica", ]) # floor(0.5*150) = 75

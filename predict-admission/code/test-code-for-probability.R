# trying out binomial distributions
x = seq(0, 100,1) # x is the number of patients
probs = dbinom(x, size = 100, .2) # prob of getting outcomes of interest in [size] trials
as_tibble(probs) %>% ggplot(aes(x = x, y = value)) +geom_point() # shows probs rising to 0.2 as the highest probability

as_tibble(probs) %>% ggplot(aes(x = value)) +geom_density()
# shows that most of the density is at zero, with small notches up to the max prob of 0.1



# trying with 1000
x = seq(0, 1000,1) # x is the number of patients
probs = dbinom(x, size = 1000, .2) # prob of getting outcomes of interest in [size] trials
as_tibble(probs) %>% ggplot(aes(x = x, y = value)) +geom_point() # shows probs rising to 0.2 as the highest probability
# with 1000, the prob of any one value are lower

# trying with 10
x = seq(0, 10,1) # x is the number of patients
probs = dbinom(x, size = 10, .2)


plot(density(probs))  # density plot of these probs; most are zero 
# density is the probabliity of choosing x 
density(probs) %>% ggplot(aes(x = x, y = value)) +geom_point()
as_tibble(probs) %>% ggplot(aes(x = x, y = value)) +geom_point() # shows probs rising to 0.2 as the highest probability


# Enoch's code
library(polynom)

y = polynomial(c(1,0))# 
for (n in 1:length(probs)){
  y = y*polynomial(c(1-probs[n],probs[n]))
}

coef(y) # get coefficients of prob generating function 
coef(y) %>% plot() # plot coefficeints

plot(density(probs))

# let's say the the actual number of admissions was 5 and the probs are distributed as if for a binomial distribution
# what do the  prob function suggest is the prob of getting 3 or less

pbinom(8, 10, .2)
pbinom(5, 10, .2) # prob of getting 5 or less
pbinom(3, 10, .2) # prob of getting 3 or less in 10 admissions
pbinom(2, 10, .2)
pbinom(1, 10, .2)
pbinom(0, 10, .2)

qbinom(.1, 10, .2)
qbinom(.3, 10, .2)
qbinom(.5, 10, .2)
qbinom(.6, 10, .2) # probably of 2 or less matches this probability of 0.6
qbinom(.8, 10, .2) # probably of 3 or less matches this probability of 0.8


# First attempt -----------------------------------------------------------



# Say we have 10 patients every day
# prob of admission is .2
# each day the probability distribution is the same - based on binomial
x = as_tibble(seq(0, 10,1)) %>% rename(num_adm = value)
x = x %>% mutate( probs = dbinom(num_adm, size = 10, .2))
x = x %>% mutate(cdf = cumsum(probs))
x %>% ggplot(aes(x = num_adm, y = cdf)) + geom_line() + 
  scale_x_continuous(breaks = seq(0,10,1))

cutoff_prob = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
cutoff_prob$model_lower = NA
cutoff_prob$model_upper = NA

for (j in 1:nrow(cutoff_prob)) {
  cutoff_prob$model_lower[j] = as.numeric(x %>% filter(cdf <= cutoff_prob$cutoff[j]) %>% summarise(max(cdf)))
  cutoff_prob$model_upper[j] = as.numeric(x %>% filter(cdf >= cutoff_prob$cutoff[j]) %>% summarise(min(cdf)))
}


# say we have 100 days when we collect data
# here is a randomly generated vector of real admissions
count = rbinom(100, 10, 0.2)
actual = as_tibble(count) %>% rename(num_adm = value)
actual %>% ggplot(aes(x = num_adm)) + geom_histogram()

actual = actual %>% mutate(cum = cumsum(num_adm))
actual %>% ggplot(aes(x = num_adm)) + stat_ecdf(geom = "step") + 
  scale_x_continuous(breaks = seq(0,10,1), lim = c(0,10))

# rather than using a density plot, I can use the frequencies of each number of admissions in the actual data
# to create a probability distribution for the actual data

df = actual %>% group_by(num_adm) %>% summarise(probs = n()/100) %>% mutate(dist = "actual") %>% mutate(cdf = cumsum(probs))
df = df %>% bind_rows(x %>% mutate(dist = "model"))

df %>% ggplot(aes(x = num_adm, y = cdf, col = dist)) + geom_line()


cutoff_prob$data_lower = NA
cutoff_prob$data_upper = NA

for (j in 1:nrow(cutoff_prob)) {
  cutoff_prob$data_lower[j] = as.numeric(df %>% filter(cdf <= cutoff_prob$cutoff[j]) %>% summarise(max(cdf)))
  cutoff_prob$data_upper[j] = as.numeric(df %>% filter(cdf >= cutoff_prob$cutoff[j]) %>% summarise(min(cdf)))
}


cutoff_prob %>% pivot_longer(model_lower:data_upper, names_to = "model", values_to = "value") %>% 
  ggplot(aes(x = cutoff, y = value, col = model)) + geom_line()



# Second attempt ----------------------------------------------------------



# Say we have 100 patients every day
# prob of admission is .2
# each day the probability distribution is the same - based on binomial
x = as_tibble(seq(0, 100,1)) %>% rename(num_adm = value)
x = x %>% mutate( probs = dbinom(num_adm, size = 100, .2))
x = x %>% mutate(cdf = cumsum(probs))
x %>% ggplot(aes(x = num_adm, y = cdf)) + geom_line() + 
  scale_x_continuous(breaks = seq(0,100,1))

cutoff_prob = as_tibble(seq(0,1,.01)) %>% rename(cutoff = value)
cutoff_prob$model_lower = NA
cutoff_prob$model_upper = NA

for (j in 1:nrow(cutoff_prob)) {
  cutoff_prob$model_lower[j] = as.numeric(x %>% filter(cdf <= cutoff_prob$cutoff[j]) %>% summarise(max(cdf)))
  cutoff_prob$model_upper[j] = as.numeric(x %>% filter(cdf >= cutoff_prob$cutoff[j]) %>% summarise(min(cdf)))
}


# say we have 150 days when we collect data
# here is a randomly generated vector of real admissions
count = rbinom(150, 100, 0.2)
actual = as_tibble(count) %>% rename(num_adm = value)
actual %>% ggplot(aes(x = num_adm)) + geom_histogram(binwidth = 1)

actual = actual %>% mutate(cum = cumsum(num_adm))
actual %>% ggplot(aes(x = num_adm)) + stat_ecdf(geom = "step") + 
  scale_x_continuous(breaks = seq(0,100,1), lim = c(0,100))

# rather than using a density plot, I can use the frequencies of each number of admissions in the actual data
# to create a probability distribution for the actual data

df = actual %>% group_by(num_adm) %>% summarise(probs = n()/150) %>% mutate(dist = "actual") %>% mutate(cdf = cumsum(probs))
df = df %>% bind_rows(x %>% mutate(dist = "model"))

df %>% ggplot(aes(x = num_adm, y = cdf, col = dist)) + geom_line()


cutoff_prob$data_lower = NA
cutoff_prob$data_upper = NA

for (j in 1:nrow(cutoff_prob)) {
  cutoff_prob$data_lower[j] = as.numeric(df %>% filter(cdf <= cutoff_prob$cutoff[j]) %>% summarise(max(cdf)))
  cutoff_prob$data_upper[j] = as.numeric(df %>% filter(cdf >= cutoff_prob$cutoff[j]) %>% summarise(min(cdf)))
}


# I get a stepped chart. Is the reason for this that all of my days have the same probability distributions
# whereas Enoch's are generated from an average of a lot of distributions? 
cutoff_prob %>% pivot_longer(model_lower:data_upper, names_to = "model", values_to = "value") %>% 
  ggplot(aes(x = cutoff, y = value, col = model)) + geom_line()




# Trying with my model's data ---------------------------------------------

library(polynom)

poly_prod = function(df){

  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-df$.pred_TRUE[n],df$.pred_TRUE[n]))
  }
  return(coef(y))
}


# creating a model distribution just for one day

load("~/EDcrowding/predict-admission/data-raw/pred_with_csn_60_2020-11-09.rda")

date_ = "2020-06-03"
df = pred_with_csn %>% filter(date == date_) %>% select(.pred_TRUE, truth)
num_adm = seq(0,nrow(df), 1) # make an array from 0 admissions to max admissions (ie all patients admitted)
pgf = poly_prod(df) # the probabilities of each of theses)

distr = bind_cols(num_adm = num_adm, probs = pgf)
distr = distr %>% mutate(cdf = cumsum(probs))

# the plot shows how far away the model's predictions are
distr %>% ggplot(aes(x = num_adm, y = cdf)) + geom_line() + 
   geom_vline(xintercept = sum(as.numeric(df$truth)-1), linetype="dotted", 
               color = "blue", size=1.5) + theme_classic() +
  labs(title = "Model probability density function for number of admissions 3 June 2020", 
       subtitle = "Dotted line shows actual number of admissions",
       y = "Probability that number admitted is less than or equal to x [P(X <= x)]",
       x = "Number of patients admitted [x]")

cutoff_cdf_at = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
cutoff_cdf_at$model_lower = NA
cutoff_cdf_at$model_upper = NA

for (j in 1:nrow(cutoff_cdf_at)) {
  cutoff_cdf_at$model_lower[j] = as.numeric(distr %>% filter(cdf <= cutoff_cdf_at$cutoff[j]) %>% summarise(max(num_adm)))
  cutoff_cdf_at$model_upper[j] = as.numeric(distr %>% filter(cdf >= cutoff_cdf_at$cutoff[j]) %>% summarise(min(num_adm)))
}



# creating an actual distribution for all days

actual = pred_with_csn %>% filter(!is.na(date)) %>% group_by(date) %>% summarise(num_adm = sum(as.numeric(truth)-1))
actual %>% ggplot(aes(x = num_adm)) + geom_histogram(binwidth = 1)

actual = actual %>% mutate(cum = cumsum(num_adm))
actual %>% ggplot(aes(x = num_adm)) + stat_ecdf(geom = "step") + 
  scale_x_continuous(lim = c(0,100)) + theme_classic() +
  labs(title = "Cumulative probability of admissions from 31 May 2020 for 94 days", 
       subtitle = "Calculatd from actual admissions",
       y = "Probability that number admitted is less than or equal to x [P(X <= x)]",
       x = "Number of patients admitted [x]")

# actual_prob <- actual %>%
#   group_by(num_adm) %>% summarise(freq = n())
# 
# actual_prob = actual_prob %>%  mutate(probs = freq/nrow(actual))  %>% mutate(cdf = cumsum(probs))
# actual_prob %>% ggplot(aes(x = num_adm, y = cdf)) + stat_ecdf(geom = "step") 

df_actual = actual %>% group_by(num_adm) %>% summarise(probs = n()/nrow(actual)) %>% mutate(dist = "actual") %>% mutate(cdf = cumsum(probs))


# Trying multiple days ----------------------------------------------------

# creating a model distribution for all days


df1 = pred_with_csn %>% filter(!is.na(date)) %>% select(date, .pred_TRUE, truth)

# get total number of patients, and total number of admissions by day
adm <- df1 %>% group_by(date) %>% summarise(tot_pat = n(), 
                                            num_adm = sum(as.numeric(truth)-1))


# save a probability distribution for each day, for each possible admission value 
# on any day, this ranges from 0 admissions to max possible admissions (ie all patients admitted)

distr_collect = tibble()

for (i in 1:nrow(adm)) {
  
  num_adm = seq(0,nrow(df1 %>% filter(date == adm$date[i])), 1) # make an array from 0 admissions to max admissions (ie all patients admitted)
  pgf = poly_prod(df1 %>% filter(date == adm$date[i])) # the probabilities of each of these
  
  distr = bind_cols( date = adm$date[i], num_adm = num_adm, probs = pgf)
  distr = distr %>% mutate(cdf = cumsum(probs))
  
  distr_collect <- distr_collect %>% bind_rows(distr)
  
}


# The correct way - within day ? ------------------------------------------

cutoff_cdf_at_collect = tibble()


for (i in 1:nrow(adm)) {
  
  distr = distr_collect %>% filter(date == adm$date[i])
  actual_adm = as.numeric(adm %>% filter(date == adm$date[i]) %>% select(num_adm))
  
  cutoff_cdf_at = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
  cutoff_cdf_at$date = adm$date[i]
  cutoff_cdf_at$actual_adm = actual_adm
  cutoff_cdf_at$model_lower = NA
  cutoff_cdf_at$model_upper = NA
  
  for (j in 1:nrow(cutoff_cdf_at)) {
    
    if (nrow(distr %>% filter(cdf <= cutoff_cdf_at$cutoff[j])) != 0) {
      cutoff_cdf_at$model_lower[j] = as.numeric(distr %>% filter(cdf <= cutoff_cdf_at$cutoff[j]) %>% summarise(max(num_adm)))
    } else {
      cutoff_cdf_at$model_lower[j] = 0
    }
    if (nrow(distr %>% filter(cdf >= cutoff_cdf_at$cutoff[j])) != 0) {
      cutoff_cdf_at$model_upper[j] = as.numeric(distr %>% filter(cdf >= cutoff_cdf_at$cutoff[j]) %>% summarise(min(num_adm)))
    } else {
      cutoff_cdf_at$model_upper[j] = 0
    }
  }
  
  cutoff_cdf_at_collect <- cutoff_cdf_at_collect %>% bind_rows(cutoff_cdf_at)
}


cutoff_cdf_at = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
cutoff_cdf_at$data_lower = NA
cutoff_cdf_at$data_upper = NA

for (j in 1:nrow(cutoff_cdf_at)) {
  if (nrow(df_actual %>% filter(cdf <= cutoff_cdf_at$cutoff[j])) != 0) {
    
    cutoff_cdf_at$data_lower[j] = as.numeric(df_actual %>% filter(cdf <= cutoff_cdf_at$cutoff[j]) %>% summarise(max(num_adm)))
  } else {
    cutoff_cdf_at$data_lower[j] = 0
  }
  if (nrow(df_actual %>% filter(cdf >= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_cdf_at$data_upper[j] = as.numeric(df_actual %>% filter(cdf >= cutoff_cdf_at$cutoff[j]) %>% summarise(min(num_adm)))
  } else {
    cutoff_cdf_at$data_upper[j] = 0
  }
}

# join to get actual values for each cutoff point
cutoff_cdf_at_collect <- cutoff_cdf_at_collect %>%  left_join(cutoff_cdf_at)


cutoff_cdf_at_collect <- cutoff_cdf_at_collect %>% 
  mutate(data_above_model_lower_bound = ifelse(model_lower > data_lower, TRUE, FALSE),
         model_below_lower_bound = ifelse(model_lower <= data_lower, TRUE, FALSE),
         model_above_upper_bound = ifelse(model_upper > data_upper, TRUE, FALSE),
         model_below_upper_bound = ifelse(model_upper <= data_upper, TRUE, FALSE))


cutoff_cdf_at_collect %>% group_by(cutoff) %>%  
  summarise(above_lower = mean(model_above_lower_bound),
            above_upper = mean(model_above_upper_bound),
            below_lower = mean(model_below_lower_bound),
            below_upper = mean(model_below_upper_bound)) %>% 
  pivot_longer(below_lower:below_upper, names_to = "proportion_under_bound", values_to = "value") %>% 
  ggplot(aes(x = cutoff, y = value, col = proportion_under_bound)) + geom_line()  + 
  scale_x_continuous(breaks = seq(0,1,.1))
  


# The incorrect way of doing it ??-------------------------------------------


# take the mean probability across all days (days with more patients will be more rare)

distr_mean <- distr_collect %>% group_by(num_adm) %>% summarise(probs = mean(probs, na.rm = TRUE)) %>% 
  mutate(cdf = cumsum(probs))

distr_mean %>% ggplot(aes(x = num_adm, y = cdf)) + geom_line() 


# this tibble combines the model cdf with the actual cumulative distribution at given levels of the cutoff, alpha

cutoff_cdf_at = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
cutoff_cdf_at$model_lower = NA
cutoff_cdf_at$model_upper = NA

for (j in 1:nrow(cutoff_cdf_at)) {
  if (nrow(distr_mean %>% filter(cdf <= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_cdf_at$model_lower[j] = as.numeric(distr_mean %>% filter(cdf <= cutoff_cdf_at$cutoff[j]) %>% summarise(max(num_adm)))
  } else {
    cutoff_cdf_at$model_lower[j] = 0
  }
  if (nrow(distr_mean %>% filter(cdf >= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_cdf_at$model_upper[j] = as.numeric(distr_mean %>% filter(cdf >= cutoff_cdf_at$cutoff[j]) %>% summarise(min(num_adm)))
  } else {
    cutoff_cdf_at$model_upper[j] = 0
  }
}



cutoff_cdf_at$data_lower = NA
cutoff_cdf_at$data_upper = NA

for (j in 1:nrow(cutoff_cdf_at)) {
  if (nrow(df_actual %>% filter(cdf <= cutoff_cdf_at$cutoff[j])) != 0) {
        
      cutoff_cdf_at$data_lower[j] = as.numeric(df_actual %>% filter(cdf <= cutoff_cdf_at$cutoff[j]) %>% summarise(max(num_adm)))
  } else {
    cutoff_cdf_at$data_lower[j] = 0
  }
  if (nrow(df_actual %>% filter(cdf >= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_cdf_at$data_upper[j] = as.numeric(df_actual %>% filter(cdf >= cutoff_cdf_at$cutoff[j]) %>% summarise(min(num_adm)))
  } else {
    cutoff_cdf_at$data_upper[j] = 0
  }
}

cutoff_cdf_at %>% pivot_longer(model_lower:data_upper, names_to = "model", values_to = "value") %>% 
  separate(model, sep = "_", into = c("source", "bound")) %>% 
  ggplot(aes(x = value, y = cutoff, linetype = source, col = bound)) + geom_point() + geom_line()  + theme_classic() +
  labs(title = "Cumulative probability of admissions from 31 May 2020 for 94 days", 
       subtitle = "Probabilities have been averaged across all of the 94 days",
       y = "Probability that number admitted is less than or equal to x [P(X <= x)]",
       x = "Number of patients admitted [x]")


# the plot compares model cdf with the actual cumulative distribution at given levels of the cutoff, alpha


cutoff_prob = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
cutoff_prob$model_lower = NA
cutoff_prob$model_upper = NA

for (j in 1:nrow(cutoff_prob)) {
  if (nrow(distr_mean %>% filter(cdf <= cutoff_cdf_at$cutoff[j])) != 0) {
    
    cutoff_prob$model_lower[j] = as.numeric(distr_mean %>% filter(cdf <= cutoff_prob$cutoff[j]) %>% summarise(max(cdf))) 
  } else {
    cutoff_prob$model_lower[j] = 0
  }
  if (nrow(distr_mean %>% filter(cdf >= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_prob$model_upper[j] = as.numeric(distr_mean %>% filter(cdf >= cutoff_prob$cutoff[j]) %>% summarise(min(cdf)))
  } else {
    cutoff_prob$model_upper[j] = 1
  }
    
}



cutoff_prob$data_lower = NA
cutoff_prob$data_upper = NA

for (j in 1:nrow(cutoff_prob)) {
  if (nrow(df_actual %>% filter(cdf <= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_prob$data_lower[j] = as.numeric(df_actual %>% filter(cdf <= cutoff_prob$cutoff[j]) %>% summarise(max(cdf))) 
  } 
  else {
    cutoff_prob$data_lower[j] = 0
  }
  if (nrow(df_actual %>% filter(cdf >= cutoff_cdf_at$cutoff[j])) != 0) {
    cutoff_prob$data_upper[j] = as.numeric(df_actual %>% filter(cdf >= cutoff_prob$cutoff[j]) %>% summarise(min(cdf)))
  } else {
    cutoff_prob$data_upper[j] = 1
  }
    
}


cutoff_prob %>% pivot_longer(model_lower:data_upper, names_to = "model", values_to = "value") %>% 
  separate(model, sep = "_", into = c("source", "bound")) %>% 
  ggplot(aes(x = cutoff, y = value, linetype = source, col = bound)) + geom_point() + geom_line() + theme_classic() +
  labs(title = "Admission probability distribution evaluation", 
       subtitle = "Model probabilities have been averaged across all of the 94 days",
       y = "P(X <= x) <= alpha",
       x = "Threshold (alpha)") + 
  scale_x_continuous(breaks = seq(0,1,.1))

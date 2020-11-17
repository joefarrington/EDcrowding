### This version is using Martin's slides
library(dplyr)
library(lubridate)
library(polynom)
library(tidyverse)

# Create functions --------------------------------------------------------


poly_prod = function(df){
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-df$.pred_TRUE[n],df$.pred_TRUE[n]))
  }
  return(coef(y))
}


# Load predictions --------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/pred_with_csn_60_2020-11-09.rda")

df1 = pred_with_csn %>% filter(!is.na(date)) %>% select(date, .pred_TRUE, truth)

# get total number of patients, and total number of admissions by day
adm <- df1 %>% group_by(date) %>% summarise(tot_pat = n(), 
                                            num_adm = sum(as.numeric(truth)-1))

adm <- adm %>% mutate(fake_adm = num_adm + 20)

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


# Looking at predictions --------------------------------------------------


cutoff_cdf_at_mult_days = tibble()


for (i in 1:nrow(adm)) {
  
  distr = distr_collect %>% filter(date == adm$date[i])
  actual_adm = as.numeric(adm %>% filter(date == adm$date[i]) %>% select(num_adm))
  
  cutoff_cdf_at = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)
  cutoff_cdf_at$date = adm$date[j]
  cutoff_cdf_at$actual_adm = actual_adm
  
  for (j in 1:nrow(cutoff_cdf_at)) {
    
    cutoff_cdf_at$model_lower[j] = 
      nrow(distr %>% filter(date == adm$date[i], cdf < cutoff_cdf_at$cutoff[j]))  
    
    cutoff_cdf_at$data_lower[j] = 
      nrow(distr %>% filter(num_adm < adm$num_adm[i]))  
    
    if (j != nrow(cutoff_cdf_at)) {
      
      cutoff_cdf_at$model_upper[j] = 
        nrow(distr %>% filter(date == adm$date[i], cdf <= cutoff_cdf_at$cutoff[j+1]))
      
    } 
    else {
      cutoff_cdf_at$model_upper[j] = 
        nrow(distr %>% filter(date == adm$date[i], cdf <= cutoff_cdf_at$cutoff[j]))
    }
    
    cutoff_cdf_at$data_upper[j] =  
      nrow(distr %>% filter(num_adm < adm$num_adm[i]+1))  

  }
  
  cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% bind_rows(cutoff_cdf_at)
}


c = cutoff_cdf_at_mult_days  %>% 
  pivot_longer(model_lower:data_upper, names_to = "limit_of_cdf", values_to = "pred_adm") %>% 
  mutate(less_than_actual = ifelse(actual_adm < pred_adm , TRUE, FALSE)) %>% 
  group_by(cutoff, limit_of_cdf) %>% summarise(prop = mean(less_than_actual))

# Looking at actual data --------------------------------------------------



# data point from day i
adm$num_adm[i]

# look at datat point relative to its distribution

distr_collect %>% filter(date == adm$date[i]) %>% 
  ggplot(aes(x = num_adm, y = probs)) + geom_bar(stat = "identity") +
  geom_vline(xintercept = adm$num_adm[i], linetype="dotted", 
             color = "blue", size=1.5)

# where does each data point fall on its distribution? 
each_data_point_cdf = tibble()

# what is upper and lower limit of cdf for this admission value? 
for (i in (1:nrow(adm))) {
  data_point_cdf = distr_collect  %>% 
    filter(date == adm$date[i], num_adm == adm$fake_adm[i]) %>% select(date, num_adm, lower = cdf)
  
  data_point_cdf$upper = as.numeric(distr_collect  %>% 
    filter(date == adm$date[i], num_adm == adm$fake_adm[i]+1) %>% select(cdf))
  
  each_data_point_cdf = each_data_point_cdf  %>% bind_rows(data_point_cdf)
}

# proportion of instances where lower <= cut-off on cdf

cutoff_cdf_at = as_tibble(seq(0,1,.1)) %>% rename(cutoff = value)

for (j in 1:nrow(cutoff_cdf_at)) {
  cutoff_cdf_at$proportion_of_instance[j] = nrow(each_data_point_cdf %>% filter(lower <= cutoff_cdf_at$cutoff[j]))
}

cutoff_cdf_at %>% ggplot(aes(x = cutoff, y = proportion_of_instance)) + geom_line()


# for 
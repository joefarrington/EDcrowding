### This version is after talking ton Martin on afternoon of Tuesday 17 November
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

load("~/EDcrowding/predict-admission/data-output/xgb_pred_60-mins_tune-scale_pos_weight_val_set_2020-11-18.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")

pred_val <- pred_val %>% left_join(ED_csn_summ %>% select(csn, arrival_dttm)) %>% mutate(date = date(arrival_dttm))

df1 = pred_val  %>% select(date, .pred_TRUE, truth)

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



# Looking at predictions and incrementing by next cdf row --------------------------------------------------


cutoff_cdf_at_mult_days = tibble()


for (i in 1:nrow(adm)) {
  
  distr = distr_collect %>% filter(date == adm$date[i])
  actual_adm = as.numeric(adm %>% filter(date == adm$date[i]) %>% select(num_adm))
  
  alpha_increments = 0.05
  cutoff_cdf_at = as_tibble(seq(alpha_increments,1,alpha_increments)) %>% rename(cutoff = value)
  cutoff_cdf_at$date = adm$date[i]
  cutoff_cdf_at$actual_adm = actual_adm
  cutoff_cdf_at$model_lower_num_adm = NA
  cutoff_cdf_at$model_lower_cdf = NA
  cutoff_cdf_at$model_upper_num_adm = NA
  cutoff_cdf_at$model_upper_cdf = NA
  
  for (j in 1:nrow(cutoff_cdf_at)) {
    
    cutoff_cdf_at$model_lower_num_adm[j] = 
      nrow(distr %>% filter(date == adm$date[i], cdf < cutoff_cdf_at$cutoff[j]))  
    
    if (j != nrow(cutoff_cdf_at)) {
      
      cutoff_cdf_at$model_upper_num_adm[j] = cutoff_cdf_at$model_lower_num_adm[j] + 1 # This is correct

    }     else {
      cutoff_cdf_at$model_upper_num_adm[j] = 
        nrow(distr %>% filter(date == adm$date[i], cdf < cutoff_cdf_at$cutoff[j]))
    }
    
    # Enoch also looks up the cdf at the model threshold 
    # (although note that Enoch adds 1 to i to get the upper threshold, returning the next row in the cdf
    # rather than setting the higher band at the next alpha threshhold for the cdf
    
    cutoff_cdf_at$model_lower_cdf[j] = distr$cdf[cutoff_cdf_at$model_lower_num_adm[j]] 
    cutoff_cdf_at$model_upper_cdf[j] = distr$cdf[cutoff_cdf_at$model_upper_num_adm[j]]
    
  }
  
  cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% bind_rows(cutoff_cdf_at)
}


# to check the calculations of model_lower_cdf and model_upper_cdf,
# they should sum to the number of days covered by the model
# in today's meeting, Martin said he was apportioning the propabilities for each day 
# into a line from 0 to 1 - so its sum should be the number of days in the test
cutoff_cdf_at_mult_days %>% group_by(cutoff) %>% summarise(tot = sum(model_lower_cdf)) %>%  arrange(desc(tot))

# Enoch's code counts the number of days where the actual admission is less than the model threshold limits
cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% 
  mutate(actual_less_than_lower = ifelse(actual_adm < model_lower_num_adm, TRUE, FALSE),
         actual_less_than_upper = ifelse(actual_adm < model_upper_num_adm, TRUE, FALSE))


# he then divides by the number of days but he's doing this across all days
# therefore I will create a grouped version across all days

cutoff_cdf_normalised <- cutoff_cdf_at_mult_days %>% 
  group_by(cutoff) %>% summarise(actual_less_than_lower_limit = mean(actual_less_than_lower),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper),
                                 model_upper_limits = mean(model_upper_cdf),
                                 model_lower_limits = mean(model_lower_cdf))


cutoff_cdf_normalised %>% pivot_longer(actual_less_than_lower_limit:model_lower_limits, 
                                       names_to = "model", 
                                       values_to = "proportion") %>% 
  ggplot(aes(x = cutoff, y = proportion, col = model)) + geom_point() + geom_line()  + theme_classic() +
  labs(title = "Admission probability distribution evaluation", 
       y = "Proportion of instances <= X on cdf",
       x = "X")




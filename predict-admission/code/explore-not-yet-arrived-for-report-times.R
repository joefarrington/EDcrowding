# work out distribution of not yet arrived

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

summ[, in_set := case_when(presentation_time < '2019-11-19 00:00:00' ~ "Train",
                         presentation_time < '2019-12-13 00:00:00' ~ "Val",
                         presentation_time < '2020-03-19 00:00:00' ~ "Test",
                         presentation_time < '2020-12-01 00:00:00' ~ "Train",
                         presentation_time < '2020-12-29 00:00:00' ~ "Val",
                         TRUE ~ "Test",)]

# Generate time points  ------------------------------

start_of_set = as.POSIXct('2019-05-01', tz = "UTC")
end_of_set = as.POSIXct('2021-04-13', tz = "UTC")
next_dt = start_of_set

time_pts = POSIXct()
# each sample to be more than 12 hours and less than 24 hours after the previous one
while (next_dt < end_of_set) {
  next_pt <- next_dt + c(hours(6), hours(12), hours(16), hours(22))
  time_pts <- c(time_pts, next_pt)
  next_dt = next_dt + days(1)
}




# Summarise number not in who get admitted later --------------------------


not_in_ED_yet_all = data.table()

for (i in 1:length(time_pts)) {
  
  if (i %% 100 == 0) {
    print(paste("Processed ", i, " dates"))
  }
  
  for (time_window in c(2,3,4,6,8,12)) {
    
    not_in_ED_yet = data.table(time_window = time_window, 
                               N = summ[first_ED_admission >time_pts[i] & first_ED_admission < time_pts[i] + hours(time_window)
                         & first_outside_proper_admission < time_pts[i] + hours(time_window), .N])
    
    not_in_ED_yet$time_pt <- time_pts[i]
    
    not_in_ED_yet_all = bind_rows(not_in_ED_yet_all, not_in_ED_yet)
    
  }
}

not_in_ED_yet_all[, in_set := case_when(time_pt < '2019-11-19 00:00:00' ~ "Train",
                           time_pt < '2019-12-13 00:00:00' ~ "Val",
                           time_pt < '2020-03-19 00:00:00' ~ "Test",
                           time_pt < '2020-12-01 00:00:00' ~ "Train",
                           time_pt < '2020-12-29 00:00:00' ~ "Val",
                           TRUE ~ "Test",)]

not_in_ED_yet_all[, epoch := case_when(date(time_pt) >= '2020-03-19' ~ "Post",
                        TRUE ~ "Pre")]

# not_in_ED_yet_all %>% ggplot(aes(x = N)) + geom_histogram(binwidth = 1) + facet_wrap(time_window ~ ., scales = "free") +
#   labs(title = "Frequency distributions for patients who have not yet arrived and who are admitted within various time windows",
#        subtitle = paste("Based on", length(time_pts_train_post), "at 6:00, 12:00, 16:00 and 22:00 daily - pre and post COVID"),
#        x = "Number of patients admitted")


not_in_ED_yet_all[, weekend:= if_else(weekdays(time_pt, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
not_in_ED_yet_all[, time_of_report:= paste0(hour(time_pt), ":", substr(time_pt, 15, 16))]

not_in_ED_yet_all[time_window == 4, mean(N), by = .(weekend, time_of_report, epoch)] 


# compare pre and post COVID
not_in_ED_yet_all[, mean(N), by = .(time_window, weekend, time_of_report, epoch)] %>% 
  mutate(weekend = factor(weekend, levels = c(0,1), labels = c("Weekday", "Weekend")), 
           epoch = factor(epoch, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(x = factor(time_of_report, levels = c("6:00", "12:00", "16:00", "22:00")), y = V1, 
             group = weekend, col = weekend)) + geom_line() +
  facet_grid(epoch~ time_window) +
  labs(title = "Number of patients not yet arrived, who will be admitted in time window, all Epic time",
       y = "Number of patients",
       x = "Time of report")

# separate by training, validation and test sets - pre COVID
p1 = not_in_ED_yet_all[epoch == "Pre", mean(N), by = .(time_window, weekend, time_of_report, in_set)] %>% 
  mutate(weekend = factor(weekend, levels = c(0,1), labels = c("Weekday", "Weekend")), 
         in_set = factor(in_set, levels = c("Train", "Val", "Test"))) %>% 
  ggplot(aes(x = factor(time_of_report, levels = c("6:00", "12:00", "16:00", "22:00")), y = V1, 
             group = weekend, col = weekend)) + geom_line() +
  facet_grid(in_set~ time_window) +
  labs(title = "Patients not yet arrived, who will be admitted in time window, pre COVID",
       y = "Number of patients (mean)",
       x = "Time of report") +
  theme(legend.position = "bottom")

# separate by training, validation and test sets - post COVID
p2 = not_in_ED_yet_all[epoch == "Post", mean(N), by = .(time_window, weekend, time_of_report, in_set)] %>% 
  mutate(weekend = factor(weekend, levels = c(0,1), labels = c("Weekday", "Weekend")), 
         in_set = factor(in_set, levels = c("Train", "Val", "Test"))) %>% 
  ggplot(aes(x = factor(time_of_report, levels = c("6:00", "12:00", "16:00", "22:00")), y = V1, 
             group = weekend, col = weekend)) + geom_line() +
  facet_grid(in_set~ time_window) +
  labs(title = "Patients not yet arrived, who will be admitted in time window, post COVID",
       y = "Number of patients (mean)",
       x = "Time of report") +
  scale_y_continuous(limits = c(0,25)) +
  theme(legend.position = "bottom")

# library(gridExtra)
grid.arrange(p1, p2, nrow = 1)

# save data for reference in making predictions with Poisson 
# multiplying by 4 because num_obs is used to later to plot Poisson chart (not sure if right)
poisson_means = not_in_ED_yet_all[, .(poisson_mean = mean(N), num_obs = .N), 
                                  by = .(epoch, weekend, in_set, time_of_report, time_window)] 
save(poisson_means, file = "~/EDcrowding/predict-admission/data-output/poisson_means.rda")
# save(poisson_means, file = "~poisson_means.rda")
# Do again for pre COVID --------------------------------------------------


# Fit distribution --------------------------------------------------------

library(fitdistrplus)
# 
# poisson = fitdist(not_in_ED_yet_all[time_window == 8, N], 'pois', method = 'mle')
# pdist = dpois(1:21, lambda = poisson$estimate)
# pdist = pdist*length(time_pts_train)
# 

pmean = poisson_means[epoch == "Post" & time_window == 8 & time_of_report == "6:00" & weekend == 1
                      & in_set == "Train", poisson_mean]
num_obs = poisson_means[epoch == "Post" & time_window == 8 & time_of_report == "6:00" & weekend == 1
                        & in_set == "Train", num_obs]

pdist = dpois(0:21, lambda = pmean)
pdist = pdist*num_obs

not_in_ED_yet_all[time_window == 8] %>% ggplot() + geom_histogram(aes(x = N), binwidth = 1) + 
  stat_function(geom = "line", fun = dpois, args = list(lambda = 5.83), colour = "red", fill = NA, n = length(time_pts_train))

# can't work out how to superimpose Poisson on a histogram, so creating my own
not_in_ED_yet_all[time_window == 8, (V1 = .N), by = N] %>% ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity")

# rdist = rpois(length(time_pts_train), lambda = not_in_ED_yet_all[time_window == 8, mean(N)])
# pois = data.table(table(rdist))
# 
# pois = setnames(pois, "N", "Poisson")
# pois = setnames(pois, "rdist", "N")
# pois$N = as.numeric(pois$N)
# 
# plotdata = merge(data.table(N = seq(0, max(rdist), 1)), pois, all.x = TRUE)
# plotdata = merge(not_in_ED_yet_all[time_window == 8, (V1 = .N), by = N] , plotdata, all.x = TRUE, by = "N")
# 
# plotdata %>% ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity") +
#   geom_line(aes(x = N, y = Poisson))

plotdata_all = data.table()


# plot with Poisson line for post-COVID weekdays 

for (epoch_ in unique(poisson_means$epoch)) {
  
  for (in_set_ in unique(poisson_means$in_set)) {
    
    for (time_of_report_ in unique(poisson_means$time_of_report)) {
      
      for (weekend_ in c(0,1)) {
        
        for (tw in c(4, 8)) {
          
          pmean = poisson_means[epoch == epoch_ & time_window == tw & time_of_report == time_of_report_ & weekend == weekend_
                                & in_set == in_set_, poisson_mean]
          num_obs = poisson_means[epoch == epoch_ & time_window == tw & time_of_report == time_of_report_ & weekend == weekend_
                                  & in_set == in_set_, num_obs]
          
          pdist = dpois(0:25, lambda = pmean)
          
          # merge with actual distribution (home-made histogram)
          plotdata = data.table(N = (seq(1, length(pdist), 1))-1, poisson = pdist*num_obs, 
                                epoch = epoch_,
                                in_set = in_set_,
                                time_of_report = time_of_report_,
                                weekend = weekend_,
                                time_window = tw)
          plotdata = merge(plotdata, not_in_ED_yet_all[time_window == tw & epoch == epoch_ & in_set == in_set_ &
                                                         time_of_report == time_of_report_ & weekend == weekend_,
                                                       (V1 = .N), by = N] , all.x = TRUE, by = "N")
          plotdata$time_window = tw
          plotdata_all = bind_rows(plotdata_all, plotdata)
          
          
        }
        
      }
      
    }
    
  }
  
}



plotdata_all[epoch == "Post" & weekend == 0 & in_set == "Train"] %>% 
  mutate(time_of_report = factor(time_of_report, levels = c("6:00", "12:00", "16:00", "22:00"))) %>% 
  ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity") +
  geom_line(aes(x = N, y = poisson), color = "red")  + 
  facet_grid(time_window ~ time_of_report, scales = "free") +
  labs(title = "Frequency distributions compared with Poisson for patients not yet arrived, who will be admitted within 4 and 8 hours",
       subtitle = "Post COVID data - weekdays only ",
       x = "Number of patients admitted",
       y = "count")


plotdata_all[epoch == "Post" & weekend == 1 & in_set == "Train"] %>% 
  mutate(time_of_report = factor(time_of_report, levels = c("6:00", "12:00", "16:00", "22:00"))) %>% 
  ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity") +
  geom_line(aes(x = N, y = poisson), color = "red")  + 
  facet_grid(time_window ~ time_of_report, scales = "free") +
  labs(title = "Frequency distributions compared with Poisson for patients not yet arrived, who will be admitted within 4 and 8 hours",
       subtitle = "Post COVID data - weekends only ",
       x = "Number of patients admitted",
       y = "count")




# plot validation set using training set poisson

plotdata_val_all = data.table()

for (time_of_report_ in unique(poisson_means$time_of_report)) {
  
  for (weekend_ in c(0,1)) {
    
    for (tw in c(4, 8)) {
      
      pmean = poisson_means[epoch == "Post"  
                            & time_window == tw & time_of_report == time_of_report_ & weekend == weekend_
                            & in_set == "Train", # retrive the Poisson distribution for the training set
                            poisson_mean]
      num_obs = poisson_means[epoch == "Post" 
                              & time_window == tw & time_of_report == time_of_report_ & weekend == weekend_
                              & in_set == "Val", # and the number of observations in the validation set
                              num_obs]
      
      pdist = dpois(0:25, lambda = pmean)
      
      # merge with actual distribution (home-made histogram)
      plotdata = data.table(N = (seq(1, length(pdist), 1))-1, poisson = pdist*num_obs, 
                            time_of_report = time_of_report_,
                            weekend = weekend_,
                            time_window = tw)
      plotdata = merge(plotdata, not_in_ED_yet_all[time_window == tw & epoch == "Post" & in_set == "Val" &
                                                     time_of_report == time_of_report_ & weekend == weekend_,
                                                   (V1 = .N), by = N] , all.x = TRUE, by = "N")
      plotdata$time_window = tw
      plotdata_val_all = bind_rows(plotdata_val_all, plotdata)
      
      
    }
    
  }
  
}

plotdata_val_all[weekend == 1] %>% 
  mutate(time_of_report = factor(time_of_report, levels = c("6:00", "12:00", "16:00", "22:00"))) %>% 
  ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity") +
  geom_line(aes(x = N, y = poisson), color = "red")  + 
  facet_grid(time_window ~ time_of_report, scales = "free") +
  labs(title = "Frequency distributions compared with Poisson for patients not yet arrived, who will be admitted within 4 and 8 hours",
       subtitle = "Post COVID data - weekdays; applying training set Poisson means to validation set",
       x = "Number of patients admitted",
       y = "count")

# this is how to get a probability of 0 admissions given the lamda provided
s = ppois(1, lambda = not_in_ED_yet_all[time_window == 8, mean(N)])
not_in_ED_yet_all[, mean(N), by = time_window]

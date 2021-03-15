
# Process Ken's predictions -----------------------------------------------

# Take input of predictions of admission



# Functions ---------------------------------------------------------------

get_random_dttm <- function(dttm_start, dttm_end) {
  dt <- as.numeric(difftime(dttm_end, dttm_start,unit="sec"))
  increment <- runif(1, 0, dt)
  return(dttm_start + increment)
}

# Libraries ---------------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(polynom)



# Load data ---------------------------------------------------------------



library(readr)
df_y_predicted2 <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/df_y_predicted2.csv",
col_types = cols(X1 = col_skip()))
library(readr)
before_covid_num_in_location_2021_02_16 <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/before_covid_num_in_location_2021-02-16.csv")




# Get a sample of time points of interest ---------------------------------



set.seed(17L)
time_pts <- get_random_dttm(as.POSIXct(min(df_y_predicted2$DateTime)),as.POSIXct(min(df_y_predicted2$DateTime)) + hours(12))
last_pt <- time_pts

while (last_pt + hours(12) < as.POSIXct(max(df_y_predicted2$DateTime))) {
  next_pt <- get_random_dttm(last_pt, last_pt + hours(12))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
  
}


# Compare -----------------------------------------------------------------

# get latest row with dttm less than  the sampled
d = df_y_predicted2[as.POSIXct(df_y_predicted2$DateTime)<time_pts[1], ]
d[nrow(d),]

a = before_covid_num_in_location_2021_02_16[as.POSIXct(before_covid_num_in_location_2021_02_16$DateTime)<time_pts[1] + hours(4), ] 


distr_coll = data.table()
adm_coll = data.table()

for (i in (1:length(time_pts))) {
  
  # for all patients irrespective of timeslice - a calc of likely number of patients
  
  num_adm = seq(0,nrow(df), 1)# make an array from 0 admissions to max admissions (ie all patients admitted)
  pgf = poly_prod(df) # the probabilities of each of these
  
  distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm, probs = pgf, cdf = cumsum(pgf))
  
  distr_coll = bind_rows(distr_coll, distr)
  
  adm = sum(df$truth == 1)
  num_adm = bind_cols(sample_time = time_pts[i], num_in_ED = nrow(in_ED), num_adm = adm)
  
  adm_coll = bind_rows(adm_coll, num_adm)
  
}

adm_coll %>% pivot_longer(num_in_ED:num_adm) %>%  
  ggplot(aes(x = sample_time, y = value, col = name, group = sample_time)) + geom_line() + geom_point() +
  theme(legend.position = "bottom") +
  labs(title = "Showing range of sample points over time with number in ED and number admitted",
       y = "Number of patients",
       x = "sampled time")

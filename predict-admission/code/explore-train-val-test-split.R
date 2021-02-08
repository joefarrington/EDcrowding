
# About this script -------------------------------------------------------

# Use this for charts comparing the allocation of visits between training set and test set. 
# Shows that proportions of train, val and test don't change between timeslices
# in a material way


# Do train-test-val split  -----------------------------------------------------------------


# I first did this - but note this does not stratify using the dependent variable
# use the following code to divide into relevant group - train, val or teset

test = data.table(row_id = sample(nrow(dm), 0.2 * nrow(dm)), set_ = "test")
rest = base::setdiff(seq_len(nrow(dm)), test$row_id)

val = data.table(row_id = sample(rest, 1/8 * length(rest)), set_ = "val")
train = data.table(row_id = setdiff(seq_len(nrow(dm)), c(val$row_id, test$row_id)), set_ = "train")

row_ids = bind_rows(bind_rows(test, val), train)

dm[, row_id := seq_len(nrow(dm))]
dm = merge(dm, row_ids, by = "row_id")

# So instead I used a tidy models split


# Generate timeslices -----------------------------------------------------

# generate timeslices using generate-timeslices.R

# process slices using beginning of run-ML.R



timeslices <- c("000", "015", "030", "060", "120", "180", "240", "300", "360")

adm_summ <- data.table()
set_summ <- data.table()
adm_set_summ <- data.table()

for (ts_ in timeslices) {
  name_ <- paste0("dm", ts_, "p")
  ts = get(name_)
  num_adm <- ts[, .N, by = .(adm)]
  num_adm[, model := ts_]
  
  num_adm_set <- ts[, .N, by = .(adm, set_)]
  num_adm_set[, model := ts_]
  
  num_set <- ts[, .N, by = .(set_)]
  num_set[, model := ts_]
  
  adm_summ <- bind_rows(adm_summ, num_adm)
  set_summ <- bind_rows(set_summ, num_set)
  adm_set_summ <- bind_rows(adm_set_summ, num_adm_set)
  
}

set_summ[, set_ := factor(set_, levels = c("train", "val", "test"))]
adm_set_summ[, set_ := factor(set_, levels = c("train", "val", "test"))]

# look at class balance as timeslices progress
adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
  labs(title = "Numbers admitted / not admitted in each timeslice", 
       fill = "Admitted (1 = TRUE)",
       x = "Timeslice") +
  theme(legend.position = "bottom") 

# same chart with proportions

adm_summ[, perc := N/sum(N), by = .(model)]
adm_summ[, label := paste(round(perc*100, 1), "%")]

adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportions admitted / not admitted in each timeslice (using prior split into train, val and test)", 
       fill = "Set",
       x = "Timeslice") +
  theme(legend.position = "bottom")


# look at class balance as timeslices progress with train-test-val split
adm_set_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity", position = "fill") + 
  labs(title = "Proportion admitted / not admitted in each timeslice", 
       fill = "Admitted (1 = TRUE)",
       x = "Timeslice") +
  theme(legend.position = "bottom")  +
  facet_wrap(. ~ set_)



# look at train, val, test props within timeslice

set_summ[, perc := N/sum(N), by = .(model)]
set_summ[, label := paste(round(perc*100, 1), "%")]

set_summ %>% ggplot(aes(x = model, y = N, fill = set_)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportions in train, validation and test sets in each timeslice (using prior split into train, val and test)", 
       fill = "Set",
       x = "Timeslice") +
  theme(legend.position = "bottom") 
  


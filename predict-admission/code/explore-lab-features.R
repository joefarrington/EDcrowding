
# About this file ---------------------------------------------------------

# Explores lab data


# Load libraries ----------------------------------------------------------


library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(readr)



# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_real_2021-03-01.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-01.rda")


# Explore -----------------------------------------------------------------

summ[,ED_duration := case_when(is.na(first_outside_proper_admission) ~ difftime(last_ED_discharge, first_ED_admission, units = "mins"),
                               TRUE ~ difftime(first_outside_proper_admission, first_ED_admission, units = "mins"))]

lab_real <- merge(lab_real, summ[, .(csn, adm, ED_duration)])
lab_real[, adm := case_when(adm %in% c("direct_adm", "indirect_adm") ~ 1,
                            TRUE ~ 0)]



lab_real <- lab_real[elapsed_mins >= -120 & elapsed_mins < ED_duration]

test_count <- lab_real[, .N, by = .(test_lab_code, adm)] %>% pivot_wider(names_from = adm, values_from = N)

lab_real

test_count = lab_real[!is.na(oor_high), .(count = .N, 
                                          prop_oor_h = sum(oor_high)/.N,
                                          prop_oor_l = sum(oor_low)/.N), by = .(test_lab_code, adm)]

test_count[, prop_in_range := 1 - prop_oor_h - prop_oor_l ]


# how many are in or out of range? 
test_count[count > 1000] %>% pivot_longer(prop_oor_h:prop_in_range) %>%  ggplot(aes(x = factor(adm), y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "fill") + facet_wrap(test_lab_code~ .)


# difference betwween admitted and not
lab_real[!is.na(oor_high) & !is.na(oor_low), .(count = .N, 
                             prop_oor_h = sum(oor_high)/.N,
                             prop_oor_l = sum(oor_low)/.N), by = .(adm)]

# how many per can
count_by_csn = lab_real[, .N, by = .(test_lab_code, csn)]

# time to get labs back
lab_real[elapsed_mins < 400, min(elapsed_mins), by = csn] %>% ggplot(aes(x = V1)) + geom_histogram(bins = 30)





# looking at labs with feature importance
load("~/EDcrowding/predict-admission/data-output/imps_2021-03-08.rda")

imps = imps[grep("^p_", feature)]
imps = imps[,test_lab_code := gsub("^p_num_", "", feature)]
imps = imps[!timeslice %in% c("task000", "task015", "task030")]

imps[, .(importance = mean(importance)), by = test_lab_code]

imp_test_count = merge(test_count, imps[, .(importance = mean(importance)), by = test_lab_code], by = "test_lab_code")
#imp_test_count = merge(test_count, imps[, .(importance, test_lab_code, timeslice), by = test_lab_code], by = "test_lab_code")


# plot of lab tests with feature importances > 0.001
imp_test_count[importance > 	0.001] %>% pivot_longer(prop_oor_h:prop_in_range) %>%  ggplot(aes(x = factor(adm), y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "fill") + facet_wrap(test_lab_code~ .)


lab_real[test_lab_code == "TDDI" & oor_high]


# Which lab results co-occur - creating adjacency matrix ----------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_real_2021-03-01.rda")

# summarise by lab test code to get raw numbers
l1 = lab_real[, .N, by = test_lab_code]
write.csv(l1, "lab_test_nums.csv")

# count number of tests returned within each timestamp
l2 = lab_real[, .N, by = .(csn, result_last_modified_time, test_lab_code)]

# pivot to wide data frame to get all tests within each timestamp
l3 = l2 %>% pivot_wider(names_from = test_lab_code, values_from = N)
# l4 = l3 %>% select(-csn, - result_last_modified_time)
# l4 = unique(l4)
# l4 = data.table(l4)
# l4[, row_id := seq_len(nrow(l4))]

# create an edgelist by counting the number of times each lab test co-occurs
# reducing it to 5
codes = unique(l1[N>5, test_lab_code])

edgelist = data.table()

for (i in 1:length(codes)) {
  print(i)
  from = codes[i]
  for (j in 1:length(codes)) {
    to = codes[j]
    vector_from = get(from, l3)
    vector_to = get(to, l3)
    weight = sum(vector_to * vector_from, na.rm = TRUE)
    edge = data.table(from = from, to = to, weight = weight)
    edgelist = bind_rows(edgelist, edge)
  }
}

save(edgelist, file = "~/EDcrowding/predict-admission/data-raw/lab_test_edgelist_gt5.rda")

lab_test_adj = edgelist %>% pivot_wider(names_from = to, values_from = weight)
save(lab_test_adj, file = "~/EDcrowding/predict-admission/data-raw/lab_test_adj_gt5.rda")

write.csv(lab_test_adj, "~/EDcrowding/predict-admission/model-output/lab_test_adj.csv")



# Network analysis --------------------------------------------------------

library(igraph)

# Create an igraph network object  
net <- graph_from_data_frame(d=edgelist[weight > 1], vertices=unique(edgelist[weight > 1, from]), directed=F) 

# Removing loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net)

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA)

graph_attr(net, "layout") <- layout_with_kk

plot(net, vertex.shape="none", 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")




# Distance matrix ---------------------------------------------------------

# from https://www.datacamp.com/community/tutorials/hierarchical-clustering-R

library(dendextend)
library(colorspace) 
library(ggdendro)

x <- scale(lab_test_adj %>% select(-from))
y = dist(x, method = "euclidian")
hclust_avg <- hclust(y, method = 'average')

plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 8)

avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 8)
plot(avg_col_dend)

lab_test_adj_cl <- mutate(lab_test_adj, cluster = cut_avg)
count(lab_test_adj_cl,cluster)

# with ggplot

ggdendrogram(hclust_avg, rotate = FALSE, size = 2) + 
  labs(title = "Dendogram of all lab results used more than 25 times during after Covid period")


dhc <- as.dendrogram(hclust_avg)
# Rectangular lines
ddata <- dendro_data(dhc, type = "rectangle")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(ddata), 
            aes(x = x, y = y, label = label), hjust = -1, size = 2)+ 
  
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) +
  theme_dendro()
p



if(require(tree)){
  data(cpus, package = "MASS")
  model <- tree(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax, 
                data = cpus)
  tree_data <- dendro_data(model)
  ggplot(segment(tree_data)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, size = n), 
                 colour = "blue", alpha = 0.5) +
    scale_size("n") +
    geom_text(data = label(tree_data), 
              aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
    geom_text(data = leaf_label(tree_data), 
              aes(x = x, y = y, label = label), vjust = 0.5, size = 2) +
    theme_dendro()
}


# can I ignore dttms that only have one lab result? 

# exlcuding any lab results that are returned as singles
l5 = lab_real[, num_results := (.N), by = .(csn,result_last_modified_time)]
l5 = l5[num_results > 1]

unique(l5$test_lab_code)

l1a = l5[, .N, by = test_lab_code]

# count number of tests returned within each timestamp
l2a = l5[num_results > 1, .N, by = .(csn, result_last_modified_time, test_lab_code)]

# pivot to wide data frame to get all tests within each timestamp
l3a = l2a %>% pivot_wider(names_from = test_lab_code, values_from = N)
# l4 = l3 %>% select(-csn, - result_last_modified_time)
# l4 = unique(l4)
# l4 = data.table(l4)
# l4[, row_id := seq_len(nrow(l4))]

# create an edgelist by counting the number of times each lab test co-occurs
# reducing it to 5
codes = unique(l1a[N>25, test_lab_code])

edgelist = data.table()

for (i in 1:length(codes)) {
  print(i)
  from = codes[i]
  for (j in 1:length(codes)) {
    to = codes[j]
    vector_from = get(from, l3)
    vector_to = get(to, l3)
    weight = sum(vector_to * vector_from, na.rm = TRUE)
    edge = data.table(from = from, to = to, weight = weight)
    edgelist = bind_rows(edgelist, edge)
  }
}


# Having another go -------------------------------------------------------



# from https://www.datacamp.com/community/tutorials/hierarchical-clustering-R

library(dendextend)
library(colorspace) 
library(ggdendro)

lab_test_adj_scaled <- scale(lab_test_adj %>% select(-from))
# note that this holds the original labels
labels(lab_test_adj_scaled)

# create distance matrix and dendogram
lab_test_dist = dist(lab_test_adj_scaled, method = "euclidian")
hclust <- hclust(lab_test_dist, method = 'average')

# simple plots
plot(hclust, main = "Dendogram of co-occuring lab results used more than 50 times since Jan 2020",
     xlab = "lab test", sub = NULL)

plot(hclust)

# plot with rectangles around
rect.hclust(hclust , k = 37, border = 2:6)


ggdendrogram(hclust, rotate = FALSE, size = 2) + 
  labs(title = "Dendogram of co-occuring lab results  used more than 50 times since Jan 2020") +
  geom_hline(aes(yintercept = 11.5, colour = "red")) +
  theme(legend.position = "none")


# get dendogram data
ddata <- dendro_data(as.dendrogram(hclust), type = "rectangle")

# the dendogram saves its labels here:
as.numeric(ddata$labels$label)
# so we can use this to get the labels for each lab test in the same order
labels(lab_test_adj_scaled)[[2]][as.numeric(ddata$labels$label)]


# from : https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
# get a dendogram objecti
dend <- as.dendrogram(hclust(dist(lab_test_adj_scaled)))

# forcing a cluster distribution of 37
dend <- color_branches(dend, k=37) #, groupLabels=iris_species)

labels_colors(dend) <-
  rainbow_hcl(37)[sort_levels_values(
    labels(lab_test_adj_scaled)[[2]][order.dendrogram(dend)]
  )]


labels(dend) <-     labels(lab_test_adj_scaled)[[2]][order.dendrogram(dend)]


# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Dendogram of co-occuring lab results used more than 50 times since Jan 2020, forced into 37 clusters", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = iris_species, fill = rainbow_hcl(3))




length(ddata$segments$y > 3)

# cut into clusters
cut_avg <- cutree(hclust, k = 37)
cluster_lookup <- data.table(lab_test_code = lab_test_adj$from, cluster = cut_avg, lab_test_num = as.numeric(ddata$labels$label))



# avg_dend_obj <- as.dendrogram(hclust_avg)
# avg_col_dend <- color_branches(avg_dend_obj, h = 8)
# plot(avg_col_dend)

lab_test_adj_cl <- mutate(lab_test_adj, cluster = cut_avg)
count(lab_test_adj_cl,cluster)


# Looking at APACHE values ------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_raw_2021-03-01.rda")


# serum sodium
nrow(lab_raw %>% filter(test_lab_code == "NA"))

lab_raw %>% filter(test_lab_code == "NA", !is.na(abnormal_flag)) %>% 
  group_by(abnormal_flag) %>% summarise(OOR = n()/ nrow(lab_raw %>% filter(test_lab_code == "NA")))

# Comparing with values in Knaus paper
nrow(lab_raw %>% filter(test_lab_code == "NA", value_as_real < 130)) / 
  nrow(lab_raw %>% filter(test_lab_code == "NA"))

nrow(lab_raw %>% filter(test_lab_code == "NA", value_as_real > 149)) / 
  nrow(lab_raw %>% filter(test_lab_code == "NA"))

# serum potassium
nrow(lab_raw %>% filter(test_lab_code == "K"))

lab_raw %>% filter(test_lab_code == "K", !is.na(abnormal_flag)) %>% 
  group_by(abnormal_flag) %>% summarise(OOR = n()/ nrow(lab_raw %>% filter(test_lab_code == "K")))

 # Comparing with values in Knaus paper
nrow(lab_raw %>% filter(test_lab_code == "K", value_as_real < 3.5)) / 
  nrow(lab_raw %>% filter(test_lab_code == "K"))

nrow(lab_raw %>% filter(test_lab_code == "K", value_as_real > 5.4)) / 
  nrow(lab_raw %>% filter(test_lab_code == "K"))


# creatinine


lab_raw %>% filter(test_lab_code == "CREA", !is.na(abnormal_flag)) %>%  left_join(summ %>% select(csn, sex)) %>% 
  group_by(abnormal_flag, sex) %>% summarise(OOR = n()/ nrow(lab_raw %>% filter(test_lab_code == "CREA")))

lab_raw = lab_raw %>%  left_join(summ %>% select(csn, sex)) 

# Comparing with values in Knaus paper - not sure how to covert this
nrow(lab_raw %>% filter(test_lab_code == "CREA", sex == "M", value_as_real < .6 * 100)) / 
  nrow(lab_raw %>% filter(test_lab_code == "CREA", sex == "M"))

# haemocrit

lab_raw %>% filter(test_lab_code == "HCTU", !is.na(abnormal_flag), sex == "M") %>% 
  group_by(abnormal_flag) %>% summarise(OOR = n()/ nrow(lab_raw %>% filter(test_lab_code == "HCTU", sex == "M")))

lab_raw %>% filter(test_lab_code == "HCTU", !is.na(abnormal_flag), sex == "F") %>% 
  group_by(abnormal_flag) %>% summarise(OOR = n()/ nrow(lab_raw %>% filter(test_lab_code == "HCTU", sex == "F")))

# white cell count
lab_raw %>% filter(test_lab_code == "WCC", !is.na(abnormal_flag)) %>% 
  group_by(abnormal_flag) %>% summarise(OOR = n()/ nrow(lab_raw %>% filter(test_lab_code == "WCC")))

# Comparing with values in Knaus paper
nrow(lab_raw %>% filter(test_lab_code == "WCC", value_as_real < 3)) / 
  nrow(lab_raw %>% filter(test_lab_code == "WCC"))

nrow(lab_raw %>% filter(test_lab_code == "WCC", value_as_real > 14.9)) / 
  nrow(lab_raw %>% filter(test_lab_code == "WCC"))



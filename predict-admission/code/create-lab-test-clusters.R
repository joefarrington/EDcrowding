
# This scripts groups the lab values into clusters ------------------------

# Takes only lab results that are used more than 50 times since Jan 2020

# Load libraries ----------------------------------------------------------


library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(readr)


library(dendextend)
library(colorspace) 
library(ggdendro)



# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_real_2021-03-10.rda")


# Create adjacency matrix -------------------------------------------------

# summarise by lab test code to get raw numbers
l1 = lab_real[, .N, by = test_lab_code]

# count number of tests returned within each timestamp
l2 = lab_real[, .N, by = .(csn, result_last_modified_time, test_lab_code)]

# pivot to wide data frame to get all tests within each timestamp
l3 = l2 %>% pivot_wider(names_from = test_lab_code, values_from = N)

# create an edgelist by counting the number of times each lab test co-occurs
# reducing it to 5
codes = unique(l1[N>50, test_lab_code])

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

lab_test_adj = edgelist %>% pivot_wider(names_from = to, values_from = weight)


# create distance matrix and dendogram
lab_test_adj_scaled <- scale(lab_test_adj %>% select(-from))
lab_test_dist = dist(lab_test_adj_scaled, method = "euclidian")
hclust <- hclust(lab_test_dist, method = 'average')

# eyeballing for right number of clusters

ggdendrogram(hclust, rotate = FALSE, size = 2) + 
  labs(title = "Dendogram of co-occuring lab results  used more than 50 times since Jan 2020") +
  geom_hline(aes(yintercept = 11.5, colour = "red")) +
  theme(legend.position = "none")

# looks like 37

# from : https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
# get a dendogram objecti
dend <- as.dendrogram(hclust(dist(lab_test_adj_scaled)))

# forcing a cluster distribution of 37
dend <- color_branches(dend, k=37) #, groupLabels=iris_species)

# not sure how I did this ???
labels_colors(dend) <-
  rainbow_hcl(37)[sort_levels_values(
    labels(lab_test_adj_scaled)[[2]][order.dendrogram(dend)]
  )]


labels(dend) <- labels(lab_test_adj_scaled)[[2]][order.dendrogram(dend)]


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
cluster_lookup <- data.table(test_lab_code = lab_test_adj$from, cluster = cut_avg, lab_test_num = as.numeric(ddata$labels$label))

lab_test_adj <- mutate(lab_test_adj, cluster = cut_avg)
count(lab_test_adj,cluster)
save(lab_test_adj, file = "~/EDcrowding/predict-admission/data-raw/lab_test_adj_gt50.rda")



# Add clusters to original lab results ------------------------------------

lab_real <- merge(lab_real, cluster_lookup[,.(test_lab_code, cluster)], by = "test_lab_code", all.x = TRUE)


outFile = paste0("EDcrowding/predict-admission/data-raw/lab_real_",today(),".rda")
save(lab_real, file = outFile)



outFile = paste0("EDcrowding/predict-admission/data-raw/cluster_lookup_",today(),".rda")
save(cluster_lookup, file = outFile)



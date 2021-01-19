## Libraries
library(Rgraphviz)
library(dplyr)
library(tidyverse)
library(lubridate)

# === Create functions

# function to selectd only required edges
keep_edges <- function(edgelist_sum, min_weight) {
  
  # remove minority edges
  keep <- edgelist_sum %>% filter(weight > min_weight)  %>% select(from, to, weight)
  
  # name the edges
  keep <- keep %>% mutate(edge = paste0(from,"~",to)) %>% arrange(from,desc(weight))
  
}


# function to create network graph
create_graph <- function(keep) {
  # create network 
  nodes <- unique(c(keep$from,keep$to))
  
  g <- new("graphNEL", nodes=nodes, edgemode="directed")
  
  for (i in 1:nrow(keep)) {
    g <- addEdge(keep$from[i], keep$to[i], g, keep$weight[i])
  }
  
  return(g)
  
}


# function to plot standard graph
plot_standard <- function(keep) {
  gb <- create_graph(keep)
  
  attrs <- getDefaultAttrs()
  attrs$graph$rankdir <- "LR"

  w <- keep$weight
  names(w) <- keep$edge
  eAttrs <- list()
  eAttrs$label <- w
  plot(gb, attrs = attrs, edgeAttrs=eAttrs, recipEdges = "distinct")
}

# === Read and process data

# all edges

load("~/EDcrowding/flow-mapping/data-raw/edgedf_2021-01-11.rda")

edgelist_summ = edgedf  %>% 
  group_by(from, to) %>%
  summarise(weight = n()) 



keep <- keep_edges(edgedf[moves[(visited_CDU), csn], nomatch = 0]  %>% 
                     group_by(from, to) %>%
                     summarise(weight = n())
                   %>% filter(!is.na(to)), 10)
plot_standard(keep)


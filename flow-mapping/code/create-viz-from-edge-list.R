# Load Libraries
# ==============
library(Rgraphviz)

# Define functions
# ================

# function to create network graph
create_graph <- function(edge_list) {
  # create network 
  nodes <- unique(c(keep$from,keep$to))
  
  g <- new("graphNEL", nodes=nodes, edgemode="directed")
  
  for (i in 1:nrow(keep)) {
    g <- addEdge(keep$from[i], keep$to[i], g, keep$weight[i])
  }
  return(g)
}

# function to selectd only edges with a minimum weight
keep_edges <- function(edgelist_summ, min_weight) {
  # remove minority edges
  keep <- edgelist_summ %>% filter(weight > min_weight)  %>% select(from, to, weight)
  
  # name the edges
  keep <- keep %>% mutate(edge = paste0(from,"~",to)) %>% arrange(from,desc(weight))
}


# Create plot
# ===========

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_edge_list_","2020-06-19",".rda")
load(inFile)

edgelist_summ <- edgedf %>% group_by(from, to) %>% summarise(weight = n()) %>% arrange(desc(weight))

# look for the right cut point for eliminating edges
ggplot(edgelist_summ, aes(x=1, y = tot)) + 
  geom_boxplot() # wide band at bottom of box plot


ggplot(edgelist_summ %>% filter(tot < 500), aes(x=1, y = tot)) + 
  geom_boxplot() # zooming in suggests 100 is a reasonable cut point


# get required graph
keep <- keep_edges(edgelist_summ, 100)

# write edgelist to file
outFile <- paste0("EDcrowding/flow-mapping/data-raw/edgelist_summ_gt100",today(),".rda")
save(keep, file = outFile)

outFile <- paste0("EDcrowding/flow-mapping/data-raw/edgelist_summ_gt100",today(),".csv")
write.csv2(keep, file = outFile)



# Create plot
# ===========
# note had to do this below in browser version of R Studio which runs Rgraphviz


gb <- create_graph(keep)



# getting a title and colouring nodes and edges
graph.par(list(graph=list(main="Flow of patients before 1 March 2020")))



# global settings for graphs
attrs <- getDefaultAttrs()
attrs$node$shape <- "ellipse" # done
attrs$node$fixedsize <- FALSE # don't know how to change
attrs$node$fontsize <- 20 #  # don't know how to change
attrs$node$fillcolor <- "white" # done
attrs$graph$rankdir <- "LR"
attrs$edge$labelfontsize <- 60

plot(gb, attrs = attrs, recipEdges = "distinct", , edgeAttrs=list(label=w))





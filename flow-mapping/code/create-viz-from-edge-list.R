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

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_edge_list_","2020-06-15",".rda")
load(inFile)

edgelist_summ <- edgedf %>% filter(dttm < "2020-03-01") %>% group_by(from, to) %>% summarise(tot = n()) %>% arrange(desc(tot))

 

# get required graph
keep <- keep_edges(edgelist_summ, 4)
gb <- create_graph(keep)



# getting a title and colouring nodes and edges
graph.par(list(graph=list(main="Flow of COVID patients after 24 March")))

# create named vector for edge labels
w <- keep$weight
names(w) <- keep$edge

# # set edge labels before doing layout
# eAttrs <- list()
# eAttrs$label <- w

gb <- layoutGraph(gb, edgeAttrs=list(label=w))
renderGraph(gb)


# formatting nodes
fill <- rep("white", length(gb@nodes))
names(fill) <- gb@nodes
for (n in c("T09N","T08N","T08S")) {
  fill[[n]]<-"green"
}
for (n in c("T07S","T07 HDRU")) {
  fill[[n]]<-"yellow"
}
for (n in c("T03 ICU","T03 Theatres")) {
  fill[[n]]<-"red"
}
fill[["ED"]]<-"grey"
fill[["T01 AMU"]]<-"lightblue"


edgeRenderInfo(gb) <- list(arrowhead = "none", col = "green")
nodeRenderInfo(gb) <- list(shape = "ellipse", fill = fill)
renderGraph(gb)

# formatting edges
# edgecol <- rep("blue", nrow(keep)) not working
# names(edgecol) <- keep$edge
# edgeRenderInfo(gb) <- list(col = edgecol)

edgeRenderInfo(gb) <- list(col=c("ED~T01 AMU"="lightblue",
                                 "ED~T03 ICU"="red",
                                 "ED~T10N"="green",
                                 "T09N~T03 ICU"="red",
                                 "T09S~T03 ICU"="red",
                                 "T07 HDRU~T03 ICU" = "red",
                                 "T03 Theatres~T03 ICU"="red",
                                 "T03 ICU~T03 Theatres"="red",
                                 "T07 HDRU~T03 Theatres"="red",
                                 "T07 HDRU~s"="red",
                                 "T03 Theatres~T07 HDRU"="yellow",
                                 "T07S~T07 HDRU"="yellow",
                                 "T07 HDRU~T07S"="yellow",
                                 "T10S~T07 HDRU"="yellow"
                                 
                                  ), 
                           lwd = c("ED~T01 AMU"=16,
                                   "ED~T09S"=10,
                                   "ED~T08S"=6,
                                   "ED~T10N"=3,
                                   "ED~T03 ICU"=2.5,
                                   "T09S~T09N"=4,
                                   "T03 ICU~T03 Theatres"=3
                                   )
                            )
renderGraph(gb)






# global settings for graphs
attrs <- getDefaultAttrs()
attrs$node$shape <- "ellipse" # done
attrs$node$fixedsize <- FALSE # don't know how to change
attrs$node$fontsize <- 10 #  # don't know how to change
attrs$node$fillcolor <- "white" # done
attrs$graph$rankdir <- "LR"
attrs$edge$labelfontsize <- 4



# check for removed edges if reciprocal graph not used
removedEdges(gb)



# trying to set line width to weights - # BUT edge weights are not drawn correctly
eAttrs$lwd <- sqrt(w)
plot(gb, attrs = attrs, edgeAttrs=eAttrs, recipEdges = "distinct")

# compare with 
eAttrs$lwd <- NULL
plot(gb, attrs = attrs, edgeAttrs=eAttrs, recipEdges = "distinct")


# trying other ways (again!) - at least got this to work but it has the same issue
eAttrs$lwd <- sqrt(w)
f <- buildEdgeList(gb, recipEdges = "distinct", edgeAttrs = eAttrs )
gc <- agopen(gb, name = "foo", attrs = attrs, edgeAttrs = eAttrs, recipEdges="distinct")
gc <- layoutGraph(gc)





## Thhis on is own returns a graphNEL object
gb2 <-layoutGraph(gb, attrs = attrs, edgeAttrs=eAttrs, recipEdges = "distinct") 

renderGraph(gc)


# trying to set edge colour
textCol <- rep("blue",length(edges))
names(textCol) <- keep$edge
eAttrs$textCol <- textCol


# trying to control arrow head
arrow <- rep("odiamond",length(edges))
names(arrow) <- keep$edge
eAttrs$arrowhead <- arrow




# To modify graph parameters (experiment)

graph.par <- savedGraphparams
savedGraphparams <- graph.par()
graph.par(list(edges=list(lty="dashed")))





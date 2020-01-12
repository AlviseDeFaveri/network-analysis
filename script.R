library(igraph)
library(centiserve)

##
## Import the graph
##
g <- read.graph("karate.gml", format="gml")

# Print the initial graph
V(g)$color <- sample( c("red", "black"), vcount(g), rep=TRUE)
E(g)$color <- "grey"
plot(g)

##
## CENTRALITIES
## Calculate centralities using different algorithms
##

centr1 <- centr_degree(g)$res
centr2 <- centr_clo(g, mode = "all")$res
centr3 <- centr_betw(g, directed = FALSE)$res
centr4 <- centr_eigen(g, directed = FALSE)$vector
centr5 <- closeness.currentflow(g)
centr6 <- subgraph_centrality(g)
#centr7 <- sna::flowbet(get.edgelist(g, names=FALSE)) 

##
## COMMUNITIES
## Calculate communities using different algorithms
##
com1 <- multilevel.community(g)
print ( compare(com1, V(g)$value, method="nmi" ) )

com2 <- edge.betweenness.community(g)
print ( compare(com2, V(g)$value, method="nmi" ) )

com3 <- walktrap.community(g)
print ( compare(com3, V(g)$value, method="nmi" ) )

com4 <- infomap.community(g)
print ( compare(com4, V(g)$value, method="nmi" ) )

# Print the community graph
# plot(g, vertex.color=membership(com4))

##
## BUILD THE MATRIX
## For each node we have the evaluation using
## each community detection algorithm and the ground truth at the end
##

# ground truth
ground_truth = V(g)$value + 1

# put together the centralities and memberships in a matrix
matrix <- do.call(rbind, Map(data.frame, C1 = centr1, C2 = centr2,
                             C3 = centr3, C4 = centr4, C5 = centr5, C6 = centr6,
                             #C7 = centr7,
                             A=membership(com1), B=membership(com2), 
                             C=membership(com3), D=membership(com4), Ground=ground_truth))

# print the matrix
print(matrix)
library(igraph)
library(centiserve)

##
## Import the graph
##
g <- read.graph("~/Documents/UNI/SEM3/Network Analysis/network-analysis/karate.gml", format="gml")

# Print the initial graph
V(g)$color <- sample( c("red", "black"), vcount(g), rep=TRUE)
E(g)$color <- "grey"
plot(g)

# Ground truth
ground_truth = V(g)$value

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

# put together the centralities in a matrix
centrMatrix <- do.call(rbind, Map(data.frame, C1 = centr1, C2 = centr2,
                                  C3 = centr3, C4 = centr4, C5 = centr5, C6 = centr6))
print("Centrality matrix")
print(centrMatrix)

##
## COMMUNITIES
## Calculate communities using different algorithms
##
com1 <- multilevel.community(g)
com2 <- edge.betweenness.community(g)
com3 <- walktrap.community(g)
com4 <- infomap.community(g)

# Print the community graph
# plot(g, vertex.color=membership(com4))

# put together all community calculated memberships in a matrix
commMatrix <- do.call(rbind, Map(data.frame, 
                                 A=membership(com1), B=membership(com2), 
                                 C=membership(com3), D=membership(com4), Ground=ground_truth))

# print the matrix
print("Community matrix")
print(commMatrix)

##
## Compare each community detection algorithm to the ground truth using NMI and choose the best one
##
print("NMI comparison")
l <- rbind(c("A", compare(com1, ground_truth, method="nmi" )), c("B", compare(com2, ground_truth, method="nmi" )),
                  c("C", compare(com3, ground_truth, method="nmi" )), c("D", compare(com4, ground_truth, method="nmi" )))
print(l)

print("Best community algorithm")
best_algo <- l[which.max(l[,2]),]
print(best_algo)

library(igraph)
library(centiserve)
library(randomForest)

######################## FUNCTIONS #####################################

##
## Plot a graph
##
plot_graph <- function(g) {
  V(g)$color <- sample( c("red", "black"), vcount(g), rep=TRUE)
  E(g)$color <- "grey"
  plot(g)
}

##
## Calculate centralities using different algorithms
##
calc_centralities <- function(g) {
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
  #print("Centrality matrix")
  #print(centrMatrix)
  return(centrMatrix)
}

##
## Calculate communities using different algorithms
##
calc_communities <- function(g) {
  # Ground truth
  ground_truth = as.integer(as.factor(V(g)$value))
  
  # Extract communities
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
  return(commMatrix)
}

##
## Compare each community detection algorithm to the ground truth using NMI and choose the best one
##
comp_communities <- function(g) {
  # Ground truth
  ground_truth = as.integer(as.factor(V(g)$value))
  
  # Extract communities
  com1 <- multilevel.community(g)
  com2 <- edge.betweenness.community(g)
  com3 <- walktrap.community(g)
  com4 <- infomap.community(g)
  
  # Compare results with ground truth
  print("NMI comparison")
  l <- rbind(c("A", compare(com1, ground_truth, method="nmi" )), c("B", compare(com2, ground_truth, method="nmi" )),
             c("C", compare(com3, ground_truth, method="nmi" )), c("D", compare(com4, ground_truth, method="nmi" )))
  print(l)
  
  # Find the best
  best_algo <- l[which.max(l[,2]),]
  return(best_algo)
}

###################################################################
## MAIN: populate the dataset

# define the files to import
filenames <- c("polbooks", "karate", "football", "dolphins" )
prefix <- "~/Documents/UNI/SEM3/Network Analysis/network-analysis/"

# define the final dataframe
df <- data.frame(C1=integer(),
                 C2=integer(),
                 C3=integer(),
                 C4=integer(),
                 C5=integer(),
                 C6=integer(),
                 best=character())

# import each file
for (n in filenames)
{
  # read graph
  print(n)
  name <- paste(prefix, n, ".gml", sep="")
  g <- read.graph(name, format="gml")
  
  # calculate centralities and best community detection algorithm
  centr <- calc_centralities(g)
  comm <- comp_communities(g)
  centr$best <- comm[1]
  
  # print results
  print("Best community detection algorithm")
  print(comm)
  
  # append to the existing dataframe
  df <- rbind(df, centr)
  #print(df)
}

df$best <- as.factor(df$best)


#######################################
# Predict using random forest


train <- sample(nrow(df), 0.7*nrow(df), replace = FALSE)
TrainSet <- df[train,]
ValidSet <- df[-train,]

rf <- randomForest(best ~ C1+C2+C3+C4+C5+C6, data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)

# Predicting on train set
pred <- predict(rf, ValidSet, type = "class")
# Checking classification accuracy
print(table(pred, ValidSet$best))
print(mean(pred == ValidSet$best))


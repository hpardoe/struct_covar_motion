library(igraph)

density2Threshold <- function(inputCorMatrix = baseline_cor_list$cor_matrix, density = 0.1) {
  thresholds <- seq(from = 0.75, to = 0.99, by = 0.01)
  edge_density_vector <- vector(length = length(thresholds))
  for (i in 1:length(thresholds)) {
    adjMatrix <- inputCorMatrix > thresholds[i]
    myGraph <- graph_from_adjacency_matrix(adjMatrix, mode = "undirected", weighted=NULL)
    edge_density_vector[i] <- edge_density(myGraph)
  }
  out <- approx(y = thresholds, x = edge_density_vector, xout = density)
  out$y
}

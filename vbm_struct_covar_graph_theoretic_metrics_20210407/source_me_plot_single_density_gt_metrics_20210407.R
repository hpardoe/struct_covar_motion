# plot graph theoretic measures as function of number of motion affected scans
# note run this first: source("source_me_graph_theoretic_measures_20210407.R")
library("RColorBrewer")
mypal <- brewer.pal("Set2", n = 8)

#pdf(file = "fig6_graph_theoretic_metrics_single_density_20210421.pdf")

par(mfrow = c(2,2), mar = c(c(5, 4, 2, 2) + 0.1))
density_index <- 2
# plot transitivity/clustering coefficient
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,density_index,1], na.rm=TRUE),
  col = mypal[1],
  type = "l",
  ylab = "Transitivity/clustering coefficient",
  xlab = "Motion affected scans (%)",
  lwd = 2)

# plot modularity
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,density_index,2], na.rm=TRUE),
  col = mypal[1],
  type = "l",
  ylab = "Modularity",
  xlab = "Motion affected scans (%)",
  lwd = 2)

# plot efficiency
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,density_index,3], na.rm=TRUE),
  col = mypal[1],
  type = "l",
  ylab = "Efficiency",
  xlab = "Motion affected scans (%)",
  lwd = 2)

# plot small world index
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,density_index,4], na.rm=TRUE),
  col = mypal[1],
  type = "l",
  ylab = "Small world index",
  xlab = "Motion affected scans (%)",
  lwd = 2)

#dev.off()

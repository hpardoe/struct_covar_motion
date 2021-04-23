# plot graph theoretic measures as function of number of motion affected scans
# note run this first: source("source_me_graph_theoretic_measures_20210407.R")
library("RColorBrewer")
mypal <- brewer.pal("Set2", n = 8)

pdf(file = "fig5_graph_theoretic_metrics_all_densities_20210421.pdf")

par(mfrow = c(2,2), mar = c(c(5, 4, 2, 2) + 0.1))

# plot transitivity/clustering coefficient
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,1,1], na.rm=TRUE),
  ylim = c(0.62,0.66),
  col = mypal[1],
  type = "l",
  ylab = "Transitivity/clustering coefficient",
  xlab = "Motion affected scans (%)",
  lwd = 2)

points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,2,1], na.rm=TRUE),
  col = mypal[2],
  type = "l",
  lwd = 2)
points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,3,1], na.rm=TRUE),
  col = mypal[3],
  type = "l",
  lwd = 2)
legend("topright", legend = c("0.1","0.15","0.2"), col = mypal[1:3], lty = 1)

# plot modularity
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,1,2], na.rm=TRUE),
  ylim = c(0.36,0.7),
  col = mypal[1],
  type = "l",
  ylab = "Modularity",
  xlab = "Motion affected scans (%)",
  lwd = 2)

points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,2,2], na.rm=TRUE),
  col = mypal[2],
  type = "l",
  lwd = 2)

points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,3,2], na.rm=TRUE),
  col = mypal[3],
  type = "l",
  lwd = 2)


# plot efficiency
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,1,3], na.rm=TRUE),
  ylim = c(0.2,0.47),
  col = mypal[1],
  type = "l",
  ylab = "Efficiency",
  xlab = "Motion affected scans (%)",
  lwd = 2)

points(x = nMotion_affected_percentage,
    y = colMeans(graph_theoretic_array[,,2,3],na.rm=TRUE),
    col = mypal[2],
    type = "l",
    lwd = 2)

points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,3,3],na.rm=TRUE),
  col = mypal[3],
  type = "l",
  lwd = 2)


# plot small world index
plot(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,1,4], na.rm=TRUE),
  ylim = c(2.5,6.1),
  col = mypal[1],
  type = "l",
  ylab = "Small world index",
  xlab = "Motion affected scans (%)",
  lwd = 2)

points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,2,4], na.rm=TRUE),
  col = mypal[2],
  type = "l",
  lwd = 2)

points(x = nMotion_affected_percentage,
  y = colMeans(graph_theoretic_array[,,3,4], na.rm=TRUE),
  col = mypal[3],
  type = "l",
  lwd = 2)

dev.off()

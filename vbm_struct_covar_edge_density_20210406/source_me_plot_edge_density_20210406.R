# run this first: source("source_me_motion_edge_density_20210406.R")
library("RColorBrewer")
mypal <- brewer.pal("Set2", n = 3)

pdf(file = "fig2_edge_density_20210406.pdf")
plot(x = percent_motion_subjects, y = colMeans(threshold_array[,,1], na.rm=TRUE), type = "l", col = mypal[1],
  ylim = c(0.89,0.95),
  ylab = "Pearson correlation (r)",
  xlab = "Motion affected scans (%)")
points(x = percent_motion_subjects, y = colMeans(threshold_array[,,2], na.rm=TRUE), type = "l", col = mypal[2])
points(x = percent_motion_subjects, y = colMeans(threshold_array[,,3], na.rm=TRUE), type = "l", col = mypal[3])
legend("bottomright", legend = c("0.1","0.15","0.2"), col = mypal, lty = 1, title = "Edge density")
dev.off()

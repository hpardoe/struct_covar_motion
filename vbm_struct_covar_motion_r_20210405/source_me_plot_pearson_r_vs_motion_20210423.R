# plot mean pearson R vs number of motion affected scans
# run this first, note it takes a long time (few days): source("source_me_calc_cor_and_lm_cor_20210423.R")
library(RColorBrewer)
mypal <- brewer.pal("Set2", n = 8)
myConfInt <- apply(myMeanMatrix, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975))

nMotionPerc <- 100*(nMotionAffected/29)

pdf(file = "fig2_motion_pearson_r_20210423.pdf", width = 3.5, height = 3.5)
par(mar = c(5,4,2,2) + 0.1)
plot(x = nMotionPerc,
  y = colMeans(myMeanMatrix),
  ylim = c(0.6,0.95),
  col = mypal[1], lwd = 2, type = "l",
  ylab = "Pearson correlation (r)",
  xlab = "Motion affected scans (%)")

points(x = nMotionPerc, y = myConfInt[1,], type = "l", lty = 2, col = mypal[2])
points(x = nMotionPerc, y = myConfInt[2,], type = "l", lty = 2, col = mypal[2])
dev.off()

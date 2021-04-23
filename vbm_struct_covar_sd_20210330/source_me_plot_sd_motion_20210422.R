# source("source_me_sd_motion_20210330.R")
library(RColorBrewer)
library(scales)

mypal <- brewer.pal("Set2", n = 8)

pdf(file = "fig1_sd_motion_20210422.pdf", width = 3.5, height = 3.5)
par(mar = c(5, 4, 2, 2) + 0.1)
plot(x = 100*(nMotionAffected/29), y = colMeans(sdArray[,,1]), ylim = c(0,0.08),
  type = "l",
  ylab = "Standard Deviation",
  xlab = "Motion affected scans (%)",
  col = mypal[1])

for (i in 2:dim(sdArray)[3]) {
  points(x = 100*(nMotionAffected/29), y = colMeans(sdArray[,,i]), type = "l", col = alpha(mypal[1],0.4))
}

points(x = 100*(nMotionAffected/29), apply(sdArray, 2, mean), type = "l", lwd = 4, col = mypal[2])
dev.off()

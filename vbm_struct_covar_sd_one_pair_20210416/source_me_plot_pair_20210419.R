# plot correlation as # motion affected scans increases
# TODO:
# plot colors by motion free/ motion affected - done
# add boxplots to margins                     - done
# adjust axis limits to fit all GM estimates across # motion affected scans
# add r value to plot
plot.df <- plot_n10.df
y_lim <- c(0.33,0.58)
x_lim <- y_lim
# default mar c(5, 4, 4, 2) + 0.1
par(fig=c(0,0.8,0,0.8), mar = c(5,4,4,1) + 0.1)
myCor <- cor(plot.df$Precentral_L, plot.df$Precentral_R)
myText <- paste("r =",round(myCor,2))
plot(Precentral_L ~ Precentral_R, data = plot.df, col = as.factor(plot.df$run), xlim = c(0.33,0.58),
  ylim = c(0.33,0.58),
  xlab = "Precentral (right)",
  ylab = "Precentral (left)")
text(x = y_lim[1] + 0.015, y = y_lim[2] - 0.01, labels = myText)
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(plot.df$Precentral_R, horizontal=TRUE, axes=FALSE, range = 0, ylim = y_lim)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(plot.df$Precentral_L, axes=FALSE, range = 0, ylim = y_lim)

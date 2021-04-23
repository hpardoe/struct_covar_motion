# plot correlation with marginal boxplots, ggplot version
# source("source_me_sd_pair_20210421.R")
library(ggplot2)
library(ggExtra)

margin_gap <- 15

myplot <- function(input_df = n0.df) {
  mycor = cor(x = input_df$Pallidum_L, y = input_df$Temporal_Inf_L)
  myText <- paste("r = ", round(mycor,2))
  nMotion <- length(which(input_df$run == "run-02"))
  myMotionText <- paste("Num. motion affected = ", nMotion)
  p <- ggplot(input_df, aes(x = Pallidum_L, y = Temporal_Inf_L, color = run)) +
    geom_point(size = 5, show.legend = FALSE) +
    #scale_x_continuous(breaks = seq(from = 0.45, to = 0.7, by = 0.05)) +
    #scale_y_continuous(breaks = seq(from = 0.45, to = 0.7, by = 0.05)) +
    theme_bw() +
    theme(
      aspect.ratio = 1,
      axis.title.x = element_text(margin=margin(margin_gap,0,0,0)),
      axis.title.y = element_text(margin=margin(0,margin_gap,0,0)),
      text = element_text(size=14)
      #legend.position = c(0.1,0.9),
      #legend.title = element_blank(),
      #legend.box.background = element_rect(colour = "black")
    ) +
    scale_color_brewer(palette="Dark2") +
    xlab("Pallidum (Left)") +
    ylab("Inferior Temporal Gyrus (Left)") +
    xlim(0.45,0.7) +
    ylim(0.45,0.7)

  # p1 <- ggMarginal(p, type="histogram", size=10)
  #p2 <- ggMarginal(p, type="boxplot", size=9, coef = 4)
  p_text <- p + annotate(geom = "text", x = 0.7, y = 0.48, label = myText, size = 5, hjust = 1) +
    annotate(geom = "text", x = 0.7, y = 0.46, label = myMotionText, size = 5, hjust = 1)
  p2 <- ggMarginal(p_text, type="boxplot", size=9, coef = 4)
  p2
}

set.seed(2027)
n0.df <- make_motion_affected_df(0)
n3.df <- make_motion_affected_df(3)
n6.df <- makeMotionDf(n3.df, 6)

#myplot(n0.df)

ggsave(file = "n0.png", myplot(n0.df), width = 4, height = 4)
ggsave(file = "n3.png", myplot(n3.df), width = 4, height = 4)
ggsave(file = "n6.png", myplot(n6.df), width = 4, height = 4)

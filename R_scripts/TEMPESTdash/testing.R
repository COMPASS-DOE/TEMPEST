
PLOT <- "Salt"

library(tibble)


north_degrees <- c("Control" = 0, "Salt" = -10, "Fresh" = 10)
lower_left <- c("Control" = "A1", "Salt" = "J1", "Fresh" = "A1")

plot_dat <- expand.grid(plot = PLOT,
                   x = as.factor(LETTERS[1:10]),
                   y = as.factor(1:8))


# If A1 is lower left, OK; otherwise reverse axis
if(substr(lower_left[PLOT], 1, 1) != "A") {
    plot_dat$x <- factor(plot_dat$x, levels = rev(levels(plot_dat$x)))
}

library(ggplot2)

p <- ggplot(plot_dat, aes(x, y)) + ggtitle(PLOT) +
    theme(panel.grid = element_blank())

# Grid lines BETWEEN axis labels
#p <- p + geom_hline(yintercept = as.numeric(plot_dat$x) - 0.5, color = "white")
#p <- p + geom_vline(xintercept = 1:10 - 0.5, color = "white")

# Add a north pointer
nd <- north_degrees[PLOT]
dy <- cos(pi * nd/90/2)
dx <- sin(pi * nd/90/2)

x <- 2
y <- 6
len <- 1
col <- "red"
p <- p + annotate("point", x = x, y = y, size = 4, color = col)
p <- p + annotate("segment", x = x, y = y, xend = x + dx * len, yend = y + dy * len,
                  color = col, arrow = arrow(), linewidth = 2)
p <- p + annotate("text", label = "North", x = x + dx * 1.25, y = y + dy * 1.25,
                  color = col, fontface = "bold", size = 6)

print(p)

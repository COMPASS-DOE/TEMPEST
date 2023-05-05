
PLOT <- "Fresh"

library(tibble)


plot_info <- tribble(
    ~plot,     ~lower_left, ~upper_left, ~north_degrees, ~color,
    "Control", "A8",        "A1",         -50,            "green",
    "Fresh",   "A8",        "A1",         -20,            "blue",
    "Salt",    "J1",        "A1",         -25,            "red"
)

plot_dat <- expand.grid(plot = PLOT,
                   x = as.factor(LETTERS[1:10]),
                   y = as.factor(1:8))


pinfo <- plot_info[plot_info$plot == PLOT,]

# If A is not lower left reverse x factor levels
if(substr(pinfo$lower_left, 1, 1) != "A") {
     plot_dat$x <- factor(plot_dat$x, levels = rev(levels(plot_dat$x)))
}
# If A is not lower left reverse y factor levels
if(substr(pinfo$lower_left, 2, 2) != "1") {
    plot_dat$y <- factor(plot_dat$y, levels = rev(levels(plot_dat$y)))
}

library(ggplot2)
theme_set(theme_bw())

p <- ggplot(plot_dat, aes(x, y)) + ggtitle(PLOT) + geom_point(color = "white")
   # theme(panel.grid = element_blank())


# If same letter not in lower left and upper left, flip axes
if(substr(pinfo$lower_left, 1, 1) != substr(pinfo$upper_left, 1, 1)) {
    p <- p + coord_flip()
}


# Grid lines BETWEEN axis labels
#p <- p + geom_hline(yintercept = as.numeric(plot_dat$x) - 0.5, color = "white")
#p <- p + geom_vline(xintercept = 1:10 - 0.5, color = "white")


library(magick)
library(cowplot)
rose_file <- "TEMPESTdash/plot_plots/compass-rose.png"

img <- magick::image_rotate(
    magick::image_read(rose_file),
    degrees = pinfo$north_degrees
)

img <- magick::image_transparent(img, color = "white")

p <- p + draw_image(img, x = 5, y = 4, scale = 8) # centered, easy

print(p)

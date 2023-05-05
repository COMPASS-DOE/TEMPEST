
PLOT <- "Salt"

library(tibble)

# The TEMPEST plots are all oriented slightly differently, and have different
# layouts in terms of how the A-J and 1-8 axes are set up, orientation wrt
# magnetic north, and color of stakes:
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
# If 1 is not lower left reverse y factor levels
if(substr(pinfo$lower_left, 2, 2) != "1") {
    plot_dat$y <- factor(plot_dat$y, levels = rev(levels(plot_dat$y)))
}

library(ggplot2)
theme_set(theme_bw())

p <- ggplot(plot_dat, aes(x, y)) + ggtitle(PLOT) + geom_point(color = "white") +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 16),
          plot.title = element_text(face = "bold", size = 16))

# If same letter not in lower left and upper left, flip axes
if(substr(pinfo$lower_left, 1, 1) != substr(pinfo$upper_left, 1, 1)) {
    p <- p + coord_flip()
}

# Add a compass rose, rotated correctly to show magnetic north
library(magick)
library(cowplot)
rose_file <- "TEMPESTdash/plot_plots/compass-rose.png"

# Rotate the image and make its background transparent
img <- magick::image_rotate(
    magick::image_read(rose_file),
    degrees = pinfo$north_degrees
)

# Draw into plot
img <- magick::image_transparent(img, color = "white")
p <- p + draw_image(img, x = 5, y = 4, scale = 8) # centered, easy

print(p)

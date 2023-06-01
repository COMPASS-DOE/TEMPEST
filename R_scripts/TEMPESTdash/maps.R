
library(tibble)

# The TEMPEST plots are all oriented slightly differently, and have different
# layouts in terms of how the A-J and 1-8 axes are set up, orientation wrt
# magnetic north, and color of stakes:
plot_info <- tribble(
    ~plot,        ~inventory_name, ~lower_left, ~upper_left, ~north_degrees, ~color,
    "Control",    "Control",       "A8",        "A1",         -50,           "green",
    "Freshwater", "Fresh",         "A8",        "A1",         -20,           "blue",
    "Seawater",   "Salt",          "J1",        "A1",         -25,           "red"
)

# Tree data - read it only once
readr::read_csv("../../Data/tree_inventory/inventory.csv") %>%
    filter(In_Plot, Status_2023 %in% c("LI", "DS")) %>%
    select(Plot, Grid, Species_code, Tag, DBH_2023) %>%
    mutate(x = substr(Grid, 1, 1), y = substr(Grid, 2, 2)) ->
    map_tree_data

# Main plotting function
make_plot_map <- function(STATUS_MAP, data_map_variable,
                          plot_name,
                          map_rose,
                          map_items,
                          sapflow_data,
                          sapflow_bad_sensors,
                          teros_data, # TEROS data loaded by the reactive d.f.
                          teros_bad_sensors,
                          aquatroll_data,
                          aquatroll_bad_sensors
) {
    show_rose <- map_rose
    show_trees <- "map_trees" %in% map_items
    show_teros <- "map_teros" %in% map_items
    show_sapflow <- "map_sapflow" %in% map_items
    show_aquatroll <- "map_aquatroll" %in% map_items

    # Construct plotting grid, flipping things around as needed
    plot_dat <- expand.grid(plot = plot_name,
                            x = as.factor(LETTERS[1:10]),
                            y = as.factor(1:8))

    pinfo <- plot_info[plot_info$plot == plot_name,]

    # If A is not lower left reverse x factor levels
    if(substr(pinfo$lower_left, 1, 1) != "A") {
        plot_dat$x <- factor(plot_dat$x, levels = rev(levels(plot_dat$x)))
    }
    # If 1 is not lower left reverse y factor levels
    if(substr(pinfo$lower_left, 2, 2) != "1") {
        plot_dat$y <- factor(plot_dat$y, levels = rev(levels(plot_dat$y)))
    }

    # Set up initial plot
    library(ggplot2)
    p <- ggplot(plot_dat, aes(x, y)) + ggtitle(plot_name) + geom_point(color = "white") +
        theme_bw() +
        theme(axis.title = element_blank(),
              axis.text = element_text(face = "bold", size = 16),
              plot.title = element_text(face = "bold", size = 16))

    # If same letter not in lower left and upper left, flip axes
    if(substr(pinfo$lower_left, 1, 1) != substr(pinfo$upper_left, 1, 1)) {
        p <- p + coord_flip()
    }

    if(show_rose) {
        # Add a compass rose, rotated correctly to show magnetic north
        library(magick)
        library(cowplot)
        rose_file <- "map_data/compass-rose.png"

        # Rotate the image and make its background transparent
        img <- magick::image_rotate(
            magick::image_read(rose_file),
            degrees = pinfo$north_degrees
        )

        # Draw into plot
        img <- magick::image_transparent(img, color = "white")
        p <- p + cowplot::draw_image(img, x = 5, y = 4, scale = 8) # centered, easy
    }

    if(show_teros && STATUS_MAP) {
        # Filter the sensors and bad sensors for the current plot and visualize
        teros_bad_sensors %>%
            filter(Plot == plot_name) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            tbs
        teros_data %>%
            distinct(Plot, ID, Grid_Square) %>%
            filter(Plot == plot_name, !ID %in% tbs$ID) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            td

        p <- p + geom_text(data = td,
                           position = position_jitter(seed = 1234),
                           aes(label = ID), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        p <- p + geom_label(data = tbs,
                            position = position_jitter(seed = 1234),
                            aes(label = ID), color = "red", fontface = "bold")
    }

    if(show_teros && !STATUS_MAP) {
        teros_data %>%
            # These need to be two separate filter steps, because timestamps can vary between plots
            filter(Plot == plot_name) %>%
            filter(Timestamp == max(Timestamp)) %>%
            filter(variable == data_map_variable) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            td
        p <- p + ggtitle(paste(plot_name, unique(td$Timestamp)))
        p <- p + geom_point(data = td,
                            position = position_jitter(seed = 1234),
                            aes(color = value, shape = variable), size = 6) +
            facet_wrap(~variable)
    }

    if(show_sapflow && STATUS_MAP) {
        # Trees
        inv <- filter(map_tree_data, Plot == pinfo$inventory_name)

        # Note to self: ideally we would merge the sapflow data with tree inventory
        # data, so as to plot trees at the same time. But I'm not sure where the
        # sapflow ID -> tree tag mapping is

        sapflow_bad_sensors %>%
            filter(Plot == plot_name) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            sbs
        sapflow_data %>%
            distinct(Plot, Tree_Code, Grid_Square) %>%
            filter(Plot == plot_name, !Tree_Code %in% sbs$Tree_Code) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            sd
        p <- p + geom_text(data = sd,
                           position = position_jitter(seed = 1234),
                           aes(label = Tree_Code), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        p <- p + geom_label(data = sbs,
                            position = position_jitter(seed = 1234),
                            aes(label = Tree_Code), color = "red", fontface = "bold")

    }

    if(show_aquatroll && STATUS_MAP) {
        # Filter the sensors and bad sensors for the current plot and visualize
        # aquatroll_data %>%
        #     distinct(Plot, ID, Grid_Square) %>%
        #     filter(Plot == plot_name) %>%
        #     filter(!ID %in% tbs$ID) %>%
        #     mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
        #     ad
        #
        # p <- p + geom_text(data = ad,
        #                    position = position_jitter(seed = 1234),
        #                    aes(label = ID), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        if(nrow(aquatroll_bad_sensors)) {
            # aquatroll_bad_sensors %>%
            #     filter(Plot == plot_name) %>%
            #     mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            #     abs
            # p <- p + geom_label(data = abs,
            #                     position = position_jitter(seed = 1234),
            #                     aes(label = ID), color = "red", fontface = "bold")
        }

    }

    if(show_trees) {
        # Trees
        inv <- filter(map_tree_data, Plot == pinfo$inventory_name)

        p <- p + geom_point(data = inv,
                            position = position_jitter(seed = 1234),
                            na.rm = TRUE,
                            aes(color = Species_code, size = DBH_2023), pch = 1) +
            guides(size = "none")
    }

    p
}

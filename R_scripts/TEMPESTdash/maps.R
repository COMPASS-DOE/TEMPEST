
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
readr::read_csv("inventory copy.csv") %>%
    filter(In_Plot, Status_2023 %in% c("LI", "DS")) %>%
    select(Plot, Grid, Species_code, Tag, DBH_2023) %>%
    mutate(x = substr(Grid, 1, 1), y = substr(Grid, 2, 2)) ->
    map_tree_data

# Mapping from sapflow to trees
sapflow_inv <- readr::read_csv("sapflow_inventory copy.csv") %>%
    select(Tree_Code, Tag)

# Do the compass rose transparency and rotation calculations (plot-specific) once and store
library(magick)
library(cowplot)
rose_dat <- magick::image_read("map_data/compass-rose.png")
roses <- list()
for(i in seq_len(nrow(plot_info))) {
    img <- magick::image_rotate(rose_dat,
                                degrees = plot_info$north_degrees[i]
    )
    roses[[plot_info$plot[i]]] <- magick::image_transparent(img, color = "white")
}

# Main plotting function
make_plot_map <- function(STATUS_MAP, data_map_variable, teros_depth,
                          plot_name,
                          map_overlays,
                          map_items,
                          sapflow_data,
                          sapflow_bad_sensors,
                          teros_data, # TEROS data loaded by the reactive d.f.
                          teros_bad_sensors,
                          aquatroll_data,
                          aquatroll_bad_sensors) {

    show_rose <- "map_rose" %in% map_overlays
    show_trees <- "map_trees" %in% map_overlays
    show_teros <- "map_teros" %in% map_items
    show_sapflow <- "map_sapflow" %in% map_items

    # Current time
    current_time <- with_tz(Sys.time(), tzone = "EST")

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

    # Overlays

    if(show_rose) {
        # Draw into plot
        p <- p + cowplot::draw_image(roses[[plot_name]], x = 5, y = 4, scale = 8) # centered, easy
    }

    inv <- filter(map_tree_data, Plot == pinfo$inventory_name)
    if(show_trees) {
        # Trees
        p <- p + geom_point(data = inv,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(size = DBH_2023), pch = 1) +
            guides(size = "none")
    }

    # Data layers

    # ------ TEROS status map ------

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
                           na.rm = TRUE,
                           position = position_jitter(seed = 1234),
                           aes(label = ID), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        p <- p + geom_label(data = tbs,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(label = ID), color = "red", fontface = "bold")
    }

    # ------ TEROS values map ------

    if(show_teros && !STATUS_MAP) {
        # If user has selected a particular depth, filter to that
        if(teros_depth != "All") {
            teros_data <- filter(teros_data, Depth == as.numeric(teros_depth))
            teros_bad_sensors <- filter(teros_bad_sensors, Depth == as.numeric(teros_depth))
        }

        teros_data %>%
            # Isolate plot and variable user wants, and then all data in last hour
            filter(Plot == plot_name, variable == data_map_variable) %>%
            filter(Timestamp >= current_time - 1 * 60 * 60) %>%
            # For each sensor, get its last timestamp of data
            group_by(ID) %>%
            filter(Timestamp == max(Timestamp)) %>%
            select(Plot, ID, Logger, Grid_Square, variable, value) ->
            td_with_data
        teros_bad_sensors %>%
            # Bad sensors: isolate to plot and variable...
            filter(Plot == plot_name, variable == data_map_variable,
                   # ...and IDs not in our good sensor list
                   !ID %in% td_with_data$ID) %>%
            bind_rows(td_with_data) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            td

        p <- p + ggtitle(paste(plot_name, current_time))
        p <- p + geom_point(data = td,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(color = value, shape = variable), size = 6) +
            facet_wrap(~variable)
    }

    # ------ Sapflow status map ------

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
            sdat
        p <- p + geom_text(data = sdat,
                           na.rm = TRUE,
                           position = position_jitter(seed = 1234),
                           aes(label = Tree_Code), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        p <- p + geom_label(data = sbs,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(label = Tree_Code), color = "red", fontface = "bold")

    }

    # ------ Sapflow values map ------

    if(show_sapflow && !STATUS_MAP) {

        sapflow_data %>%
            # Isolate plot and variable user wants, and then all data in last hour
            filter(Plot == plot_name) %>%
            filter(Timestamp >= current_time - 1 * 60 * 60) %>%
            # For each sensor, get its last timestamp of data
            group_by(Tree_Code) %>%
            filter(Timestamp == max(Timestamp)) %>%
            filter(is.finite(Value)) %>%
            select(Plot, Tree_Code, Logger, Grid_Square, Value) ->
            sd_with_data
        sapflow_bad_sensors %>%
            # Bad sensors: isolate to plot and IDs not in our good sensor list
            filter(Plot == plot_name, !Tree_Code %in% sd_with_data$Tree_Code) %>%
            bind_rows(sd_with_data) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            sd_all

        # ...and join with tree-code-to-tag mapping, and then to inventory data
        # inv %>%
        #     left_join(sapflow_inv, by = "Tag") %>%
        #     left_join(sdat, by = "Tree_Code") ->
        #     sdat
        p <- p + ggtitle(paste(plot_name, current_time))
        p <- p + geom_point(data = sd_all,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(color = Value), size = 6)
    }

    p
}

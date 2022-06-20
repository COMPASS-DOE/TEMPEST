library(dplyr)

# These RANGE variables are passed to flag_sensors() by the server, and used
# to identify sensors out of range

# For TEROS, these are all 1%/99% quantiles of test data
# This one is a tibble because TEROS is three variables in a single dataset
TEROS_RANGE <- tribble(~variable, ~low, ~high,
                       "EC",      16,   225,
                       "TSOIL",   15,   18,
                       "VWC",     2538, 2891)
SAPFLOW_RANGE <- c(0.4, 0.7) # roughly the 10%/90% quantiles of test data
VOLTAGE_RANGE <- c(12.5, 14.3) # roughly 0.05%/99.5% quantiles of test data
AQUATROLL_TEMP_RANGE <- c(6.3, 14.6) # roughly 1%/99% quantiles of test data

# Badge colors and 'trigger' values
# Currently green-yellow-red; could have more colors if desired
BADGE_COLORS <- c("green" = 0.0,    # green starts at 0% fail (this shouldn't change)
                  "yellow" = 0.05,  # yellow starts at 5% fail
                  "red" = 0.2)      # red starts at 20%

# Compute badge color(s) based on fraction(s) out
badge_color <- function(frac_out, badge_colors = BADGE_COLORS) {
    if(badge_colors[1] != 0.0) {
        stop("The first entry in badge_colors must be zero")
    }

    colors <- cut(frac_out,
                  c(BADGE_COLORS, 1),
                  labels = names(BADGE_COLORS),
                  right = FALSE)
    as.character(colors)
}

# Compute fraction (0-1) of values outside specified limits
# By default, NA counts as a failure
frac_outside_limits <- function(values, left_limit, right_limit, na.rm = FALSE) {
    if(na.rm) values <- na.omit(values)

    # In this calculation, NAs count as out-of-bounds
    sum(is.na(values) | !between(values, left_limit, right_limit)) / length(values)
}

# Return both fraction_out and associated badge color for a vector of
# values and associated limits
flag_sensors <- function(values, limits, na.rm = FALSE) {
    frac_out <- frac_outside_limits(values, min(limits), max(limits), na.rm = na.rm)
    x <- tibble(n = length(values),
                fraction_in = 1- frac_out,
                percent_in = paste0(round(fraction_in * 100, 0), "%"),
                color = badge_color(frac_out))
    # 'NaN' entries usually mean no data
    invalids <- is.nan(x$fraction_in)
    x$percent_in[invalids] <- "--"
    x$color[invalids] <- "black"
    x
}

# # Test code for flag_sensors above
# # Basic test data
# message("Basic test - grouped data")
# test <- tibble(plot = rep(1:3, each = 10),
#                values = c(1:10,         # plot 1
#                           3:7, 2:6,     # plot 2
#                           rep(4:5, 5))) # plot 3
# test %>%
#     group_by(plot) %>%
#     summarise(flag_sensors(values, limits = c(3,7))) %>%
#     print()
#
# # We can count NAs as errors, or not
# message("Basic test - NAs are errors")
# test2 <- tibble(values = c(3:4, NA, 6:7))
# test2 %>%
#     summarise(flag_sensors(values, limits = c(3,7))) %>%
#     print()
# message("Basic test - NAs are not errors")
# test2 %>%
#     summarise(flag_sensors(values, limits = c(3,7), na.rm = TRUE)) %>%
#     print()

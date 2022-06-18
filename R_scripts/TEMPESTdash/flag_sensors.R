library(dplyr)

# To be filled in based on real data
# We could provide defaults, but let the users adjust if they want; bonus time
# For TEROS, these are all 1%/99% quantiles of test data
TEROS_RANGE <- tribble(~variable, ~low, ~high,
                       "EC",      16,   225,
                       "TSOIL",   15,   18,
                       "VWC",     2538, 2891)
SAPFLOW_RANGE <- c(0.4, 0.7) # roughly the 10%/90% quantiles of test data
VOLTAGE_RANGE <- c(12.5, 14.3) # roughly 0.95%/99.5% quantiles of test data

# Green-yellow-red levels; could have more colors if desired
BADGE_COLORS <- c("green" = 0.0,    # green starts at 0% fail (this shouldn't change)
                  "yellow" = 0.05,  # yellow starts at 5% fail
                  "red" = 0.2)      # red starts at 20%

# Compute badge color(s) based on fraction(s) out
badge_color <- function(frac_out) {
    if(BADGE_COLORS[1] != 0.0) {
        stop("The first entry in BADGE_COLORS must be zero")
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
    tibble(n = length(values),
           fraction_in = 1- frac_out,
           percent_in = paste0(round(fraction_in * 100, 0), "%"),
           color = badge_color(frac_out))
}

#
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

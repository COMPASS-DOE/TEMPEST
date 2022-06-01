# Functions for the Teros data dashboard

# sapflow %>%
#   group_by(Plot, Date, Tree_Code, Port) %>%
#   mutate(in_range = between(Value, min(VOLTAGE_RANGE), max(VOLTAGE_RANGE))) ->
#   daily_range_test
#
# # Count flags and store results
# daily_range_test %>%
#   group_by(Plot, Tree_Code, Port) %>%
#   summarise(failures = sum(!in_range, na.rm = TRUE), .groups = "drop") ->
#   DAILY_TESTS[["ERR.range"]] # Store results in list

test <- c(2,3,4,10)

# Function that returns the number of out of bounds values for a given bound
teros_bounds <- function(values, bounds) {
    # 'values' is a numeric vector of
    # 'bounds' is a two value numeric vector

    sum(!between(values, min(bounds), max(bounds)))
}

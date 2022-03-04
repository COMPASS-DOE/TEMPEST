# inventory.r
# QA/QC and summarize the TEMPEST inventory data
# BBL March 22022

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_minimal())

inv <- read_csv("../Data/tree_inventory/inventory.csv")

inv <- filter(inv, In_Plot)

# Helper function: pivot_longer and separate {prefix}_{year}
pivot_and_split <- function(df, col_prefix) {
    stopifnot(any(grepl(col_prefix, colnames(df)))) # not present
    newcol <- gsub("_", "", col_prefix)
    df %>%
        select(Plot, Section, Tag, Grid, Species_code, starts_with(col_prefix)) %>%
        pivot_longer(cols = starts_with(col_prefix), values_to = newcol) %>%
        separate(name, into = c("x", "Year")) %>%
        select(-x)
}

inv_long_dbh <- pivot_and_split(inv, "DBH_")
inv_long_date <- pivot_and_split(inv, "Date_")

inv %>%
    pivot_and_split("Status_") %>%
    left_join(inv_long_dbh, by = c("Plot", "Section", "Tag", "Grid", "Species_code", "Year")) %>%
    left_join(inv_long_date, by = c("Plot", "Section", "Tag", "Grid", "Species_code", "Year")) %>%
    mutate(Year = as.integer(Year)) %>%
    arrange(Plot, Tag, Year) ->
    inv_long

# Live but unknown species report
inv_long %>%
    filter(Status == "LI", is.na(Species_code)) %>%
    write_csv("~/Desktop/Live_unknown_species.csv")

# Number of trees
inv_long %>%
    group_by(Plot, Year, Status) %>%
    summarise(n = n(), .groups = "drop") %>%
    ggplot(aes(Year, n, color = Plot, group = Plot)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Status, scales = "free")

# Live trees species mix
inv_long %>%
    filter(Status == "LI") %>%
    group_by(Plot, Year, Species_code) %>%
    summarise(n = n(),
              BA = sum(((DBH / 100) / 2) ^ 2 * pi, na.rm = TRUE) * 10000 / (50 * 40),
              .groups = "drop") ->
    species_mix

# Live trees DBH
ggplot(species_mix, aes(Year, BA, fill = Species_code)) +
    geom_area(position="stack") +
    facet_grid(Plot~.)

species_mix %>%
    group_by(Plot, Year) %>%
    summarise(n = sum(n),
              BA = sum(BA, na.rm = TRUE),
              .groups = "drop") ->
    plot_totals

ggplot(plot_totals, aes(Year, BA, color = Plot, group = Plot)) + geom_point() + geom_line()

ggplot(plot_totals, aes(Year, n, color = Plot, group = Plot)) + geom_point() + geom_line()



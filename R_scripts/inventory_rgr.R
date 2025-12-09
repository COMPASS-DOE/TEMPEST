# inventory_rgr.R
# Quickly compute and visualize relative growth rate (RGR)
# based on the annual tree inventory data
# BBL December 2025

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

inv <- read_csv("Data/tree_inventory/inventory.csv")

dbh_columns <- grep("DBH_[0-9]{4}", colnames(inv))
dbh_years <- sort(as.integer(gsub("DBH_", "", colnames(inv)[dbh_columns])))

# Work through the years, column by column, and compute RGR
for(yr in dbh_years[-1]) {
    message(yr)

    # Pull tree status
    status_name <- paste0("Status_", yr)
    live_trees <- inv[status_name] %>% pull() == "LI"
    live_trees[is.na(live_trees)] <- FALSE
    message("\tTrees alive: ", sum(live_trees))

    # Compute delta DBH
    dbh_name <- paste0("DBH_", yr)
    delta_name <- paste0("DeltaDBH_", yr)
    inv[delta_name] <- inv[dbh_name] - inv[paste0("DBH_", yr-1)]
    rhr_name <- paste0("RGR_", yr)
    inv[rhr_name] <- inv[delta_name] / inv[dbh_name] * 100
}

# Separate RGR columns, reshape, and focus on the sapflow species
inv %>%
    select(Plot, Tree_ID, Species_code, starts_with("RGR_")) %>%
    pivot_longer(starts_with("RGR_"), values_to = "RGR") %>%
    replace_na(list(RGR = 0)) %>%
    separate(name, into = c("x", "Year"), convert = TRUE) %>%
    select(-x) %>%
    filter(Species_code %in% c("ACRU", "LITU", "FAGR")) ->
    inv_rgr

ggplot(inv_rgr, aes(Year, RGR, color = Plot, group = Tree_ID)) +
    geom_line(alpha = 0.25) +
    facet_grid(Species_code ~.) +
    ylim(c(-3, 3))
ggsave("~/Desktop/rgr_raw.png", width = 8, height = 6)


# Compute RGR relative to the control plot
inv_rgr %>%
    filter(Plot == "Control") %>%
    group_by(Species_code, Year) %>%
    summarise(RGR_control = median(RGR), .groups = "drop") %>%
    right_join(inv_rgr, by = c("Year", "Species_code")) %>%
    mutate(delta_RGR = RGR - RGR_control) %>%
    filter(Plot != "Control") ->
    inv_rgr_delta

ggplot(inv_rgr_delta, aes(factor(Year), delta_RGR, color = Plot)) +
    facet_grid(Species_code ~ .) +
    geom_boxplot() + geom_hline(yintercept = 0, linetype = 2) +
    ylim(c(-3, 3))

# Boil it down to a single point per species, plot, and year
inv_rgr_delta %>%
   #filter(RGR >= -0.1) %>%
    group_by(Plot, Species_code, Year) %>%
    summarise(delta_RGR_err = sd(delta_RGR),
              delta_RGR = mean(delta_RGR), .groups = "drop") %>%
    ggplot(aes(Year, delta_RGR, group = Plot, color = Plot)) +
    geom_point(position = position_dodge(width = 0.2)) +
    facet_grid(Species_code ~ .) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_linerange(aes(ymin = delta_RGR - delta_RGR_err, ymax = delta_RGR + delta_RGR_err),
                   position = position_dodge(width = 0.2),
                   alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
    ylab("Relative growth rate (relative to control mean)") +
    coord_cartesian(ylim = c(-1, 1))

ggsave("~/Desktop/rgr.png", width = 8, height = 6)

message("All done")
